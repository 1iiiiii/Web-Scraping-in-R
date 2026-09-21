# 01_scrape_sweetgreen.R -------------------------------------------------
library(rvest)
library(tidyverse)

URL      <- "https://www.sweetgreen.com/locations"
UA       <- "Yi Li ericli815@outlook.com"
SNAPSHOT <- "2026-09-19"  
RAW_PATH <- file.path("data", "raw", paste0("sweetgreen_", SNAPSHOT, ".html"))
OUT_PATH <- file.path("data", "processed", "sweetgreen_stores.csv")

N_STATES_EXPECTED <- 27
N_CARDS_EXPECTED  <- 299

# --- fetch once and store in cache -------------------------------
dir.create(dirname(RAW_PATH), recursive = TRUE, showWarnings = FALSE)

if (!file.exists(RAW_PATH)) {
  message("fetching ", URL)
  download.file(URL, destfile = RAW_PATH, quiet = TRUE,
                headers = c("User-Agent" = UA))
  Sys.sleep(1)
} else {
  message("using cached ", RAW_PATH)
}

page <- read_html(RAW_PATH)

# --- verified selectors -----------------------------------------------------
SEL_STATE_BLOCK <- "li.page-locations__location-list-item"
SEL_STATE_TITLE <- "h2.page-locations__location-list-item-title"  # "California (60)"
SEL_CARD        <- "article.page-locations-location-card"
SEL_NAME        <- "h3.page-locations-location-card-title"
SEL_ADDRESS     <- "address.page-locations-location-card-address"
SEL_HOURS       <- "div.page-locations-location-card__entry--hours"

blocks <- html_elements(page, SEL_STATE_BLOCK)

if (length(blocks) != N_STATES_EXPECTED) {
  stop("expected ", N_STATES_EXPECTED, " state blocks, found ", length(blocks),
       " -- the page changed: re-check the selectors and update this constant")
}

# --- parse ------------------------------------------------------------------
stores_raw <- map(blocks, function(block) {

  title <- html_element(block, SEL_STATE_TITLE) |> html_text2() |> str_squish()

  m        <- str_match(title, "^(.*?)\\s*\\((\\d+)\\)$")
  state_nm <- m[, 2]
  state_n  <- as.integer(m[, 3])

  if (is.na(state_nm)) stop("could not parse state heading: '", title, "'")

  cards <- html_elements(block, SEL_CARD)

  if (length(cards) != state_n) {
    stop(state_nm, ": parsed ", length(cards),
         " cards but the page says ", state_n)
  }

  tibble(
    state_name  = state_nm,
    name        = html_element(cards, SEL_NAME)    |> html_text2() |> str_squish(),
    address_raw = html_element(cards, SEL_ADDRESS) |> html_text2() |> str_trim(),
    hours       = html_element(cards, SEL_HOURS)   |> html_text2() |> str_squish(),
    card_text   = html_text2(cards)                |> str_squish()
  )

}) |> list_rbind()

stopifnot(nrow(stores_raw) == N_CARDS_EXPECTED)

# --- split the address ------------------------------------------------------
#   line 1:  "1701 Southeast 8th Street #11"
#   line 2:  "Bentonville, AR 72712"

stores_raw <- stores_raw |>
  mutate(
    street   = str_squish(str_extract(address_raw, "^[^\n]*")),
    locality = str_squish(str_replace(address_raw, "^[^\n]*\n", ""))
  )

# city        state        zip
ADDR_RE <- "^([^,]+),\\s*([A-Z]{2})\\s+(\\d{4,5})$"

am <- str_match(stores_raw$locality, ADDR_RE)

if (anyNA(am[, 1])) {
  bad <- which(is.na(am[, 1]))
  stop(length(bad), " addresses did not match ADDR_RE. First: '",
       stores_raw$locality[bad[1]], "'")
}

stores <- stores_raw |>
  mutate(
    city        = am[, 2],
    state_abbr  = am[, 3],
    zip         = str_pad(am[, 4], width = 5, side = "left", pad = "0"),
    coming_soon = str_detect(card_text, regex("coming soon", ignore_case = TRUE))
  ) |>
  select(state_name, state_abbr, city, name, street, zip, hours, coming_soon)

# --- the trap ---------------------------------------------------------------
stopifnot(all(nchar(stores$zip) == 5))

message(nrow(stores), " stores | ",
        sum(stores$coming_soon), " coming soon | ",
        sum(!stores$coming_soon), " open | ",
        n_distinct(stores$state_abbr), " states")

dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)
write_csv(stores, OUT_PATH)
