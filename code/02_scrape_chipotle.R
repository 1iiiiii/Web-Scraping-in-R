# 02_scrape_chipotle.R ---------------------------------------------------
library(rvest)
library(xml2)
library(tidyverse)

SITEMAPS <- c("https://locations.chipotle.com/sitemap1.xml",
              "https://locations.chipotle.com/sitemap2.xml")
UA       <- "Yi Li ericli815@outlook.com"
SNAPSHOT <- "2026-09-19"
RAW_DIR  <- file.path("data", "raw")
OUT_PATH <- file.path("data", "processed", "chipotle_stores.csv")

N_STORES_EXPECTED <- 4155   # distinct 3-segment store pages

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)

fetch_cached <- function(url) {
  path <- file.path(RAW_DIR, paste0("chipotle_", tools::file_path_sans_ext(basename(url)),
                                     "_", SNAPSHOT, ".xml"))
  if (!file.exists(path)) {
    message("fetching ", url)
    download.file(url, destfile = path, quiet = TRUE,
                  headers = c("User-Agent" = UA))
    Sys.sleep(1)
  }
  path
}

paths <- map_chr(SITEMAPS, fetch_cached)

# --- pull every <loc> -------------------------------------------------------
locs <- map(paths, \(p) {
  read_xml(p) |> xml_ns_strip() |> xml_find_all("//loc") |> xml_text()
}) |> list_c()

message(length(locs), " URLs across ", length(paths), " sitemaps")

# --- URL grammar ------------------------------------------------------------
PREFIX <- "https://locations.chipotle.com/"

stores <- tibble(url = locs) |>
  filter(!str_detect(url, "/order-delivery")) |>
  mutate(path = url |> str_remove(fixed(PREFIX)) |> str_remove("/$")) |>
  filter(path != "", !str_detect(path, "^404")) |>
  filter(str_count(path, "/") == 2) |>              
  separate_wider_delim(path, delim = "/",
                       names = c("state_slug", "city_slug", "street_slug")) |>
  distinct(state_slug, city_slug, street_slug, .keep_all = TRUE)

if (nrow(stores) != N_STORES_EXPECTED) {
  stop("expected ", N_STORES_EXPECTED, " store pages, found ", nrow(stores),
       "sitemaps changed: reconcile before using this and update the constant")
}

# --- normalise --------------------------------------------------------------
# match these against Census place names

stores <- stores |>
  mutate(
    state_abbr = str_to_upper(state_slug),
    city       = city_slug   |> str_replace_all("-", " ") |> str_to_title(),
    street     = street_slug |> str_replace_all("-", " ") |> str_to_title()
  ) |>
  select(state_abbr, city, street, city_slug, street_slug, url)

message(nrow(stores), " stores across ", n_distinct(stores$state_abbr), " states")

# --- reconciliation ---------------------------------------------------------
dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)
write_csv(stores, OUT_PATH)
