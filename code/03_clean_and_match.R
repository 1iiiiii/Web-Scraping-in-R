# 03_clean_and_match.R ---------------------------------------------------
#   Sweetgreen -> full street address WITH zip
#   Chipotle   -> city + state, NO zip

library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)

options(tigris_use_cache = TRUE)

ACS_YEAR   <- 2023
TIGER_YEAR <- 2023

SG_PATH   <- file.path("data", "processed", "sweetgreen_stores.csv")
CMG_PATH  <- file.path("data", "processed", "chipotle_stores.csv")
OUT_PATH  <- file.path("data", "processed", "cbsa_panel.csv")
UNMATCHED <- file.path("results", "tables", "unmatched_places.csv")

sg  <- read_csv(SG_PATH,  show_col_types = FALSE)
cmg <- read_csv(CMG_PATH, show_col_types = FALSE)

# A rejected Census request comes back as an HTML error page, and tidycensus
# reports it as "lexical error: invalid char in json text"

# Check 1: whether it's the key's problem
key_ok <- tryCatch({
  invisible(get_acs(geography = "state", variables = "B01003_001",
                    year = ACS_YEAR, survey = "acs5", state = "DE"))
  TRUE
}, error = function(e) FALSE)

if (!key_ok) {
  stop("Census rejected a plain state-level call, so this is the KEY, not the\n",
       "  geography. Check that Sys.getenv(\"CENSUS_API_KEY\") holds the NEW key,\n",
       "  that you clicked the activation link in the signup email, and that\n",
       "  ~/.Renviron contains exactly one CENSUS_API_KEY line.")
}
message("census api key ok")

# Check 2: the CBSA GEOGRAPHY STRING.
# Census renamed this hierarchy around the 2020 vintage, and tidycensus's
# "cbsa" alias does not map to the new name in every package version

CBSA_NAMES <- c("cbsa",
                "metropolitan statistical area/micropolitan statistical area",
                "metropolitan/micropolitan statistical area")

cbsa_geography <- NULL
for (g in CBSA_NAMES) {
  probe <- tryCatch(
    suppressMessages(get_acs(geography = g, variables = "B01003_001",
                             year = ACS_YEAR, survey = "acs5")),
    error = function(e) NULL
  )
  if (!is.null(probe)) { cbsa_geography <- g; break }
}

if (is.null(cbsa_geography)) {
  stop("the key works but no CBSA geography string did, for ACS ", ACS_YEAR,
       ".\n  Tried: ", paste(CBSA_NAMES, collapse = " | "),
       "\n  Try update.packages(\"tidycensus\") -- this is usually a stale version.")
}
message("cbsa geography string: '", cbsa_geography, "'")

# --- city-name normalisation ------------------------------------------------
#   "Port St. Lucie"  (Sweetgreen)  -> "port st lucie"
#   "port-st-lucie"   (Chipotle)    -> "port st lucie"
#   "Port St. Lucie"  (TIGER NAME)  -> "port st lucie"

norm_city <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[-_]", " ") |>
    str_replace_all("\\bsaint\\b", "st") |>
    str_replace_all("\\bmount\\b", "mt") |>
    str_replace_all("\\bfort\\b", "ft") |>
    str_remove_all("[^a-z0-9 ]") |>      # periods, apostrophes, accents
    str_squish()
}

# --- geography layers -------------------------------------------------------
# Census PLACES alone leave an 8-10% hole, and the misses are not random b/c they
# concentrate in NY, MA, CT and NJ.
#
#   1. NYC boroughs are not places. Brooklyn and the Bronx do not exist in the
#      place file; the place is "New York". Dropped 84 Chipotle and 8
#      Sweetgreen stores in the largest market in the analysis.
#   2. New England "towns" are COUNTY SUBDIVISIONS, not places, so every
#      CT/MA/NH/RI store fell through. Boston is Sweetgreen's second market.
#   3. Consolidated city-counties carry administrative names: TIGER spells them
#      "Indianapolis city (balance)", "Nashville-Davidson metropolitan
#      government (balance)", "Lexington-Fayette urban county", "Boise City".

NEW_ENGLAND <- c("CT", "MA", "ME", "NH", "RI", "VT")

states_needed <- sort(union(sg$state_abbr, cmg$state_abbr))
message("fetching place geometry for ", length(states_needed), " states (slow on first run)")

geo_list <- map(states_needed, function(st) {
  places(state = st, cb = TRUE, year = TIGER_YEAR, progress_bar = FALSE) |>
    transmute(state_abbr = st, unit_name = NAME, unit_land = ALAND)
})

# add the town layer for New England only.
ne_needed <- intersect(states_needed, NEW_ENGLAND)
if (length(ne_needed) > 0) {
  message("adding county subdivisions for ", paste(ne_needed, collapse = ", "),
          " (New England towns are not Census places)")
  geo_list <- c(geo_list, map(ne_needed, function(st) {
    county_subdivisions(state = st, cb = TRUE, year = TIGER_YEAR,
                        progress_bar = FALSE) |>
      transmute(state_abbr = st, unit_name = NAME, unit_land = ALAND)
  }))
}

geo_sf <- do.call(rbind, geo_list)

places_pt <- suppressWarnings(
  geo_sf |> st_centroid() |> transmute(state_abbr, unit_name, unit_land)
)

# --- CBSA polygons ----------------------------------------------------------
cbsa_sf <- core_based_statistical_areas(cb = TRUE, year = TIGER_YEAR, progress_bar = FALSE) |>
  transmute(cbsa_geoid = GEOID,
            cbsa_name  = NAME,
            cbsa_land  = ALAND)

places_cbsa <- st_join(
  places_pt,
  st_transform(cbsa_sf, st_crs(places_pt)),
  join = st_within
) |>
  st_drop_geometry()

#   "Indianapolis city (balance)" -> indianapolis city balance
#                                 -> indianapolis city
#                                 -> indianapolis
#   "Darien town"                 -> darien town -> darien
#   "Boise City"                  -> boise city  -> boise

alt_keys <- function(nm) {
  k1 <- norm_city(nm)
  k2 <- str_squish(str_remove(k1, "\\s+balance$"))
  k3 <- str_squish(str_remove(k2, "\\s+(metropolitan government|metro government|urban county|consolidated government)$"))
  k4 <- str_squish(str_remove(k3, "\\s+(city|town|village|borough|township|municipality|cdp)$"))
  unique(c(k1, k2, k3, k4))
}

# rank 1 = the unit's own name, higher ranks = progressively stripped fallbacks
keyed <- places_cbsa |>
  mutate(key = map(unit_name, alt_keys)) |>
  unnest_longer(key, indices_to = "key_rank")

dupe_conflicts <- keyed |>
  filter(!is.na(cbsa_geoid), key_rank == 1) |>
  summarise(n_cbsa = n_distinct(cbsa_geoid), .by = c(state_abbr, key)) |>
  filter(n_cbsa > 1)

# Exact names win over stripped ones; within a rank, the larger unit wins.
lookup <- keyed |>
  arrange(state_abbr, key, key_rank, desc(unit_land)) |>
  distinct(state_abbr, key, .keep_all = TRUE) |>
  select(state_abbr, city_key = key, cbsa_geoid, cbsa_name)

message(nrow(lookup), " geography keys; ", nrow(dupe_conflicts),
        " exact keys had duplicates mapping to different CBSAs")

# --- cause 1: explicit aliases ----------------------------------------------
# Places that genuinely do not exist under the name the chain uses. This list is
# deliberately short, hand-checked and visible: every entry is a judgement you
# are making about geography, and it belongs in the post rather than buried.
#
# The boroughs are the important ones b/c without them New York is absent from the panel.

ALIASES <- tribble(
  ~state_abbr, ~from,            ~to,
  "NY",        "brooklyn",       "new york",
  "NY",        "bronx",          "new york",
  "NY",        "queens",         "new york",
  "NY",        "staten island",  "new york",
  "TN",        "nashville",      "nashville davidson",
  "KY",        "lexington",      "lexington fayette",
  "FL",        "lake worth",     "lake worth beach",   # renamed 2019
  "CA",        "northridge",     "los angeles"         # LA neighbourhood
)

apply_aliases <- function(df) {
  df |>
    left_join(ALIASES, by = c("state_abbr", "city_key" = "from")) |>
    mutate(city_key = coalesce(to, city_key)) |>
    select(-to)
}

# --- match both chains ------------------------------------------------------
sg_m <- sg |>
  mutate(city_key = norm_city(city)) |>
  apply_aliases() |>
  left_join(lookup, by = c("state_abbr", "city_key"))

cmg_m <- cmg |>
  mutate(city_key = norm_city(city)) |>
  apply_aliases() |>
  left_join(lookup, by = c("state_abbr", "city_key"))

report_match <- function(df, label) {
  matched <- sum(!is.na(df$cbsa_geoid))
  message(label, ": ", matched, "/", nrow(df), " matched to a CBSA (",
          round(100 * matched / nrow(df), 1), "%)")
  invisible(NULL)
}

report_match(sg_m,  "sweetgreen")
report_match(cmg_m, "chipotle")

unmatched <- bind_rows(
  sg_m  |> filter(is.na(cbsa_geoid)) |> transmute(chain = "sweetgreen", state_abbr, city, city_key),
  cmg_m |> filter(is.na(cbsa_geoid)) |> transmute(chain = "chipotle",   state_abbr, city, city_key)
) |>
  count(chain, state_abbr, city, city_key, sort = TRUE)

dir.create(dirname(UNMATCHED), recursive = TRUE, showWarnings = FALSE)
write_csv(unmatched, UNMATCHED)

if (nrow(unmatched) > 0) {
  top <- unmatched |> count(chain, state_abbr, wt = n, sort = TRUE) |> slice_head(n = 3)
  message("unmatched written to ", UNMATCHED,
          " -- most affected: ",
          paste(top$chain, top$state_abbr, top$n, collapse = "; "))
}

# --- aggregate to CBSA ------------------------------------------------------
sg_counts <- sg_m |>
  filter(!is.na(cbsa_geoid)) |>
  summarise(sg_stores      = sum(!coming_soon),
            sg_pipeline    = sum(coming_soon),
            .by = cbsa_geoid)

cmg_counts <- cmg_m |>
  filter(!is.na(cbsa_geoid)) |>
  count(cbsa_geoid, name = "cmg_stores")

# --- Census covariates ------------------------------------------------------
acs_vars <- c(pop       = "B01003_001",
              mhi       = "B19013_001",
              edu_total = "B15003_001",
              edu_ba    = "B15003_022",
              edu_ma    = "B15003_023",
              edu_prof  = "B15003_024",
              edu_phd   = "B15003_025")

acs <- get_acs(geography = cbsa_geography,
               variables = acs_vars,
               year      = ACS_YEAR,
               survey    = "acs5",
               output    = "wide")

# --- build the panel --------------------------------------------------------
panel <- acs |>
  transmute(
    cbsa_geoid       = GEOID,
    cbsa_name        = NAME,
    cbsa_type        = if_else(str_detect(NAME, "Micro Area$"), "micro", "metro"),
    population       = popE,
    median_hh_income = mhiE,
    ba_plus_share    = (edu_baE + edu_maE + edu_profE + edu_phdE) / edu_totalE
  ) |>
  left_join(st_drop_geometry(cbsa_sf) |> select(cbsa_geoid, cbsa_land),
            by = "cbsa_geoid") |>
  left_join(sg_counts,  by = "cbsa_geoid") |>
  left_join(cmg_counts, by = "cbsa_geoid") |>
  mutate(
    across(c(sg_stores, sg_pipeline, cmg_stores), \(x) replace_na(x, 0)),
    land_sq_mi = cbsa_land / 2589988.11,
    density    = population / land_sq_mi,
    sg_per_m   = sg_stores  / (population / 1e6),
    cmg_per_m  = cmg_stores / (population / 1e6)
  ) |>
  select(-cbsa_land) |>
  arrange(desc(population))

# --- reconciliation ---------------------------------------------------------
message("panel: ", nrow(panel), " CBSAs | ",
        sum(panel$sg_stores), " sweetgreen (of ", sum(!sg$coming_soon), " open) | ",
        sum(panel$cmg_stores), " chipotle (of ", nrow(cmg), ")")

message(sum(panel$sg_stores > 0), " CBSAs with sweetgreen | ",
        sum(panel$cmg_stores > 0), " with chipotle | ",
        sum(panel$sg_stores == 0 & panel$cmg_stores > 0),
        " with chipotle but no sweetgreen")

stopifnot(!any(is.na(panel$population)))

write_csv(panel, OUT_PATH)