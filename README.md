# Web Scraping using R

Post: [https://1iiiiii.github.io/Personal-Website/blog/posts/post1/]

## The question

[WRITE - two or three sentences. Sweetgreen has a public target of 1,000 US
units. Does its own store footprint support that number, and what does
comparing it with Chipotle's footprint say about whether the target was ever
the right shape?]

## Data

| Source | What | Requests | Accessed |
|---|---|---|---|
| `sweetgreen.com/locations` | every US Sweetgreen location | 1 | 2026-09-19 |
| `locations.chipotle.com/sitemap{1,2}.xml` | every US Chipotle store URL | 2 | 2026-09-19 |
| Census ACS 5-year via `tidycensus` | CBSA population, income, education | — | 2019–2023 |
| Census TIGER via `tigris` | place, county-subdivision and CBSA geometry | — | 2023 vintage |

Three HTTP requests total. Both sites' `robots.txt` contain no `Disallow`
directives and publish a sitemap; no logins, paywalls or CAPTCHAs were
involved. Raw responses are committed under `data/raw/` so the analysis
reproduces without contacting either site again.

## Reproducing

```r
install.packages(c("tidyverse", "rvest", "xml2", "tidycensus",
                   "tigris", "sf", "broom"))
```

A Census API key is required and must be **activated** via the link in the
signup email. `Sys.getenv("CENSUS_API_KEY")` should return it.

```r
source("code/01_scrape_sweetgreen.R")   # -> data/processed/sweetgreen_stores.csv
source("code/02_scrape_chipotle.R")     # -> data/processed/chipotle_stores.csv
source("code/03_clean_and_match.R")     # -> data/processed/cbsa_panel.csv
source("code/04_analysis.R")            # -> results/figures/, results/tables/
```

`01` and `02` read from `data/raw/` when it is populated and only fetch when it
is not, so re-running them costs nothing and changes nothing. `03` downloads
Census geometry for 49 states on its first run and caches it
(`options(tigris_use_cache = TRUE)`).

Every script stops rather than warns when a count fails to reconcile, so a
silent partial parse cannot reach the analysis.

## Layout
```
blog-post-2/
├── README.md              replication instructions
├── code/
│   ├── 01_scrape_sweetgreen.R
│   ├── 02_scrape_chipotle.R
│   ├── 03_clean_and_match.R
│   └── 04_analysis.R
├── data/
│   ├── raw/               cached HTML + XML, timestamped, committed
│   └── processed/         tidy store table, CBSA panel
└── results/
    ├── figures/
    └── tables/
```
