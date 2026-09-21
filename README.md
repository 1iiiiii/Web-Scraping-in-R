# Web Scraping using R

Post: [https://1iiiiii.github.io/Personal-Website/blog/posts/post1/]

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


```r
source("code/01_scrape_sweetgreen.R")   # -> data/processed/sweetgreen_stores.csv
source("code/02_scrape_chipotle.R")     # -> data/processed/chipotle_stores.csv
source("code/03_clean_and_match.R")     # -> data/processed/cbsa_panel.csv
source("code/04_analysis.R")            # -> results/figures/, results/tables/
```

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
