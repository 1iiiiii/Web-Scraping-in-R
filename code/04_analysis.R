# 04_analysis.R ----------------------------------------------------------
# The market sizing
#   1. penetration, both chains, by metro
#   2. the naive build -- apply Chipotle's national density to every metro
#   3. the test       -- is Sweetgreen's penetration income/density elastic?
#   4. the same spec fitted to Chipotle as the control

library(tidyverse)
library(broom)

PANEL   <- file.path("data", "processed", "cbsa_panel.csv")
FIG_DIR <- file.path("results", "figures")
TAB_DIR <- file.path("results", "tables")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

SG_TARGET <- 1000   # Sweetgreen's stated long-term unit target (by 2030)

# Okabe-Ito. Colourblind-safe by construction, which a green/red pair is not.
COL_SG  <- "#0072B2"
COL_CMG <- "#D55E00"

theme_post <- function() {
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25, colour = "grey88"),
      axis.title       = element_text(colour = "grey30"),
      plot.title       = element_text(face = "bold", size = 12),
      plot.caption     = element_text(colour = "grey45", hjust = 0),
      legend.position  = "top",
      legend.title     = element_blank()
    )
}

SRC <- "Source: company store locators, scraped 2026-09-19; ACS 2019-2023 5-year."

panel <- read_csv(PANEL, show_col_types = FALSE)

# Micropolitan areas stay in the file but out of the model. Sweetgreen has
# never opened in one, so they carry no information about where it can go, and
# including 550 structural zeros would drag every coefficient toward zero.
# Puerto Rico is dropped. Neither chain operates in any of its six metros, and
# with a wrong-signed income coefficient the model reads PR's low median income
# as HIGH predicted density
metros <- panel |>
  filter(cbsa_type == "metro", population > 0, is.finite(density),
         !is.na(median_hh_income),
         !str_detect(cbsa_name, ", PR "))

# ============================================================================
# 1. PENETRATION
# ============================================================================

top_metros <- metros |>
  slice_max(population, n = 25) |>
  mutate(short_name = str_remove(cbsa_name, ",.*$") |> str_remove("-.*$"))

pen_long <- top_metros |>
  select(short_name, population, Sweetgreen = sg_per_m, Chipotle = cmg_per_m) |>
  pivot_longer(c(Sweetgreen, Chipotle), names_to = "chain", values_to = "per_m")

fig_pen <- ggplot(pen_long,
                  aes(per_m, fct_reorder(short_name, population), colour = chain)) +
  geom_line(aes(group = short_name), colour = "grey80", linewidth = 0.6) +
  geom_point(size = 2.6) +
  scale_colour_manual(values = c(Sweetgreen = COL_SG, Chipotle = COL_CMG)) +
  labs(x = "Stores per million residents", y = NULL,
       title = "Store density, 25 largest metros",
       caption = SRC) +
  theme_post()

ggsave(file.path(FIG_DIR, "fig-penetration.png"), fig_pen,
       width = 7.5, height = 6.5, dpi = 200)

write_csv(
  metros |>
    select(cbsa_name, population, median_hh_income, density,
           sg_stores, cmg_stores, sg_per_m, cmg_per_m) |>
    arrange(desc(population)),
  file.path(TAB_DIR, "penetration_by_metro.csv")
)

# ============================================================================
# 2. THE NAIVE BUILD
# ============================================================================
cmg_national_per_m <- sum(metros$cmg_stores) / (sum(metros$population) / 1e6)
naive_ceiling      <- cmg_national_per_m * sum(metros$population) / 1e6

message("chipotle national density: ", round(cmg_national_per_m, 2), " per million")
message("naive ceiling: ", round(naive_ceiling), " sweetgreen units")

# ============================================================================
# 3. THE TEST
# ============================================================================
# Fitted only where Sweetgreen already operates. The question is whether its
# density is explained by what a metro is like e.g. income, density, education
# rather than by how long it has been there. If it is strongly elastic, metros
# unlike the ones it has chosen cannot be assumed to support the same density,
# and step 2 is invalid.

entered_sg  <- metros |> filter(sg_stores > 0)
entered_cmg <- metros |> filter(cmg_stores > 0)

f <- ~ log(median_hh_income) + log(density) + ba_plus_share

# PRIMARY SPECIFICATION: log outcome.

m_sg  <- lm(update(f, log(sg_per_m)  ~ .), data = entered_sg)
m_cmg <- lm(update(f, log(cmg_per_m) ~ .), data = entered_cmg)

# Level models retained as the robustness check, not as the headline.
m_sg_lvl  <- lm(update(f, sg_per_m  ~ .), data = entered_sg)
m_cmg_lvl <- lm(update(f, cmg_per_m ~ .), data = entered_cmg)

models <- bind_rows(
  tidy(m_sg)      |> mutate(chain = "Sweetgreen", spec = "log",   n = nobs(m_sg),      r2 = summary(m_sg)$r.squared),
  tidy(m_cmg)     |> mutate(chain = "Chipotle",   spec = "log",   n = nobs(m_cmg),     r2 = summary(m_cmg)$r.squared),
  tidy(m_sg_lvl)  |> mutate(chain = "Sweetgreen", spec = "level", n = nobs(m_sg_lvl),  r2 = summary(m_sg_lvl)$r.squared),
  tidy(m_cmg_lvl) |> mutate(chain = "Chipotle",   spec = "level", n = nobs(m_cmg_lvl), r2 = summary(m_cmg_lvl)$r.squared)
)
write_csv(models, file.path(TAB_DIR, "model_comparison.csv"))

# In the log specification the coefficients are ELASTICITIES (for the logged
# regressors) and semi-elasticities (for ba_plus_share), so they are read as
# percentage responses rather than stores-per-million responses.

# ============================================================================
# 4. THE CONTROL
# ============================================================================
# The same specification on Chipotle. The comparison of the two income
# coefficients says whether these concepts share an
# addressable geography or not.

inc_sg  <- coef(m_sg)[["log(median_hh_income)"]]
inc_cmg <- coef(m_cmg)[["log(median_hh_income)"]]
message("income coefficient -- sweetgreen: ", round(inc_sg, 2),
        " | chipotle: ", round(inc_cmg, 2))

elas <- tibble(
  chain = c("Sweetgreen", "Chipotle"),
  coef  = c(inc_sg, inc_cmg),
  se    = c(summary(m_sg)$coefficients["log(median_hh_income)", "Std. Error"],
            summary(m_cmg)$coefficients["log(median_hh_income)", "Std. Error"])
)

plot_dat <- bind_rows(
  entered_sg  |> transmute(chain = "Sweetgreen", median_hh_income, ba_plus_share, per_m = sg_per_m),
  entered_cmg |> transmute(chain = "Chipotle",   median_hh_income, ba_plus_share, per_m = cmg_per_m)
)

# PRIMARY FIGURE
fig_edu <- ggplot(plot_dat, aes(ba_plus_share, per_m, colour = chain)) +
  geom_point(alpha = 0.55, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_colour_manual(values = c(Sweetgreen = COL_SG, Chipotle = COL_CMG)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_log10() +
  labs(x = "Share of adults with a bachelor's degree or higher",
       y = "Stores per million residents (log scale)",
       title = "Store density against metro education, metros already entered",
       caption = SRC) +
  theme_post()

ggsave(file.path(FIG_DIR, "fig-education-elasticity.png"), fig_edu,
       width = 7.5, height = 4.5, dpi = 200)

# SECONDARY FIGURE
fig_inc <- ggplot(plot_dat, aes(median_hh_income, per_m, colour = chain)) +
  geom_point(alpha = 0.55, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_colour_manual(values = c(Sweetgreen = COL_SG, Chipotle = COL_CMG)) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_log10() +
  labs(x = "Median household income",
       y = "Stores per million residents (log scale)",
       title = "The same picture against income, without controlling for education",
       caption = SRC) +
  theme_post()

ggsave(file.path(FIG_DIR, "fig-income-elasticity.png"), fig_inc,
       width = 7.5, height = 4.5, dpi = 200)

message("corr(log income, BA share) among entered Sweetgreen metros: ",
        round(cor(log(entered_sg$median_hh_income), entered_sg$ba_plus_share), 3))

cap <- max(entered_sg$sg_per_m)
candidates <- metros |> filter(sg_stores == 0)

smear <- mean(exp(residuals(m_sg)))

pred <- candidates |>
  mutate(
    pred_per_m_raw = exp(predict(m_sg, newdata = candidates)) * smear,
    pred_per_m     = pmin(pred_per_m_raw, cap),          # no floor needed now
    pred_stores    = pred_per_m * population / 1e6,
    capped         = pred_per_m_raw > cap
  )

# The level model, for the range.
pred_lvl_stores <- {
  v <- pmin(pmax(predict(m_sg_lvl, newdata = candidates), 0), cap)
  sum(v * candidates$population / 1e6)
}

existing_units <- sum(metros$sg_stores)
headroom       <- sum(pred$pred_stores)
honest_ceiling <- existing_units + headroom
ceiling_lvl    <- existing_units + pred_lvl_stores

message("smearing factor: ", round(smear, 3),
        " | predictions hitting the cap: ", sum(pred$capped), "/", nrow(pred))
message("open today: ", existing_units,
        " | headroom (log): ", round(headroom),
        " | ceiling (log): ", round(honest_ceiling),
        " | ceiling (level): ", round(ceiling_lvl),
        " | stated target: ", SG_TARGET)

# --- where the gap sits -----------------------------------------------------
gap <- pred |>
  slice_max(pred_stores, n = 20) |>
  transmute(cbsa_name, population, median_hh_income, density,
            cmg_stores, pred_per_m, pred_stores = round(pred_stores, 1))

write_csv(gap, file.path(TAB_DIR, "gap_metros.csv"))

ceiling_tbl <- tibble(
  measure = factor(c("Open today", "Ceiling (log spec)", "Ceiling (level spec)", "Stated target"),
                   levels = c("Open today", "Ceiling (log spec)", "Ceiling (level spec)", "Stated target")),
  units   = c(existing_units, honest_ceiling, ceiling_lvl, SG_TARGET)
)
write_csv(ceiling_tbl, file.path(TAB_DIR, "ceiling.csv"))

fig_ceiling <- ggplot(ceiling_tbl, aes(units, fct_rev(measure))) +
  geom_col(fill = COL_SG, width = 0.55) +
  geom_text(aes(label = round(units)), hjust = -0.2, size = 3.4, colour = "grey25") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = "US units", y = NULL,
       title = "Sweetgreen: open units, modelled ceiling, stated target",
       caption = SRC) +
  theme_post()

ggsave(file.path(FIG_DIR, "fig-ceiling.png"), fig_ceiling,
       width = 7.5, height = 3.6, dpi = 200)

message("done -- figures in ", FIG_DIR, ", tables in ", TAB_DIR)
