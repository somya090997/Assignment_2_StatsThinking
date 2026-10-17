movies <- read_csv("data/movies.csv")

t2 <- movies |>
  mutate(budget = if_else(budget <= 0 | budget < 10000, NA_real_, budget)) |>
  filter(!is.na(franchise_status)) |>
  mutate(
    franchise_status = factor(franchise_status, levels = c("Original", "Franchise"))
  )
t2 |>
  group_by(franchise_status) |>
  summarise(
    n = n(),
    min = min(budget, na.rm = TRUE),
    q1 = quantile(budget, 0.25, na.rm = TRUE),
    median = median(budget, na.rm = TRUE),
    mean = mean(budget, na.rm = TRUE),
    q3 = quantile(budget, 0.75, na.rm = TRUE),
    max = max(budget, na.rm = TRUE),
    sd = sd(budget, na.rm = TRUE)
  ) |>
  mutate(across(min:sd, ~ round(.x, -3))) # rounding to nearest thousand


ggplot(t2, aes(x = budget, fill = franchise_status)) +
  geom_density(alpha = 0.4, na.rm = TRUE) +
  scale_x_log10(labels = scales::dollar) +
  labs(
    title = "Franchise budgets sit higher, but the two groups overlap heavily",
    x = "Production budget (USD, log scale)",
    y = "Density",
    fill = "Franchise status"
  ) +
  theme_minimal()


ggplot(t2, aes(x = budget, y = franchise_status)) +
  geom_boxplot(na.rm = TRUE) +
  labs(x = "Production budget", y = NULL) +
  theme_minimal()

#| label: t2-boot
# Observed difference in medians
obs_diff <- t2 |>
  group_by(franchise_status) |>
  summarise(median_budget = median(budget, na.rm = TRUE)) |>
  summarise(
    diff = median_budget[franchise_status == "Franchise"] -
      median_budget[franchise_status == "Original"]
  ) |>
  pull(diff)

# One bootstrap - resample each group with replacement at its own size
perform_bootstrap <- function(bootstrap_number) {
  t2 |>
    group_by(franchise_status) |>
    slice_sample(prop = 1, replace = TRUE) |>
    summarise(median_budget = median(budget, na.rm = TRUE)) |>
    summarise(
      boot_diff = median_budget[franchise_status == "Franchise"] -
        median_budget[franchise_status == "Original"]
    ) |>
    mutate(bootstrap_sample = bootstrap_number)
}

# Run 5000 bootstrap samples
bootstrap_results <- map_dfr(
  .x = 1:5000,
  .f = perform_bootstrap
)

# 95% percentile bootstrap interval
bootstrap_results |>
  summarise(
    observed_diff = obs_diff,
    lower_CI = quantile(boot_diff, 0.025, na.rm = TRUE),
    upper_CI = quantile(boot_diff, 0.975, na.rm = TRUE)
  )


#| label: t2-boot-plot
#| fig-height: 3.5
#| fig-width: 7
bootstrap_results |>
  ggplot(aes(x = boot_diff)) +
  geom_histogram(bins = 40, colour = "white") +
  geom_vline(xintercept = obs_diff) +
  geom_vline(
    xintercept = quantile(bootstrap_results$boot_diff, c(0.025, 0.975), na.rm = TRUE),
    linetype = "dashed"
  ) +
  labs(
    title = "Bootstrap distribution of the difference in median budget",
    x = "Franchise median minus Original median (USD)",
    y = "Count"
  ) +
  theme_minimal()


#| label: t2-concern
# Does the data support leadership's concern?
t2 |>
  group_by(franchise_status) |>
  summarise(
    n_films = n(),
    share_of_films = n() / nrow(t2),
    share_of_budget = sum(budget, na.rm = TRUE) / sum(t2$budget, na.rm = TRUE),
    prop_under_10M = mean(budget < 1e7, na.rm = TRUE),
    prop_over_100M = mean(budget > 1e8, na.rm = TRUE),
    prop_missing_budget = mean(is.na(budget))
  ) |>
  mutate(across(share_of_films:prop_missing_budget, ~ round(.x, 3)))

movies |>
  filter(revenue > 0, !is.na(franchise_status)) |>
  group_by(franchise_status) |>
  summarise(total_revenue = sum(revenue, na.rm = TRUE)) |>
  mutate(share_of_revenue = round(total_revenue / sum(total_revenue), 3))


#profit summary

