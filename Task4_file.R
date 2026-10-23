---
  title: "Task 4"
author: "Team 044"
format: html
editor: visual
---
  
  #### Part A – Defining the outcome and justifying the model
  
  
  ```{r}
movies_clean |>
  summarise(
    total_movies = n(),
    flops = sum(roi < 1, na.rm = TRUE),
    non_flops = sum(roi >= 1, na.rm = TRUE),
    missing_roi = sum(is.na(roi))
  )

movies_model2 <- movies_clean |>
  filter(!is.na(roi)) |>
  mutate(flop = if_else(roi < 1, 1, 0))

genre_focus <- c(
  "Action",
  "Comedy",
  "Drama",
  "Horror",
  "Animation"
)

movies_model2 <- movies_model2 |>
  filter(primary_genre %in% genre_focus)
```

```{r}
#creating modelling dataset
movies_model2<- movies_model2 |>
  mutate(
    flop = if_else(
      roi < 1,
      1L,
      0L,
      missing = NA_integer_
    ),
    log_budget = log(budget)
  ) |>
  filter(
    !is.na(flop),
    !is.na(primary_genre),
    !is.na(franchise_status),
    !is.na(log_budget),
    is.finite(log_budget),
    !is.na(cast_size)
  )
```

### Why Logistic Regression?

We use logistic regression to predict whether a film will flop, making this a binary classification problem. Unlike linear regression, which predicts continuous values, logistic regression estimates probabilities between 0 and 1. 

Poisson regression is mainly used for count data. Logistic regression also helps Meridian Pictures understand how factors such as genre, budget and franchise status affect flop probability, supporting more informed greenlight decisions.


### Part B

**Note: Action was selected as the reference genre because its downside risk was examined in the earlier analysis.**
  
  Frequentist and Bayesian logistic regression models were fitted using 2,265 films, with genre, franchise status, log budget and crew size as predictors of flop probability (ROI < 1). The Bayesian model used weakly informative normal priors, (N(0, 2.5)), on the log-odds scale. Both models produced similar odds ratios and intervals.

```{r}

movies_model2 |>
  mutate(
    primary_genre = relevel(
      factor(primary_genre),
      ref = "Action"
    )
  )

model_freq <- glm(
  flop ~ primary_genre +
    franchise_status +
    log_budget +
    crew_size,
  data = movies_model2,
  family = binomial(link = "logit")
)
```

```{r}
exp(coef(model_freq))
exp(confint(model_freq))
```

```{r}
#| echo: false
#| output: true
results <- broom::tidy(
  model_freq,
  conf.int = TRUE,
  exponentiate = TRUE
) |>
  dplyr::select(
    term,
    estimate,
    conf.low,
    conf.high,
    p.value
  ) |>
  dplyr::rename(
    Predictor = term,
    `Odds Ratio` = estimate,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `P-value` = p.value
  ) 

results |>
  gt() |>
  fmt_number(
    columns = c(`Odds Ratio`, `95% CI Lower`, `95% CI Upper`, `P-value`),
    decimals = 3
  ) |>
  tab_header(
    title = "Frequentist Logistic Regression Results",
    subtitle = "Odds Ratios and 95% Confidence Intervals"
  )

```

```{r}

bayesian_model <- stan_glm(
  flop ~ primary_genre +
    franchise_status +
    log_budget +
    crew_size,
  data = movies_model2,
  family = binomial(link = "logit"),
  prior = normal(location = 0, scale = 2.5, autoscale = FALSE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = FALSE),
  chains = 4,
  iter = 4000,
  seed = 123,
  refresh = 500
)
```

Both models produced similar results. For franchise status, the frequentist 95% confidence interval was 0.121–0.431, compared with the Bayesian 95% credible interval of 0.119–0.424. For crew size, the frequentist interval was 0.984–0.991, while the Bayesian interval was 0.983–0.991. All four intervals were below 1, supporting lower estimated flop odds for franchise films and larger crews.


```{r}
#| echo: false
#| output: true
posterior_draws <- as.matrix(bayesian_model)

bayes_or <- exp(
  posterior::summarise_draws(
    posterior::as_draws_matrix(bayesian_model)
  )$median
)


tidy(
  bayesian_model,
  effects = "fixed",
  conf.int = TRUE,
  conf.level = 0.95
) |>
  mutate(
    `Odds Ratio` = exp(estimate),
    `Lower 95% CrI` = exp(conf.low),
    `Upper 95% CrI` = exp(conf.high)
  ) |>
  dplyr::select(
    term,
    `Odds Ratio`,
    `Lower 95% CrI`,
    `Upper 95% CrI`
  ) |>
  gt() |>
  fmt_number(
    columns = c(
      `Odds Ratio`,
      `Lower 95% CrI`,
      `Upper 95% CrI`
    ),
    decimals = 3
  ) |>
  cols_label(
    term = "Predictor"
  ) |>
  tab_header(
    title = "Bayesian Logistic Regression",
    subtitle = "Odds Ratios and 95% Credible Intervals"
  )
```

The posterior probability that franchise films have lower flop odds than Original films was approximately 100%. The Bayesian model showed satisfactory convergence. 

**Note: Metropolis sampling was not implemented manually because `rstanarm` uses `Stan's HMC/NUTS` algorithms. Overall, both models produced consistent conclusions.**
  
  # Part C
  
  ```{r}
library(rsample)
set.seed(123)

splitting_data <- initial_split(
  movies_model2,
  prop = 0.8,
  strata = flop
)

train_data <- training(splitting_data)
test_data <- testing(splitting_data)
```


```{r}

model_freq_train <- glm(
  flop ~ primary_genre +
    franchise_status +
    log_budget +
    crew_size,
  data = train_data,
  family = binomial(link = "logit")
)

model_bayes_train <- stan_glm(
  flop ~ primary_genre +
    franchise_status +
    log_budget +
    crew_size,
  data = train_data,
  family = binomial(link = "logit"),
  prior = normal(0, 2.5, autoscale = FALSE),
  prior_intercept = normal(0, 2.5, autoscale = FALSE),
  chains = 4,
  iter = 4000,
  seed = 123,
  refresh = 500
)

prob_freq_train <- predict(
  model_freq_train,
  newdata = test_data,
  type = "response"
)

threshold <- 0.40

pred_freq_train <- ifelse(prob_freq_train >= threshold, 1, 0)

conf_train <- table(
  Actual = test_data$flop,
  Predicted = pred_freq
)

conf_train


Tp <- sum(test_data$flop == 1 & pred_freq == 1)
Tn <- sum(test_data$flop == 0 & pred_freq == 0)
Fp <- sum(test_data$flop == 0 & pred_freq == 1)
Fn <- sum(test_data$flop == 1 & pred_freq == 0)

metrics_train_data <- data.frame(
  Accuracy = (Tp + Tn) / (Tp + Tn + Fp + Fn),
  Sensitivity = Tp / (Tp + Fn),
  Specificity = Tn / (Tn + Fn),
  Precision = Tp / (Tp + Fp)
)

metrics_train_data

```
#### Evaluating Bayesian model

```{r}
prob_bayes <- posterior_epred(
  model_bayes,
  newdata = test_data
) |>
  colMeans()

pred_bayes <- ifelse(prob_bayes >= threshold, 1, 0)

table(
  Actual = test_data$flop,
  Predicted = pred_bayes
)

library(loo)



loo_bayes <- loo(model_bayes)
plot(loo_bayes)
print(loo_bayes)

```

