library(tidyverse)
library(tidymodels)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
library(dplyr)
library(ranger)

model = readRDS("../job-change-analysis/job_change_predictor.rds")
dataset = "https://raw.githubusercontent.com/RhogeDiaz/job-change-analysis/refs/heads/main/clean_hr_dataset.csv"

df = read_csv(dataset)

# --- Start cleaning pipeline ---
# Work on `df` already loaded in session

# Snapshot before changes
initial_snapshot <- tibble(
  rows_before = nrow(df),
  cols_before = ncol(df),
  col_names = list(colnames(df))
)

# NA summary before removal
na_summary_before <- df |> summarise(across(everything(), ~ sum(is.na(.)))) |> pivot_longer(everything(), names_to = "column", values_to = "n_missing")

# 1) Remove rows with any NA
df_no_na <- df |> tidyr::drop_na()
removed_na_count <- initial_snapshot$rows_before - nrow(df_no_na)

# 2) Remove duplicate rows (keep first)
n_duplicates_removed <- nrow(df_no_na) - nrow(dplyr::distinct(df_no_na))
df_dedup <- dplyr::distinct(df_no_na)

# Helper to detect numeric-like columns (safe, uses regex on non-missing values)
is_numeric_like <- function(x) {
  if (is.numeric(x)) return(TRUE)
  x_char <- as.character(x)
  x_non_na <- x_char[!is.na(x_char) & nzchar(x_char)]
  if (length(x_non_na) == 0) return(FALSE)
  all(str_detect(x_non_na, "^-?\\d+(\\.\\d+)?$"))
}

# Determine columns to coerce
cols_before_classes <- tibble(column = names(df_dedup), class_before = map_chr(df_dedup, ~ class(.x)[1]))
numeric_cols <- names(df_dedup)[map_lgl(df_dedup, is_numeric_like)]
factor_candidate_cols <- setdiff(names(df_dedup), numeric_cols)

# 3) Convert numeric-like columns to numeric (using as.numeric)
df_converted <- df_dedup |>
  mutate(across(all_of(numeric_cols), ~ as.numeric(as.character(.x))))

# 4) Convert remaining character/factor columns to factor
# (leave existing numeric columns as-is)
df_converted <- df_converted |>
  mutate(across(all_of(factor_candidate_cols), ~ as.factor(as.character(.x))))

# Post-conversion report
cols_after_classes <- tibble(column = names(df_converted), class_after = map_chr(df_converted, ~ class(.x)[1]))
conversion_report <- cols_before_classes |> left_join(cols_after_classes, by = "column") |>
  rowwise() |>
  mutate(
    n_missing_after = sum(is.na(df_converted[[column]])),
    n_unique_after = dplyr::n_distinct(df_converted[[column]])
  ) |>
  ungroup()

# Final snapshot
final_snapshot <- tibble(
  rows_after = nrow(df_converted),
  cols_after = ncol(df_converted)
)

# Package a result list (no printing)
cleaning_results <- list(
  initial_snapshot = initial_snapshot,
  na_summary_before = na_summary_before,
  removed_na_count = removed_na_count,
  n_duplicates_removed = n_duplicates_removed,
  conversion_report = conversion_report,
  final_snapshot = final_snapshot,
  cleaned_data = df_converted
)

# Return the results object for inspection
cleaning_results

# Exclude these columns from predictors
exclude_cols <- c("enrollee_id", "last_new_job", "split", "looking_for_job_change")

# Target (as factor)
target <- factor(df_converted$looking_for_job_change)

# Candidate predictors
predictors <- setdiff(names(df_converted), exclude_cols)

# Helper to run tests per variable
assoc_tests <- map_dfr(predictors, function(var) {
  x <- df_converted[[var]]
  if (is.factor(x) || is.character(x)) {
    tbl <- table(x, target)
    # safe expected count check
    chisq_ok <- tryCatch({
      exp <- suppressWarnings(stats::chisq.test(tbl)$expected)
      all(exp >= 5)
    }, error = function(e) FALSE)
    if (!chisq_ok) {
      test <- "fisher.test"
      pval <- tryCatch(stats::fisher.test(tbl)$p.value, error = function(e) NA_real_)
      note <- "Fisher used (low expected counts)"
    } else {
      test <- "chisq.test"
      pval <- tryCatch(stats::chisq.test(tbl)$p.value, error = function(e) NA_real_)
      note <- "Chi-square"
    }
    tibble(variable = var, type = "categorical", test = test, p_value = pval, note = note)
  } else if (is.numeric(x) || is.integer(x)) {
    # ANOVA: numeric ~ grouping by target
    df_tmp <- tibble(x = as.numeric(x), grp = target)
    res_aov <- tryCatch(stats::aov(x ~ grp, data = df_tmp), error = function(e) NA)
    pval <- if (inherits(res_aov, "aov")) {
      s <- summary(res_aov)
      s[[1]][["Pr(>F)"]][1]
    } else NA_real_
    tibble(variable = var, type = "numeric", test = "anova", p_value = pval, note = "one-way ANOVA")
  } else {
    tibble(variable = var, type = class(x)[1], test = NA_character_, p_value = NA_real_, note = "unsupported type")
  }
})

# Order by p-value ascending for quick selection
assoc_tests <- assoc_tests |> arrange(p_value)

assoc_tests

# Selected predictors (as you provided)
preds <- c(
  "company_size", "company_type", "experience_numeric", "experience",
  "enrolled_university", "relevant_experience", "education_level",
  "last_new_job_numeric", "gender", "major_discipline", "training_hours"
)

# --- Snapshot before changes ---
initial_n <- nrow(df_converted)

# Build working table: keep only predictors + looking flag (binary target)
# (explicitly exclude enrollee_id, last_new_job, split as requested)
keep_cols <- intersect(c(preds, "looking_for_job_change"), names(df_converted))
df_work <- df_converted |> select(all_of(keep_cols))

# Create binary factor target from looking_for_job_change (1 -> Looking, 0 -> Not Looking)
df_work <- df_work |>
  mutate(
    target = dplyr::case_when(
      looking_for_job_change == 1 ~ "Looking",
      looking_for_job_change == 0 ~ "Not Looking",
      TRUE ~ NA_character_
    ),
    target = factor(target, levels = c("Not Looking", "Looking"))
  )

# Remove rows with NA in target (drop "No Data"/ambiguous rows)
after_target_n <- nrow(df_work |> filter(!is.na(target)))
df_work <- df_work |> filter(!is.na(target))

# Remove rows with any NA among chosen predictors
before_na_drop <- nrow(df_work)
df_work <- df_work |> drop_na(all_of(preds))
after_na_drop_n <- nrow(df_work)

# Remove duplicate rows (keep first)
before_dedup <- nrow(df_work)
df_work <- dplyr::distinct(df_work)
after_dedup_n <- nrow(df_work)
n_duplicates_removed <- before_dedup - after_dedup_n

# Coerce types: numeric vars and categorical vars
numeric_vars <- intersect(c("experience_numeric", "last_new_job_numeric", "training_hours"), preds)
cat_vars <- setdiff(preds, numeric_vars)

df_work <- df_work |>
  mutate(across(all_of(numeric_vars), ~ as.numeric(as.character(.x)))) |>
  mutate(across(all_of(cat_vars), ~ as.factor(as.character(.x))))

# Final modelling table: predictors + target (drop original looking_for_job_change)
df_model <- df_work |> select(all_of(preds), target)

# Simple removal report
removal_report <- tibble(
  initial_rows = initial_n,
  after_target_filter = after_target_n,
  after_predictor_na_drop = after_na_drop_n,
  duplicates_removed = n_duplicates_removed,
  final_rows = nrow(df_model),
  n_predictors = length(preds)
)

# Return results object (no printing)
list(
  model_data = df_model,
  removal_report = removal_report,
  numeric_vars = numeric_vars,
  categorical_vars = cat_vars
)

set.seed(2026)

# 1) split (75% train, stratified)
data_split <- initial_split(df_model, prop = 0.75, strata = target)
train_data <- training(data_split)
test_data  <- testing(data_split)

# 2) recipe (from train)
rec <- recipe(target ~ ., data = train_data)

# 3) model spec: random forest (ranger)
rf_spec <- rand_forest(mode = "classification", trees = 500) %>%
  set_engine("ranger", importance = "impurity")

# 4) workflow
wf <- workflow() %>% add_model(rf_spec) %>% add_recipe(rec)

# 5) fit
wf_fit <- fit(wf, data = train_data)

# 6) predict on test set
predictions <- predict(wf_fit, new_data = test_data) %>%
  bind_cols(test_data %>% select(target))

# 7) accuracy
acc <- yardstick::accuracy(predictions, truth = target, estimate = .pred_class)

# 8) confusion matrix heatmap
conf_mat_res <- yardstick::conf_mat(predictions, truth = target, estimate = .pred_class)
cm_tbl <- as_tibble(conf_mat_res$table)   # columns: truth, prediction, n


cm_plot <- ggplot(cm_tbl, aes(x = Prediction, y = Truth, fill = n)) +
  geom_tile(color = "grey80") +
  geom_text(aes(label = n), size = 5) +
  scale_fill_gradient(low = "white", high = "firebrick") +
  labs(title = "Confusion Matrix", x = "Predicted", y = "Actual", fill = "Count") +
  theme_minimal()

# Return objects for inspection
list(
  workflow_fit = wf_fit,
  predictions = predictions,
  accuracy = acc,
  conf_mat = conf_mat_res,
  cm_plot = cm_plot
)
