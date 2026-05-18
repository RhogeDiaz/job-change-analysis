library(tidyverse)


train_raw <- read_csv("aug_train.csv", show_col_types = FALSE)
test_raw  <- read_csv("aug_test.csv",  show_col_types = FALSE)

# walay 'target' column — gi add nako as NA para ma combine later
test_raw <- test_raw %>% mutate(target = NA_real_)

# gi tag each row sa iyahang source para ma filter sa dashboard
train_raw <- train_raw %>% mutate(split = "train")
test_raw  <- test_raw  %>% mutate(split = "test")

# Combine into one data frame
combined_raw <- bind_rows(train_raw, test_raw)

# rows ug columns after gi combine
cat("Rows after combining:", nrow(combined_raw), "\n")
cat("Columns:", ncol(combined_raw), "\n")

# gi rename nako column names, gi remove lang ang no-op renames para clean
combined <- combined_raw %>%
  rename(
    city_dev_index         = city_development_index,
    relevant_experience    = relevent_experience,   # gi fix typo
    looking_for_job_change = target
  )


# HANDLING MISSING VALUES

# Strategy:
#   - sa categorical columns  -> NA to "Unknown"
#   - sa numeric columns      -> wala
#   - sa target (sa test rows)   -> NA, gi fill og "No Data" sa label

categorical_cols <- c(
  "gender", "enrolled_university", "education_level",
  "major_discipline", "experience", "company_size",
  "company_type", "last_new_job"
)

combined <- combined %>%
  mutate(across(all_of(categorical_cols), ~ replace_na(.x, "Unknown")))


# STANDARDIZE / RECODE CATEGORICAL VARIABLES

combined <- combined %>%
  mutate(

    # sa relevant_experience gi change nako / shorten labels
    relevant_experience = case_when(
      relevant_experience == "Has relevent experience" ~ "Yes",
      relevant_experience == "No relevent experience"  ~ "No",
      TRUE ~ relevant_experience
    ),

    # education_level: gi ordered factor (ana ai useful dw ni for plots)
    education_level = factor(
      education_level,
      levels = c("Primary School", "High School", "Graduate",
                 "Masters", "Phd", "Unknown"),
      ordered = TRUE
    ),

    # sa experience: gi convert ">20" / "<1" to numeric midpoints (suggest ni ai)
    # ug gi store ni as a new numeric column; pero keep original as-is
    # FIX: gi fill og median ang NA (from "Unknown" rows) para wala nay NA
    experience_numeric = case_when(
      experience == "<1"      ~ 0.5,
      experience == ">20"     ~ 21,
      experience == "Unknown" ~ NA_real_,
      TRUE                    ~ suppressWarnings(as.numeric(experience))
    ),
    experience_numeric = replace_na(
      experience_numeric,
      median(experience_numeric, na.rm = TRUE)
    ),

    # same ra gihapon, sa last_new_job: gi convert ">4" to numeric midpoint
    # FIX: gi fill og median ang NA (from "Unknown" rows)
    last_new_job_numeric = case_when(
      last_new_job == "never"   ~ 0,
      last_new_job == ">4"      ~ 5,
      last_new_job == "Unknown" ~ NA_real_,
      TRUE                      ~ suppressWarnings(as.numeric(last_new_job))
    ),
    last_new_job_numeric = replace_na(
      last_new_job_numeric,
      median(last_new_job_numeric, na.rm = TRUE)
    ),

    # sa company_size gi ordered factor
    # FIX: gi as.character() una para ma apply ang replace_na before factor conversion
    company_size = factor(
      replace_na(as.character(company_size), "Unknown"),
      levels = c("<10", "10-49", "50-99", "100-500",
                 "500-999", "1000-4999", "5000-9999", "10000+", "Unknown"),
      ordered = TRUE
    ),

    # FIX: looking_for_job_change — gi fill og -1 ang test rows (walay target)
    # para dili NA ang numeric column
    looking_for_job_change = replace_na(looking_for_job_change, -1),

    # nag create ko new column para sa target / looking for job change para rag ma readable
    # FIX: "No Data" na ang label para sa test rows, dili NA
    job_change_label = case_when(
      looking_for_job_change == 1  ~ "Looking",
      looking_for_job_change == 0  ~ "Not Looking",
      looking_for_job_change == -1 ~ "No Data",
      TRUE                         ~ "No Data"
    )
  )

View(combined)


# REMOVE DUPLICATES

before <- nrow(combined)
combined <- combined %>% distinct(enrollee_id, split, .keep_all = TRUE)

# para nays naa ni HAHAHAHAHHAHAH
cat("Duplicate rows removed:", before - nrow(combined), "\n")


# BASIC VALIDATION

cat("\n--- Missing values in cleaned data ---\n")
combined %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
  filter(na_count > 0) %>%
  print()

cat("\n--- Row counts by split ---\n")
combined %>% count(split) %>% print()

cat("\n--- Target distribution (train only) ---\n")
combined %>%
  filter(split == "train") %>%
  count(job_change_label) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

View(combined)


# EXPORT CLEAN CSV

write_csv(combined, "clean_hr_dataset.csv")
cat("\n✓ Saved: clean_hr_dataset.csv\n")
cat("  Rows:", nrow(combined), "| Columns:", ncol(combined), "\n")