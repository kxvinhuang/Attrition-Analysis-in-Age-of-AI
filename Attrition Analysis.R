# ---- 0. SETUP ----------------------------------------------------------------
# install.packages("tidyverse")  # only need to run this once
library(tidyverse)
options(dplyr.summarise.inform = FALSE)
set.seed(1234)  # so anything random later is reproducible

dir.create("figures", showWarnings = FALSE)
dir.create("tables",  showWarnings = FALSE)

# private dataset - removed for public repo
url <- ""

# read in the file. treat "NA" and blanks as missing.
# "N/A" strings are left alone here as they get audited in 1a.
raw  <- read.csv(url, encoding = "UTF-8", na.strings = c("NA", ""), stringsAsFactors = FALSE)
data <- raw
dim(data)
str(data)

# ==============================================================================
# DATASET VARIABLES (raw data, 29 columns, before cleaning)
# ==============================================================================
#  #  Variable                       Type      Description / valid values
# --  -----------------------------  --------  ----------------------------------------
#  1  employee_id                    Integer   Unique ID per employee record (1001-6000)
#  2  age                            Integer   Age in years (21-64)
#  3  gender                         Category  Female, Male, Non-binary, Prefer not to say
#  4  marital_status                 Category  Divorced, Married, Single
#  5  education_level                Category  High School, Bachelor's, Master's, PhD (ordinal, stored as text)
#  6  department                     Category  8 departments: Customer Support, Engineering, Finance,
#                                              Human Resources, Marketing, Operations, R&D, Sales
#  7  job_role                       Category  25 roles, nested within department
#  8  job_level                      Integer   Seniority, 1 (entry) to 5 (most senior), ordinal
#  9  monthly_income                 Integer   Gross monthly salary (1,500-21,609), currency unspecified
# 10  stock_option_level             Integer   Stock options granted, 0 (none) to 3 (highest), ordinal
# 11  salary_hike_pct                Decimal   % salary increase at last review (0-22.6)
# 12  years_at_company               Decimal   Tenure at current company, years (0-18.1)
# 13  total_working_years            Decimal   Total career length, years (0-30.4)
# 14  years_since_promotion          Decimal   Years since last promotion (0-11.1)
# 15  num_prior_companies            Integer   Number of previous employers (0-8)
# 16  work_mode                      Category  Hybrid, Onsite, Remote
# 17  commute_minutes                Integer   One-way commute, minutes (0-92); 0 likely = fully remote
# 18  overtime_hours_per_week        Decimal   Average overtime, hours per week (0-34.9)
# 19  business_travel_days_per_year  Integer   Business travel days per year (0-38)
# 20  training_hours                 Integer   Hours of training completed (0-67), period unspecified
# 21  manager_support_score          Decimal   Perceived manager support, 1-10 (higher = more support)
# 22  burnout_score                  Decimal   Self-reported burnout, 1-10 (higher = more burnout)
# 23  engagement_score               Decimal   Self-reported engagement, 1-10 (higher = more engaged)
# 24  work_life_balance_score        Decimal   Self-reported work-life balance, 1-9.9 (higher = better)
# 25  performance_rating             Integer   Most recent formal rating, 2-5 (no 1s observed), ordinal
# 26  last_review_score              Decimal   Score from most recent review, 2.8-10
# 27  uses_ai_tools_at_work          Boolean   TRUE / FALSE: uses AI tools in their job
# 28  perceived_ai_job_risk          Decimal   Perceived risk of job being affected by AI, 1-10 (higher = more risk)
# 29  attrition                      Category  Yes / No: whether the employee left the company
# ==============================================================================

# ---- 1. DATA QUALITY AUDIT ---------------------------------------------------

# 1a. missing values. two sources: real NA and the placeholder text "N/A".
miss_na <- raw %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_NA")
miss_placeholder <- raw %>%
  summarise(across(where(is.character), ~ sum(.x == "N/A", na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_placeholder_N/A")
# combine both into one table so we know the true missingness per column
missing_tbl <- miss_na %>%
  left_join(miss_placeholder, by = "variable") %>%
  mutate(`n_placeholder_N/A` = replace_na(`n_placeholder_N/A`, 0L),
         n_missing_total = n_NA + `n_placeholder_N/A`,
         pct_missing = round(100 * n_missing_total / nrow(raw), 2)) %>%
  filter(n_missing_total > 0) %>%
  arrange(desc(n_missing_total))
missing_tbl

# 1b. duplicates. count both full-row dupes and repeated employee ids,
# then check if every repeated id is actually an exact copy.
n_dup_rows <- sum(duplicated(raw))
n_dup_ids  <- sum(duplicated(raw$employee_id))
dup_exact  <- n_dup_rows == n_dup_ids  # TRUE means every repeated id is an exact copy
c(full_row_duplicates = n_dup_rows, duplicate_ids = n_dup_ids, all_exact_copies = dup_exact)

# 1c. invalid values. these are the variable ranges from the spec
# anything outside these are invalid
valid_ranges <- tribble(
  ~variable,                       ~lo,   ~hi,
  "employee_id",                   1001,  6000,
  "age",                             21,    64,
  "job_level",                        1,     5,
  "monthly_income",                1500, 21609,
  "stock_option_level",               0,     3,
  "salary_hike_pct",                  0,  22.6,
  "years_at_company",                 0,  18.1,
  "total_working_years",              0,  30.4,
  "years_since_promotion",            0,  11.1,
  "num_prior_companies",              0,     8,
  "commute_minutes",                  0,    92,
  "overtime_hours_per_week",          0,  34.9,
  "business_travel_days_per_year",    0,    38,
  "training_hours",                   0,    67,
  "manager_support_score",            1,    10,
  "burnout_score",                    1,    10,
  "engagement_score",                 1,    10,
  "work_life_balance_score",          1,   9.9,
  "performance_rating",               2,     5,
  "last_review_score",              2.8,    10,
  "perceived_ai_job_risk",            1,    10
)

valid_categories <- list(
  gender          = c("Female", "Male", "Non-binary", "Prefer not to say"),
  marital_status  = c("Divorced", "Married", "Single"),
  education_level = c("High School", "Bachelors", "Masters", "PhD"),
  department      = c("Customer Support", "Engineering", "Finance", "Human Resources",
                      "Marketing", "Operations", "Research & Development", "Sales"),
  work_mode       = c("Hybrid", "Onsite", "Remote"),
  attrition       = c("Yes", "No")
)

# loop over every numeric var and flag values outside its dictionary range.
# also keeps an example so we can see what the bad values look like.
range_audit <- pmap_dfr(valid_ranges, function(variable, lo, hi) {
  x   <- raw[[variable]]
  bad <- !is.na(x) & (x < lo | x > hi)
  tibble(variable, dict_min = lo, dict_max = hi,
         observed_min = min(x, na.rm = TRUE), observed_max = max(x, na.rm = TRUE),
         n_invalid = sum(bad), pct_invalid = round(100 * sum(bad) / length(x), 2),
         example_invalid = paste(head(unique(x[bad]), 5), collapse = ", "))
})
print(range_audit, n = Inf)
range_audit %>% filter(n_invalid > 0)

# same idea for categorical vars, anything not in the predefined list counts as invalid.
cat_audit <- imap_dfr(valid_categories, function(valid, v) {
  x   <- trimws(as.character(raw[[v]]))
  bad <- !is.na(x) & x != "N/A" & !x %in% valid
  tibble(variable = v, n_invalid = sum(bad),
         invalid_values = paste(unique(x[bad]), collapse = ", "))
})
cat_audit

# some cross checks for category coverage, job roles, values the dictionary
# has but never appears in the data.
table(raw$uses_ai_tools_at_work, useNA = "ifany")
n_distinct(raw$job_role)  # 25 roles
raw %>% distinct(department, job_role) %>% count(job_role) %>% filter(n > 1)
setdiff(valid_categories$gender, unique(raw$gender))
for (v in c("gender", "marital_status", "education_level", "department",
            "work_mode", "attrition", "uses_ai_tools_at_work")) {
  cat("\n", v, "\n"); print(table(raw[[v]], useNA = "ifany"))
}

# 1d. logical inconsistencies. age 999 is already caught above, so the
# age-based checks only look at valid age rows.
audit_logic <- tibble(
  check = c("years_at_company > total_working_years",
            "years_since_promotion > years_at_company",
            "total_working_years > age - 14 (started work before 14)",
            "years_at_company > age - 16",
            "Remote but commute_minutes > 0",
            "Onsite/Hybrid but commute_minutes == 0",
            "performance_rating >= 4 but last_review_score < 6 (soft check)"),
  n = c(
    sum(raw$years_at_company > raw$total_working_years, na.rm = TRUE),
    sum(raw$years_since_promotion > raw$years_at_company, na.rm = TRUE),
    sum(raw$total_working_years > raw$age - 14 & raw$age <= 100, na.rm = TRUE),
    sum(raw$years_at_company > raw$age - 16 & raw$age <= 100, na.rm = TRUE),
    sum(raw$work_mode == "Remote" & raw$commute_minutes > 0, na.rm = TRUE),
    sum(raw$work_mode %in% c("Onsite", "Hybrid") & raw$commute_minutes == 0, na.rm = TRUE),
    sum(raw$performance_rating >= 4 & raw$last_review_score < 6, na.rm = TRUE)
  ),
  base = c(nrow(raw), nrow(raw), nrow(raw), nrow(raw),
           sum(raw$work_mode == "Remote", na.rm = TRUE),
           sum(raw$work_mode %in% c("Onsite", "Hybrid")),
           nrow(raw))
) %>% mutate(pct_of_base = round(100 * n / base, 1))
audit_logic


# (invalid values like age = 999 would distort the fences, so mask them).
mask_invalid <- function(df) {
  for (i in seq_len(nrow(valid_ranges))) {
    v <- valid_ranges$variable[i]
    bad <- !is.na(df[[v]]) & (df[[v]] < valid_ranges$lo[i] | df[[v]] > valid_ranges$hi[i])
    df[[v]][bad] <- NA
  }
  df
}
# NOTE: The outliers here are mostly just right skewed values and so they aren't
# brought up and discussed in the report
# 1e. outliers. iqr rule on values that are inside the dictionary ranges
# iqr_out <- raw %>% distinct() %>% mask_invalid() %>%
#   select(where(is.numeric), -employee_id) %>%
#   pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
#   group_by(variable) %>%
#   summarise(
#     q1 = quantile(value, .25, na.rm = TRUE),
#     q3 = quantile(value, .75, na.rm = TRUE),
#     lower_fence = q1 - 1.5 * (q3 - q1),
#     upper_fence = q3 + 1.5 * (q3 - q1),
#     n_low_outliers  = sum(value < lower_fence, na.rm = TRUE),
#     n_high_outliers = sum(value > upper_fence, na.rm = TRUE),
#     n_outliers      = n_low_outliers + n_high_outliers,
#     .groups = "drop"
#   ) %>%
#   select(variable, n_low_outliers, n_high_outliers, n_outliers, lower_fence, upper_fence) %>%
#   arrange(desc(n_outliers))
# iqr_out
# # a rule i'm using in the report: iqr outliers that fall inside the data
# # dictionary range are just real values (high income, long overtime), so keep them.

# save everything from the audit + evidence so it can be referenced later
write_csv(missing_tbl, "tables/audit_missing.csv")
write_csv(range_audit, "tables/audit_range_check_all_variables.csv")
write_csv(cat_audit,   "tables/audit_category_check.csv")
write_csv(audit_logic, "tables/audit_logic.csv")
# write_csv(iqr_out,     "tables/audit_iqr_outliers.csv")

# ---- 2. DATA CLEANING ---------------------------------------------------------

# evidence to support the cleaning decisions. these three blocks
# each produce the number i'll quote when justifying a choice in section 2.
raw_flag <- raw %>% mutate(attr = as.integer(attrition == "Yes"))

# i. duplicates - do duplicate rows look any different to unique rows?
# if their attrition rate is the same, dropping them is lossless.
dup_impact <- tibble(
  group = c("Unique rows", "Duplicate rows (2nd+ copies)"),
  n = c(sum(!duplicated(raw)), sum(duplicated(raw))),
  attrition_pct = c(round(100 * mean(raw_flag$attr[!duplicated(raw)]), 1),
                    round(100 * mean(raw_flag$attr[duplicated(raw)]), 1)))
dup_impact
write_csv(dup_impact,  "tables/audit_duplicate_impact.csv")

# ii. do rows with a quality problem behave differently to clean rows?
# if attrition rate is similar, the problems don't bias the outcome.
raw_flag %>%
  mutate(any_missing_or_invalid = is.na(monthly_income) | is.na(stock_option_level) | is.na(salary_hike_pct) |
           is.na(years_at_company) | is.na(work_mode) | work_mode == "N/A" | age > 100 | job_level == 0) %>%
  group_by(any_missing_or_invalid) %>%
  summarise(n = n(), attrition_pct = round(100 * mean(attr), 1))

# there are 52 cases of any missing or invalid however a total of 53 instances of missing/invalid
# therefore there is a row where there is an instance of 2 missing or one missing one invalid
# below shows that the row that has two NA values
raw %>%
  mutate(n_missing = is.na(monthly_income) + is.na(stock_option_level) +
           is.na(salary_hike_pct) + is.na(years_at_company) +
           (is.na(work_mode) | work_mode == "N/A")) %>%
  filter(n_missing >= 2) %>%
  select(employee_id, monthly_income, stock_option_level,
         salary_hike_pct, years_at_company, work_mode)

# iii. are the numeric vars right-skewed? if mean > median, impute with
# median not mean, otherwise the imputed values drift upward.
raw %>% mask_invalid() %>%
  summarise(across(c(monthly_income, salary_hike_pct, years_at_company, age),
                   list(mean = ~ mean(.x, na.rm = TRUE), median = ~ median(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything(), names_to = c("variable", "stat"), names_pattern = "(.*)_(mean|median)$") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(skew_gap = round(mean - median, 2),
         skew_pct = round(100 * (mean - median) / median, 1))

# every step below logs what was done in cleaning_log, so the whole sequence is
# traceable
cleaning_log <- tibble(step = character(), variable = character(), n_affected = integer(), action = character())
log_add <- function(step, variable, n, action) {
  cleaning_log <<- add_row(cleaning_log, step = step, variable = variable,
                           n_affected = as.integer(n), action = action)
}
mode_val <- function(x) names(sort(table(x), decreasing = TRUE))[1]
row_log  <- tibble(step = "Raw data", rows = nrow(raw))

# 2a. text cleanup. trim whitespace on every character column, then turn
# "N/A" and blank strings into real NA so missingness is counted once.
data <- raw %>% mutate(across(where(is.character), ~ trimws(.x)))
n_placeholder <- sum(data$work_mode == "N/A", na.rm = TRUE)
data <- data %>% mutate(across(where(is.character), ~ na_if(na_if(.x, "N/A"), "")))
log_add("2a", "work_mode", n_placeholder, "Placeholder 'N/A' converted to NA")

# 2b. duplicates. 1b confirmed every repeated id is an exact copy and the
# evidence in 1f showed duplicates behave the same as unique rows, so dropping
# them loses nothing.
stopifnot(nrow(distinct(data)) == n_distinct(data$employee_id))
data <- distinct(data)
log_add("2b", "all", n_dup_rows, "Exact duplicate rows removed")
row_log <- add_row(row_log, step = "After removing duplicates", rows = nrow(data))

# 2c. invalid values -> NA. uses the same ranges as the audit.
# option was to "correct" them but we can't recover a true value from a bad one,
# so setting to NA then imputing keeps the row
for (i in seq_len(nrow(valid_ranges))) {
  v   <- valid_ranges$variable[i]
  bad <- !is.na(data[[v]]) & (data[[v]] < valid_ranges$lo[i] | data[[v]] > valid_ranges$hi[i])
  if (any(bad)) {
    log_add("2c", v, sum(bad), paste0("Out-of-range values set to NA (e.g. ",
                                      paste(head(unique(data[[v]][bad]), 3), collapse = ", "), ")"))
    data[[v]][bad] <- NA
  }
}
for (v in names(valid_categories)) {
  bad <- !is.na(data[[v]]) & !data[[v]] %in% valid_categories[[v]]
  if (any(bad)) {
    log_add("2c", v, sum(bad), "Invalid category set to NA")
    data[[v]][bad] <- NA
  }
}

# flag rows that will end up with at least one imputed value.
# used later in 2j for the sensitivity check.
imp_vars <- c("age", "job_level", "monthly_income", "stock_option_level",
              "salary_hike_pct", "years_at_company", "work_mode")
data <- data %>% mutate(flag_imputed = if_any(all_of(imp_vars), is.na))

# 2d. imputation. order matters - job_level first because income depends on it.
# for job_level: use the nearest median income band, fall back to the role's
# most common level, then the overall mode.
lvl_income <- data %>% filter(!is.na(job_level), !is.na(monthly_income)) %>%
  group_by(job_level) %>% summarise(med = median(monthly_income), .groups = "drop")
level_from_income <- function(inc) {
  vapply(inc, function(i) if (is.na(i)) NA_integer_
         else as.integer(lvl_income$job_level[which.min(abs(lvl_income$med - i))]), integer(1))
}
role_level <- data %>% filter(!is.na(job_level)) %>% count(job_role, job_level) %>%
  group_by(job_role) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>%
  transmute(job_role, role_level = as.integer(job_level))
n_lvl_na <- sum(is.na(data$job_level))
data <- data %>%
  left_join(role_level, by = "job_role") %>%
  mutate(job_level = as.integer(job_level),
         job_level = coalesce(job_level, level_from_income(monthly_income)),
         job_level = coalesce(job_level, role_level),
         job_level = coalesce(job_level, as.integer(mode_val(job_level)))) %>%
  select(-role_level)
if (n_lvl_na > 0) log_add("2d", "job_level", n_lvl_na, "Imputed from nearest median income level (fallback: role mode)")

# numeric imputation. evidence in 1f showed mean > median for income and age,
# so median keeps the imputed values near the centre.
imp_group_median <- function(df, v) {
  df %>% group_by(job_level) %>%
    mutate("{v}" := coalesce(as.numeric(.data[[v]]), median(.data[[v]], na.rm = TRUE))) %>% ungroup()
}
n_na <- sapply(c("monthly_income", "age", "salary_hike_pct", "years_at_company", "stock_option_level", "work_mode"),
               function(v) sum(is.na(data[[v]])))
data <- data %>% imp_group_median("monthly_income") %>% imp_group_median("age") %>%
  mutate(
    salary_hike_pct    = coalesce(as.numeric(salary_hike_pct), median(salary_hike_pct, na.rm = TRUE)),
    years_at_company   = coalesce(as.numeric(years_at_company), median(years_at_company, na.rm = TRUE)),
    stock_option_level = coalesce(as.numeric(stock_option_level), as.numeric(mode_val(stock_option_level))),
    work_mode          = coalesce(work_mode, mode_val(work_mode)),
    age                = round(age))
for (v in names(n_na)[n_na > 0]) log_add("2d", v, n_na[[v]],
                                         ifelse(v %in% c("stock_option_level", "work_mode"), "Imputed with mode",
                                                ifelse(v %in% c("monthly_income", "age"),
                                                       "Imputed with median within job_level", "Imputed with median")))
row_log <- add_row(row_log, step = "After invalid -> NA and imputation (no rows dropped)", rows = nrow(data))

# 2e. fix the tenure logic. years_at_company should never exceed total career.
# three options were on the table: 1 drop the row, 2 impute tenure,
# 3 raise total_working_years to match tenure. went with 3 because the
# gaps are small and tenure feeds the promotion and early-warning analysis
# dropping the rows would throw away real attrition signal.
n_ten <- sum(data$years_at_company > data$total_working_years)
data <- data %>%
  mutate(flag_tenure_fixed = years_at_company > total_working_years,
         total_working_years = pmax(total_working_years, years_at_company))
log_add("2e", "total_working_years", n_ten, "Raised to years_at_company (flag_tenure_fixed = TRUE)")

# same idea for promotion, can't be longer than tenure itself.
n_promo <- sum(data$years_since_promotion > data$years_at_company)
data <- data %>% mutate(years_since_promotion = pmin(years_since_promotion, years_at_company))
if (n_promo > 0) log_add("2e", "years_since_promotion", n_promo, "Capped at years_at_company")

# 2f. remote workers with commute > 0. 1d showed this is most of the remote
# group, so it looks like occasional office trips rather than error.
# commute isn't used in any analysis, so flag it and leave the values alone.
data <- data %>% mutate(flag_remote_commute = work_mode == "Remote" & commute_minutes > 0)
log_add("2f", "commute_minutes", sum(data$flag_remote_commute),
        "Flagged only (small values; commute not used in analysis)")

# 2g. convert types and set ordered factors. also make a numeric attrition flag.
data <- data %>%
  mutate(
    age             = as.numeric(age),
    education_level = factor(education_level, levels = c("High School", "Bachelors", "Masters", "PhD"), ordered = TRUE),
    department      = factor(department),
    work_mode       = factor(work_mode, levels = c("Onsite", "Hybrid", "Remote")),
    gender          = factor(gender),
    marital_status  = factor(marital_status),
    uses_ai_tools_at_work = as.logical(uses_ai_tools_at_work),
    ai_tool_label   = factor(uses_ai_tools_at_work, levels = c(FALSE, TRUE),
                             labels = c("Does not use AI tools", "Uses AI tools")),
    attrition       = factor(attrition, levels = c("No", "Yes")),
    attrition_flag  = as.integer(attrition == "Yes")  # 1/0 for rates and correlation
  )

# 2h. derived variables. bands that will be used in visuals and the early-warning
# flags are simple thresholds
data <- data %>%
  mutate(
    promo_band    = cut(years_since_promotion, c(-Inf, 1, 3, Inf), c("0-1 yrs", "1-3 yrs", "3+ yrs")),
    burnout_band  = cut(burnout_score, c(0, 3, 6, 10), c("Low (1-3)", "Medium (3-6)", "High (6-10)")),
    support_band  = cut(manager_support_score, c(0, 4, 7, 10), c("Low (1-4)", "Medium (4-7)", "High (7-10)")),
    ai_risk_band  = cut(perceived_ai_job_risk, c(0, 3.5, 6.5, 10), c("Low (<=3.5)", "Medium (3.5-6.5)", "High (>6.5)")),
    overtime_band = cut(overtime_hours_per_week, c(-Inf, 0, 5, 10, Inf), c("None", "1-5 hrs", "5-10 hrs", "10+ hrs")),
    tenure_band   = cut(years_at_company, c(-Inf, 2, 5, 10, Inf), c("<2 yrs", "2-5 yrs", "5-10 yrs", "10+ yrs"), right = FALSE),
    job_level_f   = factor(job_level, levels = 1:5),
    # early-warning flags - 1 means the at-risk condition is true
    ew_burnout   = as.integer(burnout_score > 6),
    ew_support   = as.integer(manager_support_score <= 4),
    ew_promo     = as.integer(years_since_promotion > 3),
    ew_overtime  = as.integer(overtime_hours_per_week > 10),
    ew_ai_risk   = as.integer(perceived_ai_job_risk > 6.5),
    risk_flags   = ew_burnout + ew_support + ew_promo + ew_overtime + ew_ai_risk
  )

# 2i. post-cleaning validation.
stopifnot(
  !any(duplicated(data$employee_id)),
  all(map_lgl(seq_len(nrow(valid_ranges)), function(i) {
    v <- valid_ranges$variable[i]
    all(!is.na(data[[v]]) & data[[v]] >= valid_ranges$lo[i] & data[[v]] <= valid_ranges$hi[i])
  })),
  all(data$years_at_company <= data$total_working_years),
  all(data$years_since_promotion <= data$years_at_company),
  !anyNA(data[, c(valid_ranges$variable, "department", "work_mode", "attrition", "gender")])
)
colSums(is.na(data))
cleaning_log
row_log
cat("Raw rows:", nrow(raw), "| Clean rows:", nrow(data),
    "| rows containing >=1 imputed value:", sum(data$flag_imputed),
    sprintf("(%.1f%%)\n", 100 * mean(data$flag_imputed)))

write_csv(cleaning_log, "tables/cleaning_log.csv")
write_csv(row_log,      "tables/cleaning_row_log.csv")
write_csv(data,         "clean_attrition_data.csv")  # the cleaned dataset (required export)

# 2j. impact of the cleaning choices. compares raw vs cleaned vs complete-case
# complete case being if bad rows were just removed (e.g instead of imputing or
# fixing the tenure)
corr_with <- function(df, vars, y) map_dbl(vars, ~ suppressWarnings(cor(df[[.x]], df[[y]], use = "complete.obs")))
cmp_vars <- c("age", "burnout_score", "perceived_ai_job_risk", "manager_support_score",
              "overtime_hours_per_week", "years_since_promotion", "monthly_income")
raw_f <- raw %>% mutate(attrition_flag = as.integer(attrition == "Yes"))
cc    <- data %>% filter(!flag_imputed, !flag_tenure_fixed)  # complete-case alternative
sensitivity <- tibble(
  variable   = cmp_vars,
  r_raw      = round(corr_with(raw_f, cmp_vars, "attrition_flag"), 3),
  r_clean    = round(corr_with(data,  cmp_vars, "attrition_flag"), 3),
  r_complete_case = round(corr_with(cc, cmp_vars, "attrition_flag"), 3)
) %>% mutate(abs_change_raw_vs_clean = round(abs(r_clean - r_raw), 3))
sensitivity

headline_impact <- tibble(
  metric = c("Rows", "Attrition rate (%)", "Mean age", "Mean monthly_income", "Mean years_at_company"),
  raw    = c(nrow(raw), 100 * mean(raw_f$attrition_flag), mean(raw$age),
             mean(raw$monthly_income, na.rm = TRUE), mean(raw$years_at_company, na.rm = TRUE)),
  clean  = c(nrow(data), 100 * mean(data$attrition_flag), mean(data$age),
             mean(data$monthly_income), mean(data$years_at_company)),
  complete_case = c(nrow(cc), 100 * mean(cc$attrition_flag), mean(cc$age),
                    mean(cc$monthly_income), mean(cc$years_at_company))
) %>% mutate(across(where(is.numeric), ~ round(.x, 2)))
headline_impact

# something i can add to my report (answers whether my cleaning majorly changed
# the data and affected the headline story)
delta_rate  <- round(headline_impact$clean[2] - headline_impact$raw[2], 2)
max_delta_r <- max(sensitivity$abs_change_raw_vs_clean)
cat(sprintf("After cleaning, the headline attrition rate changed by %+.2f pp and the largest change in any driver correlation was %.3f, so conclusions drawn are not sensitive to cleaning approach.\n",
            delta_rate, max_delta_r))

write_csv(sensitivity,     "tables/cleaning_sensitivity_correlations.csv")
write_csv(headline_impact, "tables/cleaning_headline_impact.csv")

# ---- 3. SUMMARY STATISTICS ----------------------------------------------------
# key variables for the analysis
key_vars <- c("age", "monthly_income", "years_at_company", "years_since_promotion",
              "overtime_hours_per_week", "manager_support_score", "burnout_score",
              "engagement_score", "work_life_balance_score", "performance_rating",
              "last_review_score", "perceived_ai_job_risk")

summary_tbl <- data %>%
  select(all_of(key_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "v") %>%
  group_by(Variable) %>%
  summarise(n = sum(!is.na(v)), Mean = mean(v), SD = sd(v), Min = min(v),
            Q1 = quantile(v, .25), Median = median(v), Q3 = quantile(v, .75), Max = max(v)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  arrange(match(Variable, key_vars))

# add the outcome as its own row so the table shows the base rate the rest is
# compared against.
attrition_row <- tibble(
  Variable = "attrition_rate_pct",
  n    = nrow(data),
  Mean = round(100 * mean(data$attrition_flag), 2),
  SD   = round(100 * sd(data$attrition_flag), 2),
  Min  = 0, Q1 = 0, Median = 0, Q3 = 0, Max = 100
)
summary_tbl <- bind_rows(summary_tbl, attrition_row)
summary_tbl
write_csv(summary_tbl, "tables/summary_stats.csv")

# NOTE: The below are calculations used for the plotting of figures below
# categorical breakdown of the workforce
cat_profile <- map_dfr(c("department", "work_mode", "gender", "education_level", "ai_tool_label", "attrition"),
                       function(v) data %>% count(group = as.character(.data[[v]])) %>%
                         mutate(variable = v, pct = round(100 * n / sum(n), 1), .before = 1))
cat_profile
write_csv(cat_profile, "tables/categorical_profile.csv")

# attrition rate by group, with 95% CIs since some cells are small
wilson_lo <- function(x, n, z = 1.96) { p <- x / n; ((p + z^2 / (2 * n)) - z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / (1 + z^2 / n) }
wilson_hi <- function(x, n, z = 1.96) { p <- x / n; ((p + z^2 / (2 * n)) + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / (1 + z^2 / n) }
rate_by <- function(df, var) {
  df %>% mutate(group = as.character(.data[[var]])) %>% group_by(group) %>%
    summarise(n = n(), leavers = sum(attrition_flag)) %>%
    mutate(variable = var, attrition_rate_pct = round(100 * leavers / n, 1),
           ci_lo = round(100 * wilson_lo(leavers, n), 1), ci_hi = round(100 * wilson_hi(leavers, n), 1),
           .before = 1)
}
overall_rate <- round(100 * mean(data$attrition_flag), 1); overall_rate
rate_vars <- c("department", "job_role", "work_mode", "ai_tool_label", "job_level_f", "education_level",
               "tenure_band", "promo_band", "burnout_band", "support_band", "ai_risk_band", "overtime_band", "risk_flags")
rates_all <- map_dfr(rate_vars, ~ rate_by(data, .x))
rates_all %>% filter(variable != "job_role") %>% print(n = Inf)
write_csv(rates_all, "tables/attrition_rates_by_group.csv")

# department x work mode
rate_dept_mode <- data %>% group_by(department, work_mode) %>%
  summarise(n = n(), attrition_rate_pct = round(100 * mean(attrition_flag), 1))
write_csv(rate_dept_mode, "tables/attrition_dept_by_work_mode.csv")

# perceived ai risk across segments
ai_risk_seg <- map_dfr(c("department", "job_role", "tenure_band", "work_mode", "ai_tool_label"), function(v)
  data %>% mutate(group = as.character(.data[[v]])) %>% group_by(group) %>%
    summarise(n = n(), mean_ai_risk = round(mean(perceived_ai_job_risk), 2),
              sd = round(sd(perceived_ai_job_risk), 2), pct_high_risk = round(100 * mean(ew_ai_risk), 1)) %>%
    mutate(variable = v, .before = 1))
ai_risk_seg %>% filter(variable != "job_role") %>% print(n = Inf)
write_csv(ai_risk_seg, "tables/ai_risk_by_segment.csv")

# means for leavers vs stayers on the main driver variables
driver_vars <- c("burnout_score", "perceived_ai_job_risk", "manager_support_score",
                 "overtime_hours_per_week", "years_since_promotion", "engagement_score",
                 "work_life_balance_score", "performance_rating", "last_review_score",
                 "salary_hike_pct", "monthly_income")
group_means <- data %>% group_by(attrition) %>%
  summarise(across(all_of(driver_vars), ~ round(mean(.x), 2)))
group_means
write_csv(group_means, "tables/means_by_attrition.csv")

# ---- 4. CORRELATION ANALYSIS --------------------------------------------------
# builds the matrix used for the correlation heatmap (fig 1).
cont_vars <- c("attrition_flag", "burnout_score", "perceived_ai_job_risk", "manager_support_score",
               "overtime_hours_per_week", "years_since_promotion", "engagement_score",
               "work_life_balance_score", "last_review_score", "monthly_income", "salary_hike_pct")
cor_mat <- cor(data[, cont_vars], use = "pairwise.complete.obs")

# strongest predictor-predictor pairs. used in the suitability section.
cm <- cor_mat[setdiff(cont_vars, "attrition_flag"), setdiff(cont_vars, "attrition_flag")]
cm[lower.tri(cm, diag = TRUE)] <- NA
top_pairs <- as.data.frame(as.table(cm)) %>% filter(!is.na(Freq)) %>%
  setNames(c("var1", "var2", "r")) %>% mutate(r = round(r, 2)) %>% arrange(desc(abs(r))) %>% head(10)
top_pairs

# ---- 5. VISUALISATIONS --------------------------------------------------------
theme_set(theme_minimal(base_size = 12) +
            theme(plot.title = element_text(face = "bold"),
                  plot.subtitle = element_text(colour = "grey35"),
                  legend.position = "bottom"))
cols <- c("No" = "#4C78A8", "Yes" = "#E4572E")
save_fig <- function(p, name, w = 8, h = 5) ggsave(file.path("figures", name), p, width = w, height = h, dpi = 300)

# ============ 1. CORRELATION HEATMAP ON CONT. VARIABLES =======================
pretty <- function(x) str_to_sentence(str_replace_all(x, "_", " "))
fig1 <- as.data.frame(as.table(cor_mat)) %>% setNames(c("v1", "v2", "r")) %>%
  mutate(v1 = pretty(v1), v2 = pretty(v2)) %>%
  ggplot(aes(v1, v2, fill = r)) +
  geom_tile(colour = "white") + geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", limits = c(-1, 1), name = "r") +
  labs(title = "Correlation matrix of continuous variables", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
fig1
save_fig(fig1, "fig1_correlation_heatmap.png", w = 9, h = 7)

# ========== 2. ATTRITION RATE: FAVOURABLE VS AT-RISK LEVEL OF EACH DRIVER =========
contrast_def <- tribble(
  ~driver,                 ~var,            ~fav,           ~risk,
  "Burnout",               "burnout_band",  "Low (1-3)",    "High (6-10)",
  "Manager support",       "support_band",  "High (7-10)",  "Low (1-4)",
  "Overtime",              "overtime_band", "1-5 hrs",      "10+ hrs",
  "Time since promotion",  "promo_band",    "0-1 yrs",      "3+ yrs",
  "Perceived AI job risk", "ai_risk_band",  "Low (<=3.5)",  "High (>6.5)"
)
driver_bars <- pmap_dfr(contrast_def, function(driver, var, fav, risk) {
  data %>%
    mutate(band = as.character(.data[[var]])) %>%
    filter(band %in% c(fav, risk)) %>%
    group_by(end = if_else(band == risk, "At-risk end", "Favourable end")) %>%
    summarise(n = n(), leavers = sum(attrition_flag)) %>%
    mutate(driver = driver,
           rate = 100 * leavers / n,
           lo   = 100 * wilson_lo(leavers, n),
           hi   = 100 * wilson_hi(leavers, n))
})
# order drivers from largest to smallest gap (left to right)
driver_order <- driver_bars %>%
  select(driver, end, rate) %>%
  pivot_wider(names_from = end, values_from = rate) %>%
  mutate(gap = `At-risk end` - `Favourable end`) %>%
  arrange(desc(gap)) %>% pull(driver)
driver_bars <- driver_bars %>%
  mutate(driver = factor(driver, levels = driver_order),
         end    = factor(end, levels = c("Favourable end", "At-risk end")))

fig_2 <- ggplot(driver_bars, aes(driver, rate, fill = end)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(0.8), width = 0.2) +
  geom_text(aes(y = hi + 1.5, label = paste0(round(rate, 1), "%")),
            position = position_dodge(0.8), size = 3.4) +
  geom_hline(yintercept = overall_rate, linetype = "dashed", colour = "grey40") +
  scale_x_discrete(labels = scales::label_wrap(12)) +
  scale_fill_manual(values = c("Favourable end" = "#4C78A8", "At-risk end" = "#E4572E"), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Attrition Rate: Favourable vs At-Risk Level of Each Driver",
       subtitle = paste0("Attrition rate at each end of the driver (whiskers = 95% CI); dashed line = company rate, ",
                         overall_rate, "%"),
       x = NULL, y = "Attrition rate (%)",
       caption = paste0("Favourable / at-risk: burnout 1-3 / >6; manager support 7-10 / <=4; overtime 1-5 / >10 h per week;\n",
                        "promoted <=1 / >3 yrs ago; perceived AI risk <=3.5 / >6.5"))
fig_2
save_fig(fig_2, "fig2_driver_grouped_bars.png", w = 10, h = 5.5)

# ============ 3. BURNOUT SCORE VS ATTRITION RATE ==============================
burn_bins <- data %>% mutate(bin = round(burnout_score)) %>% group_by(bin) %>%
  summarise(rate = mean(attrition_flag), n = n())
fig3 <- ggplot() +
  geom_smooth(data = data, aes(burnout_score, attrition_flag), method = "glm",
              method.args = list(family = "binomial"), colour = "#E4572E", fill = "#E4572E", alpha = 0.15) +
  geom_point(data = burn_bins, aes(bin, rate, size = n), colour = "#4C78A8", alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_size_continuous(name = "Employees in bin") +
  labs(title = "Burnout score vs Attrition Rate",
       subtitle = "Dots = observed rate per rounded burnout score; line = logistic trend with 95% band",
       x = "Burnout score (1-10)", y = "Attrition rate")
fig3
save_fig(fig3, "fig3_burnout_attrition.png")

# ============ 4. NUMBER OF EARLY WARNING FLAGS VS ATTRITION RATE ==============
fig4 <- rate_by(data, "risk_flags") %>%
  ggplot(aes(factor(group), attrition_rate_pct)) +
  geom_col(fill = "#E4572E", width = 0.7) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.2) +
  geom_text(aes(y = ci_hi + 2, label = paste0(attrition_rate_pct, "% (n=", n, ")")), size = 3.3) +
  geom_hline(yintercept = overall_rate, linetype = "dashed") +
  labs(title = "Number of Early-Warning Flags vs Attrition Rate",
       subtitle = "Flags: burnout >6, manager support <=4, >3 yrs since promotion, overtime >10 h/wk, AI risk >6.5",
       x = "Number of flags", y = "Attrition rate (%)")
fig4
save_fig(fig4, "fig4_early_warning_flags.png")

# ============ 5. ATTRITION RATE BY BURNOUT LEVEL, PERCEIVED AI JOB RISK AND PERFORMANCE RATING ============
fig_5_data <- data %>%
  mutate(perf_group = if_else(performance_rating >= 4, "Performance Rating 4-5 (high)", "Performance Rating 2-3 (low)")) %>%
  group_by(burnout_band, ai_risk_band, perf_group) %>%
  summarise(n = n(), leavers = sum(attrition_flag)) %>%
  ungroup() %>%
  mutate(rate = 100 * leavers / n,
         lo   = 100 * wilson_lo(leavers, n),
         hi   = 100 * wilson_hi(leavers, n))

fig_5 <- ggplot(fig_5_data, aes(ai_risk_band, rate, fill = perf_group)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(0.8), width = 0.2) +
  geom_text(aes(y = hi + 2, label = paste0("n=", n)), position = position_dodge(0.8), size = 2.6) +
  facet_wrap(~ burnout_band, nrow = 1,
             labeller = as_labeller(function(x) paste("Burnout:", x))) +
  scale_x_discrete(labels = function(x) str_extract(x, "^[A-Za-z]+")) +  # Low / Medium / High
  scale_fill_manual(values = c("Performance Rating 2-3 (low)" = "#E4572E", "Performance Rating 4-5 (high)" = "#4C78A8"), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Attrition Rate by Burnout Level, Perceived AI Job Risk and Performance Rating",
       subtitle = "Attrition rate by perceived AI risk and performance group, within each burnout level (whiskers = 95% CI)",
       x = "Perceived AI job risk", y = "Attrition rate (%)")
fig_5
save_fig(fig_5, "fig5_burnout_panels.png", w = 11, h = 5)

# ============ 6. ATTRITION RATE AND KEY RISK DRIVER BY DEPARTMENT =============
dept_metrics <- data %>%
  group_by(department) %>%
  summarise(n = n(),
            `Attrition rate (%)`    = 100 * mean(attrition_flag),
            `Perceived AI risk`     = mean(perceived_ai_job_risk),
            `Overtime (h/week)`     = mean(overtime_hours_per_week),
            `Burnout score`         = mean(burnout_score),
            `Years since promotion` = mean(years_since_promotion))

dept_levels <- dept_metrics %>% arrange(`Attrition rate (%)`) %>%       # highest attrition ends up on top
  mutate(label = paste0(department, " (n=", n, ")")) %>% pull(label)

dept_long <- dept_metrics %>%
  mutate(label = factor(paste0(department, " (n=", n, ")"), levels = dept_levels)) %>%
  select(-department, -n) %>%
  pivot_longer(-label, names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, levels = c("Attrition rate (%)", "Perceived AI risk", "Overtime (h/week)",
                                            "Burnout score", "Years since promotion"))) %>%
  group_by(metric) %>%
  mutate(z = as.numeric(scale(value))) %>%                              # colour = relative to other departments
  ungroup()

fig_6 <- ggplot(dept_long, aes(metric, label, fill = z)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.1f", value)), size = 3.4) +
  scale_x_discrete(position = "top", labels = scales::label_wrap(12)) +
  scale_fill_gradient2(low = "#4C78A8", mid = "white", high = "#D7301F", midpoint = 0,
                       name = "Relative to\nother departments") +
  labs(title = "Attrition Rate and Key Risk Drivers by Department",
       subtitle = "Departments ranked by attrition (top = highest); red = higher than other departments, blue = lower",
       x = NULL, y = NULL)
fig_6
save_fig(fig_6, "fig6_department_heatmap.png", w = 10, h = 5.5)


# ---- 6. STATISTICAL EVIDENCE + EARLY-WARNING INDICATORS -----------------------
# 6a. categorical variables association with attrition
cramers_v <- function(tab) { x <- suppressWarnings(chisq.test(tab)); unname(sqrt(x$statistic / (sum(tab) * (min(dim(tab)) - 1)))) }
test_cat <- map_dfr(c("department", "work_mode", "ai_tool_label", "job_level_f", "education_level", "gender"), function(v) {
  tab <- table(data[[v]], data$attrition)
  x <- suppressWarnings(chisq.test(tab))
  tibble(variable = v, chi2 = round(unname(x$statistic), 2), df = unname(x$parameter),
         p_value = signif(x$p.value, 3), cramers_v = round(cramers_v(tab), 3))
}) %>% mutate(p_holm = signif(p.adjust(p_value, "holm"), 3))
test_cat
chisq.test(table(data$job_role, data$attrition), simulate.p.value = TRUE, B = 5000)
write_csv(test_cat, "tables/tests_categorical.csv")

# 6b. numeric drivers: shows that burnout is the strongest driver
cohens_d <- function(a, b) { sp <- sqrt(((length(a) - 1) * var(a) + (length(b) - 1) * var(b)) / (length(a) + length(b) - 2)); (mean(a) - mean(b)) / sp }
test_num <- map_dfr(driver_vars, function(v) {
  a <- data[[v]][data$attrition == "Yes"]; b <- data[[v]][data$attrition == "No"]
  t <- t.test(a, b)
  tibble(variable = v, mean_leavers = round(mean(a), 2), mean_stayers = round(mean(b), 2),
         diff = round(mean(a) - mean(b), 2), cohens_d = round(cohens_d(a, b), 2), p_value = signif(t$p.value, 3))
}) %>% mutate(p_holm = signif(p.adjust(p_value, "holm"), 3)) %>% arrange(desc(abs(cohens_d)))
test_num
write_csv(test_num, "tables/tests_numeric_drivers.csv")

# 6c. early-warning indicator table
ew_vars <- c(ew_burnout = "Burnout > 6", ew_support = "Manager support <= 4",
             ew_promo = "> 3 yrs since promotion", ew_overtime = "Overtime > 10 h/wk",
             ew_ai_risk = "Perceived AI risk > 6.5")
early_warning_tbl <- imap_dfr(ew_vars, function(label, v) {
  f <- data[[v]] == 1
  tibble(indicator = label, pct_employees_flagged = round(100 * mean(f), 1),
         attrition_flagged_pct = round(100 * mean(data$attrition_flag[f]), 1),
         attrition_not_flagged_pct = round(100 * mean(data$attrition_flag[!f]), 1),
         relative_risk = round(mean(data$attrition_flag[f]) / mean(data$attrition_flag[!f]), 2),
         share_of_leavers_captured_pct = round(100 * sum(data$attrition_flag[f]) / sum(data$attrition_flag), 1))
}) %>% arrange(desc(relative_risk))
early_warning_tbl
write_csv(early_warning_tbl, "tables/early_warning_indicators.csv")

# ---- 7. DATASET SUITABILITY AND REMAINING LIMITATIONS -------------------------
# quick summary of whether the clean data is fit for the next piece of work
# and what limitations are still sitting there.
min_cell <- min(rate_dept_mode$n)
suitability <- tibble(
  check = c("Clean sample size (rows)", "Leavers (events)", "Attrition prevalence (%)",
            "Rows containing an imputed value (%)", "Rows with tenure correction (%)",
            "Remote employees with commute > 0 (flagged, retained)",
            "Smallest department x work-mode cell (n)",
            "Strongest predictor-predictor |r|",
            "Dictionary gender categories never observed",
            "performance_rating levels observed",
            "Time dimension / leave date available?"),
  value = c(nrow(data), sum(data$attrition_flag), overall_rate,
            round(100 * mean(data$flag_imputed), 1), round(100 * mean(data$flag_tenure_fixed), 1),
            sum(data$flag_remote_commute), min_cell,
            max(abs(top_pairs$r)),
            paste(setdiff(valid_categories$gender, unique(as.character(data$gender))), collapse = ", "),
            paste(sort(unique(data$performance_rating)), collapse = ", "),
            "No - single cross-section"),
  implication = c("Adequate for descriptive and predictive work",
                  "Enough events for a modest number of predictors",
                  "Imbalanced outcome: accuracy alone is misleading; use recall/precision/AUC",
                  "Low share, so imputation has little effect (see sensitivity table)",
                  "Small, but indicates measurement error in tenure or career length",
                  "Commute for remote staff is not reliable; exclude from models",
                  "Treat very small cells cautiously (wide confidence intervals)",
                  "Check multicollinearity before modelling",
                  "Dictionary lists Non-binary but data has none: cannot analyse that group",
                  "No rating of 1: range restriction limits inference about poor performers",
                  "Associations only; cannot infer causality or timing of leaving"))
suitability
write_csv(suitability, "tables/suitability_checks.csv")

# reproducibility info
sessionInfo()
