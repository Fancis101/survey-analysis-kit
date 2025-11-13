library(tidyverse)
library(janitor)


set.seed(2025)
n <- 200
set.seed(2025)
library(tibble)
library(dplyr)

n <- 200

sample_df <- tibble(
  id = 1:n,
  gender = sample(c("Male", "Female", "Other"), n, replace = TRUE, prob = c(0.47, 0.50, 0.03)),
  education = sample(c("Primary", "Secondary", "Tertiary", "Postgraduate"), n, replace = TRUE, prob = c(0.10, 0.35, 0.45, 0.10)),
  income = sample(c("<1000", "1000-2999", "3000-4999", "5000+"), n, replace = TRUE, prob = c(0.25, 0.40, 0.25, 0.10)),
  # age as categorical (bins)
  age_years = sample(18:65, n, replace = TRUE),
  age_group = cut(age_years, breaks = c(17, 24, 34, 44, 54, 65),
                  labels = c("18-24", "25-34", "35-44", "45-54", "55-65"), include.lowest = TRUE),
  # 3 categorical independent variables
  employment_status = sample(c("Employed", "Self-employed", "Student", "Unemployed"), n, replace = TRUE, prob = c(0.50, 0.18, 0.22, 0.10)),
  household_size = sample(1:8, n, replace = TRUE, prob = c(0.05,0.15,0.25,0.25,0.15,0.08,0.04,0.03)),
  urban_rural = sample(c("Urban", "Rural"), n, replace = TRUE, prob = c(0.65, 0.35)),
  # 2 new continuous independent variables
  monthly_expenditure = round(rnorm(n, mean = 3500, sd = 900), 2),   # continuous (GHS or arbitrary units)
  distance_to_market_km = round(rexp(n, rate = 0.2), 2),            # positive skewed distance in km
  # 2 dependent variables
  satisfaction_score = round(pmin(pmax(rnorm(n, mean = 70, sd = 15) - 
                                         (as.numeric(age_group) - 3)*2 + ifelse(income == "5000+", 5, 0), 0), 100)), # 0-100
  purchase_intent = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.58, 0.42))
)

glimpse(sample_df)
View(sample_df)



# -----------------------------------------------------------------------------
# Provide survey summary by selecting demographic variables
# ------------------------------------------------------------------------------

library(tidyverse); library(janitor)

# Required packages
pkgs <- c("tidyverse", "janitor")
new <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new)
library(tidyverse); library(janitor)

# Vectorized survey summary for categorical variables only
survey_summary_categorical <- function(df,
                                       vars = NULL,            # character names or integer indices (required)
                                       auto_detect = FALSE,    # not used here; provide vars explicitly
                                       treat_integer_as_categorical_if_leq = 10) {
  
  if (!is.data.frame(df)) stop("df must be a data frame")
  df_names <- names(df)
  
  # resolve vars
  if (is.null(vars) && auto_detect) stop("auto_detect = TRUE not implemented; provide vars explicitly")
  if (is.numeric(vars)) {
    if (!all(vars %in% seq_along(df_names))) stop("Some numeric indices in vars are invalid")
    vars_to_use <- df_names[vars]
  } else if (is.character(vars)) {
    if (!all(vars %in% df_names)) stop("Some names in vars not found in df")
    vars_to_use <- vars
  } else {
    stop("Provide vars as a character vector of names or numeric indices")
  }
  
  # long format for selected categorical vars
  long_cat <- df %>%
    select(all_of(vars_to_use)) %>%
    mutate(across(everything(), ~ as.character(.))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value")
  
  # normalize NA-like strings and empties
  long_cat <- long_cat %>%
    mutate(value = ifelse(is.na(value) | trimws(tolower(value)) %in% c("", "na", "n/a", "none"), NA_character_, value))
  
  # compute categorical counts/proportions in a vectorized grouped pipeline
  categorical_summary_tbl <- long_cat %>%
    group_by(variable) %>%
    # compute missing per variable once
    mutate(missing = sum(is.na(value))) %>%
    filter(!is.na(value)) %>%
    group_by(variable, value, .drop = FALSE) %>%
    summarise(count = n(), .groups = "drop_last") %>%
    group_by(variable) %>%
    mutate(total_nonmissing = sum(count), proportion = round(100 * count / total_nonmissing, 1)) %>%
    ungroup() %>%
    rename(category = value) %>%
    select(variable, category, count, proportion, total_nonmissing, missing)
  
  # Handle variables that had all values missing (ensure they appear)
  missing_vars <- setdiff(vars_to_use, unique(categorical_summary_tbl$variable))
  if (length(missing_vars) > 0) {
    missing_rows <- tibble(
      variable = missing_vars,
      category = NA_character_,
      count = 0L,
      proportion = NA_real_,
      total_nonmissing = 0L,
      missing = map_int(missing_vars, ~ sum(is.na(df[[.x]])))
    )
    categorical_summary_tbl <- bind_rows(categorical_summary_tbl, missing_rows)
  }
  
  # preserve requested order and clean names
  result_tbl <- categorical_summary_tbl %>%
    mutate(variable = factor(variable, levels = vars_to_use)) %>%
    arrange(variable, desc(ifelse(!is.na(count), count, 0)))
  
  janitor::clean_names(result_tbl)
}

# Example usage:
vars_sel <- c("gender","education","income","age_group","employment_status","household_size","urban_rural")
res <- survey_summary_categorical(sample_df, vars = vars_sel)
print(res)






# -----------------------------------------------------------------------------
# Provide descriptive statistics by selecting numeric variables
# ----------------------------------------------------------------------------

library(tidyverse)
library(broom)
library(e1071)     # skewness, kurtosis
library(nortest)   # ad test (Anderson-Darling)
library(patchwork) # combine plots


library(tidyverse); library(e1071); library(nortest); library(janitor); library(ggplot2)

# Vectorized numeric diagnostics for explicitly chosen variables
numeric_diagnostics_general <- function(df,
                                        vars = NULL,                       # character names or integer indices
                                        sample_size_threshold_for_shapiro = 5000,
                                        hist_bins = 30,
                                        plot_theme = ggplot2::theme_minimal()) {
  if (!is.data.frame(df)) stop("df must be a data frame or tibble")
  df_names <- names(df)
  
  # resolve vars: if NULL, auto-detect numeric columns
  if (is.null(vars)) {
    vars_to_use <- df_names[vapply(df, is.numeric, logical(1))]
    if (length(vars_to_use) == 0) stop("No numeric variables found in the data frame")
  } else {
    if (is.numeric(vars)) {
      if (!all(vars %in% seq_along(df_names))) stop("Some numeric indices in vars are invalid")
      vars_to_use <- df_names[vars]
    } else if (is.character(vars)) {
      if (!all(vars %in% df_names)) stop("Some names in vars are not column names in df")
      vars_to_use <- vars
    } else {
      stop("vars must be NULL, a character vector of column names, or numeric indices")
    }
    # ensure selected variables are numeric (coerceable)
    non_numeric <- vars_to_use[!vapply(df[vars_to_use], function(x) is.numeric(x) || all(grepl("^\\s*-?\\d+(\\.?\\d+)?\\s*$", na.omit(as.character(x)))), logical(1))]
    if (length(non_numeric) > 0) stop("The following selected vars are not numeric or numeric-like: ", paste(non_numeric, collapse = ", "))
  }
  
  # Pivot selected variables to long form for vectorized summary computation
  long_df <- df %>%
    select(all_of(vars_to_use)) %>%
    mutate(across(everything(), ~ replace(., trimws(as.character(.)) %in% c("", "NA", "N/A", "na", "n/a"), NA))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "raw_value") %>%
    mutate(value = as.numeric(raw_value))
  
  # Summaries in one grouped operation
  summaries <- long_df %>%
    group_by(variable) %>%
    summarise(
      n = sum(!is.na(value)),
      missing = sum(is.na(value)),
      mean = ifelse(n > 0, mean(value, na.rm = TRUE), NA_real_),
      sd = ifelse(n > 1, sd(value, na.rm = TRUE), NA_real_),
      median = ifelse(n > 0, median(value, na.rm = TRUE), NA_real_),
      iqr = ifelse(n > 0, IQR(value, na.rm = TRUE), NA_real_),
      min = ifelse(n > 0, min(value, na.rm = TRUE), NA_real_),
      max = ifelse(n > 0, max(value, na.rm = TRUE), NA_real_),
      skewness = ifelse(n > 2, e1071::skewness(value, na.rm = TRUE, type = 3), NA_real_),
      kurtosis = ifelse(n > 3, e1071::kurtosis(value, na.rm = TRUE, type = 3), NA_real_),
      .groups = "drop"
    ) %>%
    mutate(
      normality_test = case_when(
        n < 3 ~ NA_character_,
        n <= sample_size_threshold_for_shapiro ~ "Shapiro-Wilk",
        n > sample_size_threshold_for_shapiro ~ "Anderson-Darling",
        TRUE ~ NA_character_
      )
    )
  
  # Run normality tests in a vectorized-safe way and attach results
  norm_tests <- long_df %>%
    filter(!is.na(value)) %>%
    group_by(variable) %>%
    summarise(
      test_name = case_when(
        n() < 3 ~ NA_character_,
        n() <= sample_size_threshold_for_shapiro ~ "Shapiro-Wilk",
        n() > sample_size_threshold_for_shapiro ~ "Anderson-Darling",
        TRUE ~ NA_character_
      ),
      statistic = list(ifelse(n() >= 3 && n() <= sample_size_threshold_for_shapiro,
                              tryCatch(shapiro.test(value)$statistic, error = function(e) NA_real_),
                              ifelse(n() > sample_size_threshold_for_shapiro,
                                     tryCatch(nortest::ad.test(value)$statistic, error = function(e) NA_real_),
                                     NA_real_))),
      p_value = list(ifelse(n() >= 3 && n() <= sample_size_threshold_for_shapiro,
                            tryCatch(shapiro.test(value)$p.value, error = function(e) NA_real_),
                            ifelse(n() > sample_size_threshold_for_shapiro,
                                   tryCatch(nortest::ad.test(value)$p.value, error = function(e) NA_real_),
                                   NA_real_))),
      .groups = "drop"
    ) %>%
    # unpack list-columns to numeric columns (safe for NA)
    mutate(statistic = map_dbl(statistic, ~ ifelse(length(.x) == 0, NA_real_, as.numeric(.x))),
           p_value = map_dbl(p_value, ~ ifelse(length(.x) == 0, NA_real_, as.numeric(.x))))
  
  summaries <- left_join(summaries, norm_tests, by = c("variable" = "variable")) %>%
    select(variable, n, missing, mean, sd, median, iqr, min, max, skewness, kurtosis, test_name, statistic, p_value)
  
  # Create plots (histogram + density, QQ) for each variable using split + map (no explicit loop)
  plot_data <- long_df %>% filter(!is.na(value)) %>% group_split(variable)
  plot_names <- map_chr(plot_data, ~ unique(.x$variable))
  
  plots <- map(plot_data, function(df_v) {
    varname <- unique(df_v$variable)
    p_hist <- ggplot(df_v, aes(x = value)) +
      geom_histogram(aes(y = ..density..), bins = hist_bins, fill = "#69b3a2", color = "white", alpha = 0.8) +
      geom_density(color = "darkred", linewidth = 1) +
      labs(title = paste0(varname, " — histogram + density"), x = varname, y = "Density") +
      plot_theme
    p_qq <- ggplot(df_v, aes(sample = value)) +
      stat_qq(size = 1.5) +
      stat_qq_line(color = "steelblue") +
      labs(title = paste0(varname, " — QQ plot"), x = "Theoretical quantiles", y = "Sample quantiles") +
      plot_theme
    list(hist = p_hist, qq = p_qq)
  }) %>% set_names(plot_names)
  
  list(summaries = janitor::clean_names(summaries), plots = plots)
}

# Example usage:
# explicitly choose numeric variable names
num_vars <- c("satisfaction_score", "monthly_expenditure", "distance_to_market_km", "age_years")
res_diag <- numeric_diagnostics_general(sample_df, vars = num_vars)
print(res_diag$summaries)
# view histogram for satisfaction_score
print(res_diag$plots$satisfaction_score$hist)
# view qq plot for monthly_expenditure
print(res_diag$plots$monthly_expenditure$qq)




# -----------------------------------------------------------------------------
# If the dependent variable is numeric and we want to check its association
# with categorical demographics:
# -----------------------------------------------------------------------------
library(tidyverse)
library(broom)
library(rstatix)    # convenient tests: kruskal_test, levene_test
library(effectsize) # effect sizes: cohens_d, eta_squared
library(ggpubr)     # nicer ggplot helpers



# Required packages
pkgs <- c("tidyverse","broom","rstatix","effectsize","e1071") 
new <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new)
library(tidyverse); library(broom); library(rstatix); library(effectsize); library(e1071)

# Generalized run_assoc_tests where you explicitly choose categorical predictors
run_assoc_tests_general <- function(df,
                                    dep_var,
                                    cat_vars = NULL,        # character names or integer indices; if NULL and auto_detect=TRUE -> auto-detect
                                    auto_detect = FALSE,    # default: require explicit selection
                                    cat_threshold = 10,     # treat integer with <= this unique values as categorical
                                    min_group_n = 3,        # minimum n per group to run parametric tests
                                    plot_theme = theme_minimal(),
                                    shapiro_max = 5000) {
  
  # Validation and resolve dep_var
  if (missing(dep_var) || !dep_var %in% names(df)) stop("Provide dep_var that exists in df")
  if (!is.numeric(df[[dep_var]])) stop("dep_var must be numeric")
  
  # Resolve cat_vars input: indices -> names, explicit names -> validate
  df_names <- names(df)
  if (is.null(cat_vars) && auto_detect) {
    is_cat <- function(x) {
      is.character(x) || is.factor(x) || (is.integer(x) && n_distinct(x[!is.na(x)]) <= cat_threshold) || (is.numeric(x) && length(unique(na.omit(x))) <= cat_threshold)
    }
    cat_vars <- df_names[vapply(df, is_cat, logical(1))]
    cat_vars <- setdiff(cat_vars, dep_var)
  } else if (!is.null(cat_vars)) {
    if (is.numeric(cat_vars)) {
      if (!all(cat_vars %in% seq_along(df_names))) stop("Some numeric indices in cat_vars are invalid")
      cat_vars <- df_names[cat_vars]
    } else if (is.character(cat_vars)) {
      if (!all(cat_vars %in% df_names)) stop("Some names in cat_vars are not column names in df")
    } else {
      stop("cat_vars must be NULL, character vector of names, or numeric indices")
    }
    # remove dep if accidentally included
    cat_vars <- setdiff(unique(cat_vars), dep_var)
  } else {
    stop("No cat_vars provided and auto_detect is FALSE. Set cat_vars or auto_detect = TRUE.")
  }
  
  # prepare outputs
  results_list <- list()
  plots_list <- list()
  
  # iterate through chosen categorical predictors
  for (var in cat_vars) {
    tmp <- df %>%
      select(all_of(dep_var), all_of(var)) %>%
      rename(dep = !!sym(dep_var), cat = !!sym(var)) %>%
      mutate(cat = as.factor(cat)) %>%
      filter(!is.na(cat) & !is.na(dep))
    
    # group summaries
    group_sum <- tmp %>%
      group_by(cat) %>%
      summarise(
        n = sum(!is.na(dep)),
        missing = sum(is.na(dep)),
        mean = ifelse(n > 0, mean(dep, na.rm = TRUE), NA_real_),
        sd = ifelse(n > 1, sd(dep, na.rm = TRUE), NA_real_),
        median = ifelse(n > 0, median(dep, na.rm = TRUE), NA_real_),
        iqr = IQR(dep, na.rm = TRUE),
        .groups = "drop"
      )
    
    total_nonmiss <- sum(!is.na(tmp$dep))
    k_groups <- n_distinct(tmp$cat[!is.na(tmp$cat)])
    small_groups <- group_sum %>% filter(n < min_group_n) %>% nrow()
    
    # normality by group (guard Shapiro by sample size)
    normal_tests <- tmp %>%
      filter(!is.na(dep)) %>%
      group_by(cat) %>%
      summarise(
        n = n(),
        shapiro_p = ifelse(n >= 3 & n <= shapiro_max, tryCatch(shapiro.test(dep)$p.value, error = function(e) NA_real_), NA_real_),
        skew = ifelse(n > 2, tryCatch(e1071::skewness(dep, na.rm = TRUE, type = 3), error = function(e) NA_real_), NA_real_),
        .groups = "drop"
      )
    
    # Levene test (robust to non-normality)
    lev <- tryCatch({
      tmp %>% filter(!is.na(dep), !is.na(cat)) %>% levene_test(dep ~ cat)
    }, error = function(e) tibble(statistic = NA_real_, p = NA_real_, df = NA_integer_))
    
    # prepare test summary template
    test_summary <- tibble(variable = var, n_total = total_nonmiss, groups = k_groups, small_groups = small_groups)
    
    # choose and run tests
    if (k_groups <= 1 | total_nonmiss < 2) {
      test_summary <- test_summary %>% mutate(test = NA_character_, statistic = NA_real_, p.value = NA_real_, method = "insufficient data", effect = NA_real_, effect_name = NA_character_)
    } else if (k_groups == 2) {
      t_res <- tryCatch(t.test(dep ~ cat, data = tmp, var.equal = FALSE), error = function(e) NULL)
      wilc <- tryCatch(wilcox.test(dep ~ cat, data = tmp), error = function(e) NULL)
      coh <- tryCatch(cohens_d(dep ~ cat, data = tmp, pooled_sd = FALSE) %>% pull(Cohens_d), error = function(e) NA_real_)
      levene_p <- tryCatch(lev$p[1], error = function(e) NA_real_)
      test_summary <- test_summary %>%
        mutate(test = "two-sample", method = "Welch t / Wilcoxon", t_stat = if(!is.null(t_res)) unname(t_res$statistic) else NA_real_, t_p = if(!is.null(t_res)) unname(t_res$p.value) else NA_real_, wilcox_stat = if(!is.null(wilc)) unname(wilc$statistic) else NA_real_, wilcox_p = if(!is.null(wilc)) unname(wilc$p.value) else NA_real_, levene_p = levene_p, effect = coh, effect_name = "Cohens_d")
    } else {
      aov_res <- tryCatch(aov(dep ~ cat, data = tmp), error = function(e) NULL)
      anova_tbl <- if (!is.null(aov_res)) broom::tidy(anova(aov_res)) else tibble(term = NA_character_, statistic = NA_real_, p.value = NA_real_)
      krus <- tryCatch(kruskal_test(data = tmp, dep ~ cat), error = function(e) NULL)
      eta2 <- tryCatch(eta_squared(aov_res, partial = FALSE) %>% pull(ETA2), error = function(e) NA_real_)
      levene_p <- tryCatch(lev$p[1], error = function(e) NA_real_)
      test_summary <- test_summary %>%
        mutate(test = "k-sample", method = "ANOVA / Kruskal-Wallis", anova_F = if(nrow(anova_tbl) >= 1) anova_tbl$statistic[1] else NA_real_, anova_p = if(nrow(anova_tbl) >= 1) anova_tbl$p.value[1] else NA_real_, krus_stat = if(!is.null(krus)) krus$statistic else NA_real_, krus_p = if(!is.null(krus)) krus$p else NA_real_, levene_p = levene_p, effect = eta2, effect_name = "eta_squared")
    }
    
    # store details and plots
    res <- list(variable = var, summary_by_group = group_sum, normality_by_group = normal_tests, levene = lev, test_summary = test_summary, tmp_data = tmp)
    results_list[[var]] <- res
    
    p <- ggplot(tmp %>% filter(!is.na(cat) & !is.na(dep)), aes(x = cat, y = dep)) +
      geom_jitter(width = 0.18, alpha = 0.5, size = 1) +
      geom_boxplot(alpha = 0.3, outlier.shape = NA) +
      labs(title = paste(dep_var, "by", var), x = var, y = dep_var) +
      plot_theme +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red")
    plots_list[[var]] <- p
  }
  
  combined_tests <- map_dfr(results_list, function(x) {
    ts <- x$test_summary
    ts %>% mutate(variable = x$variable) %>% select(variable, everything())
  })
  
  list(detected_categorical = cat_vars, tests = combined_tests, detailed = results_list, plots = plots_list)
}

# Example usage (explicitly choose categorical predictors):
res_assoc <- run_assoc_tests_general(sample_df,
                                     dep_var = "satisfaction_score",
                                     cat_vars = c("gender","education","income","age_group","employment_status","urban_rural"))
print(res_assoc$tests)
# To view plot for a predictor:
# print(res_assoc$plots$gender)




# -----------------------------------------------------------------------------
# Continuous, categorical and categorical associations
# ----------------------------
# 1) Preferred: continuous outcome tests vs categorical demographics
# ----------------------------
# Required packages
pkgs <- c("tidyverse","broom","rstatix","effectsize")
new <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new)
library(tidyverse); library(broom); library(rstatix); library(effectsize)

# Generalized tester: numeric dependents vs chosen categorical predictors
test_continuous_vs_categorical_general <- function(df,
                                                   dep_vars,
                                                   cat_vars = NULL,
                                                   auto_detect = TRUE,
                                                   cat_threshold = 10,
                                                   min_group_n = 3) {
  # validations
  if (missing(dep_vars) || length(dep_vars) == 0) stop("Provide at least one dep_var (numeric).")
  if (!all(dep_vars %in% names(df))) stop("Some dep_vars not in df")
  if (!all(vapply(df[dep_vars], is.numeric, logical(1)))) stop("All dep_vars must be numeric")
  
  # detect categorical predictors if not supplied
  if (is.null(cat_vars) && auto_detect) {
    is_cat_fn <- function(x) (is.character(x) || is.factor(x) || (is.integer(x) && n_distinct(x[!is.na(x)]) <= cat_threshold))
    cat_vars <- names(df)[vapply(df, is_cat_fn, logical(1))]
    cat_vars <- setdiff(cat_vars, dep_vars)
  } else {
    if (!all(cat_vars %in% names(df))) stop("Some cat_vars not in df")
  }
  
  # containers
  summary_rows <- list()
  details <- list()
  
  for (dep in dep_vars) {
    for (v in cat_vars) {
      pair_key <- paste(dep, "vs", v, sep = "__")
      tmp <- df %>%
        select(all_of(c(dep, v))) %>%
        rename(dep = !!sym(dep), cat = !!sym(v)) %>%
        mutate(cat = as.factor(cat)) %>%
        filter(!is.na(cat), !is.na(dep))
      
      k <- n_distinct(tmp$cat)
      n_total <- nrow(tmp)
      group_summary <- tmp %>%
        group_by(cat) %>%
        summarise(n = n(), mean = mean(dep, na.rm = TRUE), sd = sd(dep, na.rm = TRUE),
                  median = median(dep, na.rm = TRUE), iqr = IQR(dep, na.rm = TRUE), .groups = "drop")
      
      # prepare storage
      res_row <- tibble(dep = dep, predictor = v, n = n_total, groups = k)
      detail_obj <- list(group_summary = group_summary)
      
      if (k == 0 || n_total < 2) {
        res_row <- res_row %>% mutate(test = NA_character_, method = NA_character_, statistic = NA_real_, p_value = NA_real_, effect = NA_real_, note = "insufficient data")
      } else if (k == 2) {
        # two groups: Welch t, Wilcoxon, Cohen's d
        t_res <- tryCatch(t.test(dep ~ cat, data = tmp, var.equal = FALSE), error = function(e) NULL)
        w_res <- tryCatch(wilcox.test(dep ~ cat, data = tmp), error = function(e) NULL)
        coh <- tryCatch(cohens_d(dep ~ cat, data = tmp)$Cohens_d, error = function(e) NA_real_)
        res_row <- res_row %>% mutate(test = "two-group", method = "Welch t / Wilcoxon", statistic = if(!is.null(t_res)) unname(t_res$statistic) else NA_real_, p_value = if(!is.null(t_res)) unname(t_res$p.value) else NA_real_, effect = coh, effect_name = "Cohens_d")
        detail_obj$t_test <- broom::tidy(t_res); detail_obj$wilcox <- broom::tidy(w_res); detail_obj$cohens_d <- coh
      } else {
        # 3+ groups: ANOVA, Kruskal-Wallis, eta-squared
        aov_res <- tryCatch(aov(dep ~ cat, data = tmp), error = function(e) NULL)
        anova_tbl <- if (!is.null(aov_res)) broom::tidy(anova(aov_res)) else NULL
        krus <- tryCatch(kruskal_test(tmp, dep ~ cat), error = function(e) NULL)
        eta2 <- tryCatch(eta_squared(aov_res, partial = FALSE)$ETA2[1], error = function(e) NA_real_)
        res_row <- res_row %>% mutate(test = "k-group", method = "ANOVA / Kruskal-Wallis", statistic = if(!is.null(anova_tbl)) anova_tbl$statistic[1] else NA_real_, p_value = if(!is.null(anova_tbl)) anova_tbl$p.value[1] else NA_real_, effect = eta2, effect_name = "eta_squared")
        detail_obj$aov <- aov_res; detail_obj$anova_tbl <- anova_tbl; detail_obj$kruskal <- krus; detail_obj$eta2 <- eta2
      }
      
      summary_rows[[pair_key]] <- res_row
      details[[pair_key]] <- detail_obj
    }
  }
  
  summary_tbl <- bind_rows(summary_rows) %>%
    mutate(p_value = as.numeric(p_value)) %>%
    arrange(dep, p_value)
  
  list(summary = summary_tbl, details = details)
}

# Example usage: explicitly choose variables
dep_vars <- c("satisfaction_score") 
cat_vars <- c("gender","education","income","age_group","employment_status","urban_rural")
res_cont <- test_continuous_vs_categorical_general(sample_df, dep_vars = dep_vars, cat_vars = cat_vars)

# Inspect
print(res_cont$summary)
# Example detail for a pair (satisfaction_score vs education)
print(res_cont$details[["satisfaction_score__vs__education"]]$group_summary)
print(res_cont$details[["satisfaction_score__vs__education"]]$aov)  # if available


# ----------------------------
# 2) Preferred: categorical outcome tests vs categorical demographics
# ----------------------------
library(tidyverse); library(broom)

# Helper: Cramer's V with bias correction
cramers_v <- function(tbl) {
  chi <- suppressWarnings(chisq.test(tbl, correct = FALSE))
  n <- sum(tbl)
  phi2 <- as.numeric(chi$statistic) / n
  r <- nrow(tbl); k <- ncol(tbl)
  phi2corr <- max(0, phi2 - ((k - 1) * (r - 1)) / (n - 1))
  rcorr <- r - ((r - 1)^2) / (n - 1)
  kcorr <- k - ((k - 1)^2) / (n - 1)
  denom <- min(kcorr - 1, rcorr - 1)
  if (denom <= 0) return(NA_real_)
  sqrt(phi2corr / denom)
}

# Generalized chi-square runner: categorical dependent(s) vs chosen categorical predictors
chi_square_categorical_general <- function(df,
                                           dep_vars,
                                           cat_vars = NULL,
                                           auto_detect = TRUE,
                                           cat_threshold = 10,
                                           simulate_p_when_low_expected = TRUE,
                                           simulate_B = 2000) {
  # validations
  if (missing(dep_vars) || length(dep_vars) == 0) stop("Provide at least one dep_var (categorical).")
  if (!all(dep_vars %in% names(df))) stop("Some dep_vars not found in df")
  # coerce specified dependents to factor
  for (d in dep_vars) {
    if (!is.factor(df[[d]])) df[[d]] <- as.factor(df[[d]])
  }
  # detect categorical predictors if not supplied
  if (is.null(cat_vars) && auto_detect) {
    is_cat <- function(x) is.factor(x) || is.character(x) || (is.integer(x) && n_distinct(x[!is.na(x)]) <= cat_threshold)
    cat_vars <- names(df)[vapply(df, is_cat, logical(1))]
    cat_vars <- setdiff(cat_vars, dep_vars)
  } else {
    if (!all(cat_vars %in% names(df))) stop("Some cat_vars not found in df")
  }
  results_table <- tibble()
  detailed <- list()
  # iterate over dep × predictor pairs
  for (d in dep_vars) {
    for (v in cat_vars) {
      pair_key <- paste(d, "vs", v, sep = "__")
      predictor <- df[[v]]
      if (!is.factor(predictor)) predictor <- as.factor(predictor)
      tab_df <- df %>% select(all_of(d)) %>% mutate(predictor = predictor) %>% filter(!is.na(.data[[d]]), !is.na(predictor))
      if (nrow(tab_df) == 0) next
      ct <- table(tab_df$predictor, tab_df[[d]])
      # determine small expected counts
      expected_flag <- tryCatch({
        tmp <- suppressWarnings(chisq.test(ct, correct = FALSE))
        any(tmp$expected < 5)
      }, error = function(e) TRUE)
      # choose test
      test_obj <- NULL; test_method <- NA_character_; p_value <- NA_real_; statistic <- NA_real_; df_stat <- NA_real_
      if (nrow(ct) == 2 && ncol(ct) == 2 && any(ct < 5)) {
        ft <- tryCatch(fisher.test(ct), error = function(e) NULL)
        if (!is.null(ft)) {
          test_obj <- ft; test_method <- "Fisher"; p_value <- ft$p.value
        } else {
          test_method <- "Fisher error"
        }
      } else {
        ch <- tryCatch(chisq.test(ct, correct = FALSE), error = function(e) e)
        if (inherits(ch, "error")) {
          if (simulate_p_when_low_expected) {
            chs <- tryCatch(chisq.test(ct, simulate.p.value = TRUE, B = simulate_B), error = function(e) NULL)
            if (!is.null(chs)) {
              test_obj <- chs; test_method <- "Chi-square (simulated)"; p_value <- chs$p.value; statistic <- as.numeric(chs$statistic); df_stat <- as.numeric(chs$parameter)
            } else {
              test_method <- "Chi-square error"
            }
          } else {
            test_method <- "Chi-square error"
          }
        } else {
          test_obj <- ch; test_method <- "Chi-square"; p_value <- ch$p.value; statistic <- as.numeric(ch$statistic); df_stat <- as.numeric(ch$parameter)
        }
      }
      # effect size
      cv <- tryCatch(cramers_v(ct), error = function(e) NA_real_)
      # assemble summary row
      row <- tibble(
        dep_var = d,
        predictor = v,
        predictor_levels = nrow(ct),
        dep_levels = ncol(ct),
        n = sum(ct),
        test = test_method,
        statistic = statistic,
        df = df_stat,
        p_value = p_value,
        cramers_v = cv
      )
      results_table <- bind_rows(results_table, row)
      # detailed objects
      detailed[[pair_key]] <- list(contingency_table = ct, test = test_obj, cramers_v = cv)
      # plot
      plot_df <- as.data.frame(ct) %>% set_names(c("predictor","dep","count"))
      p <- ggplot(plot_df, aes(x = predictor, y = dep, fill = count)) +
        geom_tile(color = "white") +
        geom_text(aes(label = count), color = "white", size = 3) +
        scale_fill_viridis_c(option = "C") +
        labs(title = paste(d, "vs", v), x = v, y = d, fill = "count") +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      detailed[[pair_key]]$plot <- p
    }
  }
  results_table <- results_table %>% arrange(dep_var, p_value)
  list(dependents = dep_vars, predictors = cat_vars, results_table = results_table, detailed = detailed)
}

# Example usage:
# ensure dependent is factor
sample_df$purchase_intent <- as.factor(sample_df$purchase_intent)
# choose dependents and predictors explicitly
dep_vars <- c("purchase_intent")
cat_vars <- c("gender","education","income","age_group","employment_status","urban_rural")
res_chi <- chi_square_categorical_general(sample_df, dep_vars = dep_vars, cat_vars = cat_vars)
print(res_chi$results_table)
# View contingency and plot for a pair:
key <- "purchase_intent__vs__gender"
res_chi$detailed[[key]]$contingency_table
print(res_chi$detailed[[key]]$plot)





#========================================================================================================
#========================relationship between dependent and independent (overall )
#===========================================================================================
# Helper: detect categorical (factor/chr or integer with few unique values)
is_categorical <- function(x, threshold = 10) {
  is.factor(x) || is.character(x) || (is.integer(x) && n_distinct(x[!is.na(x)]) <= threshold)
}
is_numeric_var <- function(x) is.numeric(x) || is.integer(x)

# Helper: Cramer's V (bias-corrected)
cramers_v <- function(tbl) {
  chi <- suppressWarnings(chisq.test(tbl, correct = FALSE))
  n <- sum(tbl)
  phi2 <- as.numeric(chi$statistic) / n
  r <- nrow(tbl); k <- ncol(tbl)
  phi2corr <- max(0, phi2 - ((k - 1) * (r - 1)) / (n - 1))
  rcorr <- r - ((r - 1)^2) / (n - 1)
  kcorr <- k - ((k - 1)^2) / (n - 1)
  denom <- min(kcorr - 1, rcorr - 1)
  if (denom <= 0) return(NA_real_)
  sqrt(phi2corr / denom)
}

# Main function
auto_assoc_tests <- function(df,
                             dep_vars,
                             indep_vars,
                             cat_threshold = 10,
                             shapiro_max = 5000,
                             simulate_B = 2000) {
  # validation
  if (!all(dep_vars %in% names(df))) stop("Some dep_vars not found in df")
  if (!all(indep_vars %in% names(df))) stop("Some indep_vars not found in df")
  
  summary_rows <- list()
  details <- list()
  plots <- list()
  
  for (dep in dep_vars) {
    for (ind in indep_vars) {
      # skip identical
      if (dep == ind) next
      
      x <- df[[dep]]
      y <- df[[ind]]
      # determine types
      dep_type <- if (is_numeric_var(x) && !is_categorical(x, cat_threshold)) "numeric" else "categorical"
      ind_type <- if (is_numeric_var(y) && !is_categorical(y, cat_threshold)) "numeric" else "categorical"
      
      pair_name <- paste(dep, "_vs_", ind, sep = "")
      res_row <- tibble(dep = dep, indep = ind, dep_type = dep_type, indep_type = ind_type)
      
      # prepare non-missing paired data per test type
      df_pair <- df %>% select(all_of(c(dep, ind))) %>% rename(dep = !!sym(dep), ind = !!sym(ind)) %>% filter(!is.na(dep) & !is.na(ind))
      n_pair <- nrow(df_pair)
      res_row <- res_row %>% mutate(n = n_pair)
      
      # numeric ~ numeric: correlations
      if (dep_type == "numeric" && ind_type == "numeric") {
        pear <- tryCatch(cor.test(df_pair$dep, df_pair$ind, method = "pearson"), error = function(e) NULL)
        spear <- tryCatch(cor.test(df_pair$dep, df_pair$ind, method = "spearman"), error = function(e) NULL)
        pear_tidy <- if (!is.null(pear)) broom::tidy(pear) else tibble(statistic = NA_real_, p.value = NA_real_, estimate = NA_real_)
        spear_tidy <- if (!is.null(spear)) broom::tidy(spear) else tibble(statistic = NA_real_, p.value = NA_real_, estimate = NA_real_)
        res_row <- res_row %>% mutate(
          test = "correlation",
          method = "pearson/spearman",
          statistic = pear_tidy$statistic,
          p_value = pear_tidy$p.value,
          estimate = pear_tidy$estimate
        )
        details[[pair_name]] <- list(pearson = pear, spearman = spear)
        p <- ggplot(df_pair, aes(x = ind, y = dep)) +
          geom_point(alpha = 0.6) +
          geom_smooth(method = "lm", se = TRUE, color = "blue") +
          labs(title = paste(dep, "vs", ind), x = ind, y = dep) +
          theme_minimal()
        plots[[pair_name]] <- p
      }
      
      # numeric dep ~ categorical indep (dep numeric, ind categorical)
      else if (dep_type == "numeric" && ind_type == "categorical") {
        df_pair <- df_pair %>% mutate(ind = as.factor(ind))
        k <- n_distinct(df_pair$ind)
        # group summaries
        group_sum <- df_pair %>% group_by(ind) %>% summarise(n = n(), mean = mean(dep), sd = sd(dep), median = median(dep), iqr = IQR(dep), .groups = "drop")
        # normality per group
        shapiro_by_group <- df_pair %>% group_by(ind) %>% summarise(n = n(), shapiro_p = ifelse(n >= 3 & n <= shapiro_max, tryCatch(shapiro.test(dep)$p.value, error = function(e) NA_real_), NA_real_), .groups = "drop")
        # levene
        lev <- tryCatch(car::leveneTest(dep ~ ind, data = df_pair), error = function(e) NULL)
        # tests
        if (k == 2) {
          t_res <- tryCatch(t.test(dep ~ ind, data = df_pair, var.equal = FALSE), error = function(e) NULL)
          wilc <- tryCatch(wilcox.test(dep ~ ind, data = df_pair), error = function(e) NULL)
          coh <- tryCatch(cohens_d(dep ~ ind, data = df_pair)$Cohens_d, error = function(e) NA_real_)
          res_row <- res_row %>% mutate(test = "two-group", method = "Welch t / Wilcoxon", statistic = if(!is.null(t_res)) unname(t_res$statistic) else NA_real_, p_value = if(!is.null(t_res)) unname(t_res$p.value) else NA_real_, effect = coh)
          details[[pair_name]] <- list(group_summary = group_sum, shapiro = shapiro_by_group, levene = lev, t_test = t_res, wilcox = wilc, cohens_d = coh)
        } else {
          aov_res <- tryCatch(aov(dep ~ ind, data = df_pair), error = function(e) NULL)
          anova_tbl <- if (!is.null(aov_res)) broom::tidy(anova(aov_res)) else NULL
          krus <- tryCatch(kruskal_test(df_pair, dep ~ ind), error = function(e) NULL)
          eta2 <- tryCatch(eta_squared(aov_res, partial = FALSE)$ETA2[1], error = function(e) NA_real_)
          res_row <- res_row %>% mutate(test = "k-group", method = "ANOVA / Kruskal", statistic = if(!is.null(anova_tbl)) anova_tbl$statistic[1] else NA_real_, p_value = if(!is.null(anova_tbl)) anova_tbl$p.value[1] else NA_real_, effect = eta2)
          details[[pair_name]] <- list(group_summary = group_sum, shapiro = shapiro_by_group, levene = lev, aov = aov_res, kruskal = krus, eta2 = eta2)
        }
        # plot
        p <- ggplot(df_pair, aes(x = ind, y = dep)) + geom_jitter(width = 0.2, alpha = 0.5) + geom_boxplot(alpha = 0.3, outlier.shape = NA) + stat_summary(fun = mean, geom = "point", shape = 18, color = "red", size = 2) + labs(title = paste(dep, "by", ind), x = ind, y = dep) + theme_minimal()
        plots[[pair_name]] <- p
      }
      
      # categorical dep ~ categorical indep
      else if (dep_type == "categorical" && ind_type == "categorical") {
        df_pair <- df_pair %>% mutate(dep = as.factor(dep), ind = as.factor(ind))
        ct <- table(df_pair$ind, df_pair$dep)
        # choose test
        test_obj <- NULL; test_method <- NA; p_val <- NA; stat_val <- NA; df_stat <- NA
        # 2x2 small counts -> Fisher
        if (nrow(ct) == 2 && ncol(ct) == 2 && any(ct < 5)) {
          ft <- tryCatch(fisher.test(ct), error = function(e) NULL)
          test_obj <- ft; test_method <- "Fisher"; p_val <- if(!is.null(ft)) ft$p.value else NA_real_
        } else {
          ch <- tryCatch(chisq.test(ct, correct = FALSE), error = function(e) e)
          if (inherits(ch, "error")) {
            chs <- tryCatch(chisq.test(ct, simulate.p.value = TRUE, B = simulate_B), error = function(e) NULL)
            test_obj <- chs; test_method <- "Chi-square (simulated)"; p_val <- if(!is.null(chs)) chs$p.value else NA_real_; stat_val <- if(!is.null(chs)) as.numeric(chs$statistic) else NA_real_
          } else {
            test_obj <- ch; test_method <- "Chi-square"; p_val <- ch$p.value; stat_val <- as.numeric(ch$statistic); df_stat <- as.numeric(ch$parameter)
          }
        }
        cv <- tryCatch(cramers_v(ct), error = function(e) NA_real_)
        res_row <- res_row %>% mutate(test = "chi-square", method = test_method, statistic = stat_val, p_value = p_val, effect = cv)
        details[[pair_name]] <- list(contingency = ct, test = test_obj, cramers_v = cv)
        # plot
        plot_df <- as.data.frame(ct) %>% set_names(c("ind", "dep", "count"))
        p <- ggplot(plot_df, aes(x = ind, y = dep, fill = count)) + geom_tile(color = "white") + geom_text(aes(label = count), color = "white") + scale_fill_viridis_c() + labs(title = paste(dep, "vs", ind), x = ind, y = dep) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        plots[[pair_name]] <- p
      }
      
      # categorical dep ~ numeric indep (treat like numeric dep ~ categorical indep by swapping)
      else if (dep_type == "categorical" && ind_type == "numeric") {
        # swap roles: treat ind as numeric predictor against categorical dep by summarizing numeric by dep groups
        df_pair <- df_pair %>% mutate(dep = as.factor(dep))
        group_sum <- df_pair %>% group_by(dep) %>% summarise(n = n(), mean = mean(ind), sd = sd(ind), median = median(ind), iqr = IQR(ind), .groups = "drop")
        # normality by group
        shapiro_by_group <- df_pair %>% group_by(dep) %>% summarise(n = n(), shapiro_p = ifelse(n >= 3 & n <= shapiro_max, tryCatch(shapiro.test(ind)$p.value, error = function(e) NA_real_), NA_real_), .groups = "drop")
        k <- n_distinct(df_pair$dep)
        # tests analogous: 2 groups -> t/wilcox; 3+ -> anova/kruskal
        if (k == 2) {
          t_res <- tryCatch(t.test(ind ~ dep, data = df_pair, var.equal = FALSE), error = function(e) NULL)
          wilc <- tryCatch(wilcox.test(ind ~ dep, data = df_pair), error = function(e) NULL)
          coh <- tryCatch(cohens_d(ind ~ dep, data = df_pair)$Cohens_d, error = function(e) NA_real_)
          res_row <- res_row %>% mutate(test = "two-group (numeric predictor)", method = "Welch t / Wilcoxon", statistic = if(!is.null(t_res)) unname(t_res$statistic) else NA_real_, p_value = if(!is.null(t_res)) unname(t_res$p.value) else NA_real_, effect = coh)
          details[[pair_name]] <- list(group_summary = group_sum, shapiro = shapiro_by_group, t_test = t_res, wilcox = wilc, cohens_d = coh)
        } else {
          aov_res <- tryCatch(aov(ind ~ dep, data = df_pair), error = function(e) NULL)
          anova_tbl <- if (!is.null(aov_res)) broom::tidy(anova(aov_res)) else NULL
          krus <- tryCatch(kruskal_test(df_pair, ind ~ dep), error = function(e) NULL)
          eta2 <- tryCatch(eta_squared(aov_res, partial = FALSE)$ETA2[1], error = function(e) NA_real_)
          res_row <- res_row %>% mutate(test = "k-group (numeric predictor)", method = "ANOVA / Kruskal", statistic = if(!is.null(anova_tbl)) anova_tbl$statistic[1] else NA_real_, p_value = if(!is.null(anova_tbl)) anova_tbl$p.value[1] else NA_real_, effect = eta2)
          details[[pair_name]] <- list(group_summary = group_sum, shapiro = shapiro_by_group, aov = aov_res, kruskal = krus, eta2 = eta2)
        }
        p <- ggplot(df_pair, aes(x = dep, y = ind)) + geom_jitter(width = 0.2, alpha = 0.6) + geom_boxplot(alpha = 0.3, outlier.shape = NA) + stat_summary(fun = mean, geom = "point", shape = 18, color = "red", size = 2) + labs(title = paste(ind, "by", dep), x = dep, y = ind) + theme_minimal()
        plots[[pair_name]] <- p
      } else {
        # fallback unknown combination
        res_row <- res_row %>% mutate(test = NA_character_, method = NA_character_, statistic = NA_real_, p_value = NA_real_, effect = NA_real_)
        details[[pair_name]] <- list(message = "unsupported type combination")
      }
      
      summary_rows[[pair_name]] <- res_row
    }
  }
  
  # bind summary
  summary_tbl <- bind_rows(summary_rows) %>% arrange(dep, indep)
  
  list(summary = summary_tbl, details = details, plots = plots)
}

# -----------------------------
# Example usage with your sample_df
# -----------------------------
# assume sample_df exists in environment and n is defined
# choose dependencies and independents explicitly
dep_vars <- c("satisfaction_score", "purchase_intent")
indep_vars <- c("gender", "education", "income", "age_group", "employment_status", "household_size", "urban_rural", "monthly_expenditure", "distance_to_market_km")

res_all <- auto_assoc_tests(sample_df, dep_vars = dep_vars, indep_vars = indep_vars)

# View concise summary
print(res_all$summary)

# Example: examine details for satisfaction_score vs education
key <- "satisfaction_score_vs_education"
res_all$details[[key]]$group_summary
# plot
print(res_all$plots[[key]])

# Example: view categorical association (purchase_intent vs gender)
key2 <- "purchase_intent_vs_gender"
res_all$details[[key2]]$contingency
print(res_all$plots[[key2]])







#=================================================================================
#================Predicting Dependent Variables using Independent variables
#==================================================================================



# Main function
# Required packages (install if missing)
pkgs <- c("tidyverse","broom","MASS","nnet","car","sandwich","lmtest","broom.mixed")
new <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if(length(new)) install.packages(new, dependencies = TRUE)
library(tidyverse); library(broom); library(MASS); library(nnet); library(car); library(sandwich); library(lmtest); library(broom.mixed)

# Corrected run_stat_models
run_stat_models <- function(df, dep, indeps = NULL, vif_threshold = 5, seed = 2025) {
  set.seed(seed)
  if (!dep %in% names(df)) stop("dep not found in df")
  if (is.null(indeps)) indeps <- setdiff(names(df), dep)
  if (!all(indeps %in% names(df))) stop("Some indeps not found in df")
  
  # safer type-check helpers (replace the old ones)
  is_ordered_factor <- function(x) {
    # only TRUE for explicit ordered factors
    is.ordered(x)
  }
  is_binary_factor <- function(x) {
    is.factor(x) && nlevels(x) == 2
  }
  is_multinomial_factor <- function(x) {
    is.factor(x) && nlevels(x) > 2 && !is_ordered_factor(x)
  }
  is_numeric_continuous <- function(x) {
    is.numeric(x) && (length(unique(na.omit(x))) > 10)
  }
  is_count_integer <- function(x) {
    is.integer(x) || (is.numeric(x) && all(na.omit(x) == floor(na.omit(x))) && mean(na.omit(x), na.rm = TRUE) < 50)
  }
  
  
  y <- df[[dep]]
  dep_type <- if (is_numeric_continuous(y)) "continuous" else if (is_count_integer(y)) "count" else if (is_ordered_factor(y)) "ordered" else if (is_binary_factor(y)) "binary" else if (is_multinomial_factor(y)) "multinomial" else if (is.numeric(y)) "continuous" else "categorical"
  
  formula_str <- paste(dep, "~", paste(indeps, collapse = " + "))
  fml <- as.formula(formula_str)
  
  out <- list(dep = dep, indeps = indeps, dep_type = dep_type, formula = fml)
  tidy_summaries <- list(); diagnostics <- list(); plots <- list()
  
  quiet_lm_for_vif <- tryCatch(lm(as.formula(paste("1 ~", paste(indeps, collapse = "+"))), data = df), error = function(e) NULL)
  if (!is.null(quiet_lm_for_vif)) {
    vif_vals <- tryCatch(car::vif(quiet_lm_for_vif), error = function(e) NA)
    diagnostics$vif <- vif_vals
    diagnostics$vif_flag <- ifelse(any(!is.na(vif_vals) & vif_vals > vif_threshold), TRUE, FALSE)
  } else {
    diagnostics$vif <- NA; diagnostics$vif_flag <- NA
  }
  
  # helper to tidy coeftest / robust SE output reliably
  tidy_coeftest_matrix <- function(ct_obj) {
    m <- tryCatch(as.matrix(ct_obj), error = function(e) NULL)
    if (is.null(m)) return(tibble())
    colnames(m) <- make.names(colnames(m))
    tibble(
      term = rownames(m),
      estimate = as.numeric(m[,1]),
      std.error = if (ncol(m) >= 2) as.numeric(m[,2]) else NA_real_,
      statistic = if (ncol(m) >= 3) as.numeric(m[,3]) else NA_real_,
      p.value = if (ncol(m) >= 4) as.numeric(m[,4]) else NA_real_
    )
  }
  
  if (dep_type == "continuous") {
    fit <- lm(fml, data = df)
    tidy_coef <- broom::tidy(fit, conf.int = TRUE)
    robust_se <- tryCatch(lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type = "HC3")), error = function(e) NULL)
    robust_tidy <- tidy_coeftest_matrix(robust_se)
    res <- resid(fit); fitted_vals <- fitted(fit)
    diagnostics$residuals <- summary(res)
    diagnostics$shapiro_res_p <- tryCatch(shapiro.test(res)$p.value, error = function(e) NA_real_)
    diagnostics$bp_test <- tryCatch(lmtest::bptest(fit), error = function(e) NA)
    diagnostics$durbin_watson <- tryCatch(lmtest::dwtest(fit), error = function(e) NA)
    p_resid_vs_fitted <- ggplot(tibble(fitted = fitted_vals, resid = res), aes(x = fitted, y = resid)) + geom_point(alpha = 0.6) + geom_hline(yintercept = 0, linetype = "dashed") + labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") + theme_minimal()
    p_qq <- ggplot(tibble(resid = res), aes(sample = resid)) + stat_qq() + stat_qq_line() + labs(title = "QQ plot of residuals") + theme_minimal()
    tidy_summaries$coefs <- tidy_coef
    tidy_summaries$robust_se <- robust_tidy
    plots$resid_vs_fitted <- p_resid_vs_fitted
    plots$resid_qq <- p_qq
    out$fit <- fit
    
  } else if (dep_type == "binary") {
    if (!is.factor(y)) df[[dep]] <- as.factor(y)
    fit <- glm(fml, data = df, family = binomial(link = "logit"))
    tidy_coef <- broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE)
    robust_se <- tryCatch(lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type = "HC3")), error = function(e) NULL)
    robust_tidy <- tidy_coeftest_matrix(robust_se)
    null_deviance <- fit$null.deviance; res_deviance <- fit$deviance
    diagnostics$pseudoR2_mcfadden <- if (!is.null(null_deviance) && null_deviance != 0) 1 - res_deviance/null_deviance else NA_real_
    preds_prob <- predict(fit, type = "response")
    class_levels <- levels(as.factor(df[[dep]]))
    cutoff <- 0.5
    preds_class <- factor(ifelse(preds_prob >= cutoff, class_levels[2], class_levels[1]), levels = class_levels)
    conf_tbl <- table(preds_class, df[[dep]])
    diagnostics$confusion <- conf_tbl
    tidy_summaries$odds_ratios <- tidy_coef
    tidy_summaries$robust_se <- robust_tidy
    out$fit <- fit
    
  } else if (dep_type == "multinomial") {
    if (!is.factor(df[[dep]])) df[[dep]] <- as.factor(df[[dep]])
    fit <- nnet::multinom(fml, data = df, trace = FALSE)
    tidy_coef <- broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE)
    ll_null <- logLik(update(fit, as.formula(paste(dep, "~1")), data = df))
    ll_mod <- logLik(fit)
    diagnostics$pseudoR2_mcfadden <- if (!is.null(ll_null) && as.numeric(ll_null) != 0) 1 - as.numeric(ll_mod)/as.numeric(ll_null) else NA_real_
    tidy_summaries$odds_ratios <- tidy_coef
    out$fit <- fit
    
  } else if (dep_type == "ordered") {
    if (!is.ordered(df[[dep]])) df[[dep]] <- ordered(df[[dep]])
    fit <- MASS::polr(fml, data = df, Hess = TRUE)
    coefs <- coef(summary(fit))
    pvals <- pnorm(abs(coefs[, "t value"]), lower.tail = FALSE) * 2
    tidy_coef <- tibble(term = rownames(coefs), estimate = coefs[, "Value"], std.error = coefs[, "Std. Error"], p.value = pvals) %>% mutate(odds_ratio = exp(estimate))
    tidy_summaries$odds_ratios <- tidy_coef
    diagnostics$proportional_odds_note <- "Check parallel slopes assumption manually"
    out$fit <- fit
    
  } else if (dep_type == "count") {
    if (!is.numeric(df[[dep]])) stop("Count dependent must be numeric/integer")
    fit_pois <- glm(fml, data = df, family = poisson(link = "log"))
    rdf <- df.residual(fit_pois)
    overdispersion_ratio <- sum(residuals(fit_pois, type = "pearson")^2) / rdf
    diagnostics$overdispersion_ratio <- overdispersion_ratio
    if (overdispersion_ratio > 1.5) {
      fit <- glm(fml, data = df, family = quasipoisson(link = "log"))
      diagnostics$note <- "Overdispersion detected; used quasipoisson"
    } else {
      fit <- fit_pois
      diagnostics$note <- "Poisson used"
    }
    tidy_summaries$coef <- broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE)
    out$fit <- fit
    
  } else {
    stop("Unhandled dependent variable type: ", dep_type)
  }
  
  try({
    if (!is.null(out$fit) && !is.null(quiet_lm_for_vif)) {
      vif_vals_final <- tryCatch(car::vif(quiet_lm_for_vif), error = function(e) NA)
      diagnostics$vif_final <- vif_vals_final
    }
  }, silent = TRUE)
  
  out$tidy <- tidy_summaries
  out$diagnostics <- diagnostics
  out$plots <- plots
  return(out)
}

# Example usage (with your sample_df):
# continuous dep:
res_continuous <- run_stat_models(sample_df, dep = "satisfaction_score",
                                  indeps = c("gender","education","income","age_years","monthly_expenditure","distance_to_market_km"))
# binary categorical dep:
sample_df$purchase_intent <- as.factor(sample_df$purchase_intent)
res_binary <- run_stat_models(sample_df, dep = "purchase_intent",
                              indeps = c("gender","education","income","age_group","monthly_expenditure"))
# ordered example (if you create an ordered satisfaction category)
sample_df$sat_cat <- ordered(cut(sample_df$satisfaction_score, breaks = c(0,60,80,100), labels = c("low","medium","high")))
res_ordered <- run_stat_models(sample_df, dep = "sat_cat",
                               indeps = c("gender","education","income","age_group"))

# Inspect results:
# summary coefficients
print(res_continuous$tidy$coefs)
# diagnostics
print(res_continuous$diagnostics)
# logistic odds ratios
print(res_binary$tidy$odds_ratios)
# ordinal model coefficients
print(res_ordered$tidy$odds_ratios)







#======================================================================
#=Selected-variable causal and network analysis toolkit (mediation, moderation, path, network)
#==========================================================================

library(tidyverse); library(broom); library(mediation); library(lavaan); library(qgraph); library(bootnet); library(psych); library(emmeans); library(GGally); library(broom.mixed)

# -------------------------------
# 1) Mediation analysis (explicit variable selection)
# -------------------------------
mediation_analysis <- function(df,
                               treatment,      # name of independent variable (X)
                               mediator,       # name of mediator (M)
                               outcome,        # name of dependent variable (Y)
                               covariates = NULL,   # optional vector of covariate names
                               treat_as_factor = FALSE,   # if treatment is categorical (binary) use glm
                               mediator_family = c("gaussian","binomial"),
                               outcome_family = c("gaussian","binomial"),
                               sims = 1000,
                               conf.level = 0.95) {
  mediator_family <- match.arg(mediator_family)
  outcome_family  <- match.arg(outcome_family)
  # input checks
  for (v in c(treatment, mediator, outcome, covariates)) if (!is.null(v) && length(v)>0 && !all(v %in% names(df))) stop("Some specified variables not in df")
  # build formulas
  cov_str <- ifelse(is.null(covariates) || length(covariates)==0, "", paste("+", paste(covariates, collapse = "+")))
  # mediator model: M ~ X + covariates
  med_f <- if (mediator_family == "gaussian") as.formula(paste(mediator, "~", treatment, cov_str)) else as.formula(paste(mediator, "~", treatment, cov_str))
  med_mod <- if (mediator_family == "gaussian") lm(med_f, data = df) else glm(med_f, data = df, family = binomial)
  # outcome model: Y ~ X + M + covariates
  out_f <- if (outcome_family == "gaussian") as.formula(paste(outcome, "~", treatment, "+", mediator, cov_str)) else as.formula(paste(outcome, "~", treatment, "+", mediator, cov_str))
  out_mod <- if (outcome_family == "gaussian") lm(out_f, data = df) else glm(out_f, data = df, family = binomial)
  # causal mediation using mediation::mediate
  med_obj <- tryCatch(mediation::mediate(med_mod, out_mod, treat = treatment, mediator = mediator, sims = sims, boot = FALSE), error = function(e) NULL)
  # produce tidy outputs
  med_summary <- if (!is.null(med_obj)) summary(med_obj) else list(message = "mediation::mediate failed; inspect med_mod and out_mod")
  tidy_med_mod <- broom::tidy(med_mod, conf.int = TRUE)
  tidy_out_mod <- broom::tidy(out_mod, conf.int = TRUE)
  list(mediator_model = med_mod, outcome_model = out_mod, mediation_object = med_obj, mediation_summary = med_summary, tidy_mediator = tidy_med_mod, tidy_outcome = tidy_out_mod)
}

# Example:
#res_med <- mediation_analysis(sample_df, treatment = "education", mediator = "monthly_expenditure", outcome = "satisfaction_score", covariates = c("age_years","gender"), mediator_family = "gaussian", outcome_family = "gaussian")
# print(res_med$mediation_summary)



library(dplyr); library(tidyr); library(ggplot2); library(broom)
# -------------------------------
# 2) Moderation analysis (explicit variable selection)
# -------------------------------
moderation_analysis <- function(df,
                                outcome,         # Y
                                predictor,       # X
                                moderator,       # Z
                                covariates = NULL,
                                center = TRUE,   # center numeric predictors for interpretability
                                plot = TRUE,
                                probe_values = c("mean","mean+sd","mean-sd"),
                                na.action = tidyr::drop_na) {
  
  # validate variable names
  req_vars <- c(outcome, predictor, moderator, covariates)
  req_vars <- req_vars[!is.null(req_vars)]
  if (!all(req_vars %in% names(df))) stop("Some specified variables not in df: ", paste(setdiff(req_vars, names(df)), collapse = ", "))
  
  # safely select required columns (any_of avoids error if covariates NULL)
  sel <- c(outcome, predictor, moderator, covariates)
  dat <- df %>% dplyr::select(dplyr::any_of(sel))
  dat <- na.action(dat)    # default: drop_na all selected columns
  
  if (nrow(dat) == 0) stop("No rows remaining after removing missing values on selected variables")
  
  # center numeric predictor/moderator if requested (operate on copy)
  if (center) {
    if (is.numeric(dat[[predictor]])) dat[[predictor]] <- as.numeric(scale(dat[[predictor]], center = TRUE, scale = FALSE))
    if (is.numeric(dat[[moderator]])) dat[[moderator]] <- as.numeric(scale(dat[[moderator]], center = TRUE, scale = FALSE))
  }
  
  # build formula string including covariates if any
  cov_str <- if (is.null(covariates) || length(covariates) == 0) "" else paste("+", paste(covariates, collapse = " + "))
  form_str <- paste0(outcome, " ~ ", predictor, " * ", moderator, " ", cov_str)
  mod_f <- as.formula(form_str)
  
  # fit linear model (user can adapt to glm if needed)
  lm_mod <- stats::lm(mod_f, data = dat)
  tidy_mod <- broom::tidy(lm_mod, conf.int = TRUE)
  
  # interaction term name (account for factor encoding: colon or predictor:moderator)
  inter_term1 <- paste0(predictor, ":", moderator)
  inter_term2 <- paste0(moderator, ":", predictor)
  inter_row <- tidy_mod %>% filter(term %in% c(inter_term1, inter_term2))
  
  # simple slopes via emmeans (if available)
  emm_results <- NULL
  if (requireNamespace("emmeans", quietly = TRUE)) {
    # For numeric predictor and numeric moderator we use emtrends (slope of predictor at moderator levels)
    try({
      if (is.numeric(dat[[predictor]]) && is.numeric(dat[[moderator]])) {
        # probe at mean and +/- sd of moderator
        mod_mean <- mean(dat[[moderator]], na.rm = TRUE); mod_sd <- sd(dat[[moderator]], na.rm = TRUE)
        probe_vals <- c(mod_mean, mod_mean - mod_sd, mod_mean + mod_sd)
        names(probe_vals) <- c("mean", "mean-sd", "mean+sd")
        emm_results <- lapply(probe_vals, function(zv) {
          em <- emmeans::emtrends(lm_mod, var = predictor, specs = list(!!moderator := zv), at = list(!!moderator := zv))
          broom::tidy(em)
        })
        names(emm_results) <- names(probe_vals)
      } else if (is.numeric(dat[[predictor]]) && !is.numeric(dat[[moderator]])) {
        # numeric predictor, categorical moderator: slopes per moderator level
        em <- emmeans::emtrends(lm_mod, var = predictor, specs = moderator)
        emm_results <- list(per_level = broom::tidy(em))
      } else if (!is.numeric(dat[[predictor]]) && is.numeric(dat[[moderator]])) {
        # categorical predictor, numeric moderator: simple slopes of moderator by predictor levels
        em <- emmeans::emtrends(lm_mod, var = moderator, specs = predictor)
        emm_results <- list(per_level = broom::tidy(em))
      } else {
        # both categorical: simple pairwise comparisons
        em <- emmeans::emmeans(lm_mod, ~ interaction(!!sym(predictor), !!sym(moderator)))
        emm_results <- list(emmeans = broom::tidy(em))
      }
    }, silent = TRUE)
  }
  
  # build interaction plot
  p <- NULL
  if (plot) {
    if (is.numeric(dat[[predictor]]) && is.numeric(dat[[moderator]])) {
      # create grid covering predictor and probe moderator values
      x_seq <- seq(min(dat[[predictor]], na.rm = TRUE), max(dat[[predictor]], na.rm = TRUE), length.out = 60)
      mod_mean <- mean(dat[[moderator]], na.rm = TRUE); mod_sd <- sd(dat[[moderator]], na.rm = TRUE)
      z_values <- c(mod_mean - mod_sd, mod_mean, mod_mean + mod_sd)
      grid <- expand.grid(x = x_seq, z = z_values)
      names(grid) <- c(predictor, moderator)
      # set covariates at their means / first level
      if (!is.null(covariates)) {
        for (cvar in covariates) {
          if (is.numeric(df[[cvar]])) grid[[cvar]] <- mean(df[[cvar]], na.rm = TRUE) else grid[[cvar]] <- levels(as.factor(df[[cvar]]))[1]
        }
      }
      grid$pred <- predict(lm_mod, newdata = grid)
      grid[[moderator]] <- factor(grid[[moderator]], labels = c("low","mean","high"))[as.integer(factor(grid[[moderator]]))] # label levels
      p <- ggplot(grid, aes_string(x = predictor, y = "pred", color = moderator)) +
        geom_line(size = 1) +
        labs(title = paste("Interaction:", predictor, "x", moderator), y = paste("Predicted", outcome), color = moderator) +
        theme_minimal()
    } else if (is.numeric(dat[[predictor]]) && !is.numeric(dat[[moderator]])) {
      # numeric predictor, categorical moderator
      grid <- expand.grid(x = seq(min(dat[[predictor]], na.rm = TRUE), max(dat[[predictor]], na.rm = TRUE), length.out = 60),
                          z = levels(factor(dat[[moderator]])))
      names(grid) <- c(predictor, moderator)
      if (!is.null(covariates)) {
        for (cvar in covariates) grid[[cvar]] <- if (is.numeric(df[[cvar]])) mean(df[[cvar]], na.rm = TRUE) else levels(as.factor(df[[cvar]]))[1]
      }
      grid$pred <- predict(lm_mod, newdata = grid)
      p <- ggplot(grid, aes_string(x = predictor, y = "pred", color = moderator)) + geom_line(size = 1) + labs(title = paste("Interaction:", predictor, "x", moderator), y = paste("Predicted", outcome)) + theme_minimal()
    } else {
      # categorical predictor and categorical moderator: group means with lines
      tmp <- dat
      tmp[[predictor]] <- as.factor(tmp[[predictor]])
      tmp[[moderator]] <- as.factor(tmp[[moderator]])
      p <- ggplot(tmp, aes_string(x = predictor, y = outcome, color = moderator, group = moderator)) +
        stat_summary(fun = mean, geom = "point", size = 2) +
        stat_summary(fun = mean, geom = "line") +
        theme_minimal() +
        labs(title = paste("Interaction:", predictor, "x", moderator))
    }
  }
  
  list(model = lm_mod, tidy = tidy_mod, interaction = inter_row, emmeans = emm_results, plot = p)
}

# Example usage:
# sample_df$gender <- as.factor(sample_df$gender)
# res_mod <- moderation_analysis(sample_df, outcome = "satisfaction_score", predictor = "monthly_expenditure", moderator = "gender", covariates = c("age_years"), center = TRUE)
# print(res_mod$interaction)
# print(res_mod$plot)

# -------------------------------
# 3) Path analysis (explicit path specification from selected vars)
# -------------------------------
# path_analysis: build lavaan model from user-supplied path strings (each path like "Y ~ X + M")
path_analysis <- function(df, path_lines, ordered_vars = NULL, estimator = "MLR", missing = "listwise") {
  # path_lines: character vector of lavaan-style equations, e.g. c("M ~ X", "Y ~ M + X")
  if (!is.character(path_lines) || length(path_lines) < 1) stop("Provide at least one path line (character vector).")
  # check variable existence in df
  vars_in_model <- unique(unlist(str_extract_all(paste(path_lines, collapse = " "), "\\b[[:alnum:]_]+\\b")))
  vars_in_model <- intersect(vars_in_model, names(df)) # conservative
  if (length(vars_in_model) == 0) warning("No recognised variables found among path lines and df names; lavaan may error.")
  # coerce ordered if requested
  if (!is.null(ordered_vars)) {
    for (v in intersect(ordered_vars, names(df))) df[[v]] <- ordered(df[[v]])
  }
  model_string <- paste(path_lines, collapse = "\n")
  fit <- lavaan::sem(model_string, data = df, estimator = estimator, missing = missing)
  std_est <- standardizedSolution(fit)
  fitmeasures <- fitMeasures(fit, fit.measures = c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
  list(model_string = model_string, fit = fit, standardized = std_est, fitmeasures = fitmeasures)
}

# Example:
# path_lines <- c("monthly_expenditure ~ education + age_years", "satisfaction_score ~ monthly_expenditure + education + age_years")
# res_path <- path_analysis(sample_df, path_lines = path_lines)
# print(res_path$fitmeasures); print(res_path$standardized)

# -------------------------------
# 4) Network analysis (explicitly select numeric variables)
# -------------------------------
network_analysis <- function(df, vars, type = c("gaussian","ising"), tune = TRUE, plot = TRUE, plot_layout = "spring") {
  type <- match.arg(type)
  if (missing(vars) || length(vars) == 0) stop("Provide vars vector (names or indices) to include in the network")
  if (is.numeric(vars)) vars <- names(df)[vars]
  if (!all(vars %in% names(df))) stop("Some vars not in df")
  data_sub <- df %>% select(all_of(vars)) %>% drop_na()
  # gaussian network: estimate EBIC glasso via bootnet
  if (type == "gaussian") {
    # scale numeric variables
    mat <- as.matrix(scale(data_sub))
    net <- bootnet::estimateNetwork(mat, default = "EBICglasso", tuning = 0.5)
    if (plot) {
      qg <- qgraph::qgraph(net$graph, layout = plot_layout, labels = colnames(mat), theme = "colorblind")
    } else qg <- NULL
    return(list(type = "gaussian", net = net, plot = qg))
  } else {
    # ising (binary) network requires binary data (0/1). Try to coerce and warn.
    bin_check <- all(apply(data_sub, 2, function(x) length(unique(na.omit(x))) == 2))
    if (!bin_check) stop("Ising network requires binary variables (two unique values per variable).")
    if (!requireNamespace("IsingFit", quietly = TRUE)) stop("Install IsingFit for binary network modeling")
    ising_fit <- IsingFit::IsingFit(as.matrix(data_sub))
    if (plot) {
      qg <- qgraph::qgraph(ising_fit$weiadj, labels = colnames(data_sub), layout = plot_layout)
    } else qg <- NULL
    return(list(type = "ising", ising_fit = ising_fit, plot = qg))
  }
}

# Example:
# gaussian network on numeric vars
# numeric_vars <- c("satisfaction_score","monthly_expenditure","distance_to_market_km","age_years")
# res_net <- network_analysis(sample_df, vars = numeric_vars, type = "gaussian")
# # view qgraph plot object:
# res_net$plot

# -------------------------------
# Notes
# -------------------------------
# - All functions require you to explicitly pass the variable names (or numeric column indices) you want to analyse.
# - Mediation: if mediator/outcome are binary, specify mediator_family/outcome_family = "binomial" and the function uses glm; causal mediation for generalized models has caveats (interpretation of indirect effects differs); inspect med_obj and tidy model outputs.
# - Moderation: function centers numeric predictors by default and returns simple interaction plot; for probit/logistic outcomes adapt model fitting to glm.
# - Path analysis: write lavaan path lines yourself (simple and explicit). For complex SEMs add latent variables syntax to path_lines.
# - Network: Gaussian Graphical Models assume continuous/scaled variables; IsingFit required for binary networks.
#
# If you want, I can:
# - add bootstrapped confidence intervals for indirect effects (via boot = TRUE in mediation::mediate),
# - provide a convenience wrapper to generate lavaan model strings from a mediation specification,
# - add model diagnostics and report generation for each analysis.







