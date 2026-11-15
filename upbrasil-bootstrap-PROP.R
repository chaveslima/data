
############################################################
# HOUSEHOLD UPF CONSUMPTION IN BRAZIL
# MODEL 3 - PROP
# BETA REGRESSION AND BOOTSTRAP VALIDATION
#
# Final Table 7 specification
# OVER: Yes = 1; No or Do Not Know = 0
# 2,000 nonparametric bootstrap replications
# Percentile-based 95% confidence intervals
############################################################

############################################################
# 1. LOAD PACKAGE
############################################################

library(betareg)

############################################################
# 2. LOAD ORIGINAL DATA
############################################################

url <- paste0(
  "https://raw.githubusercontent.com/chaveslima/",
  "data/refs/heads/main/upbrasil.csv"
)

upbr <- read.csv2(
  url,
  header = TRUE,
  stringsAsFactors = FALSE
)

############################################################
# 3. PREPARE VARIABLES
############################################################

required <- c(
  "PROP", "INCOME", "EDU",
  "ELDERLY", "CHILD", "OCCUPATION",
  "SWEETS", "SWEETDAIRY",
  "COOK", "CONVENIENCE", "NOTIME", "OVER"
)

stopifnot(all(required %in% names(upbr)))

for (v in required) {
  upbr[[v]] <- as.numeric(upbr[[v]])
}

# Preserve the original three-category OVER variable.
upbr$OVER_ORIGINAL <- upbr$OVER

stopifnot(all(
  na.omit(unique(upbr$OVER_ORIGINAL)) %in% c(0, 1, 2)
))

cat("\nORIGINAL OVER DISTRIBUTION\n")
print(table(upbr$OVER_ORIGINAL, useNA = "ifany"))

# Recode affirmative perception of overweight.
# Original 0 = No; original 1 = Yes; original 2 = Do Not Know.
# New 1 = Yes; new 0 = No or Do Not Know.
# Missing observations remain missing.

upbr$OVER <- ifelse(
  is.na(upbr$OVER_ORIGINAL),
  NA_integer_,
  as.integer(upbr$OVER_ORIGINAL == 1)
)

cat("\nRECODED OVER DISTRIBUTION\n")
print(table(upbr$OVER, useNA = "ifany"))

# Create occupation indicators.
upbr$JOB <- as.integer(upbr$OCCUPATION == 6)
upbr$JOBPART <- as.integer(upbr$OCCUPATION == 5)

############################################################
# 4. DEFINE ANALYTICAL SAMPLE
############################################################

vars <- c(
  "PROP", "INCOME", "EDU",
  "ELDERLY", "CHILD", "JOB", "JOBPART",
  "SWEETS", "SWEETDAIRY",
  "COOK", "CONVENIENCE", "NOTIME", "OVER"
)

valid <- complete.cases(upbr[, vars]) &
  upbr$PROP >= 0 &
  upbr$PROP <= 1 &
  upbr$INCOME > 0 &
  upbr$EDU > 0

upbr <- upbr[valid, , drop = FALSE]

n <- nrow(upbr)

cat("\nANALYTICAL SAMPLE:", n, "\n")

stopifnot(n == 562L)

# Verify OVER coding in the analytical sample.
stopifnot(
  all(upbr$OVER %in% c(0, 1)),
  sum(upbr$OVER == 1) == 280L,
  sum(upbr$OVER == 0) == 282L
)

############################################################
# 5. SMITHSON-VERKUILEN TRANSFORMATION
############################################################

upbr$PROP_beta <- (
  upbr$PROP * (n - 1) + 0.5
) / n

stopifnot(
  all(upbr$PROP_beta > 0),
  all(upbr$PROP_beta < 1)
)

############################################################
# 6. DEFINE FINAL PROP SPECIFICATION
############################################################

form_PROP <- PROP_beta ~
  log(EDU) +
  log(INCOME) +
  CHILD +
  ELDERLY +
  JOB +
  JOBPART +
  SWEETS +
  CONVENIENCE +
  NOTIME +
  OVER +
  SWEETDAIRY +
  COOK

############################################################
# 7. ESTIMATE ORIGINAL MODEL
############################################################

MODEL3 <- betareg(
  form_PROP,
  data = upbr,
  link = "logit"
)

cat("\n========================================\n")
cat("ORIGINAL MODEL 3 - PROP\n")
cat("========================================\n")

print(summary(MODEL3))

############################################################
# 8. MODEL FIT AND LR TEST
############################################################

MODEL3_NULL <- betareg(
  PROP_beta ~ 1,
  data = upbr,
  link = "logit"
)

LR <- 2 * (
  as.numeric(logLik(MODEL3)) -
    as.numeric(logLik(MODEL3_NULL))
)

df_LR <- attr(logLik(MODEL3), "df") -
  attr(logLik(MODEL3_NULL), "df")

p_LR <- pchisq(
  LR,
  df = df_LR,
  lower.tail = FALSE
)

cat("\n========================================\n")
cat("MODEL 3 - FIT STATISTICS\n")
cat("========================================\n")

cat("N:", nobs(MODEL3), "\n")
cat("LogLik:", as.numeric(logLik(MODEL3)), "\n")
cat("AIC:", AIC(MODEL3), "\n")
cat("BIC:", BIC(MODEL3), "\n")
cat(
  "Pseudo R-squared:",
  summary(MODEL3)$pseudo.r.squared,
  "\n"
)
cat("LR statistic:", LR, "\n")
cat("LR degrees of freedom:", df_LR, "\n")
cat("LR p-value:", p_LR, "\n")

# Confirm the model used in the updated Table 7.
stopifnot(
  nobs(MODEL3) == 562L,
  abs(as.numeric(logLik(MODEL3)) - 324.7619) < 0.01,
  abs(AIC(MODEL3) - (-621.5238)) < 0.01
)

############################################################
# 9. ORIGINAL COEFFICIENTS
############################################################

original <- coef(
  MODEL3,
  model = "mean"
)

coef_names <- names(original)

expected_predictors <- c(
  "log(EDU)",
  "log(INCOME)",
  "CHILD",
  "ELDERLY",
  "JOB",
  "JOBPART",
  "SWEETS",
  "CONVENIENCE",
  "NOTIME",
  "OVER",
  "SWEETDAIRY",
  "COOK"
)

stopifnot(
  length(original) == 13L,
  all(expected_predictors %in% coef_names),
  all(is.finite(original))
)

cat("\nORIGINAL COEFFICIENTS\n")
print(original)

############################################################
# 10. BOOTSTRAP SETTINGS
############################################################

set.seed(20261105)

B <- 2000L
k <- length(original)

boot_coef <- matrix(
  NA_real_,
  nrow = B,
  ncol = k,
  dimnames = list(NULL, coef_names)
)

failure_counts <- c(
  fit_error = 0L,
  convergence = 0L,
  coefficients = 0L,
  loglik = 0L
)

############################################################
# 11. BOOTSTRAP ESTIMATION FUNCTION
############################################################

fit_boot_prop <- function(data, formula, coef_names) {
  
  fit <- tryCatch(
    suppressWarnings(
      betareg(
        formula,
        data = data,
        link = "logit"
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(list(
      valid = FALSE,
      reason = "fit_error",
      estimates = NULL
    ))
  }
  
  # Validate optimizer convergence when available.
  convergence_code <- NULL
  
  if (!is.null(fit$optim) &&
      !is.null(fit$optim$convergence)) {
    convergence_code <- fit$optim$convergence
  }
  
  if (!is.null(convergence_code)) {
    
    if (
      length(convergence_code) != 1L ||
      anyNA(convergence_code) ||
      !is.numeric(convergence_code) ||
      convergence_code != 0
    ) {
      return(list(
        valid = FALSE,
        reason = "convergence",
        estimates = NULL
      ))
    }
  }
  
  # Extract only coefficients from the mean model.
  estimates <- tryCatch(
    coef(fit, model = "mean"),
    error = function(e) NULL
  )
  
  if (
    is.null(estimates) ||
    !setequal(names(estimates), coef_names)
  ) {
    return(list(
      valid = FALSE,
      reason = "coefficients",
      estimates = NULL
    ))
  }
  
  # Match coefficients by name.
  estimates <- estimates[coef_names]
  
  if (!all(is.finite(estimates))) {
    return(list(
      valid = FALSE,
      reason = "coefficients",
      estimates = NULL
    ))
  }
  
  ll <- tryCatch(
    as.numeric(logLik(fit)),
    error = function(e) NA_real_
  )
  
  if (length(ll) != 1L || !is.finite(ll)) {
    return(list(
      valid = FALSE,
      reason = "loglik",
      estimates = NULL
    ))
  }
  
  list(
    valid = TRUE,
    reason = "ok",
    estimates = estimates
  )
}

############################################################
# 12. NONPARAMETRIC BOOTSTRAP
############################################################

successful <- 0L

cat("\nSTARTING PROP BOOTSTRAP\n")

for (b in seq_len(B)) {
  
  # Resample households with replacement.
  idx <- sample.int(
    n,
    size = n,
    replace = TRUE
  )
  
  boot_data <- upbr[idx, , drop = FALSE]
  
  result <- fit_boot_prop(
    data = boot_data,
    formula = form_PROP,
    coef_names = coef_names
  )
  
  if (result$valid) {
    
    successful <- successful + 1L
    
    boot_coef[b, ] <- result$estimates
    
  } else {
    
    failure_counts[result$reason] <-
      failure_counts[result$reason] + 1L
    
  }
  
  if (b %% 200L == 0L) {
    cat(
      "Attempts:", b,
      "| Valid:", successful,
      "\n"
    )
  }
}

############################################################
# 13. SELECT VALID REPLICATIONS
############################################################

boot_coef <- boot_coef[
  complete.cases(boot_coef),
  ,
  drop = FALSE
]

B_valid <- nrow(boot_coef)

cat("\n========================================\n")
cat("BOOTSTRAP SUMMARY - MODEL 3\n")
cat("========================================\n")

cat("Attempted replications:", B, "\n")
cat("Valid replications:", B_valid, "\n")

cat(
  "Success rate:",
  round(100 * B_valid / B, 2),
  "%\n"
)

cat("\nFAILURE COUNTS\n")
print(failure_counts)

if (B_valid < 100L) {
  stop(
    "Insufficient valid bootstrap replications. ",
    "Inspect failure counts."
  )
}

if (B_valid < 1900L) {
  warning(
    "Fewer than 1,900 valid replications. ",
    "Review convergence before interpreting results."
  )
}

############################################################
# 14. BOOTSTRAP STATISTICS
############################################################

boot_mean <- colMeans(boot_coef)

boot_se <- apply(
  boot_coef,
  2,
  sd
)

ci_lower <- apply(
  boot_coef,
  2,
  quantile,
  probs = 0.025,
  names = FALSE
)

ci_upper <- apply(
  boot_coef,
  2,
  quantile,
  probs = 0.975,
  names = FALSE
)

sign_stability <- sapply(
  seq_along(original),
  function(j) {
    mean(
      sign(boot_coef[, j]) ==
        sign(original[j])
    )
  }
)

############################################################
# 15. BOOTSTRAP RESULTS TABLE
############################################################

TABLE_C3 <- data.frame(
  Predictor = coef_names,
  Estimate = as.numeric(original),
  Boot_Mean = as.numeric(boot_mean),
  Boot_SE = as.numeric(boot_se),
  CI_2.5 = as.numeric(ci_lower),
  CI_97.5 = as.numeric(ci_upper),
  Sign_Stability = as.numeric(sign_stability),
  CI_Excludes_Zero = as.logical(
    ci_lower > 0 | ci_upper < 0
  ),
  row.names = NULL
)

cat("\n========================================\n")
cat("BOOTSTRAP RESULTS - MODEL 3: PROP\n")
cat("========================================\n")

print(
  TABLE_C3,
  row.names = FALSE,
  digits = 5
)

############################################################
# 16. BOOTSTRAP STABILITY SUMMARY
############################################################

regressors <- TABLE_C3$Predictor != "(Intercept)"

cat("\n========================================\n")
cat("BOOTSTRAP STABILITY SUMMARY\n")
cat("========================================\n")

cat(
  "Regressors with 95% CI excluding zero:",
  sum(TABLE_C3$CI_Excludes_Zero[regressors]),
  "of",
  sum(regressors),
  "\n"
)

cat(
  "Regressors with sign stability >= 95%:",
  sum(
    TABLE_C3$Sign_Stability[regressors] >= 0.95
  ),
  "of",
  sum(regressors),
  "\n"
)

cat("\nREGRESSORS WITH CI INCLUDING ZERO\n")

print(
  TABLE_C3$Predictor[
    regressors &
      !TABLE_C3$CI_Excludes_Zero
  ]
)

############################################################
# 17. EXPORT RESULTS
############################################################

write.csv(
  TABLE_C3,
  "Bootstrap_PROP_Final.csv",
  row.names = FALSE
)

saveRDS(
  list(
    original = original,
    bootstrap_coefficients = boot_coef,
    results = TABLE_C3,
    attempted = B,
    valid = B_valid,
    failures = failure_counts,
    seed = 20261105,
    model_formula = form_PROP
  ),
  "Bootstrap_PROP_Final.rds"
)

############################################################
# 18. FINAL REPORT
############################################################

cat("\n========================================\n")
cat("FINAL PROP BOOTSTRAP REPORT\n")
cat("========================================\n")

cat("N:", n, "\n")
cat("Bootstrap attempts:", B, "\n")
cat("Valid replications:", B_valid, "\n")
cat("Original LogLik:", as.numeric(logLik(MODEL3)), "\n")
cat("Original AIC:", AIC(MODEL3), "\n")

cat("\nPROP BOOTSTRAP COMPLETED.\n")

