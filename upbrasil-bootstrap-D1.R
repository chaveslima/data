
############################################################
# HOUSEHOLD UPF CONSUMPTION IN BRAZIL
# APPENDIX C - BOOTSTRAP VALIDATION
#
# MODEL 1: MAIN
# MODEL 2: NOMAIN
# FINAL D1 ORDERED LOGIT SPECIFICATION
#
# 2,000 nonparametric paired bootstrap replications
# Percentile-based 95% confidence intervals
############################################################

############################################################
# 1. LOAD PACKAGE AND DATA
############################################################

library(ordinal)

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
# 2. PREPARE VARIABLES
############################################################

required <- c(
  "MAIN", "NOMAIN", "EDU", "INCOME",
  "GENDER", "CHILD", "ELDERLY", "PEOPLE",
  "OCCUPATION", "SOURCE", "COOK"
)

stopifnot(all(required %in% names(upbr)))

for (v in required) {
  upbr[[v]] <- as.numeric(upbr[[v]])
}

# Include the respondent in household size.
upbr$PEOPLE <- upbr$PEOPLE + 1

# Construct occupation and food-source indicators.
upbr$JOB <- as.integer(upbr$OCCUPATION == 6)
upbr$SMKT <- as.integer(upbr$SOURCE == 1)
upbr$LOCAL <- as.integer(upbr$SOURCE == 2)

############################################################
# 3. DEFINE COMMON ANALYTICAL SAMPLE
############################################################

vars <- c(
  "MAIN", "NOMAIN", "EDU", "INCOME",
  "GENDER", "CHILD", "ELDERLY", "PEOPLE",
  "JOB", "SMKT", "LOCAL", "COOK"
)

valid <- complete.cases(upbr[, vars]) &
  upbr$EDU > 0 &
  upbr$INCOME > 0 &
  upbr$MAIN %in% 1:5 &
  upbr$NOMAIN %in% 1:5

upbr <- upbr[valid, , drop = FALSE]

upbr$MAIN <- ordered(upbr$MAIN, levels = 1:5)
upbr$NOMAIN <- ordered(upbr$NOMAIN, levels = 1:5)

n <- nrow(upbr)

cat("\nANALYTICAL SAMPLE:", n, "\n")
stopifnot(n == 562L)

############################################################
# 4. FINAL D1 SPECIFICATIONS
############################################################

form_MAIN <- MAIN ~
  log(EDU) + log(INCOME) +
  GENDER + CHILD + ELDERLY + PEOPLE +
  JOB + SMKT + LOCAL + COOK

form_NOMAIN <- NOMAIN ~
  log(EDU) + log(INCOME) +
  GENDER + CHILD + ELDERLY + PEOPLE +
  JOB + SMKT + LOCAL + COOK

predictors <- c(
  "log(EDU)", "log(INCOME)",
  "GENDER", "CHILD", "ELDERLY",
  "PEOPLE", "JOB", "SMKT", "LOCAL", "COOK"
)

############################################################
# 5. DEFINE CONVERGENCE AND EXTRACTION FUNCTIONS
############################################################

# In current ordinal versions, convergence is a list
# containing the numeric vector convergence$code.

get_convergence_codes <- function(fit) {
  
  conv <- fit$convergence
  
  if (is.list(conv)) {
    return(conv$code)
  }
  
  # Compatibility with older ordinal versions.
  if (is.numeric(conv)) {
    return(conv)
  }
  
  NULL
}

is_valid_fit <- function(fit, predictors) {
  
  if (is.null(fit)) {
    return(FALSE)
  }
  
  codes <- get_convergence_codes(fit)
  
  if (
    is.null(codes) ||
    length(codes) == 0L ||
    anyNA(codes) ||
    !is.numeric(codes) ||
    !all(codes == 0)
  ) {
    return(FALSE)
  }
  
  # Check slope identification and numerical validity.
  beta <- fit$beta
  
  if (
    is.null(beta) ||
    is.null(names(beta)) ||
    !all(predictors %in% names(beta))
  ) {
    return(FALSE)
  }
  
  estimates <- beta[predictors]
  
  if (!all(is.finite(estimates))) {
    return(FALSE)
  }
  
  ll <- tryCatch(
    as.numeric(logLik(fit)),
    error = function(e) NA_real_
  )
  
  length(ll) == 1L && is.finite(ll)
}

extract_slopes <- function(fit, predictors) {
  
  beta <- fit$beta
  
  if (
    is.null(beta) ||
    !all(predictors %in% names(beta))
  ) {
    stop("Expected slope coefficients were not found.")
  }
  
  setNames(
    as.numeric(beta[predictors]),
    predictors
  )
}

############################################################
# 6. ESTIMATE ORIGINAL MODELS
############################################################

MODEL1 <- clm(
  form_MAIN,
  data = upbr,
  link = "logit",
  threshold = "flexible"
)

MODEL2 <- clm(
  form_NOMAIN,
  data = upbr,
  link = "logit",
  threshold = "flexible"
)

cat("\nORIGINAL MODEL CONVERGENCE\n")
cat("MAIN codes: ",
    get_convergence_codes(MODEL1), "\n")
cat("NOMAIN codes: ",
    get_convergence_codes(MODEL2), "\n")

stopifnot(
  is_valid_fit(MODEL1, predictors),
  is_valid_fit(MODEL2, predictors)
)

coef_main <- extract_slopes(MODEL1, predictors)
coef_nomain <- extract_slopes(MODEL2, predictors)

fit_table <- data.frame(
  Model = c("MAIN", "NOMAIN"),
  N = c(nobs(MODEL1), nobs(MODEL2)),
  LogLik = c(
    as.numeric(logLik(MODEL1)),
    as.numeric(logLik(MODEL2))
  ),
  AIC = c(AIC(MODEL1), AIC(MODEL2))
)

cat("\nORIGINAL MODEL FIT\n")
print(fit_table, row.names = FALSE, digits = 7)

cat("\nORIGINAL MAIN COEFFICIENTS\n")
print(coef_main)

cat("\nORIGINAL NOMAIN COEFFICIENTS\n")
print(coef_nomain)

############################################################
# 7. BOOTSTRAP SETTINGS
############################################################

set.seed(20261105)

B <- 2000L
k <- length(predictors)

boot_main <- matrix(
  NA_real_,
  nrow = B,
  ncol = k,
  dimnames = list(NULL, predictors)
)

boot_nomain <- matrix(
  NA_real_,
  nrow = B,
  ncol = k,
  dimnames = list(NULL, predictors)
)

# Record model-specific reasons for failure.
fail_main <- c(
  fit_error = 0L,
  convergence = 0L,
  coefficients = 0L
)

fail_nomain <- c(
  fit_error = 0L,
  convergence = 0L,
  coefficients = 0L
)

############################################################
# 8. SAFE BOOTSTRAP ESTIMATION FUNCTION
############################################################

fit_boot <- function(formula, data, predictors) {
  
  fit <- tryCatch(
    suppressWarnings(
      clm(
        formula,
        data = data,
        link = "logit",
        threshold = "flexible"
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
  
  codes <- get_convergence_codes(fit)
  
  if (
    is.null(codes) ||
    length(codes) == 0L ||
    !is.numeric(codes) ||
    anyNA(codes) ||
    !all(codes == 0)
  ) {
    return(list(
      valid = FALSE,
      reason = "convergence",
      estimates = NULL
    ))
  }
  
  if (!is_valid_fit(fit, predictors)) {
    return(list(
      valid = FALSE,
      reason = "coefficients",
      estimates = NULL
    ))
  }
  
  list(
    valid = TRUE,
    reason = "ok",
    estimates = extract_slopes(fit, predictors)
  )
}

############################################################
# 9. PAIRED NONPARAMETRIC BOOTSTRAP
############################################################

successful_pairs <- 0L

cat("\nSTARTING BOOTSTRAP\n")

for (b in seq_len(B)) {
  
  # Resample households using the same indices
  # for MAIN and NOMAIN.
  
  idx <- sample.int(
    n,
    size = n,
    replace = TRUE
  )
  
  boot_data <- upbr[idx, , drop = FALSE]
  
  result_main <- fit_boot(
    form_MAIN,
    boot_data,
    predictors
  )
  
  result_nomain <- fit_boot(
    form_NOMAIN,
    boot_data,
    predictors
  )
  
  if (!result_main$valid) {
    fail_main[result_main$reason] <-
      fail_main[result_main$reason] + 1L
  }
  
  if (!result_nomain$valid) {
    fail_nomain[result_nomain$reason] <-
      fail_nomain[result_nomain$reason] + 1L
  }
  
  # Keep only complete, valid pairs.
  if (result_main$valid && result_nomain$valid) {
    
    successful_pairs <- successful_pairs + 1L
    
    boot_main[b, ] <- result_main$estimates
    boot_nomain[b, ] <- result_nomain$estimates
  }
  
  if (b %% 200L == 0L) {
    cat(
      "Attempts:", b,
      "| Valid pairs:", successful_pairs,
      "\n"
    )
  }
}

############################################################
# 10. SELECT VALID REPLICATIONS
############################################################

keep <- complete.cases(boot_main) &
  complete.cases(boot_nomain)

boot_main <- boot_main[keep, , drop = FALSE]
boot_nomain <- boot_nomain[keep, , drop = FALSE]

B_valid <- nrow(boot_main)

cat("\n========================================\n")
cat("BOOTSTRAP SUMMARY\n")
cat("========================================\n")

cat("Attempted replications:", B, "\n")
cat("Valid paired replications:", B_valid, "\n")
cat(
  "Success rate:",
  round(100 * B_valid / B, 2),
  "%\n"
)

cat("\nMAIN FAILURE COUNTS\n")
print(fail_main)

cat("\nNOMAIN FAILURE COUNTS\n")
print(fail_nomain)

if (B_valid < 100L) {
  stop(
    "Too few valid bootstrap replications. ",
    "Review failure counts."
  )
}

if (B_valid < 1900L) {
  warning(
    "Fewer than 1,900 valid pairs. ",
    "Review convergence and failure counts."
  )
}

############################################################
# 11. BUILD BOOTSTRAP TABLES
############################################################

bootstrap_table <- function(original, samples) {
  
  boot_mean <- colMeans(samples)
  
  boot_se <- apply(
    samples,
    2,
    sd
  )
  
  ci_lower <- apply(
    samples,
    2,
    quantile,
    probs = 0.025,
    names = FALSE
  )
  
  ci_upper <- apply(
    samples,
    2,
    quantile,
    probs = 0.975,
    names = FALSE
  )
  
  sign_stability <- sapply(
    seq_along(original),
    function(j) {
      mean(
        sign(samples[, j]) == sign(original[j])
      )
    }
  )
  
  data.frame(
    Predictor = names(original),
    Estimate = as.numeric(original),
    Boot_Mean = as.numeric(boot_mean),
    Boot_SE = as.numeric(boot_se),
    CI_2.5 = as.numeric(ci_lower),
    CI_97.5 = as.numeric(ci_upper),
    Sign_Stability = as.numeric(sign_stability),
    CI_Excludes_Zero = (
      ci_lower > 0 | ci_upper < 0
    ),
    row.names = NULL
  )
}

TABLE_C1 <- bootstrap_table(
  coef_main,
  boot_main
)

TABLE_C2 <- bootstrap_table(
  coef_nomain,
  boot_nomain
)

############################################################
# 12. PRINT TABLE C1
############################################################

cat("\n========================================\n")
cat("TABLE C1 - BOOTSTRAP RESULTS: MAIN\n")
cat("========================================\n")

print(
  TABLE_C1,
  row.names = FALSE,
  digits = 5
)

############################################################
# 13. PRINT TABLE C2
############################################################

cat("\n========================================\n")
cat("TABLE C2 - BOOTSTRAP RESULTS: NOMAIN\n")
cat("========================================\n")

print(
  TABLE_C2,
  row.names = FALSE,
  digits = 5
)

############################################################
# 14. SAVE TABLES
############################################################

write.csv(
  TABLE_C1,
  "Table_C1_Bootstrap_MAIN.csv",
  row.names = FALSE
)

write.csv(
  TABLE_C2,
  "Table_C2_Bootstrap_NOMAIN.csv",
  row.names = FALSE
)

############################################################
# 15. FINAL REPORT
############################################################

cat("\n========================================\n")
cat("FINAL BOOTSTRAP REPORT\n")
cat("========================================\n")

cat("Sample size:", n, "\n")
cat("Attempted bootstrap replications:", B, "\n")
cat("Valid paired replications:", B_valid, "\n")
cat("MAIN AIC:", AIC(MODEL1), "\n")
cat("NOMAIN AIC:", AIC(MODEL2), "\n")

cat("\nBOOTSTRAP COMPLETED.\n")
