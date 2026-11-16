
############################################################
# HOUSEHOLD UPF CONSUMPTION IN BRAZIL
# APPENDIX E - FRACTIONAL LOGIT ROBUSTNESS CHECK
#
# MODEL 3: PROP
#
# Original specification: Beta regression
# Alternative specification: Fractional logit
# Robust HC3 standard errors for fractional logit
#
# Comparison of coefficient signs, significance,
# and confidence intervals
############################################################

############################################################
# 1. LOAD PACKAGES
############################################################

required_packages <- c("betareg", "sandwich")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(betareg)
library(sandwich)

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

# Preserve original OVER responses.
upbr$OVER_ORIGINAL <- upbr$OVER

cat("\n========================================\n")
cat("ORIGINAL OVER DISTRIBUTION\n")
cat("========================================\n")

print(table(upbr$OVER_ORIGINAL, useNA = "ifany"))

# Define binary OVER:
# 1 = Yes
# 0 = No or Do Not Know

stopifnot(
  all(na.omit(upbr$OVER_ORIGINAL) %in% c(0, 1, 2))
)

upbr$OVER <- ifelse(
  is.na(upbr$OVER_ORIGINAL),
  NA_integer_,
  as.integer(upbr$OVER_ORIGINAL == 1)
)

cat("\nRECODED OVER DISTRIBUTION\n")
print(table(upbr$OVER, useNA = "ifany"))

# Create employment indicators.
upbr$JOB <- as.integer(upbr$OCCUPATION == 6)
upbr$JOBPART <- as.integer(upbr$OCCUPATION == 5)

############################################################
# 4. DEFINE COMMON ANALYTICAL SAMPLE
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

cat("\n========================================\n")
cat("ANALYTICAL SAMPLE\n")
cat("========================================\n")

cat("N:", n, "\n")

stopifnot(
  n == 562L,
  all(upbr$OVER %in% c(0, 1)),
  sum(upbr$OVER == 1) == 280L,
  sum(upbr$OVER == 0) == 282L
)

############################################################
# 5. TRANSFORM PROP FOR BETA REGRESSION
############################################################

# Smithson-Verkuilen transformation for beta regression.
# Fractional logit uses the original PROP variable.

upbr$PROP_beta <- (
  upbr$PROP * (n - 1) + 0.5
) / n

stopifnot(
  all(upbr$PROP_beta > 0),
  all(upbr$PROP_beta < 1)
)

############################################################
# 6. DEFINE IDENTICAL REGRESSORS
############################################################

rhs <- ~
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

form_BETA <- update(
  rhs,
  PROP_beta ~ .
)

form_FRAC <- update(
  rhs,
  PROP ~ .
)

############################################################
# 7. ESTIMATE ORIGINAL BETA REGRESSION
############################################################

MODEL3_BETA <- betareg(
  form_BETA,
  data = upbr,
  link = "logit"
)

cat("\n========================================\n")
cat("ORIGINAL BETA REGRESSION\n")
cat("========================================\n")

print(summary(MODEL3_BETA))

cat("\nBETA REGRESSION FIT\n")
cat("N:", nobs(MODEL3_BETA), "\n")
cat("LogLik:", as.numeric(logLik(MODEL3_BETA)), "\n")
cat("AIC:", AIC(MODEL3_BETA), "\n")
cat(
  "Pseudo R-squared:",
  summary(MODEL3_BETA)$pseudo.r.squared,
  "\n"
)

# Confirm the final Table 7 specification.
stopifnot(
  nobs(MODEL3_BETA) == 562L,
  abs(as.numeric(logLik(MODEL3_BETA)) - 324.7619) < 0.01,
  abs(AIC(MODEL3_BETA) - (-621.5238)) < 0.01
)

############################################################
# 8. ESTIMATE FRACTIONAL LOGIT
############################################################

# Fractional logit models E(PROP | X) using a logit link.
# Quasibinomial avoids treating the fractional response
# as binomial counts.
#
# PROP is used directly, without transformation.

MODEL3_FRAC <- glm(
  form_FRAC,
  data = upbr,
  family = quasibinomial(link = "logit")
)

cat("\n========================================\n")
cat("FRACTIONAL LOGIT REGRESSION\n")
cat("========================================\n")

print(summary(MODEL3_FRAC))

stopifnot(
  nobs(MODEL3_FRAC) == 562L,
  isTRUE(MODEL3_FRAC$converged)
)

############################################################
# 9. HC3 ROBUST INFERENCE FOR FRACTIONAL LOGIT
############################################################

# Use a heteroskedasticity-consistent covariance matrix.
# HC3 provides a finite-sample leverage adjustment.

V_FRAC_HC3 <- sandwich::vcovHC(
  MODEL3_FRAC,
  type = "HC3"
)

coef_FRAC <- coef(MODEL3_FRAC)

se_FRAC <- sqrt(diag(V_FRAC_HC3))

z_FRAC <- coef_FRAC / se_FRAC

p_FRAC <- 2 * pnorm(
  abs(z_FRAC),
  lower.tail = FALSE
)

frac_ci_lower <- coef_FRAC - qnorm(0.975) * se_FRAC
frac_ci_upper <- coef_FRAC + qnorm(0.975) * se_FRAC

FRAC_RESULTS <- data.frame(
  Predictor = names(coef_FRAC),
  Estimate = as.numeric(coef_FRAC),
  Robust_SE_HC3 = as.numeric(se_FRAC),
  z_value = as.numeric(z_FRAC),
  p_value = as.numeric(p_FRAC),
  CI_2.5 = as.numeric(frac_ci_lower),
  CI_97.5 = as.numeric(frac_ci_upper),
  row.names = NULL
)

cat("\n========================================\n")
cat("FRACTIONAL LOGIT - HC3 ROBUST RESULTS\n")
cat("========================================\n")

print(
  FRAC_RESULTS,
  row.names = FALSE,
  digits = 6
)

############################################################
# 10. EXTRACT BETA REGRESSION INFERENCE
############################################################

BETA_SUMMARY <- summary(MODEL3_BETA)

# Extract the coefficient table for the mean submodel.
beta_table <- BETA_SUMMARY$coefficients$mean

stopifnot(
  is.matrix(beta_table),
  ncol(beta_table) >= 4L
)

coef_BETA <- beta_table[, 1]
se_BETA <- beta_table[, 2]
p_BETA <- beta_table[, 4]

beta_ci_lower <- coef_BETA - qnorm(0.975) * se_BETA
beta_ci_upper <- coef_BETA + qnorm(0.975) * se_BETA

############################################################
# 11. COMPARE BETA AND FRACTIONAL LOGIT
############################################################

common_predictors <- intersect(
  names(coef_BETA),
  names(coef_FRAC)
)

stopifnot(
  length(common_predictors) == 13L
)

COMPARISON <- data.frame(
  Predictor = common_predictors,
  
  Beta_Estimate = as.numeric(
    coef_BETA[common_predictors]
  ),
  
  Beta_p = as.numeric(
    p_BETA[common_predictors]
  ),
  
  Fractional_Estimate = as.numeric(
    coef_FRAC[common_predictors]
  ),
  
  Fractional_HC3_SE = as.numeric(
    se_FRAC[common_predictors]
  ),
  
  Fractional_HC3_p = as.numeric(
    p_FRAC[common_predictors]
  ),
  
  row.names = NULL
)

COMPARISON$Same_Sign <- (
  sign(COMPARISON$Beta_Estimate) ==
    sign(COMPARISON$Fractional_Estimate)
)

COMPARISON$Beta_Significant_5pct <- (
  COMPARISON$Beta_p < 0.05
)

COMPARISON$Fractional_Significant_5pct <- (
  COMPARISON$Fractional_HC3_p < 0.05
)

COMPARISON$Same_Significance_5pct <- (
  COMPARISON$Beta_Significant_5pct ==
    COMPARISON$Fractional_Significant_5pct
)

cat("\n========================================\n")
cat("BETA VS FRACTIONAL LOGIT COMPARISON\n")
cat("========================================\n")

print(
  COMPARISON,
  row.names = FALSE,
  digits = 6
)

############################################################
# 12. ROBUSTNESS SUMMARY
############################################################

# Exclude intercept from substantive comparisons.
regressors <- COMPARISON$Predictor != "(Intercept)"

cat("\n========================================\n")
cat("APPENDIX E - ROBUSTNESS SUMMARY\n")
cat("========================================\n")

cat(
  "Observations:",
  n,
  "\n"
)

cat(
  "Regressors with matching signs:",
  sum(COMPARISON$Same_Sign[regressors]),
  "of",
  sum(regressors),
  "\n"
)

cat(
  "Regressors with matching 5% significance:",
  sum(COMPARISON$Same_Significance_5pct[regressors]),
  "of",
  sum(regressors),
  "\n"
)

cat(
  "Regressors significant in beta regression:",
  sum(COMPARISON$Beta_Significant_5pct[regressors]),
  "of",
  sum(regressors),
  "\n"
)

cat(
  "Regressors significant in fractional logit (HC3):",
  sum(COMPARISON$Fractional_Significant_5pct[regressors]),
  "of",
  sum(regressors),
  "\n"
)

cat("\nREGRESSORS WITH DIFFERENT SIGNS\n")

print(
  COMPARISON$Predictor[
    regressors & !COMPARISON$Same_Sign
  ]
)

cat("\nREGRESSORS WITH DIFFERENT 5% SIGNIFICANCE\n")

print(
  COMPARISON$Predictor[
    regressors &
      !COMPARISON$Same_Significance_5pct
  ]
)

############################################################
# 13. FRACTIONAL LOGIT CONFIDENCE INTERVALS
############################################################

cat("\n========================================\n")
cat("FRACTIONAL LOGIT - 95% ROBUST CI\n")
cat("========================================\n")

print(
  FRAC_RESULTS[
    FRAC_RESULTS$Predictor != "(Intercept)",
    c("Predictor", "CI_2.5", "CI_97.5")
  ],
  row.names = FALSE,
  digits = 6
)

############################################################
# 14. EXPORT REPRODUCIBILITY OUTPUTS
############################################################

write.csv(
  COMPARISON,
  "Appendix_E_Beta_vs_Fractional_Logit.csv",
  row.names = FALSE
)

write.csv(
  FRAC_RESULTS,
  "Appendix_E_Fractional_Logit_HC3.csv",
  row.names = FALSE
)

saveRDS(
  list(
    sample_size = n,
    beta_model = MODEL3_BETA,
    fractional_logit_model = MODEL3_FRAC,
    fractional_HC3_covariance = V_FRAC_HC3,
    comparison = COMPARISON,
    fractional_results = FRAC_RESULTS
  ),
  "Appendix_E_Robustness_Results.rds"
)

############################################################
# 15. FINAL REPORT
############################################################

cat("\n========================================\n")
cat("APPENDIX E - FINAL REPORT\n")
cat("========================================\n")

cat("Beta regression N:", nobs(MODEL3_BETA), "\n")
cat("Fractional logit N:", nobs(MODEL3_FRAC), "\n")
cat(
  "Matching coefficient signs:",
  sum(COMPARISON$Same_Sign[regressors]),
  "of 12\n"
)
cat(
  "Matching significance classifications:",
  sum(COMPARISON$Same_Significance_5pct[regressors]),
  "of 12\n"
)

cat("\nAPPENDIX E VALIDATION COMPLETED.\n")
