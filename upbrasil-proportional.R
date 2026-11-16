
############################################################
# HOUSEHOLD UPF CONSUMPTION IN BRAZIL
# APPENDIX D - PROPORTIONAL ODDS DIAGNOSTICS
#
# MODEL 1: MAIN - ORDERED LOGIT
# MODEL 2: NOMAIN - ORDERED LOGIT
#
# Sensitivity analysis:
# Partial proportional odds model for NOMAIN
# with ELDERLY as a nominal effect
############################################################

############################################################
# 1. LOAD PACKAGE
############################################################

library(ordinal)

############################################################
# 2. LOAD DATA
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

# Construct employment indicator.
upbr$JOB <- as.integer(upbr$OCCUPATION == 6)

# Construct food-source indicators.
upbr$SMKT <- as.integer(upbr$SOURCE == 1)
upbr$LOCAL <- as.integer(upbr$SOURCE == 2)

############################################################
# 4. DEFINE COMMON ANALYTICAL SAMPLE
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
# 5. DEFINE FINAL D1 SPECIFICATIONS
############################################################

form_MAIN <- MAIN ~
  log(EDU) +
  log(INCOME) +
  GENDER +
  CHILD +
  ELDERLY +
  PEOPLE +
  JOB +
  SMKT +
  LOCAL +
  COOK

form_NOMAIN <- NOMAIN ~
  log(EDU) +
  log(INCOME) +
  GENDER +
  CHILD +
  ELDERLY +
  PEOPLE +
  JOB +
  SMKT +
  LOCAL +
  COOK

############################################################
# 6. ESTIMATE ORIGINAL ORDERED LOGIT MODELS
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

# Check convergence using the clm convergence structure.
get_codes <- function(fit) {
  if (is.list(fit$convergence)) {
    return(fit$convergence$code)
  }
  fit$convergence
}

stopifnot(
  all(get_codes(MODEL1) == 0),
  all(get_codes(MODEL2) == 0)
)

############################################################
# 7. VERIFY ORIGINAL MODEL FIT
############################################################

cat("\n========================================\n")
cat("ORIGINAL MODEL FIT\n")
cat("========================================\n")

fit_original <- data.frame(
  Model = c("MAIN", "NOMAIN"),
  N = c(nobs(MODEL1), nobs(MODEL2)),
  LogLik = c(
    as.numeric(logLik(MODEL1)),
    as.numeric(logLik(MODEL2))
  ),
  AIC = c(
    AIC(MODEL1),
    AIC(MODEL2)
  ),
  BIC = c(
    BIC(MODEL1),
    BIC(MODEL2)
  )
)

print(
  fit_original,
  row.names = FALSE,
  digits = 7
)

# Confirm that the models match final Table 7.
stopifnot(
  nobs(MODEL1) == 562L,
  nobs(MODEL2) == 562L,
  abs(AIC(MODEL1) - 1473.293) < 0.01,
  abs(AIC(MODEL2) - 1511.708) < 0.01
)

############################################################
# 8. PROPORTIONAL ODDS DIAGNOSTICS
############################################################

# nominal_test adds each regressor to the nominal
# component separately and performs likelihood-ratio tests.

cat("\n========================================\n")
cat("PROPORTIONAL ODDS TEST - MAIN\n")
cat("========================================\n")

PO_MAIN <- nominal_test(MODEL1)
print(PO_MAIN)

cat("\n========================================\n")
cat("PROPORTIONAL ODDS TEST - NOMAIN\n")
cat("========================================\n")

PO_NOMAIN <- nominal_test(MODEL2)
print(PO_NOMAIN)

# An NA result means the auxiliary model did not
# produce a usable test. It must not be interpreted
# as evidence that proportional odds holds.

############################################################
# 9. PARTIAL PROPORTIONAL ODDS MODEL
############################################################

# ELDERLY was previously identified as a possible
# source of proportional odds violation in NOMAIN.
#
# Move ELDERLY from the location formula to
# the nominal formula, avoiding redundant parameters.

form_NOMAIN_PPO <- NOMAIN ~
  log(EDU) +
  log(INCOME) +
  GENDER +
  CHILD +
  PEOPLE +
  JOB +
  SMKT +
  LOCAL +
  COOK

MODEL2_PPO <- clm(
  form_NOMAIN_PPO,
  nominal = ~ ELDERLY,
  data = upbr,
  link = "logit",
  threshold = "flexible"
)

cat("\n========================================\n")
cat("PARTIAL PROPORTIONAL ODDS - NOMAIN\n")
cat("========================================\n")

print(summary(MODEL2_PPO))

cat("\nPPO CONVERGENCE CODES\n")
print(get_codes(MODEL2_PPO))

############################################################
# 10. COMPARE PO AND PPO MODELS
############################################################

cat("\n========================================\n")
cat("NOMAIN - PO VERSUS PPO\n")
cat("========================================\n")

comparison <- data.frame(
  Model = c(
    "NOMAIN - Proportional Odds",
    "NOMAIN - Partial Proportional Odds"
  ),
  N = c(
    nobs(MODEL2),
    nobs(MODEL2_PPO)
  ),
  LogLik = c(
    as.numeric(logLik(MODEL2)),
    as.numeric(logLik(MODEL2_PPO))
  ),
  AIC = c(
    AIC(MODEL2),
    AIC(MODEL2_PPO)
  ),
  BIC = c(
    BIC(MODEL2),
    BIC(MODEL2_PPO)
  )
)

print(
  comparison,
  row.names = FALSE,
  digits = 7
)

############################################################
# 11. LIKELIHOOD-RATIO COMPARISON
############################################################

# The PPO specification relaxes the common-slope
# restriction for ELDERLY.

LR_PPO <- 2 * (
  as.numeric(logLik(MODEL2_PPO)) -
    as.numeric(logLik(MODEL2))
)

df_PPO <- attr(logLik(MODEL2_PPO), "df") -
  attr(logLik(MODEL2), "df")

p_PPO <- pchisq(
  LR_PPO,
  df = df_PPO,
  lower.tail = FALSE
)

cat("\n========================================\n")
cat("LIKELIHOOD-RATIO TEST: PO VS PPO\n")
cat("========================================\n")

cat("LR statistic:", LR_PPO, "\n")
cat("Degrees of freedom:", df_PPO, "\n")
cat("p-value:", p_PPO, "\n")

cat("\nPACKAGE MODEL COMPARISON\n")
print(anova(MODEL2, MODEL2_PPO))

############################################################
# 12. COMPARE COMMON SLOPE COEFFICIENTS
############################################################

# Compare coefficients of variables that remain
# proportional in both specifications.

common_predictors <- c(
  "log(EDU)",
  "log(INCOME)",
  "GENDER",
  "CHILD",
  "PEOPLE",
  "JOB",
  "SMKT",
  "LOCAL",
  "COOK"
)

po_beta <- MODEL2$beta
ppo_beta <- MODEL2_PPO$beta

stopifnot(all(
  common_predictors %in% names(po_beta)
))

stopifnot(all(
  common_predictors %in% names(ppo_beta)
))

slope_comparison <- data.frame(
  Predictor = common_predictors,
  PO_Estimate = as.numeric(
    po_beta[common_predictors]
  ),
  PPO_Estimate = as.numeric(
    ppo_beta[common_predictors]
  )
)

slope_comparison$Same_Sign <- (
  sign(slope_comparison$PO_Estimate) ==
    sign(slope_comparison$PPO_Estimate)
)

cat("\n========================================\n")
cat("COMMON SLOPE COMPARISON\n")
cat("========================================\n")

print(
  slope_comparison,
  row.names = FALSE,
  digits = 6
)

############################################################
# 13. FINAL DIAGNOSTIC SUMMARY
############################################################

cat("\n========================================\n")
cat("APPENDIX D - FINAL SUMMARY\n")
cat("========================================\n")

cat("Analytical sample:", n, "\n")

cat("MAIN AIC:", AIC(MODEL1), "\n")
cat("NOMAIN PO AIC:", AIC(MODEL2), "\n")
cat("NOMAIN PPO AIC:", AIC(MODEL2_PPO), "\n")

cat("PPO LR statistic:", LR_PPO, "\n")
cat("PPO LR degrees of freedom:", df_PPO, "\n")
cat("PPO LR p-value:", p_PPO, "\n")

cat(
  "Common coefficients retaining their signs:",
  sum(slope_comparison$Same_Sign),
  "of",
  nrow(slope_comparison),
  "\n"
)

cat("\nAPPENDIX D DIAGNOSTICS COMPLETED.\n")
