############################################################
# HOUSEHOLD UPF CONSUMPTION IN BRAZIL
# MODEL D1 - PURE SPECIFICATION
# ORDERED LOGIT: MAIN AND NOMAIN
############################################################

############################################################
# 1. LOAD PACKAGE
############################################################

library(ordinal)

############################################################
# 2. LOAD DATA FROM GITHUB
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

# View(upbr)
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

# Employment indicator.
upbr$JOB <- as.integer(
  upbr$OCCUPATION == 6
)

# Supermarket acquisition indicator.
upbr$SMKT <- as.integer(
  upbr$SOURCE == 1
)

# Local market acquisition indicator.
upbr$LOCAL <- as.integer(
  upbr$SOURCE == 2
)

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

upbr$MAIN <- ordered(
  upbr$MAIN,
  levels = 1:5
)

upbr$NOMAIN <- ordered(
  upbr$NOMAIN,
  levels = 1:5
)

cat("\nAnalytical sample:", nrow(upbr), "\n")

stopifnot(nrow(upbr) == 562)

############################################################
# 5. MODEL D1 - HARMONIZED SPECIFICATION
############################################################

# Ten identical explanatory variables in both models.

form_MAIN <- MAIN ~
  log(EDU) + log(INCOME) +
  GENDER + CHILD + ELDERLY + PEOPLE +
  JOB + SMKT + LOCAL + COOK

form_NOMAIN <- NOMAIN ~
  log(EDU) + log(INCOME) +
  GENDER + CHILD + ELDERLY + PEOPLE +
  JOB + SMKT + LOCAL + COOK

############################################################
# 6. MODEL 1 - MAIN
############################################################

MODEL1 <- clm(
  form_MAIN,
  data = upbr,
  link = "logit",
  threshold = "flexible"
)

############################################################
# 7. MODEL 2 - NOMAIN
############################################################

MODEL2 <- clm(
  form_NOMAIN,
  data = upbr,
  link = "logit",
  threshold = "flexible"
)

############################################################
# 8. ESTIMATION RESULTS
############################################################

cat("\n========== MODEL D1: MAIN ==========\n")
print(summary(MODEL1))

cat("\n========== MODEL D1: NOMAIN ==========\n")
print(summary(MODEL2))

############################################################
# 9. PROPORTIONAL ODDS DIAGNOSTICS
############################################################

cat("\n========== PO TEST: MAIN ==========\n")
print(nominal_test(MODEL1))

cat("\n========== PO TEST: NOMAIN ==========\n")
print(nominal_test(MODEL2))

############################################################
# 10. MODEL FIT
############################################################

fit_table <- data.frame(
  Model = c("D1_MAIN", "D1_NOMAIN"),
  N = c(
    nobs(MODEL1),
    nobs(MODEL2)
  ),
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

cat("\n========== MODEL D1: FIT ==========\n")

print(
  fit_table,
  row.names = FALSE,
  digits = 5
)

cat("\nMODEL D1 COMPLETED.\n")
