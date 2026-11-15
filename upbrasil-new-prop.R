############################################################
# HOUSEHOLD UPF CONSUMPTION IN BRAZIL
# MODEL 3 - BINARY PERCEPTION OF OVERWEIGHT
# BETA REGRESSION - TABLE 7
############################################################

library(betareg)

############################################################
# 1. LOAD DATA
############################################################

url <- paste0(
  "https://raw.githubusercontent.com/chaveslima/",
  "data/refs/heads/main/upbrasil.csv"
)

upbr <- read.csv2(url, header = TRUE, stringsAsFactors = FALSE)

############################################################
# 2. PREPARE VARIABLES
############################################################

required <- c(
  "PROP", "INCOME", "EDU", "ELDERLY", "CHILD",
  "OCCUPATION", "SWEETS", "SWEETDAIRY", "COOK",
  "CONVENIENCE", "NOTIME", "OVER"
)
stopifnot(all(required %in% names(upbr)))

for (v in required) {
  upbr[[v]] <- as.numeric(upbr[[v]])
}

# Keep original coding for a transparent comparison.
upbr$OVER_ORIGINAL <- upbr$OVER
stopifnot(all(na.omit(unique(upbr$OVER_ORIGINAL)) %in% c(0, 1, 2)))

cat("\nORIGINAL OVER RESPONSES\n")
print(table(upbr$OVER_ORIGINAL, useNA = "ifany"))

# Binary affirmative perception: Yes = 1; No or Do Not Know = 0.
# Preserve missing observations as NA rather than recoding them to No.
upbr$OVER <- ifelse(
  is.na(upbr$OVER_ORIGINAL), NA_integer_,
  as.integer(upbr$OVER_ORIGINAL == 1)
)

cat("\nRECODED OVER RESPONSES\n")
print(table(upbr$OVER, useNA = "ifany"))

# Create occupation indicators.
upbr$JOB <- as.integer(upbr$OCCUPATION == 6)
upbr$JOBPART <- as.integer(upbr$OCCUPATION == 5)

############################################################
# 3. ANALYTICAL SAMPLE
############################################################

vars <- c(
  "PROP", "INCOME", "EDU", "ELDERLY", "CHILD", "JOB",
  "JOBPART", "SWEETS", "SWEETDAIRY", "COOK",
  "CONVENIENCE", "NOTIME", "OVER", "OVER_ORIGINAL"
)

valid <- complete.cases(upbr[, vars]) &
  upbr$PROP >= 0 & upbr$PROP <= 1 &
  upbr$EDU > 0 & upbr$INCOME > 0

upbr <- upbr[valid, , drop = FALSE]
n <- nrow(upbr)
cat("\nANALYTICAL SAMPLE:", n, "\n")
stopifnot(n == 562)

############################################################
# 4. SMITHSON-VERKUILEN TRANSFORMATION
############################################################

upbr$PROP_beta <- (upbr$PROP * (n - 1) + 0.5) / n

############################################################
# 5. CORRECTED MODEL 3
############################################################

form_PROP <- PROP_beta ~
  log(EDU) + log(INCOME) + CHILD + ELDERLY +
  JOB + JOBPART + SWEETS + CONVENIENCE + NOTIME +
  OVER + SWEETDAIRY + COOK

MODEL3 <- betareg(form_PROP, data = upbr, link = "logit")
cat("\n========== CORRECTED PROP MODEL ==========\n")
print(summary(MODEL3))

############################################################
# 6. NULL MODEL AND LIKELIHOOD-RATIO TEST
############################################################

MODEL3_NULL <- betareg(PROP_beta ~ 1, data = upbr, link = "logit")
LR <- 2 * (as.numeric(logLik(MODEL3)) -
             as.numeric(logLik(MODEL3_NULL)))
df_LR <- attr(logLik(MODEL3), "df") -
  attr(logLik(MODEL3_NULL), "df")
p_LR <- pchisq(LR, df = df_LR, lower.tail = FALSE)

s <- summary(MODEL3)
cat("\n========== MODEL FIT ==========\n")
cat("N:", nobs(MODEL3), "\n")
cat("LogLik:", as.numeric(logLik(MODEL3)), "\n")
cat("AIC:", AIC(MODEL3), "\n")
cat("BIC:", BIC(MODEL3), "\n")
cat("Pseudo R-squared:", s$pseudo.r.squared, "\n")
cat("LR statistic:", LR, "\n")
cat("LR degrees of freedom:", df_LR, "\n")
cat("LR p-value:", p_LR, "\n")

############################################################
# 7. COMPARE AGAINST ORIGINAL NUMERIC OVER CODING
############################################################

# This comparison is descriptive; the original encoding
# incorrectly imposes a numerical ordering on the DK category.
MODEL3_OLD_OVER <- betareg(
  update(form_PROP, . ~ . - OVER + OVER_ORIGINAL),
  data = upbr,
  link = "logit"
)

comparison <- data.frame(
  Model = c("Previous numeric OVER", "Corrected binary OVER"),
  N = c(nobs(MODEL3_OLD_OVER), nobs(MODEL3)),
  LogLik = c(as.numeric(logLik(MODEL3_OLD_OVER)),
             as.numeric(logLik(MODEL3))),
  AIC = c(AIC(MODEL3_OLD_OVER), AIC(MODEL3)),
  BIC = c(BIC(MODEL3_OLD_OVER), BIC(MODEL3))
)

cat("\n========== MODEL COMPARISON ==========\n")
print(comparison, row.names = FALSE, digits = 7)
cat("\nCORRECTED PROP ESTIMATION COMPLETED.\n")
