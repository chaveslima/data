
############################################################
# HOUSEHOLD ULTRA-PROCESSED FOOD CONSUMPTION IN BRAZIL
# MULTICOLLINEARITY DIAGNOSTICS - FINAL TABLE 7 MODELS
#
# Model 1: MAIN   - Ordered logit
# Model 2: NOMAIN - Ordered logit
# Model 3: PROP   - Beta regression
#
# Expected analytical sample: N = 562
# No regression models are re-estimated.
# No additional packages are required.
############################################################

############################################################
# 1. LOAD DATA
############################################################

data_url <- paste0(
  "https://raw.githubusercontent.com/",
  "chaveslima/data/refs/heads/main/upbrasil.csv"
)

upbr <- read.csv2(
  data_url,
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = TRUE
)

stopifnot(nrow(upbr) == 562L)

############################################################
# 2. PREPARE VARIABLES
############################################################

required <- c(
  "MAIN", "NOMAIN", "PROP",
  "EDU", "INCOME", "GENDER",
  "CHILD", "ELDERLY", "PEOPLE",
  "OCCUPATION", "SOURCE",
  "COOK", "OVER",
  "SWEETS", "SWEETDAIRY",
  "CONVENIENCE", "NOTIME"
)

stopifnot(all(required %in% names(upbr)))

for (v in required) {
  upbr[[v]] <- as.numeric(upbr[[v]])
}

# Include the respondent in household size.
upbr$PEOPLE <- upbr$PEOPLE + 1

# Construct occupation indicators.
upbr$JOB <- as.integer(upbr$OCCUPATION == 6)
upbr$JOBPART <- as.integer(upbr$OCCUPATION == 5)

# Construct food-source indicators.
upbr$SMKT <- as.integer(upbr$SOURCE == 1)
upbr$LOCAL <- as.integer(upbr$SOURCE == 2)

# Preserve original OVER coding.
upbr$OVER_ORIGINAL <- upbr$OVER

stopifnot(all(
  na.omit(unique(upbr$OVER_ORIGINAL)) %in% c(0, 1, 2)
))

# Recode OVER as affirmative perception.
# Yes = 1; No or Do Not Know = 0.
upbr$OVER <- ifelse(
  is.na(upbr$OVER_ORIGINAL),
  NA_integer_,
  as.integer(upbr$OVER_ORIGINAL == 1)
)

cat("\nOVER CODING\n")
print(table(
  Original = upbr$OVER_ORIGINAL,
  Recoded = upbr$OVER,
  useNA = "ifany"
))

# Check logarithmic transformations.
stopifnot(
  all(upbr$EDU > 0, na.rm = TRUE),
  all(upbr$INCOME > 0, na.rm = TRUE)
)

############################################################
# 3. EXACT EXPLANATORY-VARIABLE SPECIFICATIONS
############################################################

# MAIN and NOMAIN use the same ten regressors.

formula_m1 <- ~
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

formula_m2 <- formula_m1

# PROP uses twelve regressors and binary OVER.

formula_m3 <- ~
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
# 4. DIAGNOSTIC FUNCTIONS
############################################################

get_design_matrix <- function(formula, data) {
  
  frame <- model.frame(
    formula,
    data = data,
    na.action = na.omit
  )
  
  X <- model.matrix(formula, data = frame)
  
  list(
    X = X,
    n = nrow(X)
  )
}

analyze_design <- function(formula, data, model_name) {
  
  design <- get_design_matrix(formula, data)
  
  X <- design$X
  
  Z <- X[
    ,
    colnames(X) != "(Intercept)",
    drop = FALSE
  ]
  
  if (ncol(Z) < 2L) {
    stop("At least two regressors are required.")
  }
  
  variable_sd <- apply(Z, 2, sd)
  
  if (any(!is.finite(variable_sd) |
          variable_sd == 0)) {
    stop(paste(
      "Zero-variance predictor in",
      model_name
    ))
  }
  
  ##########################################################
  # VIF AND TOLERANCE
  ##########################################################
  
  R <- cor(Z)
  
  if (qr(R)$rank != ncol(R)) {
    stop(paste(
      "Rank deficiency detected in",
      model_name
    ))
  }
  
  inverse_R <- solve(R)
  
  vif <- diag(inverse_R)
  tolerance <- 1 / vif
  
  vif_table <- data.frame(
    Predictor = colnames(Z),
    VIF = as.numeric(vif),
    Tolerance = as.numeric(tolerance),
    row.names = NULL
  )
  
  vif_table <- vif_table[
    order(-vif_table$VIF),
    ,
    drop = FALSE
  ]
  
  ##########################################################
  # CONDITION INDICES
  ##########################################################
  
  eigenvalues <- eigen(
    R,
    symmetric = TRUE,
    only.values = TRUE
  )$values
  
  if (any(eigenvalues <= 0)) {
    stop(paste(
      "Nonpositive eigenvalue in",
      model_name
    ))
  }
  
  condition_indices <- sqrt(
    max(eigenvalues) / eigenvalues
  )
  
  condition_table <- data.frame(
    Dimension = seq_along(eigenvalues),
    Eigenvalue = eigenvalues,
    Condition_Index = condition_indices
  )
  
  condition_table <- condition_table[
    order(-condition_table$Condition_Index),
    ,
    drop = FALSE
  ]
  
  # Raw condition number is reported separately.
  # It depends on the units of measurement.
  
  raw_condition_number <- kappa(
    X,
    exact = TRUE
  )
  
  ##########################################################
  # PRINT RESULTS
  ##########################################################
  
  cat("\n========================================\n")
  cat(model_name, "\n")
  cat("========================================\n")
  
  cat("Observations:", design$n, "\n")
  cat("Regressors:", ncol(Z), "\n")
  
  cat(
    "Maximum VIF:",
    max(vif),
    "\n"
  )
  
  cat(
    "Minimum tolerance:",
    min(tolerance),
    "\n"
  )
  
  cat(
    "Maximum standardized condition index:",
    max(condition_indices),
    "\n"
  )
  
  cat(
    "Raw condition number:",
    raw_condition_number,
    "\n"
  )
  
  cat("\nVIF AND TOLERANCE\n")
  
  print(
    vif_table,
    row.names = FALSE,
    digits = 5
  )
  
  cat("\nSTANDARDIZED CONDITION INDICES\n")
  
  print(
    condition_table,
    row.names = FALSE,
    digits = 5
  )
  
  invisible(list(
    observations = design$n,
    regressors = ncol(Z),
    vif = vif_table,
    condition_indices = condition_table,
    raw_condition_number = raw_condition_number
  ))
}

############################################################
# 5. MODEL-SPECIFIC DIAGNOSTICS
############################################################

DIAG1 <- analyze_design(
  formula_m1,
  upbr,
  "MODEL 1 - MAIN"
)

DIAG2 <- analyze_design(
  formula_m2,
  upbr,
  "MODEL 2 - NOMAIN"
)

DIAG3 <- analyze_design(
  formula_m3,
  upbr,
  "MODEL 3 - PROP"
)

############################################################
# 6. VALIDATE SAMPLE SIZE
############################################################

stopifnot(
  DIAG1$observations == 562L,
  DIAG2$observations == 562L,
  DIAG3$observations == 562L
)

############################################################
# 7. FINAL SUMMARY - TABLE A2
############################################################

# AIC values come from the final fitted models.
# No regression models are estimated in this script.

summary_table <- data.frame(
  Model = c(
    "Model 1 - MAIN",
    "Model 2 - NOMAIN",
    "Model 3 - PROP"
  ),
  
  N = c(
    DIAG1$observations,
    DIAG2$observations,
    DIAG3$observations
  ),
  
  Max_VIF = c(
    max(DIAG1$vif$VIF),
    max(DIAG2$vif$VIF),
    max(DIAG3$vif$VIF)
  ),
  
  Min_Tolerance = c(
    min(DIAG1$vif$Tolerance),
    min(DIAG2$vif$Tolerance),
    min(DIAG3$vif$Tolerance)
  ),
  
  Max_Condition_Index = c(
    max(DIAG1$condition_indices$Condition_Index),
    max(DIAG2$condition_indices$Condition_Index),
    max(DIAG3$condition_indices$Condition_Index)
  ),
  
  AIC = c(
    1473.293,
    1511.708,
    -621.5238
  )
)

summary_table$Max_VIF <- round(
  summary_table$Max_VIF, 2
)

summary_table$Min_Tolerance <- round(
  summary_table$Min_Tolerance, 3
)

summary_table$Max_Condition_Index <- round(
  summary_table$Max_Condition_Index, 2
)

summary_table$AIC <- round(
  summary_table$AIC, 2
)

cat("\n========================================\n")
cat("TABLE A2 - MULTICOLLINEARITY DIAGNOSTICS\n")
cat("========================================\n")

print(
  summary_table,
  row.names = FALSE
)

############################################################
# END OF SCRIPT
############################################################

