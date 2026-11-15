
############################################################
# HOUSEHOLD ULTRA-PROCESSED FOOD CONSUMPTION IN BRAZIL
#
# REPRODUCTION OF DESCRIPTIVE STATISTICS
# TABLES 3, 4, 5, AND 6
#
# Data source: GitHub
# Expected sample size: 562 observations
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

stopifnot(nrow(upbr) == 562)

cat("\nDataset observations:", nrow(upbr), "\n")


############################################################
# 2. PREPARE VARIABLES
############################################################

upbr$INCOME <- as.numeric(upbr$INCOME)
upbr$EDU <- as.numeric(upbr$EDU)
upbr$PROP <- as.numeric(upbr$PROP)

# Include the respondent in household size.
upbr$PEOPLE <- as.numeric(upbr$PEOPLE) + 1

# Occupation indicators
upbr$STUDENT <- as.integer(upbr$OCCUPATION == 1)
upbr$JOBPART <- as.integer(upbr$OCCUPATION == 5)
upbr$JOB <- as.integer(upbr$OCCUPATION == 6)

# Food purchasing location indicators
upbr$SMKT <- as.integer(upbr$SOURCE == 1)
upbr$LOCAL <- as.integer(upbr$SOURCE == 2)


############################################################
# 3. AUXILIARY FUNCTIONS
############################################################

mean_sd <- function(x, digits = 2) {
  x <- as.numeric(x)
  
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f)"),
    mean(x, na.rm = TRUE),
    sd(x, na.rm = TRUE)
  )
}

percentage <- function(x, value = 1) {
  valid <- !is.na(x)
  
  if (!any(valid)) {
    return(NA_real_)
  }
  
  round(
    100 * mean(x[valid] == value),
    2
  )
}

percentage_label <- function(x) {
  sprintf("%.2f%%", x)
}

frequency_distribution <- function(x) {
  x <- as.integer(as.character(x))
  
  counts <- table(
    factor(x, levels = 1:5)
  )
  
  round(100 * prop.table(counts), 2)
}


############################################################
# 4. TABLE 3
# FREQUENCY DISTRIBUTION OF DEPENDENT VARIABLES
############################################################

main_frequency <- frequency_distribution(upbr$MAIN)
nomain_frequency <- frequency_distribution(upbr$NOMAIN)

TABLE3 <- data.frame(
  Variable = c("MAIN", "NOMAIN", "PROP"),
  Never = c(
    main_frequency[1],
    nomain_frequency[1],
    NA
  ),
  Rarely = c(
    main_frequency[2],
    nomain_frequency[2],
    NA
  ),
  Sometimes = c(
    main_frequency[3],
    nomain_frequency[3],
    NA
  ),
  Often = c(
    main_frequency[4],
    nomain_frequency[4],
    NA
  ),
  Always = c(
    main_frequency[5],
    nomain_frequency[5],
    NA
  ),
  Mean_SD = c(
    "-",
    "-",
    mean_sd(upbr$PROP)
  ),
  row.names = NULL
)

cat("\n============================================\n")
cat("TABLE 3 - DEPENDENT VARIABLES\n")
cat("============================================\n")

print(TABLE3, row.names = FALSE)


############################################################
# 5. TABLE 4
# SUMMARY STATISTICS OF ORDINAL AND DISCRETE VARIABLES
############################################################

TABLE4 <- data.frame(
  Variable = c(
    "CHILD",
    "ELDERLY",
    "PEOPLE",
    "INCOME (1000 BRL)",
    "EDU",
    "GENDER (Female)",
    "MARRY",
    "OVER"
  ),
  Mean_SD = c(
    mean_sd(upbr$CHILD),
    mean_sd(upbr$ELDERLY),
    mean_sd(upbr$PEOPLE),
    mean_sd(upbr$INCOME / 1000, 1),
    mean_sd(upbr$EDU),
    "-",
    "-",
    "-"
  ),
  Yes_Percent = c(
    rep(NA_real_, 5),
    percentage(upbr$GENDER, 1),
    percentage(upbr$MARRY, 1),
    percentage(upbr$OVER, 1)
  ),
  No_Percent = c(
    rep(NA_real_, 5),
    percentage(upbr$GENDER, 0),
    percentage(upbr$MARRY, 0),
    percentage(upbr$OVER, 0)
  ),
  Do_Not_Know_Percent = c(
    rep(NA_real_, 7),
    percentage(upbr$OVER, 2)
  ),
  row.names = NULL
)

cat("\n============================================\n")
cat("TABLE 4 - ORDINAL AND DISCRETE VARIABLES\n")
cat("============================================\n")

print(TABLE4, row.names = FALSE)

# Display recorded categories to verify binary coding.
cat("\nGENDER - Recorded categories:\n")
print(table(upbr$GENDER, useNA = "ifany"))

cat("\nMARRY - Recorded categories:\n")
print(table(upbr$MARRY, useNA = "ifany"))

cat("\nOVER - Recorded categories:\n")
print(table(upbr$OVER, useNA = "ifany"))


############################################################
# 6. TABLE 5
# OCCUPATIONAL AND BEHAVIORAL CHARACTERISTICS
############################################################

TABLE5 <- data.frame(
  Category = c(
    rep("Occupation", 3),
    rep("Cooking frequency", 5),
    "UPF consumption"
  ),
  Variable = c(
    "STUDENT",
    "JOBPART",
    "JOB",
    "Never",
    "Rarely",
    "Sometimes",
    "Often",
    "Always",
    "UPFINTAKE (mean)"
  ),
  Value = c(
    percentage_label(percentage(upbr$STUDENT)),
    percentage_label(percentage(upbr$JOBPART)),
    percentage_label(percentage(upbr$JOB)),
    percentage_label(percentage(upbr$COOK, 1)),
    percentage_label(percentage(upbr$COOK, 2)),
    percentage_label(percentage(upbr$COOK, 3)),
    percentage_label(percentage(upbr$COOK, 4)),
    percentage_label(percentage(upbr$COOK, 5)),
    sprintf(
      "%.2f",
      mean(upbr$UPFINTAKE, na.rm = TRUE)
    )
  ),
  row.names = NULL
)

cat("\n============================================\n")
cat("TABLE 5 - OCCUPATIONAL AND BEHAVIORAL\n")
cat("CHARACTERISTICS\n")
cat("============================================\n")

print(TABLE5, row.names = FALSE)


############################################################
# 7. TABLE 6
# FOOD ENVIRONMENT AND UPF CONSUMPTION
############################################################

table6_variables <- c(
  "CONVENIENCE",
  "EASY",
  "NOTIME",
  "SMKT",
  "LOCAL",
  "SWEETS",
  "SWEETDAIRY",
  "PROCMEAT",
  "SOFTDRINKS"
)

TABLE6 <- data.frame(
  Category = c(
    rep("Reasons for UPF consumption", 3),
    rep("Food source", 2),
    rep("UPF type consumed", 4)
  ),
  Variable = table6_variables,
  Value = vapply(
    table6_variables,
    function(variable) {
      percentage_label(
        percentage(upbr[[variable]], 1)
      )
    },
    character(1)
  ),
  row.names = NULL
)

cat("\n============================================\n")
cat("TABLE 6 - FOOD ENVIRONMENT AND\n")
cat("UPF CONSUMPTION\n")
cat("============================================\n")

print(TABLE6, row.names = FALSE)


############################################################
# 8. FINAL VERIFICATION
############################################################

cat("\n============================================\n")
cat("FINAL VERIFICATION\n")
cat("============================================\n")

cat("Sample size:", nrow(upbr), "\n")
cat("Tables reproduced: 3, 4, 5, and 6\n")

############################################################
# END OF SCRIPT
############################################################
