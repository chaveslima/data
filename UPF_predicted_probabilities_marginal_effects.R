############################################################
# HOUSEHOLD UPF CONSUMPTION IN BRAZIL
# PREDICTED PROBABILITIES AND AVERAGE DISCRETE EFFECTS
# Models 1-2: ordered logit, Pr(Y >= 4)
# Model 3: beta regression, expected PROP
############################################################

# 1. Load required packages.
for (pkg in c("ordinal", "betareg")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Install the required package first: ", pkg)
  }
}

# 2. Read the original dataset.
data_url <- paste0(
  "https://raw.githubusercontent.com/chaveslima/data/",
  "refs/heads/main/upbrasil.csv"
)
dat <- read.csv2(data_url, header = TRUE, stringsAsFactors = FALSE)

numeric_vars <- c(
  "MAIN", "NOMAIN", "PROP", "EDU", "INCOME", "GENDER",
  "CHILD", "ELDERLY", "PEOPLE", "OCCUPATION", "SOURCE",
  "COOK", "SWEETS", "SWEETDAIRY", "CONVENIENCE",
  "NOTIME", "OVER"
)
stopifnot(all(numeric_vars %in% names(dat)))
for (v in numeric_vars) dat[[v]] <- as.numeric(dat[[v]])

# 3. Apply the final variable definitions.
dat$PEOPLE <- dat$PEOPLE + 1
# 1 = affirmative perception; 0 = No or Do Not Know.
dat$OVER_ORIGINAL <- dat$OVER
dat$OVER <- ifelse(
  is.na(dat$OVER_ORIGINAL), NA_integer_,
  as.integer(dat$OVER_ORIGINAL == 1)
)
dat$JOB <- as.integer(dat$OCCUPATION == 6)
dat$JOBPART <- as.integer(dat$OCCUPATION == 5)
dat$SMKT <- as.integer(dat$SOURCE == 1)
dat$LOCAL <- as.integer(dat$SOURCE == 2)

# Use a common complete-case sample for all three models.
model_vars <- c(
  "MAIN", "NOMAIN", "PROP", "EDU", "INCOME", "GENDER",
  "CHILD", "ELDERLY", "PEOPLE", "COOK", "JOB", "JOBPART",
  "SMKT", "LOCAL", "SWEETS", "SWEETDAIRY", "CONVENIENCE",
  "NOTIME", "OVER"
)
keep <- complete.cases(dat[, model_vars]) &
  dat$EDU > 0 & dat$INCOME > 0 &
  dat$MAIN %in% 1:5 & dat$NOMAIN %in% 1:5 &
  dat$PROP >= 0 & dat$PROP <= 1
dat <- dat[keep, , drop = FALSE]
stopifnot(nrow(dat) == 562L)
stopifnot(sum(dat$OVER == 1) == 280L,
          sum(dat$OVER == 0) == 282L)

# 4. Estimate exactly the models in final Table 7.
dat$MAIN <- ordered(dat$MAIN, levels = 1:5)
dat$NOMAIN <- ordered(dat$NOMAIN, levels = 1:5)
n <- nrow(dat)
dat$PROP_beta <- (dat$PROP * (n - 1) + 0.5) / n

rhs_ordered <- paste(
  "log(EDU) + log(INCOME) + GENDER + CHILD + ELDERLY +",
  "PEOPLE + JOB + SMKT + LOCAL + COOK"
)
f_MAIN <- as.formula(paste("MAIN ~", rhs_ordered))
f_NOMAIN <- as.formula(paste("NOMAIN ~", rhs_ordered))
f_PROP <- PROP_beta ~ log(EDU) + log(INCOME) + CHILD +
  ELDERLY + JOB + JOBPART + SWEETS + CONVENIENCE + NOTIME +
  OVER + SWEETDAIRY + COOK

m_MAIN <- ordinal::clm(f_MAIN, data = dat,
                       link = "logit", threshold = "flexible")
m_NOMAIN <- ordinal::clm(f_NOMAIN, data = dat,
                         link = "logit", threshold = "flexible")
m_PROP <- betareg::betareg(f_PROP, data = dat, link = "logit")

stopifnot(
  abs(AIC(m_MAIN) - 1473.293) < 0.01,
  abs(AIC(m_NOMAIN) - 1511.708) < 0.01,
  abs(AIC(m_PROP) - (-621.5238)) < 0.01
)

# 5. Prediction functions, expressed on a 0-100 percentage scale.
pred_ordered <- function(fit, newdata) {
  # Remove ordered outcomes so ordinal::predict returns all five classes.
  newdata <- newdata[, setdiff(names(newdata),
                                c("MAIN", "NOMAIN")), drop = FALSE]
  probs <- predict(fit, newdata = newdata, type = "prob")$fit
  stopifnot(is.matrix(probs), nrow(probs) == nrow(newdata),
            all(c("4", "5") %in% colnames(probs)))
  100 * rowSums(probs[, c("4", "5"), drop = FALSE])
}
pred_prop <- function(fit, newdata) {
  # Convert the fitted mean of transformed PROP back to PROP units.
  mu_beta <- as.numeric(predict(fit, newdata = newdata,
                                type = "response"))
  100 * ((n * mu_beta - 0.5) / (n - 1))
}

# 6. Define meaningful, within-range counterfactual contrasts.
# The effect equals the mean prediction under scenario B minus
# the mean prediction under scenario A, in percentage points.
make_contrast <- function(data, variable, change) {
  d <- data
  if (change == "one_year") {
    a <- d
    b <- d
    b[[variable]] <- b[[variable]] + 1
    label <- "+1 year"
  } else if (change == "ten_percent") {
    a <- d
    b <- d
    b[[variable]] <- b[[variable]] * 1.10
    label <- "+10%"
  } else if (change == "one_person") {
    a <- d
    b <- d
    b[[variable]] <- b[[variable]] + 1
    label <- "+1 person"
  } else if (change == "one_cook") {
    # The COOK scale ranges from 1 to 5; avoid going above 5.
    d <- d[d[[variable]] < 5, , drop = FALSE]
    a <- d
    b <- d
    b[[variable]] <- b[[variable]] + 1
    label <- "+1 category, among COOK < 5"
  } else if (change == "binary") {
    a <- d
    b <- d
    a[[variable]] <- 0
    b[[variable]] <- 1
    label <- "0 to 1"
  } else {
    stop("Unknown contrast: ", change)
  }
  list(a = a, b = b, change = label)
}

# 7. Define comparisons for the ordered and beta models.
ordered_contrasts <- data.frame(
  variable = c("EDU", "INCOME", "CHILD", "ELDERLY",
               "PEOPLE", "JOB", "COOK"),
  change = c("one_year", "ten_percent", "one_person",
             "one_person", "one_person", "binary", "one_cook")
)
prop_contrasts <- data.frame(
  variable = c("EDU", "INCOME", "CHILD", "ELDERLY",
               "CONVENIENCE", "NOTIME", "OVER", "SWEETS",
               "SWEETDAIRY", "COOK"),
  change = c("one_year", "ten_percent", "one_person",
             "one_person", "binary", "binary", "binary",
             "binary", "binary", "one_cook")
)

# 8. Compute average standardized predictions and effects.
results_for_model <- function(fit, data, specs, outcome, pred_fun) {
  rows <- lapply(seq_len(nrow(specs)), function(i) {
    variable <- specs$variable[i]
    contrast <- make_contrast(data, variable, specs$change[i])
    p0 <- pred_fun(fit, contrast$a)
    p1 <- pred_fun(fit, contrast$b)
    stopifnot(length(p0) == length(p1),
              all(is.finite(p0)), all(is.finite(p1)))
    data.frame(
      Outcome = outcome,
      Predictor = variable,
      Change = contrast$change,
      N_used = length(p0),
      Predicted_A_percent = mean(p0),
      Predicted_B_percent = mean(p1),
      Difference_pp = mean(p1 - p0)
    )
  })
  do.call(rbind, rows)
}

results <- rbind(
  results_for_model(m_MAIN, dat, ordered_contrasts,
                    "MAIN: Pr(Y >= 4)", pred_ordered),
  results_for_model(m_NOMAIN, dat, ordered_contrasts,
                    "NOMAIN: Pr(Y >= 4)", pred_ordered),
  results_for_model(m_PROP, dat, prop_contrasts,
                    "PROP: E(PROP)", pred_prop)
)
rownames(results) <- NULL

cat("\nPREDICTED PROBABILITIES AND AVERAGE EFFECTS\n")
print(results, row.names = FALSE, digits = 5)

# 9. Report observed-sample baseline mean predictions.
baselines <- data.frame(
  Outcome = c("MAIN: Pr(Y >= 4)", "NOMAIN: Pr(Y >= 4)",
              "PROP: E(PROP)"),
  Predicted_percent = c(
    mean(pred_ordered(m_MAIN, dat)),
    mean(pred_ordered(m_NOMAIN, dat)),
    mean(pred_prop(m_PROP, dat))
  )
)
cat("\nAVERAGE FITTED PROBABILITIES / PROP\n")
print(baselines, row.names = FALSE, digits = 5)

# 10. Export results for the Results section revision.
write.csv(results, "UPF_Average_Discrete_Effects.csv",
          row.names = FALSE)
write.csv(baselines, "UPF_Predicted_Baselines.csv",
          row.names = FALSE)

cat("\nAnalysis complete. All effects are in percentage points.\n")
cat("These are point estimates; confidence intervals are not computed.\n")
