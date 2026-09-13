# ============================================================
# FINAL SPECIFICATION: THREE MODELS FOR CHINA UPF PAPER
# ============================================================

library(ordinal)
library(betareg)
library(lmtest)

# ------------------------------------------------------------
# 1. LOAD AND PREPARE DATA
# ------------------------------------------------------------

dados_url <- 'https://raw.githubusercontent.com/chaveslima/data/main/upchina.csv'
upchina <- read.csv(dados_url)

upchina <- subset(
  upchina,
  age >= 18 & age <= 80 &
    wrksty >= 0 & wrksty <= 16
)

edu_mapping <- c(0, 6, 9, 12, 15, 16, 19, 23)
inc_mapping <- c(1500, 4500, 8000, 12500, 17500, 25000, 35000)

upchina <- transform(
  upchina,
  
  EDU      = edu_mapping[edu],
  LINCOME  = log(inc_mapping[housdispinc]),
  
  GENDER   = ifelse(gender == 2, 1, 0),      # 1 = Female
  MARRY    = ifelse(mrysta == 2, 1, 0),      # 1 = Married
  STUDENT  = ifelse(occu == 1, 1, 0),
  JOBFULL  = ifelse(occu == 6, 1, 0),
  WILLING  = ifelse(wltock == 2, 0, 1),      # 1 = Willing to cook
  OVER     = ifelse(ovwob == 1, 1, 0),
  EC       = ifelse(region == 1, 1, 0),       # 1 = Eastern China
  CITY     = ifelse(ctyctry == 1, 1, 0),      # 1 = Urban
  ONLINE   = ifelse(olgroc == 1, 1, 0),
  
  TAKEOUT  = freqtakout,
  COOKING  = freqck,
  AGE      = age,
  
  MAIN     = ordered(mmUPFscons, levels = 1:5),
  NOMAIN   = ordered(snkUPFscons, levels = 1:5),
  
  # Keep the original proportion
  PROP_RAW = propUPFscons / 100
)


# ------------------------------------------------------------
# 2. TRANSFORM PROP FROM [0,1] TO (0,1)
#    FOR BETA REGRESSION
# ------------------------------------------------------------

N_PROP <- sum(!is.na(upchina$PROP_RAW))

upchina$PROP <- (
  upchina$PROP_RAW * (N_PROP - 1) + 0.5
) / N_PROP

# Check
range(upchina$PROP_RAW)
range(upchina$PROP)

sum(upchina$PROP_RAW == 0)
sum(upchina$PROP_RAW == 1)


# ------------------------------------------------------------
# 3. MEAN-CENTER VARIABLES USED IN INTERACTIONS
# ------------------------------------------------------------

upchina$AGE_c <-
  upchina$AGE - mean(upchina$AGE, na.rm = TRUE)

upchina$EDU_c <-
  upchina$EDU - mean(upchina$EDU, na.rm = TRUE)

upchina$LINCOME_c <-
  upchina$LINCOME - mean(upchina$LINCOME, na.rm = TRUE)


# ------------------------------------------------------------
# 4. MODEL 1: ORDERED LOGIT — MAIN MEALS
# ------------------------------------------------------------

MODEL_MAIN <- clm(
  MAIN ~ AGE + EDU + LINCOME +
    MARRY + COOKING + WILLING +
    JOBFULL + STUDENT +
    CITY + ONLINE + TAKEOUT +
    GENDER + EC + OVER,
  data = upchina,
  link = "logit",
  threshold = "flexible"
)

summary(MODEL_MAIN)


# ------------------------------------------------------------
# 5. MODEL 2: ORDERED LOGIT — NON-MAIN MEALS
# ------------------------------------------------------------

MODEL_NOMAIN <- clm(
  NOMAIN ~ AGE + EDU + LINCOME +
    MARRY + COOKING + WILLING +
    JOBFULL + STUDENT +
    CITY + ONLINE + TAKEOUT +
    GENDER + EC + OVER,
  data = upchina,
  link = "logit",
  threshold = "flexible"
)

summary(MODEL_NOMAIN)


# ------------------------------------------------------------
# 6. MODEL 3: BETA REGRESSION — UPF SHARE
# ------------------------------------------------------------

MODEL_PROP <- betareg(
  PROP ~ AGE_c + EDU_c + LINCOME_c + JOBFULL + 
    MARRY + COOKING + WILLING +
    STUDENT +
    CITY + ONLINE + TAKEOUT +
    GENDER + EC + OVER +
    LINCOME_c:EDU_c +
    AGE_c:EDU_c,
  data = upchina,
  link = "logit"
)

summary(MODEL_PROP)


# ------------------------------------------------------------
# 7. PROP BASE MODEL — FOR JOINT LR TEST OF INTERACTIONS
# ------------------------------------------------------------

MODEL_PROP_BASE <- betareg(
  PROP ~ AGE_c + EDU_c + LINCOME_c +
    MARRY + COOKING + WILLING +
     STUDENT + JOBFULL + 
    CITY + ONLINE + TAKEOUT +
    GENDER + EC + OVER,
  data = upchina,
  link = "logit"
)

lrtest(MODEL_PROP_BASE, MODEL_PROP)
