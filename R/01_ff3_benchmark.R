# ============================================================
# ECON 307 – Benchmark Fama–French 3-Factor Model for NVIDIA
# ============================================================

# --------- 0. Setup ---------
library(quantmod)
library(dplyr)
library(zoo)
library(pastecs)
library(ggcorrplot)
library(lmtest)
library(sandwich)
library(car)

options(scipen = 100)
options(digits = 3)

# --------- 1. Get NVIDIA monthly returns ---------

# 1.1 Download NVDA daily from Yahoo
nvda_xts <- getSymbols(
  "NVDA",
  src  = "yahoo",
  from = "2010-01-01",
  auto.assign = FALSE
)

# 1.2 Adjusted close and monthly log returns (%)
nvda_prices <- Ad(nvda_xts)

nvda_ret_xts <- monthlyReturn(nvda_prices, type = "log") * 100

nvda_m <- data.frame(
  date     = as.Date(index(nvda_ret_xts)),
  nvda_ret = as.numeric(nvda_ret_xts)
) %>%
  mutate(ym = as.yearmon(date))  # year-month key

# --------- 2. Get Fama–French 3 factors (U.S.) ---------

ff_url  <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"
tmp_zip <- tempfile()
download.file(ff_url, tmp_zip, mode = "wb")

ff_raw <- read.csv(
  unz(tmp_zip, "F-F_Research_Data_Factors.csv"),
  skip = 3,
  stringsAsFactors = FALSE
)

# Drop footer / blank rows
ff_raw <- ff_raw[ff_raw$X != "" & ff_raw$X != " ", ]

# Keep first 5 columns: yyyymm, Mkt-RF, SMB, HML, RF
ff3 <- ff_raw[, 1:5]
colnames(ff3) <- c("yyyymm", "Mkt_RF", "SMB", "HML", "RF")

ff3 <- ff3 %>%
  mutate(
    yyyymm = trimws(yyyymm),
    Mkt_RF = as.numeric(Mkt_RF),
    SMB    = as.numeric(SMB),
    HML    = as.numeric(HML),
    RF     = as.numeric(RF),
    date   = as.Date(paste0(yyyymm, "01"), format = "%Y%m%d"),
    ym     = as.yearmon(date)
  ) %>%
  filter(!is.na(ym),
         date >= as.Date("2010-01-01"))

# --------- 3. Merge NVDA with FF3 and build excess return ---------

data_ff3 <- ff3 %>%
  select(ym, Mkt_RF, SMB, HML, RF) %>%
  inner_join(nvda_m %>% select(ym, nvda_ret), by = "ym") %>%
  mutate(
    date        = as.Date(ym, frac = 1),      # end of month
    excess_nvda = nvda_ret - RF               # excess return over risk-free
  ) %>%
  select(date, ym, excess_nvda, nvda_ret, Mkt_RF, SMB, HML, RF) %>%
  drop_na()

cat("FF3 sample size:", nrow(data_ff3), "\n")
cat("Date range:", format(min(data_ff3$date)), "to", format(max(data_ff3$date)), "\n\n")

# --------- 4. Descriptive stats & correlations ---------

vars_ff3 <- data_ff3 %>%
  select(excess_nvda, Mkt_RF, SMB, HML)

cat("===== DESCRIPTIVE STATISTICS: FF3 DATA =====\n")
print(stat.desc(vars_ff3, basic = TRUE))

cat("\n===== CORRELATION MATRIX: FF3 DATA =====\n")
if (nrow(vars_ff3) >= 2) {
  corr_ff3 <- cor(vars_ff3, use = "complete.obs")
  print(round(corr_ff3, 3))
  
  ggcorrplot(
    corr_ff3,
    type  = "lower",
    lab   = TRUE,
    title = "Correlation Heatmap – NVDA Excess Return & FF3 Factors",
    ggtheme = theme_minimal()
  )
} else {
  cat("Not enough observations for correlation matrix.\n")
}

# --------- 5. Fama–French 3-factor regression ---------

# Model: excess_nvda ~ Mkt_RF + SMB + HML
ff3_model <- lm(excess_nvda ~ Mkt_RF + SMB + HML, data = data_ff3)

cat("\n===== OLS SUMMARY: Fama–French 3-Factor Model =====\n")
print(summary(ff3_model))

# Robust (HC1) standard errors
cat("\n===== COEFFICIENTS WITH ROBUST (HC1) SE =====\n")
print(coeftest(ff3_model, vcov = vcovHC(ff3_model, type = "HC1")))

# AIC
cat("\nAIC (FF3 model):", AIC(ff3_model), "\n")

# --------- 6. Plots (like the lab) ---------

ff3_resid  <- resid(ff3_model)
ff3_fitted <- fitted(ff3_model)

# Residuals vs Fitted
plot(ff3_fitted, ff3_resid,
     main = "Residuals vs Fitted: FF3 Model for NVDA",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")

# Q–Q Plot
qqnorm(ff3_resid, main = "Q–Q Plot of Residuals: FF3 NVDA Model")
qqline(ff3_resid, col = "red")

# NVDA excess return time series
plot(data_ff3$date, data_ff3$excess_nvda,
     main = "NVIDIA Excess Monthly Returns (FF3 Sample)",
     xlab = "Year",
     ylab = "Excess Return (%)",
     pch  = 1)

# Excess NVDA vs Market
plot(data_ff3$Mkt_RF, data_ff3$excess_nvda,
     main = "NVDA Excess Return vs Market (Mkt_RF)",
     xlab = "Market Risk Premium (%)",
     ylab = "NVIDIA Excess Return (%)",
     pch  = 1)

# Excess NVDA vs SMB
plot(data_ff3$SMB, data_ff3$excess_nvda,
     main = "NVDA Excess Return vs Size Factor (SMB)",
     xlab = "SMB (%)",
     ylab = "NVIDIA Excess Return (%)",
     pch  = 1)

# Excess NVDA vs HML
plot(data_ff3$HML, data_ff3$excess_nvda,
     main = "NVDA Excess Return vs Value Factor (HML)",
     xlab = "HML (%)",
     ylab = "NVIDIA Excess Return (%)",
     pch  = 1)

# --------- 7. Optional: Compare to YOUR NVDA model ---------
# (Only runs if reg_nvda already exists in your environment)

if (exists("reg_nvda")) {
  cat("\n===== COMPARISON: FF3 vs Your NVDA Model =====\n")
  cat("FF3 R-squared:   ", summary(ff3_model)$r.squared, "\n")
  cat("Your model R^2:  ", summary(reg_nvda)$r.squared, "\n")
  cat("FF3 AIC:         ", AIC(ff3_model), "\n")
  cat("Your model AIC:  ", AIC(reg_nvda), "\n")
}