# ============================================================
# ECON 307 Project – NVIDIA Risk Factor Model (Fama–French + VIX)
# Ready to use with knitr::spin("log.R")
# ============================================================

# --------- 0. Setup ---------

# Packages
library(quantmod)
library(dplyr)
library(lubridate)
library(pastecs)
library(ggcorrplot)
library(lmtest)
library(sandwich)
library(car)
library(zoo)
library(tidyr)

# Nice printing
options(scipen = 100)  # avoid scientific notation
options(digits = 3)

# --------- 1. Build data_ff_vix (NVDA + FF3 + VIX) ---------

# 1.1 Get NVDA and VIX daily data from Yahoo (U.S. market)
# Use auto.assign = FALSE so we don't rely on weird object names like ^VIX

nvda_xts <- getSymbols("NVDA",
                       src  = "yahoo",
                       from = "2010-01-01",
                       auto.assign = FALSE)

vix_xts <- getSymbols("^VIX",
                      src  = "yahoo",
                      from = "2010-01-01",
                      auto.assign = FALSE)

# Adjusted close for NVDA, close for VIX
nvda_prices <- Ad(nvda_xts)
vix_levels  <- Cl(vix_xts)

# 1.2 Convert to monthly and compute returns / changes

# Monthly NVDA log returns (%) – one obs per month
nvda_ret_xts <- monthlyReturn(nvda_prices, type = "log") * 100
nvda_ret_m <- data.frame(
  date     = as.Date(index(nvda_ret_xts)),
  nvda_ret = as.numeric(nvda_ret_xts)
) %>%
  mutate(ym = as.yearmon(date))   # year-month key

# Monthly VIX level and monthly change
vix_m_xts <- to.monthly(vix_levels, indexAt = "lastof", OHLC = FALSE)
vix_m <- data.frame(
  date      = as.Date(index(vix_m_xts)),
  vix_close = as.numeric(vix_m_xts)
) %>%
  arrange(date) %>%
  mutate(
    vix_chg = vix_close - dplyr::lag(vix_close),
    ym      = as.yearmon(date)    # same year-month key
  )
#new variable
data_spec_sq <- data_spec %>%
  mutate(Mkt_RF2 = Mkt_RF^2)



# 1.3 Download Fama–French 3-factor monthly data (U.S.)

ff_url  <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"
tmp_zip <- tempfile()
download.file(ff_url, tmp_zip, mode = "wb")

# NOTE: internal file is lowercase .csv, not .CSV
ff_raw <- read.csv(unz(tmp_zip, "F-F_Research_Data_Factors.csv"),
                   skip = 3, stringsAsFactors = FALSE)

# Drop footer and blank rows
ff_raw <- ff_raw[ff_raw$X != "" & ff_raw$X != " ", ]

# Keep first 5 columns: yyyymm, Mkt-RF, SMB, HML, RF
ff3 <- ff_raw[, 1:5]
colnames(ff3) <- c("yyyymm", "Mkt_RF", "SMB", "HML", "RF")

# Clean types and build Date + yearmon key
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

# 1.4 Merge everything by year-month and construct excess NVDA return

data_ff_vix <- ff3 %>%
  select(ym, Mkt_RF, HML, RF) %>%
  inner_join(nvda_ret_m %>% select(ym, nvda_ret), by = "ym") %>%
  inner_join(vix_m %>% select(ym, vix_chg), by = "ym") %>%
  mutate(
    date        = as.Date(ym, frac = 1),     # end-of-month date
    excess_nvda = nvda_ret - RF              # excess return over RF
  ) %>%
  select(date, ym, excess_nvda, nvda_ret, Mkt_RF, HML, RF, vix_chg) %>%
  drop_na()

cat("data_ff_vix observations:", nrow(data_ff_vix), "\n")
print(range(data_ff_vix$date))

# --------- 2. Descriptive statistics & correlations ---------

vars <- data_ff_vix %>%
  select(excess_nvda, Mkt_RF, HML, vix_chg)

cat("\n===== DESCRIPTIVE STATISTICS =====\n")
print(stat.desc(vars, basic = TRUE))

cat("\n===== CORRELATION MATRIX =====\n")
if (nrow(vars) >= 2) {
  corr_mat <- cor(vars, use = "complete.obs")
  print(round(corr_mat, 3))
  
  # Correlation heatmap
  ggcorrplot(corr_mat,
             type = "lower",
             lab = TRUE,
             title = "Correlation Heatmap – NVDA Model Variables",
             ggtheme = theme_minimal())
} else {
  cat("Not enough observations for a correlation matrix.\n")
}

# --------- 3. Add AI boom dummy (post-2020) ---------

data_spec <- data_ff_vix %>%
  mutate(
    AI_boom = ifelse(date >= as.Date("2020-01-01"), 1, 0)
  )

cat("\nFinal sample size:", nrow(data_spec), "\n")
cat("Date range:", format(min(data_spec$date)), "to", format(max(data_spec$date)), "\n")

# --------- 4. Regression model (simple final spec) ---------

# Model:
# excess_nvda ~ Mkt_RF + HML + vix_chg + AI_boom

reg_nvda <- lm(excess_nvda ~ Mkt_RF + HML + vix_chg + AI_boom,
               data = data_spec)

cat("\n===== OLS SUMMARY: SIMPLE NVDA MODEL =====\n")
print(summary(reg_nvda))

# --------- 5. Residual analysis & plots ---------

nvda_resid  <- resid(reg_nvda)
nvda_fitted <- fitted(reg_nvda)

# Residuals vs Fitted
plot(nvda_fitted, nvda_resid,
     main = "Residuals vs Fitted: NVDA Model",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")

# Q–Q Plot
qqnorm(nvda_resid, main = "Q–Q Plot of Residuals: NVDA Model")
qqline(nvda_resid, col = "red")

# NVIDIA excess return over time
plot(data_spec$date, data_spec$excess_nvda,
     main = "NVIDIA Excess Monthly Returns, 2010–2025",
     xlab = "Year",
     ylab = "NVIDIA Excess Return (%)",
     pch  = 1)

# Excess NVDA vs Market risk premium
plot(data_spec$Mkt_RF, data_spec$excess_nvda,
     main = "NVIDIA Excess Return vs Market Risk Premium (MKT_RF)",
     xlab = "Market Risk Premium MKT_RF (%)",
     ylab = "NVIDIA Excess Return (%)",
     pch  = 1)

# Excess NVDA vs HML (value factor)
plot(data_spec$HML, data_spec$excess_nvda,
     main = "NVIDIA Excess Return vs Value Factor (HML)",
     xlab = "HML (%)",
     ylab = "NVIDIA Excess Return (%)",
     pch  = 1)

# Excess NVDA vs change in VIX
plot(data_spec$vix_chg, data_spec$excess_nvda,
     main = "NVIDIA Excess Return vs Change in VIX",
     xlab = "Change in VIX (Index Points)",
     ylab = "NVIDIA Excess Return (%)",
     pch  = 1)

# --------- 6. Tests: Normality, Heteroskedasticity, Serial Corr, VIF ---------

cat("\n===== SHAPIRO–WILK NORMALITY TEST =====\n")
print(shapiro.test(nvda_resid))

cat("\n===== BREUSCH–PAGAN (HETEROSKEDASTICITY) =====\n")
print(bptest(reg_nvda))

cat("\n===== BREUSCH–GODFREY (AR(1) SERIAL CORR) =====\n")
print(bgtest(reg_nvda, order = 1))

cat("\n===== VIF (MULTICOLLINEARITY) =====\n")
print(vif(reg_nvda))

# --------- 7. RESET & Robust SE (like lab) ---------

cat("\n===== RAMSEY RESET TEST =====\n")
print(resettest(reg_nvda, power = 2:3, type = "regressor"))

cat("\n===== COEFFICIENTS WITH ROBUST (HC1) SE =====\n")
print(coeftest(reg_nvda, vcov = vcovHC(reg_nvda, type = "HC1")))

#new reg
model_sq <- lm(
  excess_nvda ~ Mkt_RF + HML + vix_chg + AI_boom + Mkt_RF2,
  data = data_spec_sq
)

cat("\n===== MODEL WITH MARKET SQUARED =====\n")
summary(model_sq)

cat("\nAIC (Original): ", AIC(reg_nvda), "\n")
cat("AIC (Squared Model): ", AIC(model_sq), "\n")

cat("\n===== RESET (Squared Model) =====\n")
resettest(model_sq, power = 2:3, type = "regressor")

coeftest(model_sq, vcov = vcovHC(model_sq, type = "HC1"))
vif(model_sq)
bptest(model_sq)


#test



library(dplyr)
library(zoo)

# ---- Download ZIP from Kenneth French ----
temp <- tempfile()
download.file(
  "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip",
  temp, mode = "wb"
)

# ---- The file inside the ZIP is called F-F_Research_Data_Factors.csv ----
ff_raw <- read.csv(
  unz(temp, "F-F_Research_Data_Factors.csv"),
  skip = 3, header = TRUE, stringsAsFactors = FALSE
)

# ---- Clean dataset ----
ff_factors <- ff_raw %>%
  filter(X != "") %>%                    # remove footers
  mutate(
    yyyymm = as.character(X),
    ym = as.yearmon(yyyymm, "%Y%m"),     # convert YYYYMM to yearmon
    Mkt_RF = as.numeric(Mkt.RF),
    SMB = as.numeric(SMB),
    HML = as.numeric(HML),
    RF = as.numeric(RF)
  ) %>%
  select(ym, Mkt_RF, SMB, HML, RF)       # keep final columns only

rm(temp, ff_raw)                         # clean workspace

# 1. NVDA with yearmon key
nvda_m <- nvda_ret_m %>%
  mutate(ym = as.yearmon(date))

# 2. VIX with yearmon key
vix_m2 <- vix_m %>%
  mutate(ym = as.yearmon(date))

# 3. Merge all
data_final <- nvda_m %>%
  left_join(vix_m2, by = "ym") %>%
  left_join(ff_factors, by = "ym") %>%
  arrange(ym)

# Confirm
head(data_final)

data_spec_sq <- data_final %>%
  mutate(Mkt_RF2 = Mkt_RF^2)

model_sq <- lm(
  excess_nvda ~ Mkt_RF + HML + vix_chg + AI_boom + Mkt_RF2,
  data = data_spec_sq
)

summary(model_sq)
resettest(model_sq, power = 2:3, type = "regressor")
