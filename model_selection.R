#======================================= Model Estimation ===================================================
# Pay careful attention to the two most important argument
# 1. specify_prior_phi ...... This relates to prior on VAR coefficients
# 2. specify_prior_sigma .... This relates to the prior on variance-covariance matrix

#----------- 1. Prior on VAR coefficients ----------
# specify_prior_phi(
# data = NULL,
# M = ncol(data),
# lags = 1L,
# prior = "HS",
# priormean = 0,
# PHI_tol = 1e-18,
# DL_a = "1/K",
# DL_tol = 0,
# R2D2_a = 0.1,
# R2D2_b = 0.5,
# R2D2_tol = 0,
# NG_a = 0.1,
# NG_b = 1,
# NG_c = 1,
# NG_tol = 0,
# SSVS_c0 = 0.01,
# SSVS_c1 = 100,
# SSVS_semiautomatic = TRUE,
# SSVS_p = 0.5,
# HMP_lambda1 = c(0.01, 0.01),
# HMP_lambda2 = c(0.01, 0.01),
# normal_sds = 10,
# global_grouping = "global")

#-----------2. Prior on varinace-covariance matrix
# specify_prior_sigma(
# data = NULL,
# M = ncol(data),
# type = c("factor", "cholesky"),
# factor_factors = 1L,
# factor_restrict = c("none", "upper"),
# factor_priorfacloadtype = c("rowwiseng", "colwiseng", "normal"),
# factor_priorfacload = 0.1,
# factor_facloadtol = 1e-18,
# factor_priorng = c(1, 1),
# factor_priormu = c(0, 10),
# factor_priorphiidi = c(10, 3),
# factor_priorphifac = c(10, 3),
# factor_priorsigmaidi = 1,
# factor_priorsigmafac = 1,
# factor_priorh0idi = "stationary",
# factor_priorh0fac = "stationary",
# factor_heteroskedastic = TRUE,
# factor_priorhomoskedastic = NA,
# factor_interweaving = 4,
# cholesky_U_prior = c("HS", "DL", "R2D2", "NG", "SSVS", "normal", "HMP"),
# cholesky_U_tol = 1e-18,
# cholesky_heteroscedastic = TRUE,
# cholesky_priormu = c(0, 100),
# cholesky_priorphi = c(20, 1.5),
# cholesky_priorsigma2 = c(0.5, 0.5),
# cholesky_priorh0 = "stationary",
# cholesky_priorhomoscedastic = as.numeric(NA),
# cholesky_DL_a = "1/n",
# cholesky_DL_tol = 0,
# cholesky_R2D2_a = 0.4,
# cholesky_R2D2_b = 0.5,
# cholesky_R2D2_tol = 0,
# cholesky_NG_a = 0.5,
# cholesky_NG_b = 0.5,
# cholesky_NG_c = 0.5,
# cholesky_NG_tol = 0,
# cholesky_SSVS_c0 = 0.001,
# cholesky_SSVS_c1 = 1,
# cholesky_SSVS_p = 0.5,
# cholesky_HMP_lambda3 = c(0.01, 0.01),
# cholesky_normal_sds = 10,
# expert_sv_offset = 0,
# quiet = FALSE)



#---------------------------- Factor Stochastic Volatility ------------------------------------------------
my_bvar_HS_fsv <- bvar(data, lags = 1L, draws = 1000L, burnin = 1000L, thin = 1L, prior_intercept = 10,
prior_phi = specify_prior_phi(data = data, lags = lags, prior = "HS"),
prior_sigma = specify_prior_sigma(data = data, type = "factor", quiet = TRUE),
sv_keep = "last",
quiet = FALSE,
startvals = list(),
expert = list())



my_bvar_R2D2_fsv <- bvar(data, lags = 1L, draws = 1000L, burnin = 1000L, thin = 1L, prior_intercept = 10,
prior_phi = specify_prior_phi(data = data, lags = lags, prior = "R2D2"),
prior_sigma = specify_prior_sigma(data = data, type = "factor", quiet = TRUE),
sv_keep = "last",
quiet = FALSE,
startvals = list(),
expert = list())





my_bvar_NG_fsv <- bvar(data, lags = 1L, draws = 1000L, burnin = 1000L, thin = 1L, prior_intercept = 10,
prior_phi = specify_prior_phi(data = data, lags = lags, prior = "NG"),
prior_sigma = specify_prior_sigma(data = data, type = "factor", quiet = TRUE),
sv_keep = "last",
quiet = FALSE,
startvals = list(),
expert = list())



my_bvar_DL_fsv <- bvar(data, lags = 1L, draws = 1000L, burnin = 1000L, thin = 1L, prior_intercept = 10,
prior_phi = specify_prior_phi(data = data, lags = lags, prior = "DL"),
prior_sigma = specify_prior_sigma(data = data, type = "factor", quiet = TRUE),
sv_keep = "last",
quiet = FALSE,
startvals = list(),
expert = list())



my_bvar_SSVS_fsv <- bvar(data, lags = 1L, draws = 1000L, burnin = 1000L, thin = 1L, prior_intercept = 10,
prior_phi = specify_prior_phi(data = data, lags = lags, prior = "SSVS"),
prior_sigma = specify_prior_sigma(data = data, type = "factor", quiet = TRUE),
sv_keep = "last",
quiet = FALSE,
startvals = list(),
expert = list())


my_bvar_HMP_fsv <- bvar(data, lags = 1L, draws = 1000L, burnin = 1000L, thin = 1L, prior_intercept = 10,
prior_phi = specify_prior_phi(data = data, lags = lags, prior = "HMP"),
prior_sigma = specify_prior_sigma(data = data, type = "factor", quiet = TRUE),
sv_keep = "last",
quiet = FALSE,
startvals = list(),
expert = list())
#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------


#--------------- Cholesky Stochastic Volatility-----------------------

my_bvar_HS_chl <- bvar(data, lags = 1L, draws = 1000L, burnin = 1000L, thin = 1L, prior_intercept = 10,
prior_phi = specify_prior_phi(data = data, lags = lags, prior = "HS"),
prior_sigma = specify_prior_sigma(data = data, type = "cholesky", quiet = TRUE),
sv_keep = "last",
quiet = FALSE,
startvals = list(),
expert = list())



my_bvar_R2D2_chl <- bvar(data, lags = 1L, draws = 1000L, burnin = 1000L, thin = 1L, prior_intercept = 10,
prior_phi = specify_prior_phi(data = data, lags = lags, prior = "R2D2"),
prior_sigma = specify_prior_sigma(data = data, type = "cholesky", quiet = TRUE),
sv_keep = "last",
quiet = FALSE,
startvals = list(),
expert = list())





my_bvar_NG_chl <- bvar(data, lags = 1L, draws = 1000L, burnin = 1000L, thin = 1L, prior_intercept = 10,
prior_phi = specify_prior_phi(data = data, lags = lags, prior = "NG"),
prior_sigma = specify_prior_sigma(data = data, type = "cholesky", quiet = TRUE),
sv_keep = "last",
quiet = FALSE,
startvals = list(),
expert = list())



my_bvar_DL_chl <- bvar(data, lags = 1L, draws = 1000L, burnin = 1000L, thin = 1L, prior_intercept = 10,
prior_phi = specify_prior_phi(data = data, lags = lags, prior = "DL"),
prior_sigma = specify_prior_sigma(data = data, type = "cholesky", quiet = TRUE),
sv_keep = "last",
quiet = FALSE,
startvals = list(),
expert = list())



my_bvar_SSVS_chl <- bvar(data, lags = 1L, draws = 1000L, burnin = 1000L, thin = 1L, prior_intercept = 10,
prior_phi = specify_prior_phi(data = data, lags = lags, prior = "SSVS"),
prior_sigma = specify_prior_sigma(data = data, type = "cholesky", quiet = TRUE),
sv_keep = "last",
quiet = FALSE,
startvals = list(),
expert = list())


my_bvar_HMP_chl <- bvar(data, lags = 1L, draws = 1000L, burnin = 1000L, thin = 1L, prior_intercept = 10,
prior_phi = specify_prior_phi(data = data, lags = lags, prior = "HMP"),
prior_sigma = specify_prior_sigma(data = data, type = "cholesky", quiet = TRUE),
sv_keep = "last",
quiet = FALSE,
startvals = list(),
expert = list())

#================================================================================================================
# 1. Let us estimate the model with HS prior for VAR coefficient and factor SV for variance-covariance
# 2. Generate the posterior
# 3. Predict
# 4. Model selection 

#--------------- Loading the library ----------------------------------
library(bayesianVARs)
#----------------------------------------------------------------------

#---------------- Data Preprocessing ----------------------------------
variables <- c("GDPC1", "GDPCTPI", "FEDFUNDS", "EXUSUKx", "S&P 500")
train_data <- 100 * usmacro_growth[1:230, variables]
test_data <- 100 * usmacro_growth[231:234, variables]
#-----------------------------------------------------------------------


#------------ Prior for VAR coefficients setting -----------------------
prior_phi <- specify_prior_phi(data = train_data, lags = 2L,
prior = "HS",
global_grouping = "olcl-lagwise")
#-----------------------------------------------------------------------


#--------- Prior for variance-covariance settings ----------------------
prior_sigma <- specify_prior_sigma(data = train_data, type = "factor",
factor_factors = 4L)
#-----------------------------------------------------------------------


#-------------------- Model Estimation ---------------------------------
mod_1 <- bvar(train_data, lags = 2L, draws = 10000, burnin = 2000,
prior_phi = prior_phi, prior_sigma = prior_sigma, sv_keep = "all")
#-----------------------------------------------------------------------

#--------------- Visualizing the Posterior -----------------------------
plot(mod_1, quantiles = c(0.05,0.5,0.95), dates = rownames(mod_1$Yraw))
phi <- coef(mod_1)
posterior_heatmap(phi,median)
posterior_heatmap(phi, IQR)
#-----------------------------------------------------------------------


#----------- Let's produce forecast ------------------------------------
pred_1 <- predict(mod_1, ahead = 1:4, LPL = TRUE, Y_obs = test_data)
#-----------------------------------------------------------------------


#-------------- Let's plot the forecast --------------------------------
plot(pred_1, first_obs = 216,
dates = c(rownames(train_data[-c(1:215),]), rownames(test_data)))
#-----------------------------------------------------------------------

#-------------- Let's assess the quality of the forecast----------------
pred_1$LPL
#------------------------------------------------------------------------


#---------------- Let's get a competing model --------------------------
# We achieve this either by:
# 1. Hyperparameter tuning
# 2. Changing the prior

#-----------------------------------------------------------------------
# LET'S TAKE THE SECOND OPTION (Changing the prior)
#-----------------------------------------------------------------------


#------------ Model 2 Estiamted with Hierarchical Minnesota Prior ------

#------------ Prior for VAR coefficients setting -----------------------
prior_phi <- specify_prior_phi(data = train_data, lags = 2L,
prior = "HMP",
global_grouping = "olcl-lagwise")
#-----------------------------------------------------------------------


#--------- Prior for variance-covariance settings ----------------------
prior_sigma <- specify_prior_sigma(data = train_data, type = "factor",
factor_factors = 4L)
#-----------------------------------------------------------------------


#-------------------- Model Estimation ---------------------------------
mod_2 <- bvar(train_data, lags = 2L, draws = 10000, burnin = 2000,
prior_phi = prior_phi, prior_sigma = prior_sigma, sv_keep = "all")
#-----------------------------------------------------------------------

#--------------- Visualizing the Posterior -----------------------------
plot(mod_2, quantiles = c(0.05,0.5,0.95), dates = rownames(mod_2$Yraw))
phi <- coef(mod_2)
posterior_heatmap(phi,median)
posterior_heatmap(phi, IQR)
#-----------------------------------------------------------------------


#----------- Let's produce forecast ------------------------------------
pred_2 <- predict(mod_2, ahead = 1:4, LPL = TRUE, Y_obs = test_data)
#-----------------------------------------------------------------------


#-------------- Let's plot the forecast --------------------------------
plot(pred_2, first_obs = 216,
dates = c(rownames(train_data[-c(1:215),]), rownames(test_data)))
#-----------------------------------------------------------------------

#-------------- Let's assess the quality of the forecast----------------
pred_2$LPL
#------------------------------------------------------------------------


#------------ Model 3 Estiamted with SSVS Prior -------------------------

#------------ Prior for VAR coefficients setting -----------------------
prior_phi <- specify_prior_phi(data = train_data, lags = 2L,
prior = "SSVS",
global_grouping = "olcl-lagwise")
#-----------------------------------------------------------------------


#--------- Prior for variance-covariance settings ----------------------
prior_sigma <- specify_prior_sigma(data = train_data, type = "factor",
factor_factors = 4L)
#-----------------------------------------------------------------------


#-------------------- Model Estimation ---------------------------------
mod_3 <- bvar(train_data, lags = 2L, draws = 10000, burnin = 2000,
prior_phi = prior_phi, prior_sigma = prior_sigma, sv_keep = "all")
#-----------------------------------------------------------------------

#--------------- Visualizing the Posterior -----------------------------
plot(mod_3, quantiles = c(0.05,0.5,0.95), dates = rownames(mod_3$Yraw))
phi <- coef(mod_3)
posterior_heatmap(phi,median)
posterior_heatmap(phi, IQR)
#-----------------------------------------------------------------------


#----------- Let's produce forecast ------------------------------------
pred_3 <- predict(mod_3, ahead = 1:4, LPL = TRUE, Y_obs = test_data)
#-----------------------------------------------------------------------


#-------------- Let's plot the forecast --------------------------------
plot(pred_3, first_obs = 216,
dates = c(rownames(train_data[-c(1:215),]), rownames(test_data)))
#-----------------------------------------------------------------------

#-------------- Let's assess the quality of the forecast----------------
pred_3$LPL
#------------------------------------------------------------------------


#------------ Model 4 Estiamted with DL Prior -------------------------

#------------ Prior for VAR coefficients setting -----------------------
prior_phi <- specify_prior_phi(data = train_data, lags = 2L,
prior = "DL",
global_grouping = "olcl-lagwise")
#-----------------------------------------------------------------------


#--------- Prior for variance-covariance settings ----------------------
prior_sigma <- specify_prior_sigma(data = train_data, type = "factor",
factor_factors = 4L)
#-----------------------------------------------------------------------


#-------------------- Model Estimation ---------------------------------
mod_4 <- bvar(train_data, lags = 2L, draws = 10000, burnin = 2000,
prior_phi = prior_phi, prior_sigma = prior_sigma, sv_keep = "all")
#-----------------------------------------------------------------------

#--------------- Visualizing the Posterior -----------------------------
plot(mod_4, quantiles = c(0.05,0.5,0.95), dates = rownames(mod_4$Yraw))
phi <- coef(mod_4)
posterior_heatmap(phi,median)
posterior_heatmap(phi, IQR)
#-----------------------------------------------------------------------


#----------- Let's produce forecast ------------------------------------
pred_4 <- predict(mod_4, ahead = 1:4, LPL = TRUE, Y_obs = test_data)
#-----------------------------------------------------------------------


#-------------- Let's plot the forecast --------------------------------
plot(pred_4, first_obs = 216,
dates = c(rownames(train_data[-c(1:215),]), rownames(test_data)))
#-----------------------------------------------------------------------

#-------------- Let's assess the quality of the forecast----------------
pred_4$LPL
#------------------------------------------------------------------------





#---------------- Let's examine the forecast --------------------------
pred_1$LPL
pred_2$LPL
pred_3$LPL
pred_4$LPL

pred_1$LPL - pred_2$LPL
abs(pred_1$LPL - pred_2$LPL)

# Visualize
pairs(pred_1, vars = 1:3, ahead = 1:3)
pairs(pred_2, vars = 1:3, ahead = 1:3)
pairs(pred_3, vars = 1:3, ahead = 1:3)
pairs(pred_4, vars = 1:3, ahead = 1:3)

# Visualize
plot(mod_1, predictions = pred_1)
plot(mod_2, predictions = pred_2)
plot(mod_3, predictions = pred_3)
plot(mod_4, predictions = pred_4)


sum_pred_1 <- summary(pred_1)
print(sum_pred_1)

sum_pred_2 <- summary(pred_2)
print(sum_pred_2)

sum_pred_3 <- summary(pred_3)
print(sum_pred_3)
#-------------------------------------------------------------------------

# Simulate predicted historical values including the error term.
pred_1_1 <- fitted(mod_1, error_term = TRUE)
# Simulate fitted historical values not including the error term.
fit_1 <- fitted(mod_1, error_term = FALSE)
# Visualize
plot(pred_1_1)
plot(fit_1)


#====================== Some Basic Interrogation =========================
# Compute summary statistics
mod_1_coeff <- coef(mod_1)
mod_2_coeff <- coef(mod_2)
mod_3_coeff <- coef(mod_3)
mod_4_coeff <- coef(mod_4)

summary_stats_1 <- summary(mod_1_coeff)
summary_stats_2 <- summary(mod_2_coeff)
summary_stats_3 <- summary(mod_3_coeff)
summary_stats_4 <- summary(mod_4_coeff)

# Compute summary statistics of VAR coefficients without using coef()
summary_stats_1 <- summary(mod_1$phi)
summary_stats_2 <- summary(mod_2$phi)
summary_stats_3 <- summary(mod_3$phi)
summary_stats_4 <- summary(mod_4$PHI)




# Extract posterior draws of the variance-covariance matrix
vcov_mod_1 <- vcov(mod_1)
vcov_mod_2 <- vcov(mod_2)
vcov_mod_3 <- vcov(mod_3)
vcov_mod_4 <- vcov(mod_4)

vcov_mod_1
vcov_mod_2
vcov_mod_3
vcov_mod_4


#================================================================================================================
# 1. Let us estimate the model with HS prior for VAR coefficient and cholesky SV for variance-covariance
# 2. Generate the posterior
# 3. Predict
# 4. Model selection 


#---------------- Data Preprocessing ----------------------------------
variables <- c("GDPC1", "GDPCTPI", "FEDFUNDS", "EXUSUKx", "S&P 500")
train_data <- 100 * usmacro_growth[1:230, variables]
test_data <- 100 * usmacro_growth[231:234, variables]
#-----------------------------------------------------------------------


#------------ Prior for VAR coefficients setting -----------------------
prior_phi <- specify_prior_phi(data = train_data, lags = 2L,
prior = "HS",
global_grouping = "olcl-lagwise")
#-----------------------------------------------------------------------


#--------- Prior for variance-covariance settings ----------------------
prior_sigma <- specify_prior_sigma(data = train_data, type = "cholesky")
#-----------------------------------------------------------------------


#-------------------- Model Estimation ---------------------------------
mod_1 <- bvar(train_data, lags = 2L, draws = 10000, burnin = 2000,
prior_phi = prior_phi, prior_sigma = prior_sigma, sv_keep = "all")
#-----------------------------------------------------------------------

#--------------- Visualizing the Posterior -----------------------------
plot(mod_1, quantiles = c(0.05,0.5,0.95), dates = rownames(mod_1$Yraw))
phi <- coef(mod_1)
posterior_heatmap(phi,median)
posterior_heatmap(phi, IQR)
#-----------------------------------------------------------------------


#----------- Let's produce forecast ------------------------------------
pred_1 <- predict(mod_1, ahead = 1:4, LPL = TRUE, Y_obs = test_data)
#-----------------------------------------------------------------------


#-------------- Let's plot the forecast --------------------------------
plot(pred_1, first_obs = 216,
dates = c(rownames(train_data[-c(1:215),]), rownames(test_data)))
#-----------------------------------------------------------------------

#-------------- Let's assess the quality of the forecast----------------
pred_1$LPL
#------------------------------------------------------------------------


#---------------- Let's get a competing model --------------------------
# We achieve this either by:
# 1. Hyperparameter tuning
# 2. Changing the prior

#-----------------------------------------------------------------------
# LET'S TAKE THE SECOND OPTION (Changing the prior)
#-----------------------------------------------------------------------


#------------ Model 2 Estiamted with Hierarchical Minnesota Prior ------

#------------ Prior for VAR coefficients setting -----------------------
prior_phi <- specify_prior_phi(data = train_data, lags = 2L,
prior = "HMP",
global_grouping = "olcl-lagwise")
#-----------------------------------------------------------------------


#--------- Prior for variance-covariance settings ----------------------
prior_sigma <- specify_prior_sigma(data = train_data, type = "cholesky")
#-----------------------------------------------------------------------


#-------------------- Model Estimation ---------------------------------
mod_2 <- bvar(train_data, lags = 2L, draws = 10000, burnin = 2000,
prior_phi = prior_phi, prior_sigma = prior_sigma, sv_keep = "all")
#-----------------------------------------------------------------------

#--------------- Visualizing the Posterior -----------------------------
plot(mod_2, quantiles = c(0.05,0.5,0.95), dates = rownames(mod_2$Yraw))
phi <- coef(mod_2)
posterior_heatmap(phi,median)
posterior_heatmap(phi, IQR)
#-----------------------------------------------------------------------


#----------- Let's produce forecast ------------------------------------
pred_2 <- predict(mod_2, ahead = 1:4, LPL = TRUE, Y_obs = test_data)
#-----------------------------------------------------------------------


#-------------- Let's plot the forecast --------------------------------
plot(pred_2, first_obs = 216,
dates = c(rownames(train_data[-c(1:215),]), rownames(test_data)))
#-----------------------------------------------------------------------

#-------------- Let's assess the quality of the forecast----------------
pred_2$LPL
#------------------------------------------------------------------------


#------------ Model 3 Estiamted with SSVS Prior -------------------------

#------------ Prior for VAR coefficients setting -----------------------
prior_phi <- specify_prior_phi(data = train_data, lags = 2L,
prior = "SSVS",
global_grouping = "olcl-lagwise")
#-----------------------------------------------------------------------


#--------- Prior for variance-covariance settings ----------------------
prior_sigma <- specify_prior_sigma(data = train_data, type = "cholesky")
#-----------------------------------------------------------------------


#-------------------- Model Estimation ---------------------------------
mod_3 <- bvar(train_data, lags = 2L, draws = 10000, burnin = 2000,
prior_phi = prior_phi, prior_sigma = prior_sigma, sv_keep = "all")
#-----------------------------------------------------------------------

#--------------- Visualizing the Posterior -----------------------------
plot(mod_3, quantiles = c(0.05,0.5,0.95), dates = rownames(mod_3$Yraw))
phi <- coef(mod_3)
posterior_heatmap(phi,median)
posterior_heatmap(phi, IQR)
#-----------------------------------------------------------------------


#----------- Let's produce forecast ------------------------------------
pred_3 <- predict(mod_3, ahead = 1:4, LPL = TRUE, Y_obs = test_data)
#-----------------------------------------------------------------------


#-------------- Let's plot the forecast --------------------------------
plot(pred_3, first_obs = 216,
dates = c(rownames(train_data[-c(1:215),]), rownames(test_data)))
#-----------------------------------------------------------------------

#-------------- Let's assess the quality of the forecast----------------
pred_3$LPL
#------------------------------------------------------------------------


#------------ Model 4 Estiamted with DL Prior -------------------------

#------------ Prior for VAR coefficients setting -----------------------
prior_phi <- specify_prior_phi(data = train_data, lags = 2L,
prior = "DL",
global_grouping = "olcl-lagwise")
#-----------------------------------------------------------------------


#--------- Prior for variance-covariance settings ----------------------
prior_sigma <- specify_prior_sigma(data = train_data, type = "cholesky")
#-----------------------------------------------------------------------


#-------------------- Model Estimation ---------------------------------
mod_4 <- bvar(train_data, lags = 2L, draws = 10000, burnin = 2000,
prior_phi = prior_phi, prior_sigma = prior_sigma, sv_keep = "all")
#-----------------------------------------------------------------------

#--------------- Visualizing the Posterior -----------------------------
plot(mod_4, quantiles = c(0.05,0.5,0.95), dates = rownames(mod_4$Yraw))
phi <- coef(mod_4)
posterior_heatmap(phi,median)
posterior_heatmap(phi, IQR)
#-----------------------------------------------------------------------


#----------- Let's produce forecast ------------------------------------
pred_4 <- predict(mod_4, ahead = 1:4, LPL = TRUE, Y_obs = test_data)
#-----------------------------------------------------------------------


#-------------- Let's plot the forecast --------------------------------
plot(pred_4, first_obs = 216,
dates = c(rownames(train_data[-c(1:215),]), rownames(test_data)))
#-----------------------------------------------------------------------

#-------------- Let's assess the quality of the forecast----------------
pred_4$LPL
#------------------------------------------------------------------------





#---------------- Let's examine the forecast --------------------------
pred_1$LPL
pred_2$LPL
pred_3$LPL
pred_4$LPL

pred_1$LPL - pred_2$LPL
abs(pred_1$LPL - pred_2$LPL)

# Visualize
pairs(pred_1, vars = 1:3, ahead = 1:3)
pairs(pred_2, vars = 1:3, ahead = 1:3)
pairs(pred_3, vars = 1:3, ahead = 1:3)
pairs(pred_4, vars = 1:3, ahead = 1:3)

# Visualize
plot(mod_1, predictions = pred_1)
plot(mod_2, predictions = pred_2)
plot(mod_3, predictions = pred_3)
plot(mod_4, predictions = pred_4)


sum_pred_1 <- summary(pred_1)
print(sum_pred_1)

sum_pred_2 <- summary(pred_2)
print(sum_pred_2)

sum_pred_3 <- summary(pred_3)
print(sum_pred_3)
#-------------------------------------------------------------------------

# Simulate predicted historical values including the error term.
pred_1_1 <- fitted(mod_1, error_term = TRUE)
# Simulate fitted historical values not including the error term.
fit_1 <- fitted(mod_1, error_term = FALSE)
# Visualize
plot(pred_1_1)
plot(fit_1)


#====================== Some Basic Interrogation =========================
# Compute summary statistics
mod_1_coeff <- coef(mod_1)
mod_2_coeff <- coef(mod_2)
mod_3_coeff <- coef(mod_3)
mod_4_coeff <- coef(mod_4)

summary_stats_1 <- summary(mod_1_coeff)
summary_stats_2 <- summary(mod_2_coeff)
summary_stats_3 <- summary(mod_3_coeff)
summary_stats_4 <- summary(mod_4_coeff)

# Compute summary statistics of VAR coefficients without using coef()
summary_stats_1 <- summary(mod_1$phi)
summary_stats_2 <- summary(mod_2$phi)
summary_stats_3 <- summary(mod_3$phi)
summary_stats_4 <- summary(mod_4$PHI)




# Extract posterior draws of the variance-covariance matrix
vcov_mod_1 <- vcov(mod_1)
vcov_mod_2 <- vcov(mod_2)
vcov_mod_3 <- vcov(mod_3)
vcov_mod_4 <- vcov(mod_4)

vcov_mod_1
vcov_mod_2
vcov_mod_3
vcov_mod_4





