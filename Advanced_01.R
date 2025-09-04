
##########################################################################################################################################################
# Nafiu Bashir A. (PhD)                                                                                                                                  #
# Central Bank of Nigeria (CBN)                                                                                                                          #
# Monetary Policy Department  (Inflation Analysis and Forecasting Office)                                                                                                                            #
# Email: nafiu_bashir13@yahoo.co.uk/ nafiu13bashir@gmail.com/ nafiu_bashir13@outlook.com / NABASHIR@CBN.GOV.NG                                           #                           
# Phone No: +234 80 2035 9815/ +234 70 3788 0962                                                                                                         #
########################################################################################################################################################## 



##########################################################################################################################################################
# Code organized and composed by Nafiu B.A. 2025 for WAIFEM Training Course                                                                              #
##########################################################################################################################################################

##########################################################################################################################################################
# REGIONAL COURSE ON ECONOMETRIC METHODS FOR POLICY ANALYSIS (LAGOS, NIGERIA, August 25 -September 5, 2025)                                                         #
# WEST AFRICAN INSTITUTE FOR FINANCIAL AND ECONOMIC MANAGEMENT (WAIFEM)                                                                                  #
##########################################################################################################################################################

#=======================================================
# WHO I AM ?
#======================================================

# I am an experts in:
# 1- Bayesian Econometrics/
# 2- Frequantist/Classical Econometrics
# 3- Data Science/
# 4- Machine Learning/
# 5- Natural Language Processing 
# 6- Statistical Data Analysis
# 7- Data Story Telling

# Competent with:
# 1- Python (Visual Studio Code, PyCharm, Notepad++, Positron, Jupyter Notebook, Spyder)
# 2- R & RStudio
# 3- MATLAB (Dynare, Octave) 
# 4- Stata
# 5- Eviews
# 6- Microsoft Power BI/ Excel Packages / Power Query

# Average with:
# 1- SQL
# 2- Tableau
# 3- Google Looker
# 4- E-Draw
# 4- Microsoft Fabric
# 6- Maltesers
#======================================================





#======================== joergrieger/bvar: Estimation and forecasting of Bayesian VAR models ========================================== 

#======================== Loading the library ==========================================================================================
library(bvars)
library(xts)
#=======================================================================================================================================


#=========================================== The Data ==================================================================================
data("USMonPol")
my_bvarData<-USMonPol
#=======================================================================================================================================


#=================================================Prior_Specification_Function==========================================================
                       ##Conjugate Normal Wishart/Minnesota/Uninformative/SSVS 
##........................Uninformative_Prior...............................................................
my_uninfor<-set_prior_uninformative(mydata = my_bvarData, factordata = NULL,
                                    no_factors = 0, nolags = 1, intercept = TRUE)
##...........................................................................................................

##...............................Minnesota_Prior............................................................ 
my_minn<-set_prior_minnesota(mydata= my_bvarData, factordata = NULL, no_factors = 0, nolags=1,
                             intercept = TRUE, lambda1 = 1, lambda2 = 1, lambda3 = 1, lambda4 = 2)
##.......................Stochastic Search Variable Selection.................................................
my_ssvs<-set_prior_ssvs(mydata= my_bvarData, factordata = NULL, no_factors = 0, nolags=1,
                        intercept = TRUE, tau=1, kappa=1)

##....................................conjugate Normal-Wishart prior........................................ 
my_cnw<-set_prior_cnw(mydata = my_bvarData, factordata = NULL, no_factors = 0,
                      coefprior = NULL, coefpriorvar = NULL, varprior = NULL,
                      varpriordof = NULL, nolags = 1, intercept = TRUE)

#===========================================================================================================================================



#================================ Models Estimation ========================================================================================
                        ##.............bvar.........................
my_bvar_uninfor<-bvar(mydata=USMonPol, priorObj=my_uninfor, stabletest = FALSE, nreps = 15000,
                     burnin = 5000, nthin = 1)
my_bvar_minn<-bvar(mydata=my_bvarData, priorObj=my_minn, stabletest = FALSE, nreps = 15000,
                   burnin = 5000, nthin = 1)
my_bvar_ssvs<-bvar(mydata=my_bvarData, priorObj=my_ssvs, stabletest = FALSE, nreps = 15000,
                   burnin = 5000, nthin = 1)
my_bvar_cnw<-bvar(mydata=my_bvarData, priorObj=my_cnw, stabletest = FALSE, nreps = 15000,
                  burnin = 5000, nthin = 1)
##.........................................................................................................
                    ##...............msvar........................
##.......................Estimate regime-switching models with fixed transition........
my_msvar_uninfor<-msvar(mydata=my_bvarData, priorObj=my_uninfor, stabletest = FALSE, noregimes = 2,
                nreps = 15000, burnin = 10000, nthin = 1)
my_msvar_minn<-msvar(mydata=my_bvarData, priorObj=my_minn, stabletest = FALSE, noregimes = 2,
                        nreps = 15000, burnin = 10000, nthin = 1)
my_msvar_ssvs<-msvar(mydata=my_bvarData, priorObj=my_ssvs, stabletest = FALSE, noregimes = 2,
                        nreps = 15000, burnin = 10000, nthin = 1)
my_msvar_cnw<-msvar(mydata=my_bvarData, priorObj=my_cnw, stabletest = FALSE, noregimes = 2,
                        nreps = 15000, burnin = 10000, nthin = 1)
##..........................................................................................................
                   ##...................tvar....................................
##.........................Bayesian Estimation of TVAR.....................................................
my_tvar_uninfor<-tvar(mydata=my_bvarData, priorObj=my_uninfor, thMax = 2, thVar = 1, nreps = 1100,
               burnin = 100, nthin = 1, stabletest = TRUE)
my_tvar_minn<-tvar(mydata=my_bvarData, priorObj=my_minn, thMax = 2, thVar = 1, nreps = 1100,
                      burnin = 100, nthin = 1, stabletest = TRUE)
my_tvar_ssvs<-tvar(mydata=my_bvarData, priorObj=my_ssvs, thMax = 2, thVar = 1, nreps = 1100,
                   burnin = 100, nthin = 1, stabletest = TRUE)
my_tvar_cnw<-tvar(mydata=my_bvarData, priorObj=my_cnw, thMax = 2, thVar = 1, nreps = 1100,
                   burnin = 100, nthin = 1, stabletest = TRUE)
##........................................................................................................

##...........................bayesian estimation of threshold VAR....................................
my_ftvar_uninfor<-ftvar(mydata=my_bvarData, factordata, priorObj=my_uninfor, 
        thMax, thVar, nreps, burnin, nthin, stabletest, alpha, beta, tau2, c2, li_prvar, priorm)
my_ftvar_minn<-ftvar(mydata=my_bvarData, factordata, priorObj=my_minn, 
        thMax, thVar, nreps, burnin, nthin, stabletest, alpha, beta, tau2, c2, li_prvar, priorm)
my_ftvar_ssvs<-ftvar(mydata=my_bvarData, factordata, priorObj=my_ssvs, 
        thMax, thVar, nreps, burnin, nthin, stabletest,alpha, beta, tau2, c2, li_prvar, priorm)
my_ftvar_cnw<-ftvar(mydata=my_bvarData, factordata, priorObj=my_cnw, 
        thMax, thVar, nreps, burnin, nthin, stabletest, alpha, beta, tau2, c2, li_prvar, priorm)
##...............................................................................................................
            ##......Factor Augmeneted VAR................................
my_favar_uninfor<-favar(data=my_bvarData, priorObj=my_uninfor, factordata, nreps, burnin, alpha, beta, tau2, c2,
      li_prvar, priorm, stabletest = TRUE, nthin = 1)
my_favar_minn<-favar(data=my_bvarData, priorObj=my_minn, factordata, nreps, burnin, alpha, beta, tau2, c2,
                        li_prvar, priorm, stabletest = TRUE, nthin = 1)
my_favar_ssvs<-favar(data=my_bvarData, priorObj=my_ssvs, factordata, nreps, burnin, alpha, beta, tau2, c2,
                        li_prvar, priorm, stabletest = TRUE, nthin = 1)
my_favar_cnw<-favar(data=my_bvarData, priorObj=my_cnw, factordata, nreps, burnin, alpha, beta, tau2, c2,
                        li_prvar, priorm, stabletest = TRUE, nthin = 1)
##############################################################################################################



############################## Identification&Functions&Description ###########################################
##.....................................................................
id_set1<-set_identification_cholesky()
#This function creates an object of the class chol needed for the identification of structural. 
#Identification of structural shocks is important for further analytic steps such as 
#Impulse-Response-#Functions or Historical Decomposition. 
#For a Cholesky-decomposition no further inputs are needed, however 
#the ordering of variables in the VAR-model becomes important. 
id_set2<-set_identification_sign(restrictions="sign")
#This functions creates an object of the class sign needed for the identification of structural shocks. 
#Identification of structural shocks is needed for further analytic steps such as 
#studying Impulse-Responses #or Historical Decomposition. 
#Necessary input is a KxK-matrix with the sign restrictions, i.e. a positive or 
#negative number. And K is the number of variables in the VAR-model. 
#Sign-Restriction is implemented using #the Algorithm proposed by Rubio-Ramirez et al. (2010). 
################################################################################################################



######################### Forecast for a Bayesian VAR Models ################################################

##......................................................
my_uncod_forecast<-forecast(obj, forecastHorizon = 16, interval = c(0.95, 0.05))

## S3 method for class 'bvar'
my_uncod_forecast_bvar_uninfor<-forecast(obj=my_bvar_uninfor, forecastHorizon = 16, interval = c(0.95,0.05))
my_uncod_forecast_bvar_minn<-forecast(obj=my_bvar_minn, forecastHorizon = 16, interval = c(0.95,0.05))
my_uncod_forecast_bvar_ssvs<-forecast(obj=my_bvar_ssvs, forecastHorizon = 16, interval = c(0.95,0.05))
my_uncod_forecast_bvar_cnw<-forecast(obj=my_bvar_cnw, forecastHorizon = 16, interval = c(0.95,0.05))

## S3 method for class 'msvar'
my_uncod_forecast_msvar_uninfor<-forecast(obj=my_msvar_uninfor, forecastHorizon = 16, interval = c(0.95,0.05))
my_uncod_forecast_msvar_minn<-forecast(obj=my_msvar_minn, forecastHorizon = 16, interval = c(0.95,0.05))
my_uncod_forecast_msvar_ssvs<-forecast(obj=my_msvar_ssvs, forecastHorizon = 16, interval = c(0.95,0.05))
my_uncod_forecast_msvar_cnw<-forecast(obj=my_msvar_cnw, forecastHorizon = 16, interval = c(0.95,0.05))

## S3 method for class 'tvar'
my_uncod_forecast_tvar_uninfor<-forecast(obj=my_tvar_uninfor, forecastHorizon=16, interval = c(0.05, 0.95))
my_uncod_forecast_tvar_minnr<-forecast(obj=my_tvar_minn, forecastHorizon=16, interval = c(0.05, 0.95))
my_uncod_forecast_tvar_ssvs<-forecast(obj=my_tvar_ssvs, forecastHorizon=16, interval = c(0.05, 0.95))
my_uncod_forecast_tvar_cnw<-forecast(obj=my_tvar_cnw, forecastHorizon, interval = c(0.05, 0.95))

## s3 method for class ftvar
my_uncod_forecast_tvar2_uninfor<-forecast(obj=my_tvar_uninfor2, forecastHorizon, interval = c(0.05, 0.95))
my_uncod_forecast_tvar2_minnr<-forecast(obj=my_tvar_minn2, forecastHorizon, interval = c(0.05, 0.95))
my_uncod_forecast_tvar2_ssvs<-forecast(obj=my_tvar_ssvs2, forecastHorizon, interval = c(0.05, 0.95))
my_uncod_forecast_tvar2_cnw<-forecast(obj=my_tvar_cnw2, forecastHorizon, interval = c(0.05, 0.95))

## S3 method for class favar
my_uncod_forecast_favar_uninfor<-forecast(obj=my_favar_uninfor, forecastHorizon, interval = c(0.05, 0.95))
my_uncod_forecast_favar_minnr<-forecast(obj=my_favar_minn, forecastHorizon, interval = c(0.05, 0.95))
my_uncod_forecast_favar_ssvs<-forecast(obj=my_favar_ssvs, forecastHorizon, interval = c(0.05, 0.95))
my_uncod_forecast_favar_cnw<-forecast(obj=my_favar_cnw, forecastHorizon, interval = c(0.05, 0.95))
##########################################################################################################


#########################################################################################################

##.............conditional_forecast..................
#my_cond_forecat_<-cforecast(obj, forecastHorizon, id_obj, cfconds,interval= (0.05,0.95))
##..........................................................

##........Function to draw one single path for the impulse-response.........
#my_irf<-compirf(Alpha, Sigma, id_obj=id_set2, nolags, intercept = TRUE, nhor)
##............................................................................

##.........................Geweke Convergence diagnostics..............
#my_diag<-diag_geweke(obj, frac1, frac2)
## S3 method for class 'bvar'
#my_diag<-diag_geweke(obj, frac1, frac2)
##......................................................................

######################Plot of Tthe Uncondiotionl Forecast ###########################################

##...................plot forecasts bvar..................................................
## S3 method for class 'fcbvar'
plot(my_uncod_forecast_bvar_uninfor)
plot(my_uncod_forecast_bvar_minn)
plot(my_uncod_forecast_bvar_ssvs)
##...................plot forecasts msvar..................................................
## S3 method for class 'fcbvar'
plot(my_uncod_forecast_msvar_uninfor)
plot(my_uncod_forecast_msvar_minn)
plot(my_uncod_forecast_msvar_ssvs)
##...................plot forecasts tvar..................................................
## S3 method for class 'fcbvar'
plot(my_uncod_forecast_tvar_uninfor)
plot(my_uncod_forecast_tvar_minn)
plot(my_uncod_forecast_tvar_ssvs)
##...................plot forecasts tvar2..................................................
## S3 method for class 'fcbvar'
plot(my_uncod_forecast_ftvar_uninfor)
plot(my_uncod_forecast_ftvar_minn)
plot(my_uncod_forecast_ftvar_ssvs)
##...................plot forecasts favar..................................................
## S3 method for class 'fcbvar'
plot(my_uncod_forecast_favar_uninfor)
plot(my_uncod_forecast_favar_minn)
plot(my_uncod_forecast_favar_ssvs)

############################################################################################




################################Plot of the Condotional Forecast###############################################


#                  TO BE IMPLEMENTED.....................WORK IN PROGRESS




################################################################################################################


#########################Compute Impulse-Response Functions#####################################################

##................bvar..................................................................
my_irf_bvar_uninfor1<-irf(my_bvar_uninfor, id_obj=id_set1, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_bvar_uninfor1)

my_irf_bvar_uninfor2<-irf(my_bvar_uninfor, id_obj=id_set2, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_bvar_uninfor2)

my_irf_bvar_minn1<-irf(my_bvar_minn, id_obj=id_set1, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_bvar_minn1)

my_irf_bvar_minn2<-irf(my_bvar_minn, id_obj=id_set2, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_bvar_minn2)

my_irf_bvar_ssvs1<-irf(my_bvar_ssvs, id_obj=id_set1, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_bvar_ssvs1)

my_irf_bvar_ssvs2<-irf(my_bvar_ssvs, id_obj=id_set2, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_bvar_ssvs2)

##................msvar..................................................................
my_irf_msvar_uninfor1<-irf(my_msvar_uninfor, id_obj=id_set1, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_msvar_uninfor1)

my_irf_msvar_uninfor2<-irf(my_msvar_uninfor, id_obj=id_set2, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_msvar_uninfor2)

my_irf_msvar_minn1<-irf(my_msvar_minn, id_obj=id_set1, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_msvar_minn1)

my_irf_msvar_minn2<-irf(my_msvar_minn, id_obj=id_set2, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_msvar_minn2)

my_irf_msvar_ssvs1<-irf(my_msvar_ssvs, id_obj=id_set1, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_msvar_ssvs1)

my_irf_msvar_ssvs2<-irf(my_msvar_ssvs, id_obj=id_set2, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_msvar_ssvs2)


##................tvar..................................................................
my_irf_tvar_uninfor1<-irf(my_tvar_uninfor, id_obj=id_set1, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_tvar_uninfor1)
plot.tvirf(m)

my_irf_tvar_uninfor2<-irf(my_tvar_uninfor, id_obj=id_set2, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_tvar_uninfor2)

my_irf_tvar_minn1<-irf(my_tvar_minn, id_obj=id_set1, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_tvar_minn1)

my_irf_tvar_minn2<-irf(my_tvar_minn, id_obj=id_set2, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_tvar_minn2)

my_irf_tvar_ssvs1<-irf(my_tvar_ssvs, id_obj=id_set1, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_msvar_ssvs1)

my_irf_tvar_ssvs2<-irf(my_tvar_ssvs, id_obj=id_set2, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
plot(my_irf_tvar_ssvs2)


#..........................favar.......................................................................................
my_irf_favar<-irf(obj, id_obj=id_set1, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
my_irf_favar<-irf(obj, id_obj=id_set2, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
#........................ftvar.........................................................................................
my_irf_ftvar<-irf(obj, id_obj=id_set1, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))
my_irf_ftvar<-irf(obj, id_obj=id_set2, nhor = 12, ncores = 1, irfquantiles = c(0.05, 0.95))


##......................................................................................

##############################################################################################################



#######################Forecast Error Variance Decomposition##################################################

##................bvar...........................................................
my_fevd_bvar_uninfor<-fevd(my_bvar_uninfor, h = 12, id_obj = NULL, type = "general", normalize = TRUE)
my_fevd_bvar_minn<-fevd(my_bvar_minn, h = 12, id_obj = NULL, type = "general", normalize = TRUE)
my_fevd_bvar_ssvs<-fevd(my_bvar_ssvs, h = 12, id_obj = NULL, type = "general", normalize = TRUE)
##................tvar...........................................................
my_fevd_msvar_uninfor<-fevd(my_msvar_uninfor, h = 12, id_obj = NULL, type = "general", normalize = TRUE)
my_fevd_msvar_minn<-fevd(my_msvar_minn, h = 12, id_obj = NULL, type = "general", normalize = TRUE)
my_fevd_msvar_ssvs<-fevd(my_msvar_ssvs, h = 12, id_obj = NULL, type = "general", normalize = TRUE)
##................tvar...........................................................
my_fevd_tvar_uninfor<-fevd(my_tvar_uninfor, h = 12, id_obj = NULL, type = "general", normalize = TRUE)
my_fevd_tvar_minn<-fevd(my_tvar_minn, h = 12, id_obj = NULL, type = "general", normalize = TRUE)
my_fevd_tvar_ssvs<-fevd(my_tvar_ssvs, h = 12, id_obj = NULL, type = "general", normalize = TRUE)

##................favar...........................................................
my_fevd_favar_uninfor<-fevd(my_favar_uninfor, h = 12, id_obj = NULL, type = "general", normalize = TRUE)
my_fevd_favar_minn<-fevd(my_favar_minn, h = 12, id_obj = NULL, type = "general", normalize = TRUE)
my_fevd_favar_ssvs<-fevd(my_favar_ssvs, h = 12, id_obj = NULL, type = "general", normalize = TRUE)
##................ftvar...........................................................
my_fevd_ftvar_uninfor<-fevd(my_ftvar_uninfor, h = 12, id_obj = NULL, type = "general", normalize = TRUE)
my_fevd_ftvar_minn<-fevd(my_ftvar_minn, h = 12, id_obj = NULL, type = "general", normalize = TRUE)
my_fevd_ftvar_ssvs<-fevd(my_ftvar_ssvs, h = 12, id_obj = NULL, type = "general", normalize = TRUE)

##################################################################################################################




########################## Historical Decomposition##############################################################
#.........................bvar.............................................................
my_hd_bvar_uninfor<-hd(my_bvar_uninfor)
my_hd_bvar_uninfor<-hd(my_bvar_minn)
my_hd_bvar_uninfor<-hd(my_bvar_ssvs)
#.........................msvar.............................................................
my_hd_msvar_uninfor<-hd(my_msvar_uninfor)
my_hd_bvar_uninfor<-hd(my_bvar_minn)
my_hd_msvar_uninfor<-hd(my_msvar_ssvs)
#.........................tvar.............................................................
my_hd_tvar_uninfor<-hd(my_tvar_uninfor)
my_hd_tvar_uninfor<-hd(my_tvar_minn)
my_hd_tvar_uninfor<-hd(my_tvar_ssvs)
#.........................ftvar.............................................................
my_hd_ftvar_uninfor<-hd(my_ftvar_uninfor)
my_hd_ftvar_uninfor<-hd(my_ftvar_minn)
my_hd_ftvar_uninfor<-hd(my_ftvar_ssvs)
#.........................favar.............................................................
my_hd_fvar_uninfor<-hd(my_favar_uninfor)
my_hd_favar_uninfor<-hd(my_favar_minn)
my_hd_favar_uninfor<-hd(my_favar_ssvs)
#############################################################################################





###############################posterior density plots########################################################
plot_density(obj, lag = 1, hpd = NULL)

## S3 method for class 'bvar'
my_bvar_post_density_uninfor<-plot_density(obj=my_bvar_uninfor, lag = 1, hpd = NULL)
my_bvar_post_density_minn<-plot_density(obj=my_bvar_minn, lag = 1, hpd = NULL)
my_bvar_post_density_ssvs<-plot_density(obj=my_bvar_ssvs, lag = 1, hpd = NULL)
my_bvar_post_density_cnw<-plot_density(obj=my_bvar_cnw, lag = 1, hpd = NULL)

## S3 method for class 'msvar'
my_msvar_post_density_uninfor_1<-plot_density(obj=my_msvar_uninfor, lag = 1, hpd = NULL, regime = 1)
my_msvar_post_density_uninfor_2<-plot_density(obj=my_msvar_uninfor, lag = 1, hpd = NULL, regime = 2)

my_msvar_post_density_minn_1<-plot_density(obj=my_msvar_minn, lag = 1, hpd = NULL, regime = 1)
my_msvar_post_density_minn_2<-plot_density(obj=my_msvar_minn, lag = 1, hpd = NULL, regime = 2)

my_msvar_post_density_ssvs_1<-plot_density(obj=my_msvar_ssvs, lag = 1, hpd = NULL, regime = 1)
my_msvar_post_density_ssvs_2<-plot_density(obj=my_msvar_minn, lag = 1, hpd = NULL, regime = 2)

my_msvar_post_density_cnw_1<-plot_density(obj=my_msvar_cnw, lag = 1, hpd = NULL, regime = 1)
my_msvar_post_density_cnw_2<-plot_density(obj=my_msvar_cnw, lag = 1, hpd = NULL, regime = 2)
##......................................................................................
## S3 method for class 'tvar'
my_tvar_post_density_uninfor<-plot_density(obj=my_tvar_uninfor, lag = 1, hpd = NULL, regime = 1)
my_tvar_post_density_minn<-plot_density(obj=my_tvar_minn, lag = 1, hpd = NULL, regime = 1)
my_tvar_post_density_ssvs<-plot_density(obj=my_tvar_ssvs, lag = 1, hpd = NULL, regime = 1)
my_tvar_post_density_cnw<-plot_density(obj=my_tvar_cnw, lag = 1, hpd = NULL, regime = 1)
##......................................................................................
## S3 method for class 'msvar'
my_msvar_post_density_uninfor<-plot_density(obj=my_msvar_uninfor, lag = 1, hpd = NULL, regime = 1)
my_msvar_post_density_minn<-plot_density(obj=my_msvar_minn, lag = 1, hpd = NULL, regime = 1)
my_msvar_post_density_ssvs<-plot_density(obj=my_msvar_ssvs, lag = 1, hpd = NULL, regime = 1)
my_msvar_post_density_cnw<-plot_density(obj=my_msvar_cnw, lag = 1, hpd = NULL, regime = 1)
##......................................................................................
#############################################################################################################





######################################Autocorrelation of Posterior Draws#####################################
my_plot_autocorr<-plot_autocorr(obj, lag = 1, maxlag = 20)

## S3 method for class 'bvar'
my_plot_bvar_autocorr_uninfor<-plot_autocorr(my_bvar_uninfor, lag = 1, maxlag = 20)
my_plot_bvar_autocorr_minn<-plot_autocorr(my_bvar_minn, lag = 1, maxlag = 20)
my_plot_bvar_autocorr_ssvs<-plot_autocorr(my_bvar_ssvs, lag = 1, maxlag = 20)
my_plot_bvar_autocorr_cnw<-plot_autocorr(my_bvar_cnw, lag = 1, maxlag = 20)
##......................................................................................

## S3 method for class 'msvar'
my_plot_msvar_autocorr_uninfor<-plot_autocorr(my_msvar_uninfor, lag = 1, maxlag = 20)
my_plot_msvar_autocorr_minn<-plot_autocorr(my_msvar_minn, lag = 1, maxlag = 20)
my_plot_msvar_autocorr_ssvs<-plot_autocorr(my_msvar_ssvs, lag = 1, maxlag = 20)
my_plot_msvar_autocorr_cnw<-plot_autocorr(my_msvar_cnw, lag = 1, maxlag = 20)
##.....................................................................................


## S3 method for class 'tvar'
my_plot_tvar_autocorr_uninfor<-plot_autocorr(my_tvar_uninfor, lag = 1, maxlag = 20)
my_plot_tvar_autocorr_minn<-plot_autocorr(my_tvar_minn, lag = 1, maxlag = 20)
my_plot_tvar_autocorr_ssvs<-plot_autocorr(my_tvar_ssvs, lag = 1, maxlag = 20)
my_plot_tvar_autocorr_cnw<-plot_autocorr(my_tvar_cnw, lag = 1, maxlag = 20)
##.....................................................................................


## S3 method for class 'favar'
my_plot_favar_autocorr_uninfor<-plot_autocorr(my_favar_uninfor, lag = 1, maxlag = 20)
my_plot_favar_autocorr_minn<-plot_autocorr(my_favar_minn, lag = 1, maxlag = 20)
my_plot_favar_autocorr_ssvs<-plot_autocorr(my_favar_ssvs, lag = 1, maxlag = 20)
my_plot_favar_autocorr_cnw<-plot_autocorr(my_favar_cnw, lag = 1, maxlag = 20)

## S3 method for class 'ftvar'
my_plot_ftvar_autocorr_uninfor<-plot_autocorr(my_ftvar_uninfor, lag = 1, maxlag = 20)
my_plot_ftvar_autocorr_minn<-plot_autocorr(my_ftvar_minn, lag = 1, maxlag = 20)
my_plot_ftvar_autocorr_ssvs<-plot_autocorr(my_ftvar_ssvs, lag = 1, maxlag = 20)
my_plot_ftvar_autocorr_cnw<-plot_autocorr(my_ftvar_cnw, lag = 1, maxlag = 20)

###################################################################################################




##################################PLOT OF RESIDUALS###############################################
##.............plot residuals......................................
my_residual_plot<-plot_residuals(obj)

## S3 method for class 'bvar'
my_residual_plot_bvar_uninfor<-plot_residuals(obj=my_bvar_uninfor)
my_residual_plot_bvar_minn<-plot_residuals(obj=my_bvar_minn)
my_residual_plot_bvar_ssvs<-plot_residuals(obj=my_bvar_ssvs)
my_residual_plot_bvar_cnw<-plot_residuals(obj=my_bvar_cnw)

## S3 method for class 'msvar'
my_residual_plot_msvar_uninfor<-plot_residuals(obj=my_msvar_uninfor)
my_residual_plot_msvar_minn<-plot_residuals(obj=my_msvar_minn)
my_residual_plot_msvar_ssvs<-plot_residuals(obj=my_msvar_ssvs)
my_residual_plot_msvar_cnw<-plot_residuals(obj=my_msvar_cnw)

## S3 method for class 'tvar'
my_residual_plot_tvar_uninfor<-plot_residuals(obj=my_tvar_uninfor)
my_residual_plot_tvar_minn<-plot_residuals(obj=my_tvar_minn)
my_residual_plot_tvar_ssvs<-plot_residuals(obj=my_tvar_ssvs)
my_residual_plot_tvar_cnw<-plot_residuals(obj=my_tvar_cnw)
##...............................................................

####################################################################################################



#############################Posterior Trace Plots###################################################
##........................................
my_post_trace_plot<-plot_trace(obj, lag = 1)

## S3 method for class 'bvar'
my_post_trace_plot_bvar_uninfor<-plot_trace(obj=my_tvar_uninfor, lag = 1)
my_post_trace_plot_bvar_minn<-plot_trace(obj=my_tvar_minn, lag = 1)
my_post_trace_plot_bvar_ssvs<-plot_trace(obj=my_tvar_ssvs, lag = 1)
my_post_trace_plot_bvar_cnw<-plot_trace(obj=my_tvar_cnw, lag = 1)
##.............................................................

## S3 method for class 'msvar'
my_post_trace_plot_msvar_uninfor<-plot_trace(obj=my_msvar_uninfor, lag = 1)
my_post_trace_plot_msvar_minn<-plot_trace(obj=my_msvar_minn, lag = 1)
my_post_trace_plot_msvar_ssvs<-plot_trace(obj=my_msvar_ssvs, lag = 1)
my_post_trace_plot_msvar_cnw<-plot_trace(obj=my_msvar_cnw, lag = 1)
##.............................................................

## S3 method for class 'tvar'
my_post_trace_plot_tvar_uninfor<-plot_trace(obj=my_tvar_uninfor, lag = 1)
my_post_trace_plot_tvar_minn<-plot_trace(obj=my_tvar_minn, lag = 1)
my_post_trace_plot_tvar_ssvs<-plot_trace(obj=my_tvar_ssvs, lag = 1)
my_post_trace_plot_tvar_cnw<-plot_trace(obj=my_tvar_cnw, lag = 1)
##.............................................................

## S3 method for class 'favar'
my_post_trace_plot_favar_uninfor<-plot_trace(obj=my_favar_uninfor, lag = 1)
my_post_trace_plot_favar_minn<-plot_trace(obj=my_favar_minn, lag = 1)
my_post_trace_plot_favar_ssvs<-plot_trace(obj=my_favar_ssvs, lag = 1)
my_post_trace_plot_favar_cnw<-plot_trace(obj=my_favar_cnw, lag = 1)
##.............................................................

## S3 method for class 'bvar'
my_post_trace_plot_ftvar_uninfor<-plot_trace(obj=my_ftvar_uninfor, lag = 1)
my_post_trace_plot_ftvar_minn<-plot_trace(obj=my_ftvar_minn, lag = 1)
my_post_trace_plot_ftvar_ssvs<-plot_trace(obj=my_ftvar_ssvs, lag = 1)
my_post_trace_plot_ftvar_cnw<-plot_trace(obj=my_ftvar_cnw, lag = 1)
##.............................................................



##......................Parallel Computation of IRF_TVAR....................................................
my_pc_irf_tvar<-tirf1(y, Alpha, Sigma, tart, thVar, thDelay, nolags, nhor, intercept,
                      bootrep, id_obj, K)
##..........................................................................................................






