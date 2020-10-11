
#1.Use simple linear regression approaches with residual sum of square error minimization to find the best fit model predicting the extent
# of human benefit (HB) provided by green stormwater infrastructure based on the average wealth (INCOME) of the communities the 
#infrastructure was built in (addressing the question: Do wealthy areas have green stormwater infrastructure elements capable
# of providing more human benefits than poorer areas?)

library(readxl)
DATA <- as.data.frame(read_excel("C:/Users/vt135/Downloads/Data analysis for env engr/Assignment3/Data_for_Assignment3.xlsx", sheet = 1))
INCOME = DATA$Income_dollarsperweek
HB = DATA$HB
SE = function (BETA) {sum( (HB - (BETA[1]*INCOME + BETA[2]))^2)}
library(neldermead)
B_SE = fminsearch(SE,c(1000,0.5))
B_SE$optbase$xopt

#B1 (slope) = 0.00151 and B0 (intercept) = -2.115
par(mar = c(4.5, 5.1, 2.1, 1.0),family = "serif")
Y = B_SE$optbase$xopt[1]*sort(INCOME) + B_SE$optbase$xopt[2]
plot(INCOME, HB, col = "black", pch = 19, xlab = 'INCOME ($/week)', ylab = 'HB')
lines(sort(INCOME), Y, col = "red")
axis(1, seq(700, 2250, 500))
axis(2, seq(-0.8, 3, 0.5))

Lin_mod = lm(HB~INCOME)
summary(Lin_mod)
R2_Lin_mod = summary(Lin_mod)$r.squared

# R2 = R2_Lin_mod = 0.2319
# The R2 value is low, indicating that our model is not good.
# Looking at the plot above, there are outliers that do not follow the trend.

#2.Use bootstrap approaches to estimate 95% percentile confidence bounds about the slope of the best-fit line using residual resampling. 

N = 10000
n = length(INCOME)
r = HB - (B_SE$optbase$xopt[1]*INCOME + B_SE$optbase$xopt[2])

rboot = matrix(NA,nrow = n,ncol = N)
for (i in 1:N)
{rboot[1:n,i] = cbind(sample(r,n,replace = TRUE))}

HBnew = (B_SE$optbase$xopt[1]*INCOME + B_SE$optbase$xopt[2]) + rboot
B = matrix(NA,N,2)
for (i in 1:N){
  B[i,] = polyfit(INCOME,HBnew[,i],1)}
CIs_prctile = quantile(B[,1], c(0.025, 0.975), type=1)

# answer in Slope (lower bound, upper bound) form is 0.00151 (0.000452, 0.002713).
# The confidence bounds do exclude 0, which suggests that community income is a significant predictor of biofilter benefits

# 3. Evaluate what factors control whether or not green stormwater infrastructure provides net human benefits (NB = 1) or not (NB = 0) using
#income, rainfall, and the dummy variable Rural as possible X variablesGLM regression is used to evaluate the question of interest.

RAINFALL = DATA$Rainfall_mm
RURAL =  DATA$Rural
RR = cor(cbind(INCOME, RAINFALL, RURAL), method = "pearson")
INV = inv(RR)
VIF = diag(INV)
VIF
#VIF (INCOME) = 1.025567
#VIF (RAINFALL) = 1.020191
#VIF (RURAL) = 1.008438
#Since VIF scores for all three possible X variables are greater than 5, all variables will be retained in my model (rule of thumb).

#4.Find the best fit coefficients for the model that includes the maximum number of X variables allowable under VIF (this will 
# depend on your results from step 2). Then fit all possible models with 2 X variables allowable under VIF that include Income 
# as one of the two variables (i.e., with the form Y~Income + Xi). Finally, fit a simple model with Income as the only X variable.
NB = DATA[,2]
s = length(NB)
pfun = function(BETA,MX) 
{exp(rowSums(tcrossprod(rep(1,s),BETA)*MX))/
    (1+(exp(rowSums(tcrossprod(rep(1,s),BETA)*MX))))}

nLogLik = function(BETA,DEP,MX) 
{-1.*log(prod((pfun(BETA,MX)^DEP)*
                ((1-pfun(BETA,MX))^(1-DEP)),na.rm = TRUE))}
ONES = matrix(1,s,1)
M1X = cbind(ONES,INCOME,RAINFALL,RURAL)
M2X = cbind(ONES,INCOME,RAINFALL)
M3X = cbind(ONES,INCOME,RURAL)
M4X = cbind(ONES,INCOME)
library(optimbase)
options <- optimset(TolX=1.e-20,MaxIter = 1000,
                    MaxFunEvals = 100000)
#model_1
fM1 = function(BETA) {nLogLik(BETA,NB,M1X)}
OUT1 = fminsearch(fM1,c(0, 0, 0, 0),options)
OUT1$optbase$xopt
nlnL_M1 = OUT1$optbase$fopt
#model_2
fM2 = function(BETA) {nLogLik(BETA,NB,M2X)}
OUT2 = fminsearch(fM2,c(0, 0, 0),options)
OUT2$optbase$xopt
nlnL_M2 = OUT2$optbase$fopt
#model_3
fM3 = function(BETA) {nLogLik(BETA,NB,M3X)}
OUT3 = fminsearch(fM3,c(0, 0, 0),options)
OUT3$optbase$xopt
nlnL_M3 = OUT3$optbase$fopt
#model_4
fM4 = function(BETA) {nLogLik(BETA,NB,M4X)}
OUT4 = fminsearch(fM4,c(0, 0),options)
OUT4$optbase$xopt
nlnL_M4 = OUT4$optbase$fopt

# For model1:
#B0 (intercept)   = -30.32023
#B1 (INCOME)    = 0.021997
#B2 (RAINFALL)  = -0.00533
#B3 (RURAL)       = 1.774280

# For model2:
#B0 (intercept)  = -29.18034
#B1 (INCOME)    = 0.022140
#B2 (RAINFALL)  = -0.00665

# For model3:
#B0 (intercept)  = -26.15152
#B1 (INCOME)    = 0.017005
#B2 (RURAL)       = 1.981394


# For model4:
#B0 (intercept)  = -24.10871
#B1 (INCOME)    = 0.016082

#5.Evaluate AICc for each model (report each score).Based on these scores, which model is best? Do you have a family of best-fit models? 
#If so, which models are included in the family? Interpret the best-fit model.
AICc = function(lnL,k) { (2*k - 2*lnL) +
    ((2*k*(k+1))/(n-k-1)) }
lnL_models = -1*c(nlnL_M1,nlnL_M2,nlnL_M3,nlnL_M4)
K = c(4,3,3,2)

AICc_MOD = matrix(NA,nrow=1,ncol=4)
for (i in 1:4) {
  AICc_MOD[1,i] = AICc(lnL_models[i],K[i])
}
AICc_MOD
# AICc (model 1) = 20.13401
# AICc (model 2) = 18.32296
# AICc (model 3) = 17.78827
# AICc (model 4) = 16.72145
# AICc coefficients for model 1, 2, and 3 are within 2 AICc coefficient of model 4 (the best model). Therefore, all models evaluated 
#are included in the family of best-fit models.
# Model 4 is the best-fit model since it minimizes AICc the most - it has the lowest AICc coefficient.
# The positive sign of each AICc coefficient indicates that the greater the value of the Independent variable, the higher would be 
#the benefits.

#6.What is the chance that the best-fit model identified by AICc is truly best? Is the chance that it is truly best close to any of the 
# other models?
delta_AICc_mat = AICc_MOD - min(AICc_MOD)
AICcw = function(delta_AICcI,deltaAICcr)
{exp(-1/2*delta_AICcI)/sum(exp(-1/2*deltaAICcr))}
Weights = matrix(NA,nrow = 1,ncol = 4)
for (i in 1:4)
{Weights[1,i] = AICcw(delta_AICc_mat[i],delta_AICc_mat)}

Weights
# Weight (model 1) = 0.0818
# Weight (model 2) = 0.2025
# Weight (model 3) = 0.2646
# Weight (model 4) = 0.4510
# There is a ~ 45% chance that the best-fit model (model 4) is truly best - that is more than 1.5 times as likely than model 3 - more than twice as likely than model 2 - and more than 5 times as likely than model 1. Thus, the chance that model 4 is truly best is not really close to any of the other models.

#7.Evaluate R-squared for the best-fit model (and the best-fit family of models if any others come close). Report each score
.	R2 =function(BETA,DEP,MX)
{1-((sum((DEP -(pfun(BETA,MX)))^2,na.rm = TRUE))
    /(sum((DEP-mean(DEP,na.rm = TRUE))^2,na.rm = TRUE))) }

R2_M1 = R2(OUT1$optbase$xopt,NB,M1X)
R2_M2 = R2(OUT2$optbase$xopt,NB,M2X)
R2_M3 = R2(OUT3$optbase$xopt,NB,M3X)
R2_M4 = R2(OUT4$optbase$xopt,NB,M4X)

# R2 (model 1) = 0.672
# R2 (model 2) = 0.656
# R2 (model 3) = 0.659
# R2 (model 4) = 0.615
# Given the high variability that likely exists in the dataset, I would consider the R2  values for the family of models to be solid, 
# suggesting that all models - model 1, model 2, model 3, and model 4 are acceptable models.






