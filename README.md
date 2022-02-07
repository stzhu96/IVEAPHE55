# IVEAPHE
 Instrumental Variables Estimate Air Pollution’s Health Effects. Instrumental Variables Estimate Air Pollution‘s Health Effects(IVEAPHE).Instrumental Variables Air Pollution’s Health Effects .The reliability of instrumental variable method mainly depends on the effectiveness of instrumental variables, so the selection of instrumental variables is very important. According to previous studies, the height of the planetary boundary layer and wind speed were chosen as  instrumental  variables.However the height of the planetary boundary layer and wind speed may vary with season and temperature, in addition,too high and too low temperature will also affect the concentration of air pollutant pm2.5, so the height of the planetary boundary layer and wind speed do not meet  basic assumptions of instrumental variables. So,before the two-stage predictor substitution and control function , the influence of temperature and time trend should be removed through regression, and the generalized additive model(generalized additive model is executed using the gam function of mgcv package)is used for regression and the link function is Gaussian. At this time, the residual of the generalized additive model includes the influence of instrumental variables, At this time, the , For details, please refer to Joel Schwartz's article(https://ehp.niehs.nih.gov/doi/10.1289/EHP2732). At this time, the residual error and pm2.5 and daily death constitute the instrumental variable model. At this stage, we call it separation.Then the instrumental variable method - two-stage predictor substitution and control function can be carried out. 
The first instrumental variable estimation method is two-stage predictor substitution. two-stage predictor substitution is a nonlinear simple extension of the commonly used two-stage least squares method(2SLS). In the first stage, the predicted value of exposure is obtained through instrumental variables and separation'result regression, and then the predicted value as a dependent variable for the second stage regression, The previous article of Joel Schwartz 'also adopts this method. In the first stage, the svm function of e1071 package is used to obtain pm2.5'predicted value. In the second stage, the gam function of mgcv package is used to estimate the health effect of pm2.5 with the predicted value of pm2.5 in the first stage and the known confounding (time and temperature) as independent variables and the number of deaths per day as dependent variables.
The second method is the control function, which is also divided into two stages. The calculation in the first stage is the same as that in the first stage of the two-stage predictor substitution, the residual of the first stage needs to be retained. In the second stage,the gam function of mgcv package is used for generalized additive model regression, with the residual of the first stage, pm2.5 and known confounding factors (time and temperature) as independent variables, The health effects of pm2.5 were estimated with the number of deaths per day as the dependent variable.
# Installation
The required R package needs to be installed before analysis,You can copy the following code. 
install.packages(c("mgcv", "splines","splines2","openxlsx","e1071","mda","boot","pacman")) 
library("pacman") 
p_load(mgcv, splines,splines2,openxlsx,e1071,mda,boot)
# Usage
Whether you use two-stage predictor substitution or control function, you need separation and see separation.R . If you plan to adopt the two-stage prediction substitution method, you can view the two-stage_prediction_substitution.R. If you use the control function method, you can view the control function.R.When using, you only need to modify the variable name according to your research and data.
# Development
This R code are develpoed by guimingzhu.
