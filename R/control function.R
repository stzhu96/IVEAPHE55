#' method2: control function
#'
#' @param data Data includes temperature, daily death toll, PM2.5 concentration, 
#'             time variable, boundary layer height and wind speed.
#' @param time Enter the time in a Date format, eg. 2016-01-01.
#' @param dow Enter the day of week variable in a factor format, eg. Monday.
#' @param temperature Enter the temperature in a numeric format, eg. 12.
#' @param resid1 Separation function gets result.You can see the details in Separation.R and README.
#' @param optimal.df1 The degrees of freedom of natural spline function with time variable 
#'                    for predicting the deaths in a numeric format, eg. 15.
#' @param optimal.df2 The degrees of freedom of natural spline function with temperature 
#'                    for predicting the deaths in a numeric format, eg. 15.
#' @param resid2 The residual resid1 and the instrument variables are obtained for support vector regression.
#' @param pbl Enter the height of the planetary boundary layer in a numeric format, eg. 302.
#' @param ws Enter the wind speed in a numeric format, eg. 0.8.
#' @param deaths The daily death toll in a numeric format, eg. 40.
#'
#' @return the health effect of pm2.5 and its 95% CI by bootstrapping of time series.
#' @export
#'
#' @examples
control_function<-function(data,time,dow,temperature,resid1,optimal.df1,optimal.df2,resid2,pbl,ws,deaths) {
model4<-svm(resid1~pbl+ws,data=data,kernel='radial')
resid2<-residuals(model4)
data$resid2<-resid2

deaths<-data$deaths
time<-unclass(data$time)
use <- complete.cases(deaths, time)
br.fit<- bruto(time[use], deaths[use])
optimal.df1<- br.fit$df

temperature<-unclass(data$temperature)
use <- complete.cases(deaths,temperature)
br.fit<- bruto(temperature[use], deaths[use])
optimal.df2<- br.fit$df

model5<-gam(deaths~pm2.5+resid2+
              ns(time,df=optimal.df1)+
              as.factor(dow)+
              ns(temperature,df=optimal.df2),
            family=quasipoisson,data=data)
summary(model5)

bootf<-function(data)
{mod<-gam(deaths~pm2.5+resid2+
            ns(time,df=optimal.df1)+
            as.factor(dow)+
            ns(temperature,df=optimal.df2),
          family=quasipoisson,data=data)
return(mod$coef[2])
}
boot4<-tsboot(data,bootf, R=2000, l=20, sim ="fixed")
quantile(boot4$t[,1], probs = c(0.025,0.975))
}