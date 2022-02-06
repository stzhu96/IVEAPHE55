#' separation function
#'
#' @param data Data includes temperature, daily death toll, PM2.5 concentration, time variable, boundary layer height and wind speed.
#' @param pm2.5 Enter the concentration of pm2.5 in a numeric format, eg. 150.
#' @param time Enter the time in a Date format, eg. 2016-01-01.
#' @param dow Enter the day of week variable in a factor format, eg. Monday. 
#' @param temperature Enter the temperature in a numeric format, eg. 12.
#' @param optimal.df The degrees of freedom of natural spline function with time variable 
#'                  for predicting the exposure pm2.5 in a numeric format, eg. 15.
#'
#' @return  resid1-calculate the residual change of pm2.5 after controlling the confounding factors of 
#'         temperature and time trend.
#' @export
#'
#' @examples
#' 
separation <- function(data,pm2.5,time,dow,temperature,optimal.df) {
  pm2.5<-data$pm2.5
  time<-unclass(data$time)
  use <- complete.cases(pm2.5, time)
  br.fit<- bruto(time[use], pm2.5[use])
  optimal.df<- br.fit$df
  
  model1<-gam(pm2.5~ns(time,df=optimal.df)+
                as.factor(dow)+
                s(temperature,bs="ad"),
              data=data,family=gaussian)
  resid1<-residuals(model1)
data$resid1<-resid1
}