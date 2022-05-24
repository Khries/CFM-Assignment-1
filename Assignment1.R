#Q1


AR2.sim <- function(n, ar1, ar2, sd)
{
  Xt = c(0,0)
  for (i in 2:n)
  {
    Xt[i+1] = ar1*Xt[i] + ar2*Xt[i-1] + rnorm(1,mean=0,sd=sd)
  }
  Xt
}
Xt = AR2.sim(100, 1, 0, 0.5)
plot(Xt, type = "o",xlab ="Time" ,ylab =expression("Z"[t]) ,main = expression(paste("AR(2) plot with, ", Phi[1], "=0.5, ", Phi[2],
                                                                     "=0.4")))
acf.results<-acf(Xt, lag.max = 20, type = "correlation",plot = TRUE, na.action = na.pass)
plot(acf.results,main= expression(paste("Plot of ACF of AR(2)  with, ", Phi[1], "=0.5, ", Phi[2],
                                   "=0.4")))

#Q2

Pt <- function(n, ar1, sd)
{
  Xt = c(0,0)
  for (i in 2:n)
  {
    Xt[i+1] = ar1*Xt[i] + rnorm(1,mean=0,sd=sd)
  }
  Xt
}
Yt = Pt(100, 0.8, 1)

Zt=c(0,0)
for (i in 1:100)
{
    Zt[i]=Yt[i]+ rnorm(1,mean=0,sd=1)
}

plot(Zt, type = "o",xlab ="Time" ,ylab =expression("Z"[t]) ,main = expression(paste("Plot of Z "[t], "= Y, "[t],"+,",
                                                                                    mu[t])))
acf.results<-acf(Zt, lag.max = 20, type = "partial",plot = TRUE, na.action = na.pass)
plot(acf.results,main= expression(paste("Partial ACF Plot of Z "[t], "= Y, "[t],"+,",
                                        mu[t])))
#since the given ts is basically an auto regressive model, and since it has
#highest autocorrelation at lag=2 we set p=2, and q=0

AR <- arima(Zt, order = c(2,0,0))
print(AR)

ts.plot(Zt,xlab="Time", ylab =expression("Z"[t]),main = expression(paste("Plot of Z "[t], "= Y, "[t],"+,",
                                                                         mu[t])))
AR_fit <- Zt - residuals(AR)
points(AR_fit, type = "l", col = 2, lty = 2)



#Q3
install.packages('data.table')
library(data.table)

# get data
data("C:/4th_sem/CFM/For MSc, MTech/For MSc, MTech/Average population age clean.csv")

# transpose
t_mtcars <- transpose(mtcars)

# get row and colnames in order
  colnames(t_mtcars) <- rownames(mtcars)
  rownames(t_mtcars) <- colnames(mtcars)

data<-read.csv('')
tseries_diff2 <- diff(data, lag = 2)


data
