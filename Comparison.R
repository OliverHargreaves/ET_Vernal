# Compare the actual ET values as measured by the EC tower with the ET estimated 
# using the water balance (methods A-C) and "slopes" (methods D-F)

# Packages ####
library(readxl)
library(basicTrendline)
library(Metrics)
library(MASS)
install.packages("MASS")

# Load data ####
ET.data=read_xlsx('DailyET.xlsx')

# Box plots ####
boxplot(ET.data$ETr, ET.data$ET.A, ET.data$ET.B, ET.data$ET.C, ET.data$ET.D,
        ET.data$ET.E, ET.data$ET.F, ET.data$ET_orig, ET.data$ET_closed,
        col='goldenrod', outcol='goldenrod', pch=20, ylab='ET (mm)',
        names=c('ETr', 'A', 'B', 'C', 'D', 'E', 'F', 'Original', 'Closed'))


# Time series ####
plot(ET.data$date, ET.data$ET.A, type='h', col='darkslategray3', lwd=2, 
     ylim=c(0, 15), ylab='ET (mm)', xlab='')
lines(ET.data$date, ET.data$ET_orig, type='l', lwd=2)
lines(ET.data$date, ET.data$ET_closed, type='l', lwd=2, col='purple')
legend('topright', lty=1, lwd=3, inset=0.02, legend=c('EC unclosed', 'EC closed', 'Method A'), col = c('black', 'purple', 'darkslategray3'))

plot(ET.data$date, ET.data$ET.B, type='h', col='darkslategray3', lwd=2, 
     ylim=c(0, 15), ylab='ET (mm)', xlab='')
lines(ET.data$date, ET.data$ET_orig, type='l', lwd=2)
lines(ET.data$date, ET.data$ET_closed, type='l', lwd=2, col='purple')
legend('topright', lty=1, lwd=3, inset=0.02, legend=c('EC unclosed', 'EC closed', 'Method B'), col = c('black', 'purple', 'darkslategray3'))

plot(ET.data$date, ET.data$ET.C, type='h', col='darkslategray3', lwd=2, 
     ylim=c(0, 15), ylab='ET (mm)', xlab='')
lines(ET.data$date, ET.data$ET_orig, type='l', lwd=2)
lines(ET.data$date, ET.data$ET_closed, type='l', lwd=2, col='purple')
legend('topright', lty=1, lwd=3, inset=0.02, legend=c('EC unclosed', 'EC closed', 'Method C'), col = c('black', 'purple', 'darkslategray3'))

plot(ET.data$date, ET.data$ET.D, type='h', col='darkslategray3', lwd=2, 
     ylim=c(0, 15), ylab='ET (mm)', xlab='')
lines(ET.data$date, ET.data$ET_orig, type='l', lwd=2)
lines(ET.data$date, ET.data$ET_closed, type='l', lwd=2, col='purple')
legend('topright', lty=1, lwd=3, inset=0.02, legend=c('EC unclosed', 'EC closed', 'Method D'), col = c('black', 'purple', 'darkslategray3'))

plot(ET.data$date, ET.data$ET.E, type='h', col='darkslategray3', lwd=2, 
     ylim=c(0, 15), ylab='ET (mm)', xlab='')
lines(ET.data$date, ET.data$ET_orig, type='l', lwd=2)
lines(ET.data$date, ET.data$ET_closed, type='l', lwd=2, col='purple')
legend('topright', lty=1, lwd=3, inset=0.02, legend=c('EC unclosed', 'EC closed', 'Method E'), col = c('black', 'purple', 'darkslategray3'))

plot(ET.data$date, ET.data$ET.F, type='h', col='darkslategray3', lwd=2, 
     ylim=c(0, 15), ylab='ET (mm)', xlab='')
lines(ET.data$date, ET.data$ET_orig, type='l', lwd=2)
lines(ET.data$date, ET.data$ET_closed, type='l', lwd=2, col='purple')
legend('topright', lty=1, lwd=3, inset=0.02, legend=c('EC unclosed', 'EC closed', 'Method F'), col = c('black', 'purple', 'darkslategray3'))


       
# Wilcox test ####

# Method A
ET.A.df=data.frame(date=ET.data$date, ET.A=ET.data$ET.A, ET.original=ET.data$ET_orig, ET.closed=ET.data$ET_closed)
ET.A.df=na.omit(ET.A.df)
# Original (not-closed) EC data
wilcox.test(x=ET.A.df$ET.original, y=ET.A.df$ET.A, alternative='two.sided', mu=0, paired=T, conf.int=T)
# Closed EC data
wilcox.test(x=ET.A.df$ET.closed, y=ET.A.df$ET.A, alternative='two.sided', mu=0, paired=T, conf.int=T)
# Box plots
dET.original=ET.A.df$ET.original-ET.A.df$ET.A
dET.closed=ET.A.df$ET.closed-ET.A.df$ET.A
boxplot(dET.original, dET.closed, main='Method A', col='cornflowerblue', 
        pch=19, outcol='cornflowerblue', names=c('Original', 'Closed'))
abline(h=0, lwd=2, col='coral3')

# Method B
ET.B.df=data.frame(date=ET.data$date, ET.B=ET.data$ET.B, ET.original=ET.data$ET_orig, ET.closed=ET.data$ET_closed)
ET.B.df=na.omit(ET.B.df)
# Original (not-closed) EC data
wilcox.test(x=ET.B.df$ET.original, y=ET.B.df$ET.B, alternative='two.sided', mu=0, paired=T, conf.int=T)
# Closed EC data
wilcox.test(x=ET.B.df$ET.closed, y=ET.B.df$ET.B, alternative='two.sided', mu=0, paired=T, conf.int=T)
# Box plots
dET.original=ET.B.df$ET.original-ET.B.df$ET.B
dET.closed=ET.B.df$ET.closed-ET.B.df$ET.B
boxplot(dET.original, dET.closed, main='Method B', col='cornflowerblue', 
        pch=19, outcol='cornflowerblue', names=c('Original', 'Closed'))
abline(h=0, lwd=2, col='coral3')

# Method C
ET.C.df=data.frame(date=ET.data$date, ET.C=ET.data$ET.C, ET.original=ET.data$ET_orig, ET.closed=ET.data$ET_closed)
ET.C.df=na.omit(ET.C.df)
# Original (not-closed) EC data
wilcox.test(x=ET.C.df$ET.original, y=ET.C.df$ET.C, alternative='two.sided', mu=0, paired=T, conf.int=T)
# Closed EC data
wilcox.test(x=ET.C.df$ET.closed, y=ET.C.df$ET.C, alternative='two.sided', mu=0, paired=T, conf.int=T)
# Box plots
dET.original=ET.C.df$ET.original-ET.C.df$ET.C
dET.closed=ET.C.df$ET.closed-ET.C.df$ET.C
boxplot(dET.original, dET.closed, main='Method C', col='cornflowerblue', 
        pch=19, outcol='cornflowerblue', names=c('Original', 'Closed'))
abline(h=0, lwd=2, col='coral3')

# Method D
ET.D.df=data.frame(date=ET.data$date, ET.D=ET.data$ET.D, ET.original=ET.data$ET_orig, ET.closed=ET.data$ET_closed)
ET.D.df=na.omit(ET.D.df)
# Original (not-closed) EC data
wilcox.test(x=ET.D.df$ET.original, y=ET.D.df$ET.D, alternative='two.sided', mu=0, paired=T, conf.int=T)
# Closed EC data
wilcox.test(x=ET.D.df$ET.closed, y=ET.D.df$ET.D, alternative='two.sided', mu=0, paired=T, conf.int=T)
# Box plots
dET.original=ET.D.df$ET.original-ET.D.df$ET.D
dET.closed=ET.D.df$ET.closed-ET.D.df$ET.D
boxplot(dET.original, dET.closed, main='Method D', col='cornflowerblue', 
        pch=19, outcol='cornflowerblue', names=c('Original', 'Closed'))
abline(h=0, lwd=2, col='coral3')

# Method E
ET.E.df=data.frame(date=ET.data$date, ET.E=ET.data$ET.E, ET.original=ET.data$ET_orig, ET.closed=ET.data$ET_closed)
ET.E.df=na.omit(ET.E.df)
# Original (not-closed) EC data
wilcox.test(x=ET.E.df$ET.original, y=ET.E.df$ET.E, alternative='two.sided', mu=0, paired=T, conf.int=T)
# Closed EC data
wilcox.test(x=ET.E.df$ET.closed, y=ET.E.df$ET.E, alternative='two.sided', mu=0, paired=T, conf.int=T)
# Box plots
dET.original=ET.E.df$ET.original-ET.E.df$ET.E
dET.closed=ET.E.df$ET.closed-ET.E.df$ET.E
boxplot(dET.original, dET.closed, main='Method E', col='cornflowerblue', 
        pch=19, outcol='cornflowerblue', names=c('Original', 'Closed'))
abline(h=0, lwd=2, col='coral3')

# Method F
ET.F.df=data.frame(date=ET.data$date, ET.F=ET.data$ET.F, ET.original=ET.data$ET_orig, ET.closed=ET.data$ET_closed)
ET.F.df=na.omit(ET.F.df)
# Original (not-closed) EC data
wilcox.test(x=ET.F.df$ET.original, y=ET.F.df$ET.F, alternative='two.sided', mu=0, paired=T, conf.int=T)
# Closed EC data
wilcox.test(x=ET.F.df$ET.closed, y=ET.F.df$ET.F, alternative='two.sided', mu=0, paired=T, conf.int=T)
# Box plots
dET.original=ET.F.df$ET.original-ET.F.df$ET.F
dET.closed=ET.F.df$ET.closed-ET.F.df$ET.F
boxplot(dET.original, dET.closed, main='Method F', col='cornflowerblue', 
        pch=19, outcol='cornflowerblue', names=c('Original', 'Closed'))
abline(h=0, lwd=2, col='coral3')

# 1:1 plots: estimated values vs original measured values ----

# Method C
X=ET.C.df$ET.C
Y=ET.C.df$ET.original

trendline(y=Y, x=X, ylim=c(0, 15), xlim=c(0,15), ylab='EC tower', xlab='Method C',
          linecolor='darkgoldenrod', lwd=2, eDigit=2, CI.color='lightgoldenrod', 
          pch=19, col='olivedrab4')
abline(0, 1, lwd=1.5)
robust=rlm(Y~X)
abline(robust, col='red', lwd=2)

legend('topright', inset=0.01, lty=1, lwd=2, legend=c('Identity line', 'Linear regression'), col=c('black', 'darkgoldenrod'))

X=ET.C.df$ET.original
Y=ET.C.df$ET.C

trendline(y=Y, x=X, ylim=c(0, 15), xlim=c(0,15), ylab='Method C', xlab='EC tower',
          linecolor='darkgoldenrod', lwd=2, eDigit=2, CI.color='lightgoldenrod', 
          pch=19, col='olivedrab4')
abline(0, 1, lwd=1.5)
legend('topright', inset=0.01, lty=1, lwd=2, legend=c('Identity line', 'Linear regression'), col=c('black', 'darkgoldenrod'))

RMSE.C=rmse(actual=Y, predicted=X)
R2.C=summary(lm(Y~X))$adj.r.squared
Points.C=length(ET.C.df$ET.C)

Summary.df=data.frame('Method'='C', 'RMSE'=RMSE.C, 'R squared'=R2.C, 'Points'=Point.C)

robust=rlm(Y~X)
abline(robust, col='red', lwd=2)

n=length(ET.C.df$ET.C)
for (i in 1:n) {if (ET.C.df$ET.C[i]>10) {ET.C.df$ET.C[i]=NA}}
ET.C.df=na.omit(ET.C.df)

plot(ET.C.df$ET.C, ET.C.df$ET.original)
