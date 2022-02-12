# Compare the actual ET values as measured by the EC tower with the ET estimated 
# using the water balance (methods A-C) and "slopes" (methods D-F)

# Packages ####
library(readxl)
library(Metrics)
library(MASS)

# Data preparation ####
ET.data=read_xlsx('DailyET.xlsx')
EC.data=read_xlsx('EC tower.xlsx')

# Method A
ET.A.df=data.frame(date=ET.data$date, ET.A=ET.data$ET.A, 
                   ET.original=EC.data$ET_orig, ET.closed=EC.data$ET_closed)
ET.A.df=na.omit(ET.A.df)

n=length(ET.A.df$date)
ET.A.df$index=c(0)

for (i in 1:n) {if (ET.A.df$ET.A[i]>=ET.A.df$ET.original[i] && 
                  ET.A.df$ET.A[i]<=ET.A.df$ET.closed[i])
        {ET.A.df$index[i]=1}}
success.A=sum(ET.A.df$index)
success.A/n*100

# Method B
ET.B.df=data.frame(date=ET.data$date, ET.B=ET.data$ET.B, 
                   ET.original=EC.data$ET_orig, ET.closed=EC.data$ET_closed)
ET.B.df=na.omit(ET.B.df)

n=length(ET.B.df$date)
ET.B.df$index=c(0)

for (i in 1:n) {if (ET.B.df$ET.B[i]>=ET.B.df$ET.original[i] && 
                    ET.B.df$ET.B[i]<=ET.B.df$ET.closed[i])
{ET.B.df$index[i]=1}}
success.B=sum(ET.B.df$index)
success.B/n*100

# Method C
ET.C.df=data.frame(date=ET.data$date, ET.C=ET.data$ET.C, 
                     ET.original=EC.data$ET_orig, ET.closed=EC.data$ET_closed)
ET.C.df=na.omit(ET.C.df)

n=length(ET.C.df$date)
ET.C.df$index=c(0)

for (i in 1:n) {if (ET.C.df$ET.C[i]>=ET.C.df$ET.original[i] && 
                    ET.C.df$ET.C[i]<=ET.C.df$ET.closed[i])
{ET.C.df$index[i]=1}}
success.C=sum(ET.C.df$index)
success.C/n*100

# Method D
ET.D.df=data.frame(date=ET.data$date, ET.D=ET.data$ET.D, 
                     ET.original=EC.data$ET_orig, ET.closed=EC.data$ET_closed)
ET.D.df=na.omit(ET.D.df)

n=length(ET.D.df$date)
ET.D.df$index=c(0)

for (i in 1:n) {if (ET.D.df$ET.D[i]>=ET.D.df$ET.original[i] && 
                    ET.D.df$ET.D[i]<=ET.D.df$ET.closed[i])
{ET.D.df$index[i]=1}}
success.D=sum(ET.D.df$index)
success.D/n*100

# Method E
ET.E.df=data.frame(date=ET.data$date, ET.E=ET.data$ET.E, 
                     ET.original=EC.data$ET_orig, ET.closed=EC.data$ET_closed)
ET.E.df=na.omit(ET.E.df)

n=length(ET.E.df$date)
ET.E.df$index=c(0)

for (i in 1:n) {if (ET.E.df$ET.E[i]>=ET.E.df$ET.original[i] && 
                    ET.E.df$ET.E[i]<=ET.E.df$ET.closed[i])
{ET.E.df$index[i]=1}}
success.E=sum(ET.E.df$index)
success.E/n*100

# Method F
ET.F.df=data.frame(date=ET.data$date, ET.F=ET.data$ET.F, 
                     ET.original=EC.data$ET_orig, ET.closed=EC.data$ET_closed)
ET.F.df=na.omit(ET.F.df)

n=length(ET.F.df$date)
ET.F.df$index=c(0)

for (i in 1:n) {if (ET.F.df$ET.F[i]>=ET.F.df$ET.original[i] && 
                    ET.F.df$ET.F[i]<=ET.F.df$ET.closed[i])
{ET.F.df$index[i]=1}}
success.F=sum(ET.F.df$index)
success.F/n*100



# Box plots ####
boxplot(ET.data$ETr, ET.data$ET.A, ET.data$ET.B, ET.data$ET.C, ET.data$ET.D,
        ET.data$ET.E, ET.data$ET.F, EC.data$ET_orig, EC.data$ET_closed,
        col='goldenrod', outcol='goldenrod', pch=20, ylab='ET (mm)',
        names=c('ETr', 'A', 'B', 'C', 'D', 'E', 'F', 'Original', 'Closed'))


# Time series ####
plot(ET.data$date, ET.data$ET.A, type='h', col='darkslategray3', lwd=2, 
     ylim=c(0, 15), ylab='ET (mm)', xlab='')
lines(EC.data$date, EC.data$ET_orig, type='l', lwd=2)
lines(EC.data$date, EC.data$ET_closed, type='l', lwd=2, col='purple')
legend('topright', lty=1, lwd=3, inset=0.02, legend=c('EC unclosed', 'EC closed', 'Method A'), col = c('black', 'purple', 'darkslategray3'))

plot(ET.data$date, ET.data$ET.B, type='h', col='darkslategray3', lwd=2, 
     ylim=c(0, 15), ylab='ET (mm)', xlab='')
lines(EC.data$date, EC.data$ET_orig, type='l', lwd=2)
lines(EC.data$date, EC.data$ET_closed, type='l', lwd=2, col='purple')
legend('topright', lty=1, lwd=3, inset=0.02, legend=c('EC unclosed', 'EC closed', 'Method B'), col = c('black', 'purple', 'darkslategray3'))

plot(ET.data$date, ET.data$ET.C, type='h', col='darkslategray3', lwd=2, 
     ylim=c(0, 15), ylab='ET (mm)', xlab='')
lines(EC.data$date, EC.data$ET_orig, type='l', lwd=2)
lines(EC.data$date, EC.data$ET_closed, type='l', lwd=2, col='purple')
legend('topright', lty=1, lwd=3, inset=0.02, legend=c('EC unclosed', 'EC closed', 'Method C'), col = c('black', 'purple', 'darkslategray3'))

plot(ET.data$date, ET.data$ET.D, type='h', col='darkslategray3', lwd=2, 
     ylim=c(0, 15), ylab='ET (mm)', xlab='')
lines(EC.data$date, EC.data$ET_orig, type='l', lwd=2)
lines(EC.data$date, EC.data$ET_closed, type='l', lwd=2, col='purple')
legend('topright', lty=1, lwd=3, inset=0.02, legend=c('EC unclosed', 'EC closed', 'Method D'), col = c('black', 'purple', 'darkslategray3'))

plot(ET.data$date, ET.data$ET.E, type='h', col='darkslategray3', lwd=2, 
     ylim=c(0, 15), ylab='ET (mm)', xlab='')
lines(EC.data$date, EC.data$ET_orig, type='l', lwd=2)
lines(EC.data$date, EC.data$ET_closed, type='l', lwd=2, col='purple')
legend('topright', lty=1, lwd=3, inset=0.02, legend=c('EC unclosed', 'EC closed', 'Method E'), col = c('black', 'purple', 'darkslategray3'))

plot(ET.data$date, ET.data$ET.F, type='h', col='darkslategray3', lwd=2, 
     ylim=c(0, 15), ylab='ET (mm)', xlab='')
lines(EC.data$date, EC.data$ET_orig, type='l', lwd=2)
lines(EC.data$date, EC.data$ET_closed, type='l', lwd=2, col='purple')
legend('topright', lty=1, lwd=3, inset=0.02, legend=c('EC unclosed', 'EC closed', 'Method F'), col = c('black', 'purple', 'darkslategray3'))


       
# Wilcox test ####

# Method A
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

# 1:1 plots: estimated values vs EC tower values ----

# Method A
X=ET.A.df$ET.A
Y=ET.A.df$ET.original

plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower - Original', xlab='Method A')
abline(a=0, b=1)
lines(X, Y, type='p', pch=19, col='darkolivegreen3')

n=length(X)                         # Number of observations
R2.A=summary(lm(Y~X))$adj.r.squared # Calculate adjusted R-squared
RMSE.A=rmse(actual=Y, predicted=X)  # Calculate root mean square error
legend('topleft', inset=0.01, bty='n', legend=c(paste('n=', n), 
                                       paste('R-squared=', sprintf(R2.A, fmt = '%#.2f')  ),
                                       paste('RMSE=', sprintf(RMSE.A, fmt = '%#.2f'))))
Y=ET.A.df$ET.closed

plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower - Closed', xlab='Method A')
abline(a=0, b=1)
lines(X, Y, type='p', pch=19, col='salmon2')

n=length(X)                         # Number of observations
R2.A=summary(lm(Y~X))$adj.r.squared # Calculate adjusted R-squared
RMSE.A=rmse(actual=Y, predicted=X)  # Calculate root mean square error
legend('topleft', inset=0.01, bty='n', legend=c(paste('n=', n), 
                                                paste('R-squared=', sprintf(R2.A, fmt = '%#.2f')  ),
                                                paste('RMSE=', sprintf(RMSE.A, fmt = '%#.2f'))))

# Method B
X=ET.B.df$ET.B
Y=ET.B.df$ET.original

plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower - Original', xlab='Method B')
abline(a=0, b=1)
lines(X, Y, type='p', pch=19, col='darkolivegreen3')

n=length(X)                         # Number of observations
R2.B=summary(lm(Y~X))$adj.r.squared # Calculate adjusted R-squared
RMSE.B=rmse(actual=Y, predicted=X)  # Calculate root mean square error
legend('topleft', inset=0.01, bty='n', legend=c(paste('n=', n),
                                       paste('R-squared=', sprintf(R2.B, fmt = '%#.2f')  ),
                                       paste('RMSE=', sprintf(RMSE.B, fmt = '%#.2f'))))

Y=ET.B.df$ET.closed

plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower - Closed', xlab='Method B')
abline(a=0, b=1)
lines(X, Y, type='p', pch=19, col='salmon2')

n=length(X)                         # Number of observations
R2.B=summary(lm(Y~X))$adj.r.squared # Calculate adjusted R-squared
RMSE.B=rmse(actual=Y, predicted=X)  # Calculate root mean square error
legend('topleft', inset=0.01, bty='n', legend=c(paste('n=', n), 
                                                paste('R-squared=', sprintf(R2.B, fmt = '%#.2f')  ),
                                                paste('RMSE=', sprintf(RMSE.B, fmt = '%#.2f'))))


# Method C
X=ET.C.df$ET.C
Y=ET.C.df$ET.original

plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower - Original', xlab='Method C')
abline(a=0, b=1)
lines(X, Y, type='p', pch=19, col='darkolivegreen3')

n=length(X)                         # Number of observations
R2.C=summary(lm(Y~X))$adj.r.squared # Calculate adjusted R-squared
RMSE.C=rmse(actual=Y, predicted=X)  # Calculate root mean square error
legend('topleft', inset=0.01, bty='n', legend=c(paste('n=', n),
                                       paste('R-squared=', sprintf(R2.C, fmt = '%#.2f')  ),
                                       paste('RMSE=', sprintf(RMSE.C, fmt = '%#.2f'))))

Y=ET.C.df$ET.closed

plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower - Closed', xlab='Method C')
abline(a=0, b=1)
lines(X, Y, type='p', pch=19, col='salmon2')

n=length(X)                         # Number of observations
R2.C=summary(lm(Y~X))$adj.r.squared # Calculate adjusted R-squared
RMSE.C=rmse(actual=Y, predicted=X)  # Calculate root mean square error
legend('topleft', inset=0.01, bty='n', legend=c(paste('n=', n), 
                                                paste('R-squared=', sprintf(R2.C, fmt = '%#.2f')  ),
                                                paste('RMSE=', sprintf(RMSE.C, fmt = '%#.2f'))))

# Method D
X=ET.D.df$ET.D
Y=ET.D.df$ET.original

plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower - Original', xlab='Method D')
abline(a=0, b=1)
lines(X, Y, type='p', pch=19, col='darkolivegreen3')

n=length(X)                         # Number of observations
R2.D=summary(lm(Y~X))$adj.r.squared # Calculate adjusted R-squared
RMSE.D=rmse(actual=Y, predicted=X)  # Calculate root mean square error
legend('topleft', inset=0.01, bty='n', legend=c(paste('n=', n),
                                       paste('R-squared=', sprintf(R2.D, fmt = '%#.2f')  ),
                                       paste('RMSE=', sprintf(RMSE.D, fmt = '%#.2f'))))

Y=ET.D.df$ET.closed

plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower - Closed', xlab='Method D')
abline(a=0, b=1)
lines(X, Y, type='p', pch=19, col='salmon2')

n=length(X)                         # Number of observations
R2.D=summary(lm(Y~X))$adj.r.squared # Calculate adjusted R-squared
RMSE.D=rmse(actual=Y, predicted=X)  # Calculate root mean square error
legend('topleft', inset=0.01, bty='n', legend=c(paste('n=', n), 
                                                paste('R-squared=', sprintf(R2.D, fmt = '%#.2f')  ),
                                                paste('RMSE=', sprintf(RMSE.D, fmt = '%#.2f'))))

# Method E
X=ET.E.df$ET.E
Y=ET.E.df$ET.original

plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower - Original', xlab='Method E')
abline(a=0, b=1)
lines(X, Y, type='p', pch=19, col='darkolivegreen3')

n=length(X)                         # Number of observations
R2.E=summary(lm(Y~X))$adj.r.squared # Calculate adjusted R-squared
RMSE.E=rmse(actual=Y, predicted=X)  # Calculate root mean square error
legend('topleft', inset=0.01, bty='n', legend=c(paste('n=', n),
                                       paste('R-squared=', sprintf(R2.E, fmt = '%#.2f')  ),
                                       paste('RMSE=', sprintf(RMSE.E, fmt = '%#.2f'))))
Y=ET.E.df$ET.closed

plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower - Closed', xlab='Method E')
abline(a=0, b=1)
lines(X, Y, type='p', pch=19, col='salmon2')

n=length(X)                         # Number of observations
R2.E=summary(lm(Y~X))$adj.r.squared # Calculate adjusted R-squared
RMSE.E=rmse(actual=Y, predicted=X)  # Calculate root mean square error
legend('topleft', inset=0.01, bty='n', legend=c(paste('n=', n), 
                                                paste('R-squared=', sprintf(R2.E, fmt = '%#.2f')  ),
                                                paste('RMSE=', sprintf(RMSE.E, fmt = '%#.2f'))))

# Method F
X=ET.F.df$ET.F
Y=ET.F.df$ET.original

plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower - Original', xlab='Method F')
abline(a=0, b=1)
lines(X, Y, type='p', pch=19, col='darkolivegreen3')

n=length(X)                         # Number of observations
R2.F=summary(lm(Y~X))$adj.r.squared # Calculate adjusted R-squared
RMSE.F=rmse(actual=Y, predicted=X)  # Calculate root mean square error
legend('topleft', inset=0.01, bty='n', legend=c(paste('n=', n),
                                       paste('R-squared=', sprintf(R2.F, fmt = '%#.2f')  ),
                                       paste('RMSE=', sprintf(RMSE.F, fmt = '%#.2f'))))

Y=ET.F.df$ET.closed

plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower - Closed', xlab='Method F')
abline(a=0, b=1)
lines(X, Y, type='p', pch=19, col='salmon2')

n=length(X)                         # Number of observations
R2.F=summary(lm(Y~X))$adj.r.squared # Calculate adjusted R-squared
RMSE.F=rmse(actual=Y, predicted=X)  # Calculate root mean square error
legend('topleft', inset=0.01, bty='n', legend=c(paste('n=', n), 
                                                paste('R-squared=', sprintf(R2.F, fmt = '%#.2f')  ),
                                                paste('RMSE=', sprintf(RMSE.F, fmt = '%#.2f'))))

# Scatter plots w/ ranges ----

# Method A
plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower', xlab='Method A')
abline(a=0, b=1)
arrows(x0=ET.A.df$ET.A, y0=ET.A.df$ET.original, y1=ET.A.df$ET.closed, length=0)

X=ET.A.df$ET.A        # ET from method A on x-axis
Y=ET.A.df$ET.original # Original ET from EC tower on y-axis
lines(X, Y, type='p', pch=24, bg='darkolivegreen3', cex=0.8)

Y=ET.A.df$ET.closed   # Closed ET from EC tower on y-axis
lines(X, Y, type='p', pch=25, bg='salmon2', cex=0.8)

legend('topright', inset=0.01, legend=c('Original', 'Closed'), pch=c(24, 25), pt.bg=c('darkolivegreen3', 'salmon2'))

# Method B
plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower', xlab='Method B')
abline(a=0, b=1)
arrows(x0=ET.B.df$ET.B, y0=ET.B.df$ET.original, y1=ET.B.df$ET.closed, length=0)

X=ET.B.df$ET.B        # ET from Method B on x-axis
Y=ET.B.df$ET.original # Original ET from EC tower on y-axis
lines(X, Y, type='p', pch=24, bg='darkolivegreen3', cex=0.8)

Y=ET.B.df$ET.closed   # Closed ET from EC tower on y-axis
lines(X, Y, type='p', pch=25, bg='salmon2', cex=0.8)

legend('topright', inset=0.01, legend=c('Original', 'Closed'), pch=c(24, 25), pt.bg=c('darkolivegreen3', 'salmon2'))

# Method C
plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower', xlab='Method C')
abline(a=0, b=1)
arrows(x0=ET.C.df$ET.C, y0=ET.C.df$ET.original, y1=ET.C.df$ET.closed, length=0)

X=ET.C.df$ET.C        # ET from Method C on x-axis
Y=ET.C.df$ET.original # Original ET from EC tower on y-axis
lines(X, Y, type='p', pch=24, bg='darkolivegreen3', cex=0.8)

Y=ET.C.df$ET.closed   # Closed ET from EC tower on y-axis
lines(X, Y, type='p', pch=25, bg='salmon2', cex=0.8)

legend('topright', inset=0.01, legend=c('Original', 'Closed'), pch=c(24, 25), pt.bg=c('darkolivegreen3', 'salmon2'))

# Method D
plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower', xlab='Method D')
abline(a=0, b=1)
arrows(x0=ET.D.df$ET.D, y0=ET.D.df$ET.original, y1=ET.D.df$ET.closed, length=0)

X=ET.D.df$ET.D        # ET from Method D on x-axis
Y=ET.D.df$ET.original # Original ET from EC tower on y-axis
lines(X, Y, type='p', pch=24, bg='darkolivegreen3', cex=0.8)

Y=ET.D.df$ET.closed   # Closed ET from EC tower on y-axis
lines(X, Y, type='p', pch=25, bg='salmon2', cex=0.8)

legend('topright', inset=0.01, legend=c('Original', 'Closed'), pch=c(24, 25), pt.bg=c('darkolivegreen3', 'salmon2'))

# Method E
plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower', xlab='Method E')
abline(a=0, b=1)
arrows(x0=ET.E.df$ET.E, y0=ET.E.df$ET.original, y1=ET.E.df$ET.closed, length=0)

X=ET.E.df$ET.E        # ET from Method E on x-axis
Y=ET.E.df$ET.original # Original ET from EC tower on y-axis
lines(X, Y, type='p', pch=24, bg='darkolivegreen3', cex=0.8)

Y=ET.E.df$ET.closed   # Closed ET from EC tower on y-axis
lines(X, Y, type='p', pch=25, bg='salmon2', cex=0.8)

legend('topright', inset=0.01, legend=c('Original', 'Closed'), pch=c(24, 25), pt.bg=c('darkolivegreen3', 'salmon2'))

# Method F
plot(NULL, ylim=c(0,15), xlim=c(0,15), ylab='EC tower', xlab='Method F')
abline(a=0, b=1)
arrows(x0=ET.F.df$ET.F, y0=ET.F.df$ET.original, y1=ET.F.df$ET.closed, length=0)

X=ET.F.df$ET.F        # ET from Method F on x-axis
Y=ET.F.df$ET.original # Original ET from EC tower on y-axis
lines(X, Y, type='p', pch=24, bg='darkolivegreen3', cex=0.8)

Y=ET.F.df$ET.closed   # Closed ET from EC tower on y-axis
lines(X, Y, type='p', pch=25, bg='salmon2', cex=0.8)

legend('topright', inset=0.01, legend=c('Original', 'Closed'), pch=c(24, 25), pt.bg=c('darkolivegreen3', 'salmon2'))

