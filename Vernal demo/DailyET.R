# Daily Et (mm) from the Vernal research site 2019-2021

# Packages ####
library(readxl)
library(writexl)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(multcompView)
library(tidyverse)

# Prepare the data ####
sm.data=read_xlsx('Volumetric water content.xlsx') # Soil moisture data
sm.data=sm.data[1:8]
names(sm.data)=c('date', 's1', 's2', 's3', 's4', 's5', 's6', 's7')

ETr.data=read_xlsx('DailyETr.xlsx') # Reference ET data

# Set variables ####

date=sm.data$date
n=length(date) # Number of days

sm1=sm.data$s1/100 # Soil moisture content at sensor 1
sm2=sm.data$s2/100 # Soil moisture content at sensor 2
sm3=sm.data$s3/100 # ...
sm4=sm.data$s4/100
sm5=sm.data$s5/100
sm6=sm.data$s6/100
sm7=sm.data$s7/100

z1=0.03*1000 # Depth of sensor 1 (mm)
z2=0.10*1000 # Depth of sensor 2 (mm)
z3=0.25*1000 # ... 
z4=0.65*1000
z5=1.05*1000
z6=1.45*1000
z7=2.15*1000

z12=(z1+z2)/2 # Sensor 1-2 soil interface depth
z23=(z2+z3)/2 # Sensor 2-3 soil interface depth
z34=(z3+z4)/2 # ... 
z45=(z4+z5)/2
z56=(z5+z6)/2
z67=(z6+z7)/2

zr=2000 # Alfalfa root depth (mm)

wc1=sm1*z12          # water content of soil layer 1
wc2=sm2*(z23-z12)    # water content of soil layer 2
wc3=sm3*(z34-z23)    # water content of soil layer 3
wc4=sm4*(z45-z34)    # ...
wc5=sm5*(z56-z45)     
wc6=sm6*(z67-z56)     
wc7=sm7*(z7-z67)*2

wc=wc1+wc2+wc3+wc4+wc5+wc6+wc7  # Total water depth (mm) up to 2.15 m
sm.data$wc=wc

ETr=ETr.data$ETr

# window used to calculate the "derivatives" (days) - Methods D, E, F
w=3 

# Soil moisture plots ####
plot(date, sm1, type='l', lwd=2, ylim=c(0, 0.5), ylab='Volumetric water content (mm/mm)', xlab='', col='firebrick3')
lines(date, sm2, type='l', lwd=2, col='darkorange2')
lines(date, sm3, type='l', lwd=2, col='gold2')
lines(date, sm4, type='l', lwd=2, col='olivedrab3')
lines(date, sm5, type='l', lwd=2, col='royalblue3')
lines(date, sm6, type='l', lwd=2, col='purple3')
lines(date, sm7, type='l', lwd=2, col='violetred3')
legend('bottomright', inset=0.01, lty=1, lwd=2, legend=c('Sensor 1', 'Sensor 2', 'Sensor 3', 'Sensor 4', 'Sensor 5', 'Sensor 6', 'Sensor 7'),
       col=c('firebrick3', 'darkorange2', 'gold2', 'olivedrab3', 'royalblue3', 'purple3', 'violetred3'))

plot(date, wc, type='l', lwd=2, ylab='Water depth (mm)', xlab='', col='royalblue')

# Method A: Water balance on the soil profile as a whole ####
# This method estimates ET (mm/day) by applying the water balance method to the entire soil profile at once.

# ET estimation
ET.A=c()
for(i in 1:n) {ET.A[i]=wc[i]-wc[i+1]}

# Eliminate values of ET<0
for(i in 1:(n-1)) {if (ET.A[i]<0) {ET.A[i]=0}}  
# Eliminate values of ET>15mm
for (i in 1:(n-1)) {if (ET.A[i]>15) {ET.A[i]=0}} 

ET.A[ET.A==0]=NA

plot(date, ET.A, 
     type='h', lwd=2, col='darkslategray3',
     main='Method A', xlab='', ylab='ET (mm/day)')
lines(date, ETr, type='l', col='palegreen3', lwd=2)
legend('topright', legend=c('ETr', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Method B: Water balance on each sensors soil depth NOT accounting for root growth ####
# This method estimates ET (mm/day) by applying the water balance to each soil
# layer and then adds them all up after having eliminated all negative 
# contributions. Method B does not account for root growth throughout the season.

# ET from soil depth 1
ET.B.1=c() 
for(i in 1:n) {ET.B.1[i]=(sm1[i]-sm1[i+1])*z12}

# Eliminate values of ET<0
for(i in 1:(n-1)) {if (ET.B.1[i]<0) {ET.B.1[i]=0}}   

plot(date, ET.B.1,
     type='h', lwd=2, 
     main='ET from soil depth 1', xlab='', ylab='ET (mm)')

# ET from soil depth 2
ET.B.2=c()
for(i in 1:n) {ET.B.2[i]=(sm2[i]-sm2[i+1])*(z23-z12)}

# Eliminate values of ET<0
for(i in 1:(n-1)) {if (ET.B.2[i]<0) {ET.B.2[i]=0}}    

plot(date, ET.B.2, 
     type='h', lwd=2, 
     main='ET from soil depth 2', xlab='', ylab='ET (mm)')

# ET from soil depth 3
ET.B.3=c()
for(i in 1:n) {ET.B.3[i]=(sm3[i]-sm3[i+1])*(z34-z23) }

# Eliminate values of ET<0
for(i in 1:(n-1)) {if (ET.B.3[i]<0) {ET.B.3[i]=0} }   

plot(date, ET.B.3, 
     type='h', lwd=2,
     main='ET from soil depth 3', xlab='', ylab='ET (mm)')

# ET from soil depth 4
ET.B.4=c()
for(i in 1:n) {ET.B.4[i]=(sm4[i]-sm4[i+1])*(z45-z34) }

# Eliminate values of ET<0
for(i in 1:(n-1)) {if (ET.B.4[i]<0) {ET.B.4[i]=0} }         

plot(date, ET.B.4, 
     type='h', lwd=2,
     main='ET from soil depth 4', xlab='', ylab='ET (mm)')

# ET from soil depth 5
ET.B.5=c()
for(i in 1:n) {ET.B.5[i]=(sm5[i]-sm5[i+1])*(z56-z45) }

# Eliminate values of ET<0
for(i in 1:(n-1)) {if (ET.B.5[i]<0) {ET.B.5[i]=0} }

plot(date, ET.B.5, 
     type='h', lwd=2,
     main='ET from soil depth 5', xlab='', ylab='ET (mm)')

# ET from soil depth 6
ET.B.6=c()
for(i in 1:n) {ET.B.6[i]=(sm6[i]-sm6[i+1])*(z67-z56) }

# Eliminate values of ET<0
for(i in 1:(n-1)) {if (ET.B.6[i]<0) {ET.B.6[i]=0} }         

plot(date, ET.B.6, 
     type='h', lwd=2,
     main='ET from soil depth 6', xlab='', ylab='ET (mm)')

# ET from soil depth 7
ET.B.7=c()
for(i in 1:n) {ET.B.7[i]=(sm7[i]-sm7[i+1])*(z7-z67)*2 }

# Eliminate values of ET<0
for(i in 1:(n-1)) {if (ET.B.7[i]<0) {ET.B.7[i]=0} }  

plot(date, ET.B.7, 
     type='h', lwd=2,
     main='ET from soil depth 7', xlab='', ylab='ET (mm)')

# Total ET 
ET.B=ET.B.1+ET.B.2+ET.B.3+ET.B.4+ET.B.5+ET.B.6+ET.B.7
ET.B[n]=NA # can't calculate ET the last day
ET.B[ET.B==0]=NA

plot(date, ET.B, 
     type='h', col='darkslategray3', lwd=2, ylim=c(0,15), 
     main='Method B', xlab='', ylab='ET (mm/day)')
lines(date, ETr, type='l', col='palegreen3', lwd=2)
legend('topright', legend=c('ETr', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Heat map
x=date
y=paste('Soil depth', 7:1)
ET=c(ET.B.7, ET.B.6, ET.B.5, ET.B.4, ET.B.3, ET.B.2, ET.B.1)
ET[ET==0]=NA
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - method B') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Method C: Water balance on each sensors soil depth accounting for root growth ####
# This method estimates ET (mm/day) by applying the water balance to each soil layer and then adds them all up after having eliminated all negative contributions. Method C takes root growth throughout the season in account.

# NOTE: since the root depth (zr) is constant throughout the season the contribution
# from soil layers 1-6 are the same as for method B.

# ET from soil depth 1
ET.C.1=ET.B.1

plot(date, ET.C.1, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 1', xlab='', ylab='ET (mm)')

# ET from soil depth 2
ET.C.2=ET.B.2

plot(date, ET.C.2, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 2', xlab='', ylab='ET (mm)')

# ET from soil depth 3
ET.C.3=ET.B.3

plot(date, ET.C.3, 
     type='h', lwd=2,
     main='ET from soil depth 3', xlab='', ylab='ET (mm)')

# ET from soil depth 4
ET.C.4=ET.B.4

plot(date, ET.C.4, 
     type='h', lwd=2,
     main='ET from soil depth 4', xlab='', ylab='ET (mm)')

# ET from soil depth 5
ET.C.5=ET.B.5

plot(date, ET.C.5, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 5', xlab='', ylab='ET (mm)')

# ET from soil depth 6
ET.C.6=ET.B.6

plot(date, ET.C.6, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 6', xlab='', ylab='ET (mm)')

# ET from soil depth 7
ET.C.7=c()
for(i in 1:n) {ET.C.7[i]=(sm7[i]-sm7[i+1])*(zr-z67) }

# Eliminate values of ET<0
for(i in 1:(n-1)) {if (ET.C.7[i]<0) {ET.C.7[i]=0} }  

plot(date, ET.C.7, 
     type='h', lwd=2,
     main='ET from soil depth 7', xlab='', ylab='ET (mm)')

# Total ET 
ET.C=ET.C.1+ET.C.2+ET.C.3+ET.C.4+ET.C.5+ET.C.6+ET.C.7
ET.C[n]=NA # can't calculate ET the last day
ET.C[ET.C==0]=NA

plot(date, ET.C, 
     type='h', col='darkslategray3', lwd=2,  ylim=c(0,15),
     main='Method C', xlab='', ylab='ET (mm/day)')
lines(date, ETr, type='l', col='palegreen3', lwd=2)
legend('topright', legend=c('ETr', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Heat map
x=date
y=paste('Soil depth', 7:1)
ET=c(ET.C.7, ET.C.6, ET.C.5, ET.C.4, ET.C.3, ET.C.2, ET.C.1)
ET[ET==0]=NA
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - method C') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Method D: Derivatives on the soil profile as a whole ####
# This method estimates ET (mm/day) by analyzing the first and second derivatives
# of the soil moisture plots. When the first derivative is negative and the second
# derivative is positive we are assuming that water is leaving the system only via 
# evapotranspiration at a rate equal to the slope of the soil moisture graph.

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the 1st day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}
plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

D.df=data.frame(Date=date, SM=wc, f1, f2) # data frame with the results for the D.df side

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
D.df$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (D.df$f1[i]<0 & D.df$f2[i]>0) {D.df$ET[i]=abs(D.df$f1[i])} 
}

plot(D.df$Date, D.df$ET,
     type='h', lwd=2, col='darkslategray3', ylim=c(0,15),
     main='Method D', xlab='', ylab='ET (mm/day)')
lines(date, ETr, type='l', col='palegreen3', lwd=2)
legend('topright', legend=c('ETr', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

ET.D=D.df$ET

# Method E: Derivatives NOT accounting for root growth####
# Same as method D but on each soil layer individually and then summed up and not accounting for
# root growth over the season.

# Soil depth 1

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 1', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

E.1.df=data.frame(Date=date, SM=wc1, f1, f2) # data frame with the results for E.1.df

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
E.1.df$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (E.1.df$f1[i]<0 & E.1.df$f2[i]>0) {E.1.df$ET[i]=abs(E.1.df$f1[i])} 
}
E.1.df$ET[is.na(E.1.df$ET)]=0 # replaces NA values with zero
plot(E.1.df$Date, E.1.df$ET,
     type='h', lwd=2,
     main='ET - Soil depth 1', xlab='', ylab='ET (mm)',
     ylim=c(0,10))
ET.E.1=E.1.df$ET

# Soil depth 2

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc2[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 2', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

E.2.df=data.frame(Date=date, SM=wc2, f1, f2) # data frame with the results for E.2.df

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
E.2.df$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (E.2.df$f1[i]<0 & E.2.df$f2[i]>0) {E.2.df$ET[i]=abs(E.2.df$f1[i])} 
}
E.2.df$ET[is.na(E.2.df$ET)]=0 # replaces NA values with zero
plot(E.2.df$Date, E.2.df$ET,
     type='h', lwd=2,
     main='ET - Soil depth 2', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

ET.E.2=E.2.df$ET

# Soil depth 3

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc3[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 3', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

E.3.df=data.frame(Date=date, SM=wc3, f1, f2) # data frame with the results for E.3.df

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
E.3.df$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (E.3.df$f1[i]<0 & E.3.df$f2[i]>0) {E.3.df$ET[i]=abs(E.3.df$f1[i])} 
}
E.3.df$ET[is.na(E.3.df$ET)]=0 # replaces NA values with zero
plot(E.3.df$Date, E.3.df$ET,
     type='h', lwd=2,
     main='ET - Soil depth 3', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

ET.E.3=E.3.df$ET

# Soil depth 4

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc4[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 4', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

E.4.df=data.frame(Date=date, SM=wc4, f1, f2) # data frame with the results for E.4.df

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
E.4.df$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (E.4.df$f1[i]<0 & E.4.df$f2[i]>0) {E.4.df$ET[i]=abs(E.4.df$f1[i])} 
}
E.4.df$ET[is.na(E.4.df$ET)]=0 # replaces NA values with zero
plot(E.4.df$Date, E.4.df$ET,
     type='h', lwd=2,
     main='ET - Soil depth 4', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

ET.E.4=E.4.df$ET

# Soil depth 5

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc5[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 5', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

E.5.df=data.frame(Date=date, SM=wc5, f1, f2) # data frame with the results for E.5.df

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
E.5.df$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (E.5.df$f1[i]<0 & E.5.df$f2[i]>0) {E.5.df$ET[i]=abs(E.5.df$f1[i])} 
}
E.5.df$ET[is.na(E.5.df$ET)]=0 # replaces NA values with zero
plot(E.5.df$Date, E.5.df$ET,
     type='h', lwd=2,
     main='ET - Soil depth 5', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

ET.E.5=E.5.df$ET

# Soil depth 6

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc6[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 6', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

E.6.df=data.frame(Date=date, SM=wc6, f1, f2) # data frame with the results for E.6.df

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
E.6.df$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (E.6.df$f1[i]<0 & E.6.df$f2[i]>0) {E.6.df$ET[i]=abs(E.6.df$f1[i])} 
}
E.6.df$ET[is.na(E.6.df$ET)]=0 # replaces NA values with zero
plot(E.6.df$Date, E.6.df$ET,
     type='h', lwd=2,
     main='ET - Soil depth 6', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

ET.E.6=E.6.df$ET

# Soil depth 7

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc7[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(date, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 7', xlab='', ylab='')
lines(date, f2, 
      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
abline(h=0, lwd=2)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

E.7.df=data.frame(Date=date, SM=wc7, f1, f2) # data frame with the results for E.7.df

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
E.7.df$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (E.7.df$f1[i]<0 & E.7.df$f2[i]>0) {E.7.df$ET[i]=abs(E.7.df$f1[i])} 
}
E.7.df$ET[is.na(E.7.df$ET)]=0 # replaces NA values with zero
plot(E.7.df$Date, E.7.df$ET,
     type='h', lwd=2,
     main='ET - Soil depth 7', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

ET.E.7=E.7.df$ET

# Total ET 
ET.E=ET.E.1+ET.E.2+ET.E.3+ET.E.4+ET.E.5+ET.E.6+ET.E.7

for (i in 1:n) {
  if (ET.E[i]<0.01) {ET.E[i]=0}}
ET.E[ET.E==0]=NA

plot(date, ET.E, 
     type='h', lwd=2, col='darkslategray3', ylim=c(0,15),
     main='Method E', xlab='', ylab='ET (mm/day)')
lines(date, ETr, type='l', col='palegreen3', lwd=2)
legend('topright', legend=c('ETr', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)

# Heat map
x=date
y=paste('Soil depth', 7:1)
ET=c(ET.E.7, ET.E.6, ET.E.5, ET.E.4, ET.E.3, ET.E.2, ET.E.1)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - method E') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()


# Method F: Derivatives accounting for root growth ####
# Same as method F but accounting for root growth over the season.

# NOTE: since the root depth (zr) is constant throughout the season the contribution
# from soil layers 1-6 are the same as for method E.

# ET from soil depths 1-6

ET.F.1=ET.E.1
ET.F.2=ET.E.2
ET.F.3=ET.E.3
ET.F.4=ET.E.4
ET.F.5=ET.E.5
ET.F.6=ET.E.6

# Soil depth 7

ET.F.7=E.7.df$ET*(zr-z67)/(2*(z7-z67))

plot(date, ET.F.7,
     type='h', lwd=2,
     main='ET - Soil depth 7', xlab='', ylab='ET (mm)')

# Total ET for the left side of the onion bed
ET.F=ET.F.1+
     ET.F.2+
     ET.F.3+
     ET.F.4+
     ET.F.5+
     ET.F.6+
     ET.F.7

for (i in 1:n) {
  if (ET.F[i]<0.01) {ET.F[i]=0}}
ET.F[ET.F==0]=NA

plot(date, ET.F, 
     type='h', lwd=2, col='darkslategray3',
     main='Method F', xlab='', ylab='ET (mm/day)',
     ylim=c(0, 15))
lines(date, ETr, type='l', col='palegreen3', lwd=2)

legend('topright', legend=c('ETr', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)

# Heat map
x=date
y=paste('Soil depth', 7:1)
ET=c(ET.F.7, ET.F.6, ET.F.5, ET.F.4, ET.F.3, ET.F.2, ET.F.1)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - method F') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Results ####
Results=data.frame(date, ETr, ET.A, ET.B, ET.C, ET.D, ET.E, ET.F)
write_xlsx(Results, path='DailyET.xlsx')
