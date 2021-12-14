# Vernal project site 2019-2021

# Sensor depth (mm)
z1=0.03*1000 # Depth of sensor 1
z1=rep(z1, 10)
z2=0.10*1000
z2=rep(z2, 10)
z3=0.25*1000
z3=rep(z3, 10)
z4=0.65*1000
z4=rep(z4, 10)
z5=1.05*1000
z5=rep(z5, 10)
z6=1.45*1000
z6=rep(z6, 10)
z7=2.15*1000
z7=rep(z7, 10)

# Surface
z0=0
z0=rep(z0, 10)

# Sensor influence bands
z12=(z1+z2)/2
z23=(z2+z3)/2
z34=(z3+z4)/2
z45=(z4+z5)/2
z56=(z5+z6)/2
z67=(z6+z7)/2

# Alfalfa root depth (mm)
rd=2000

x=c(1:10)

# Plot

plot(x, z0, type='l', ylim=c(-2800, 0), xlim=c(2, 9), ylab='Depth (mm)', xlab='', xaxt='n')

polygon(x=c(1:10, 10:1), y=c(z0, -z12), col='firebrick1')
polygon(x=c(1:10, 10:1), y=c(-z12, -z23), col='darkorange')
polygon(x=c(1:10, 10:1), y=c(-z23, -z34), col='gold')
polygon(x=c(1:10, 10:1), y=c(-z34, -z45), col='olivedrab2')
polygon(x=c(1:10, 10:1), y=c(-z45, -z56), col='royalblue1')
polygon(x=c(1:10, 10:1), y=c(-z56, -z67), col='purple1')
polygon(x=c(1:10, 10:1), y=c(-z67, -(z7+(z7-z67))), col='violetred1')

abline(h=z0, lwd=2, col='black')
abline(h=-z1, lwd=2, col='firebrick4')
abline(h=-z2, lwd=2, col='darkorange3')
abline(h=-z3, lwd=2, col='gold3')
abline(h=-z4, lwd=2, col='olivedrab4')
abline(h=-z5, lwd=2, col='royalblue4')
abline(h=-z6, lwd=2, col='purple4')
abline(h=-z7, lwd=2, col='violetred4')
abline(h=-rd, lwd=3, col='springgreen4')

