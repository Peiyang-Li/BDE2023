curve(dnorm(x), -6, 6, xlab = "x", ylab = "Density", n = 200)
curve(dt(x, 2), -6, 6, add = TRUE, col = 4, n = 200)
legend("topright", c("Normal density", "t density"), 
       col = c(1, 4), bty = "n", lty = 1)

x <- seq(-6,6, length.out=10000)
max(dnorm(x)/dt(x,2))


#Cauchy Distribution
x<- seq(-6, 6, 0.01)
y <- dnorm(x, 0, 1)
plot(x, y, 'l', ylim=c(0,0.5), col='red')
lines(x, dcauchy(x), col='blue')

#Student t5 Distribution
x<- seq(-6, 6, 0.01)
y <- dnorm(x, 0, 1)
plot(x, y, 'l', ylim=c(0,0.5), col='red')
lines(x, dt(x,5), col='blue')

#N(1,2) Distribution
x<- seq(-6, 6, 0.01)
y <- dnorm(x, 0, 1)
plot(x, y, 'l', ylim=c(0,0.5), col='red')
lines(x, dnorm(x,1,2), col='blue')


#计算四个分布的理论接受概率
#Cauchy Distribution
norm_cauchy <- function(x) -dnorm(x,0,1)/dcauchy(x)
cauchy_min <- optim(6,norm_cauchy,lower=-10,upper=10,method = 'Brent')
cat(' Maximum of M:',-as.numeric(cauchy_min['value']),'\n',
    'At the point:',as.numeric(cauchy_min['par']),'\n')

#Student t5 Distribution
norm_student <- function(x) -dnorm(x,0,1)/dt(x,5)
student_min <- optim(6,norm_student,lower=-10,upper=10,method = 'Brent')
cat(' Maximum of M:',-as.numeric(student_min['value']),'\n',
    'At the point:',as.numeric(student_min['par']),'\n')

#N(1,2) Distribution
norm_norm <- function(x) -dnorm(x,0,1)/dnorm(x,1,sqrt(2))
norm_min <- optim(6,norm_norm,lower=-10,upper=10,method = 'Brent')
cat(' Maximum of M:',-as.numeric(norm_min['value']),'\n',
    'At the point:',as.numeric(norm_min['par']),'\n')
