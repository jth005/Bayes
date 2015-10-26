##8
##a

data <- c(1.2, 2.4, 1.3, 1.3, 0.0, 1.0,1.8, 0.8, 4.6, 1.4)
length(data)
k <- 10

s2 <- (1/k)*sum(((data) - mean(data))^2)

tausq.hat <- s2 - 1

b.hat <- 1 / (1 + tausq.hat)

js.theta <- (1 - (k-2) / (sum( data^2)) )  * data;js.theta


##b 

js.prime.theta <- mean(data) + (1 - (k-3)/(sum( (data - mean(data))^2)))*(data - mean(data));js.prime.theta

##c



center <- b.hat*mean(data) + (1 - b.hat)*data[9]

lower <- -1.96*sqrt(1 - b.hat)
upper <- 1.96*sqrt(1 - b.hat)

center + lower
center + upper

##d

b.hat.m <- (k-3)/(k-1) * b.hat

center.m <- b.hat.m * mean(data) + (1- b.hat.m)*data[9]

v.hat.m <- (1 - ((k-1)/k)*b.hat.m) + (2/(k-3))*(b.hat.m^2)*(data[9] - mean(data))^2

lower <- -1.96*sqrt(v.hat.m)

upper <- 1.96*sqrt(v.hat.m)

center+lower
center+upper

##9 (c)

theta.true <- c(rep(0,m))
k <- 5
a<-3;b<-3
m <- 10000
count <- c(rep(0,m))
data <-matrix( c(rep(0,k*m)), nrow = m)
post.data<-matrix( c(rep(0,5*5000)), nrow = m)
a.hat <- c(rep(0,m))
b.hat <- c(rep(0,m))
lower <- c(rep(0,m))
upper <- c(rep(0,m))

for (i in 1:m){

    theta.true[i] <- rgamma(1,shape = 3, scale = 3)
    
    data[i,] <-rpois(5,theta.true[i])

    a.hat[i] <- (mean(data[i,]))^2 / (sum ( (data - mean(data[i,]))^2 ) -  mean(data[i,]))

    b.hat[i] <- mean(data[i,]) / a.hat[i]


    lower[i] <- qgamma(0.05, shape = a.hat[i] + sum(data[i,]), scale = b.hat[i] / (k*b.hat[i]+1))
    
    upper[i] <- qgamma(0.95, shape = a.hat[i] + sum(data[i,]), scale = b.hat[i] / (k*b.hat[i]+1))

    if ((lower[i] < theta.true[i]) && (theta.true[i] < upper[i])) {count[i] = 1}
    else {count[i]=0}
}

p <- sum(count)/m;p  #estimate
se <- sd(count)/sqrt(m); se #standard error 

