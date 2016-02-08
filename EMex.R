
m <-2000
lambda <- c(.6 , .25, .15) #rate is given by 1/(2lambda)
lam <- sample(lambda , size = 2000, replace=TRUE)
y <- rgamma(m, shape = .5, rate = 1/(2*lam))

N <- 10000 #maximum number of iterations
L <- c(.5, .4, .1) #initial target estimates
tol <- 0.000000001
L.old <- L+1

for (j in 1:N){
    f1 <- dgamma(y , shape = 0.5, rate = 1/(2*L[1]))
    f2 <- dgamma(y , shape = 0.5, rate = 1/(2*L[2]))
    f3 <- dgamma(y , shape = 0.5, rate = 1/(2*L[3]))
    py <- f1 / (f1 + f2 + f3) #posterior probability y from 1
    qy <- f2 / (f1 + f2 + f3)
    ry <- f3 / (f1 + f2 + f3)

    mu1 <- sum(y*py) / sum(py) #update means
    mu2 <- sum(y*qy) / sum(qy)
    mu3 <- sum(y*ry) / sum(ry)
    L <- c(mu1, mu2, mu3) #update lambdas
    L <- L / sum(L)

    if ( sum( abs( L - L.old) / L.old) < tol) break
    L.old <-L
    
}

print(list(lambda = L/sum(L) , iter =j, tol =tol))

