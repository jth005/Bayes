## 15 (a)



a <- 1 
b <- 0.5



dbeta ( -1, a, b)

left <- 0
right <- seq(0,1,0.01)
int <- c(rep(0,100))
N <- 10
sum.int <- c(rep(0,1000))


for (i in 1:100){
    
    h <- right[i]/N
    x <- seq(0, right[i], h)
    
    for ( k in 1:N){
        sum.int[i] <- sum.int[i] + (dbeta(x[k+1],a,b) + dbeta(x[k],a,b))
    }
    
    int[i] <- (h/2)*sum.int[i]

    if (int[i] >= 0.5) {print (right[i]) & break}
}

qbeta(0.1, a,b)

qbeta(1, a,b)

qbeta(0,a,b)

    
i=5
