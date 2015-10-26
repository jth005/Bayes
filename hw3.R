igamma

install.packages("pscl")

library(pscl)
d <- c(rep(0,10))
for( i in 1:9){
    d[i]<-qigamma(0.1*i, 1,1)}
d
