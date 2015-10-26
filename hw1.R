2*pbinom(4,17,0.5)

s <-c(rep(0,10))

for ( i in 5:12){

    s[i] <- pbinom(15-i,27,0.5) * dbinom(i,17,0.5)
    }

prob <-2* sum(s);prob

pvalue <- prob + 0.049;pvalue
