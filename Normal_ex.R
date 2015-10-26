
#Handout 1 for Math 6480 Bayesian Analysis

#Draw graphs for the prior, the likelihood, and the posterior 

par(mfrow = c(2, 2))

#par(mfrow=c(1,1) 
mu<-5
sigmasq<-2
tausq<-4
y<-4

theta<-seq(-5, 15, 0.1)
 
likeli<-1/sqrt(2*pi*sigmasq)*exp(-1/2/sigmasq*(y-theta)^2)
 
plot(theta, likeli, type='p', xlab='theta', ylab='Density Curves', ylim=c(0,0.4), main='Sketches for Likelihood, Prior, and Posterior', pch=0)

prior<-1/sqrt(2*pi*tausq)*exp(-1/2/tausq*(theta-mu)^2)

#lines(theta, prior, type='l', lwd=1, pch=1)
lines(theta, prior, type='l', pch=1)

B<-sigmasq/(sigmasq+tausq)

post<-1/sqrt(2*pi)/(sqrt((1-B)*sigmasq))*exp(-1/(2*(1-B)*sigmasq)*(theta-(B*mu+(1-B)*y))^2) 

#lines(theta, post, type='b', lwd=4, pch=2)

lines(theta, post, type='b',  pch=2) 

legend(-5, 0.30, cex=0.5, c('Likelihood', 'Prior', 'Posterior'), pch=c(0,1,2))


dev.off()

mu<-5
sigmasq<-2
tausq<-4
y<-4

theta<-seq(-5, 15, 0.1)
 
likeli<-1/sqrt(2*pi*sigmasq)*exp(-1/2/sigmasq*(y-theta)^2)
 
plot(theta, likeli, lty=1, xlab='theta', ylab='Density Curves', ylim=c(0,0.4), main='Sketches for Likelihood, Prior, and Posterior')

prior<-1/sqrt(2*pi*tausq)*exp(-1/2/tausq*(theta-mu)^2)

#lines(theta, prior, type='l', lwd=1, pch=1)
lines(theta, prior, lty=2)

B<-sigmasq/(sigmasq+tausq)

post<-1/sqrt(2*pi*((1-B)*sigmasq))*exp(-1/2/((1-B)*sigmasq)*(theta-(B*mu+(1-B)*y))^2) 

#lines(theta, post, type='b', lwd=4, pch=2)

lines(theta, post, lty=3)

legend(-5, 0.30, cex=0.5, c('Likelihood', 'Prior', 'Posterior'), lty=1:3)

# Add colors

mu<-5
sigmasq<-2
tausq<-4
y<-4

theta<-seq(-5, 15, 0.1)
 
likeli<-1/sqrt(2*pi*sigmasq)*exp(-1/2/sigmasq*(y-theta)^2)
 
plot(theta, likeli, lty=1, xlab='theta', ylab='Density Curves', ylim=c(0,0.4), main='Sketches for Likelihood, Prior, and Posterior', col="red")

prior<-1/sqrt(2*pi*tausq)*exp(-1/2/tausq*(theta-mu)^2)

#lines(theta, prior, type='l', lwd=1, pch=1)
lines(theta, prior, lty=2, col="blue")

B<-sigmasq/(sigmasq+tausq)

post<-1/sqrt(2*pi*((1-B)*sigmasq))*exp(-1/2/((1-B)*sigmasq)*(theta-(B*mu+(1-B)*y))^2) 

lines(theta, post, type='b', lwd=4, pch=2)

lines(theta, post, lty=3, col="green")

#legend(-5, 0.30, cex=0.8, c('Likelihood', 'Prior', 'Posterior'), lty=1:3)

legend(-5, 0.30, cex=0.5, c('Likelihood', 'Prior', 'Posterior'), col=c("red","blue", "green"), lty=1:3)
