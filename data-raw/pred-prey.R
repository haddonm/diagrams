


library(rutilsMH)
library(diagrams)
library(microbenchmark)


nyrs <- 50
Nt <- numeric(nyrs)
Pt <- numeric(nyrs)


Nt[1] = 1000; r = 1.75; K = 1000; alpha1 = 0.0007
Pt[1] = 10; alpha2 = 0.075; K2 = 50

for (i in 2:nyrs) {
  Nt[i] <- Nt[i-1] + (r * Nt[i-1])*(1 - Nt[i-1]/K) - (alpha1 * Pt[i-1] * Nt[i-1])
  Pt[i] <- Pt[i-1] + (alpha2 * (alpha1 * Pt[i-1] * Nt[i-1]))* Pt[i-1] * (1 - Pt[i-1]/K2) 
  if ((Nt[i] <= 0) | (Pt[i] <= 0)) break
}





plotprep(width=6,height=4,newdev=FALSE)
plotxyy(1:nyrs,Nt,Pt,xlab="Number of Years",ylab1="Number of Prey",
        ylab2="Number of Predators")




plot(1:nyrs,Nt,type="l",lwd=2,col=1,ylim=c(0,1010),yaxs="i",
     panel.first=grid())
lines(1:nyrs,5*Pt,lwd=2,col=2)


x <- 1:20
yval1 <- rnorm(20,mean=5,sd=1)
yval2 <- rnorm(20,mean=10,sd=1)
plotxyy(x,yval1,yval2,ylab1="Mean=5",ylab2="Mean=10")




















