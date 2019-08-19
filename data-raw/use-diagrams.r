
library(rutilsMH)
library(diagrams)

plotprep(height=5)
canvas()


canvas()
circle(origx=35,origy=70,radius=30,lwd=2)
circle(origx=65,origy=60,radius=30,lwd=2,col=1)
circle(origx=45,origy=40,radius=30,lwd=2,col=1)
text(35,105,"Previous Year - p",cex=1.0,pos=1)
text(70,95,"Current Year - c",cex=1.0,pos=1)
text(45,10,"Next Year - n",cex=1.0,pos=1)


# ZXspectrum graphics --------------------------------------


plotprep(height=5)
canvas()
circle(50,50,40)
N <-  12
A <-  0
for (i in 1:N) { # i = 1
  adif <- (2 * pi)/N
  x <- 40 * cos(A); y <- 40 * sin(A)
}


xorig <- 0; yorig <- 0
xpen <- 0;  ypen <- 0
nxpix <- 100; nypix=100
xyscale <- 1.0; yscale <- 1.0

xval <- function(x) round(((xorig + x) * xyscale + 0.5))
yval <- function(y) round(((yorig + y) * yscale + 0.5))

setorigin <- function(xori,yori,xmov,ymov) {
  xorig <- xori + xmov
  yorig <- yori + ymov
  xpen <- xval(0)
  ypen <- yval(0)
  return(xorig,yorig,xpen,ypen)
}

moveto <- function(xpt,ypt) {
  
  
}

orig <- c(0,0)
loc <- c(0,0)
rge <- c(100,100)
xscale <- 1.0; yscale <- 1.0

#' @title setorig resets the origin for a graphic
#' 
#' @description setorig adds two two digit vectors
#'     together. This can be used to reset the origin in a
#'     graphic to simplify subsequent plotting.
#'
#' @param pt new origin for all subsequent points. It must
#'     be a vector of two values
#' @param ori original origin to be changed  default=c(0,0)
#' 
#' @return a vector of two values  c(xvalue, yvalue) 
#' @export
#'
#' @examples
#' \dontrun{
#'   orig=c(0,0)
#'   neworig=c(50,50)
#'   orig <- setorig(pt=neworig,ori=orig)
#' }
setorig <- function(pt,ori=c(0,0)) {
  return(ori + pt) 
}

startpt <- function(pt) {
  xval <- round(((orig[1] + pt[1]) * xscale + 0.5))
  yval <- round(((orig[2] + pt[2]) * yscale + 0.5))
  loc <- c(xval,yval)
  return(loc)
}



lineto <- function(pt1,pt2,lwd=2,col=1) {
   lines(pt1,pt2,lwd=lwd,col=col)
}



#' @title getpt selects pairs of points along a sequence
#' 
#' @description getpt is used when drawing a graphic that
#'     is decribed by sequences of x values and y values.
#'     It is necessary to draw them sequentially and this
#'     is faciliated by getpt.
#'
#' @param X a sequence of points to be drawn 
#' @param index 
#'
#' @return
#' @export
#'
#' @examples
getpt <- function(X,index) {
  xval <- c(X[index],X[index+1])
  if (index == length(X)) xval <- c(X[index],X[1])
  return(xval)
}

#' @title drawcircle literally draws an approximate circle
#' 
#' @description drawcircle draws a circle of given radius with
#'     a given centre, which becomes the new origin for 
#'     subsequent plotting. It assmes a canvas has already 
#'     been generated ready for plotting. The circle is only
#'     approximate and is generating by drawing lines between
#'     a set of points defined around a given circle
#'
#' @param radius the radius of the circle in the same units as
#'     used to define the canvas on which to draw the circle 
#' @param neworig the location of the centre of the circle
#'     relative to an origin of c(0,0)
#' @param lwd the width of the lines drawn. default=1
#' @param col the colour of the fill drawn. default=1
#' @param N the number of lines drawn default=64, which gives
#'     a very reasonable circle when the axes are 100 units.
#'
#' @return nothing but it does plot a circle.
#' @export
#'
#' @examples
#' \dontrun{
#'   canvas(xstart=0,xfinish=100,ystart=0,yfinish=100)
#'   drawcircle(radius=35,neworig=c(40,40))
#' }
drawcircle <- function(radius,neworig=c(50,50),
                       lwd=1,col=1,N=64) {
  orig <- setorig(neworig)
  alpha <- (2*pi/N) * 1:N
  x <- (cos(alpha) * radius) + orig[1]
  y <- (sin(alpha) * radius) + orig[2]
 # polygon(x,y,lwd=lwd,border=col)
  for (i in 1:N) 
    lineto(getpt(x,i),getpt(y,i),lwd=lwd,col=col)
}



orig <- c(0,0)
oorig <- orig
loc <- c(0,0)
rge <- c(100,100)
xscale <- 1.0; yscale <- 1.0
orig <- setorig(c(50,50))
N <- 15
alpha <- (2*pi/N) * 1:N
x <- (cos(alpha) * orig[1]*0.9) + orig[1]
y <- (sin(alpha) * orig[2]*0.9) + orig[2]

plotprep(height=5,newdev=FALSE)
canvas()
for (i in 1:(N-1)) {
  for (j in (i+1):N) {
    lineto(c(x[i],x[j]),c(y[i],y[j]),col=2,lwd=1)
  }
}
drawcircle(radius=4,col=4)
drawcircle(radius=12.5,col=3)
drawcircle(radius=20.5,col=2)
drawcircle(radius=45,col=1,N=64,neworig = c(50,50))





plotprep(height=5)
canvas()
drawcircle(radius=4)






points(1:12,y,pch=16,col=2)

round(cbind(x,y),5)

# Game of Life -------------------------------------------
library(rutilsMH)
library(diagrams)

plotprep(height=5)
canvas()



#  useglidergun --------------------------------------
num <- 100
arena <- glidergun(N=num)

#plotprep(height=5,newdev=FALSE)
canvas()
plotstep(num,arena)
for (iter in 1:300) {
  arena <- dolife(num, arena, dostep)
 # plotprep(height=5,newdev=FALSE)
  canvas()
  plotstep(num,arena,cex=1.5,sleep=0.1)
}

# solid block -------------------------------------
num <- 21
pat=matrix(1,nrow=7,ncol=7)
arena <- makeblock(N=num,pattern=pat)
canvasN(101)
plotstep(num,arena,cex=2.0)

for (iter in 1:20) { # iter=1
  arena <- dolife(num, arena, dostep)
  canvasN(25)
  plotstep(num,arena,cex=2.0,sleep=0.2)
} 


# random block ------------------------------------


begin <- gettime()
seed <- MQMF::getseed()
seed <- mixuprand(seed)
set.seed(seed)
tmp <- random(21)
num <- 100
arena <- matrix(0,nrow=num,ncol=num)
arena <- placematrix(arena,tmp)
canvasN(101)
plotstep(num,arena,cex=1.0)

count <- numeric(2000)
count[1] <- sum(arena)
for (iter in 2:2000) {
  arena <- dolife(num, arena, dostep2)
  canvasN(101)
  plotstep(num,arena,cex=1.0,sleep=0.04)
  xc <- sum(arena)
  count[iter] <- xc
  if (xc == 0) break
  if (iter > 5) {
    if (all(count[(iter-4):iter] == count[iter])) break
  }
} 
cat(seed,iter,gettime() - begin,"\n")

plotcount(count)


extract <- function(frame,left,top,size) { 
  if (((left+size) > ncol(frame)) | (top > nrow(frame))) {
    stop(cat("location in extract outside size of frame \n"))
  }
  ans <- frame[left:(left+size),(top-size):top]
  return(ans)
}

out <- extract(arena,50,58,5)
nout <- nrow(out)
canvasN(7)
plotstep(nout,out,cex=5.0,sleep=0.2)
abline(v=1.5:11.5,col="grey")
abline(h=1.5:11.5,col="grey")


# repeat random runs -------------------------------------

numtrial <- 10
answer <- vector("list",numtrial)
ansname <- numeric(numtrial)
for (trial in 1:numtrial) { # trial=1
  begin <- gettime()
  seed <- MQMF::getseed()
  seed <- mixuprand(seed)
  set.seed(seed)
  ansname[trial] <- seed
  tmp <- random(10)
  num <- 100
  arena <- matrix(0,nrow=num,ncol=num)
  arena <- placematrix(arena,tmp)
  count <- numeric(1000)
  count[1] <- sum(arena)
  for (iter in 2:1000) {
    arena <- dolife(num, arena)
    xc <- sum(arena)
    count[iter] <- xc
    if (xc == 0) break
    if (iter > 5) {
      if (all(count[(iter-4):iter] == count[iter])) break
    }
  }
  result <- list(count=count,arena=arena,seed=seed,iter=iter)
  answer[[trial]] <- result
}
print(gettime()- begin)

plotprep(width=6,height=4)
count <- answer[[1]]$count
num <- length(count)
plot(1:num,count,type="l",lwd=1,ylim=c(0,400))
for (i in 2:numtrial) lines(1:num,answer[[i]]$count)


plotcount(count)
maxy <- 3000
plotprep(width=6,height=4)
count <- answer[[1]]$count
num <- length(count)
plot(count[1:(num-1)],count[2:num],type="p",lwd=1,ylim=c(0,maxy),
     xlim=c(0,maxy))
for (i in 2:numtrial) {
  count <- answer[[i]]$count
  points(count[1:(num-1)],count[2:num])
}
lines(c(0,maxy),c(0,maxy),lwd=2,col=2)

for (i in 1:10) print(answer[[i]]$count[1000])

answer[[10]]$count[980:1000]

answer[[10]]$seed
# 898662

num <- 100
canvasN(101)
arena <- answer[[10]]$arena
plotstep(num,arena,cex=1.0)

for (iter in 1:30) {
  arena <- dolife(num, arena)
  canvasN(101)
  plotstep(num,arena,cex=1.0,sleep=0.5)
}
# improve random seed ---------------------------------


seed <- MQMF::getseed()
print(seed)

print(seed)


# Colour Field ------------------------------------------------------ 

library(rutilsMH)
library(diagrams)

#plotprep(height=5)
canvas()
h <- 50
f <- 100
x <- 1:100
incex <- 2
cols <- numeric(100)
for (i in seq(1,h,0.5)) {
  cols <- rgb(i/h,(h-i)/h,0,1)
  points(c(2:(h+1))+50,rep(i+50,h),pch=16,cex=incex,
         col=cols)
}
for (i in seq(1,h,0.5)) {
  cols <- rgb(0,i/h,(h-i)/h,1)
  points(c(2:(h+1))+50,rep(i,h),pch=16,cex=incex,
         col=cols)
}  
for (i in seq(1,h,0.5)) {
  cols <- rgb(i/h,(h-i)/h,0,1)
  points(c(2:(h+1)),rep(i+50,h),pch=16,cex=incex,
         col=cols)
}
for (i in seq(1,h,0.5)) {
  cols <- rgb(0,i/h,(h-i)/h,1)
  points(c(2:(h+1)),rep(i,h),pch=16,cex=incex,
         col=cols)
}  


canvas()
x <- c(1,90,45,1)
y <- c(1,1,77.9423,1)
polygon(x,y,col=0,border=1)
d60 <- pi/3

bx <- seq(5,45,5)
nb <- length(bx)
by <- rep(1,nb)
lx <- bx
ly <- c(bx*tan(d60))
rx <- sort(seq(45,85,5),decreasing = TRUE)
ry <- ly
midx <- rowMeans(cbind(bx,lx,rx))
midy <- rowMeans(cbind(by,ly,ry))
#for (i in 1:nb) lines(c(bx[i],lx[i]),c(by[i],ly[i]),lwd=1,col=3)
points(bx,by,pch=20,cex=1)
points(lx,ly,pch=20,cex=1)
points(rx,ry,pch=20,cex=1)

points(midx,midy,pch=20,cex=1,col=2)
# Y2 <- c(B[B > 45] *sin(d60))
# for (i in (ny1+1):nb) lines(c(B[i],B[i]),c(1,Y2[i]),lwd=1,col=4)
abline(h=seq(10,80,10),col="grey")


geth <- function(adj,opp) {
  hyp <- sqrt(adj^2 + opp^2)
  return(hyp)
}

geto <- function(adj, hyp) {
  opp <- sqrt(hyp^2 - adj^2)
  return(opp)
}

geth(45,77.94229)

geto(45,90)



makearena <- function(N=100) {
  x <- matrix(0,nrow=N,ncol=N,dimnames=list(1:N,1:N))
  return(x)
}

  set.seed(56173) # pick any random seed
  num <- 100
  arena <- canvasN(num)
  mat <- symmetric(5)
  arena <- placematrix(arena,mat)
  plotstep(arena,cex=1.0)
  iter <- 200
  count <- numeric(iter)
  for (i in 1:iter) {
    arena <- dolife(num, arena, dostep)
    canvasN(num)
    plotstep(arena,cex=1.0,sleep=0.05)
    count[i] <- sum(arena)
  }

  plotcount(count)









