
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


glidergun <- function(N=50) {
  # A Godper's glider gun
  if (N < 50) N = 50
  x <- matrix(0,nrow=N,ncol=N,dimnames=list(1:N,1:N))
  x[2:3,94:95] <- 1
  x[36:37,96:97] <- 1
  x[22:23,95:97] <- 1
  x[c(24,26),c(94,98)] <- 1
  x[c(26,26),c(93,99)] <- 1
  x[c(12,12,12),c(93:95)] <- 1  
  x[c(13,13),c(92,96)] <- 1
  x[c(14,14),c(91,97)] <- 1
  x[c(15,15),c(91,97)] <- 1
  x[c(16,19),c(94,94)] <- 1
  x[c(17,17),c(92,96)] <- 1
  x[c(18,18,18),c(93:95)] <- 1
  return(x)
} # end of glider gun

#  
fourblink <- function(N=21) {
  x <- matrix(0,nrow=N,ncol=N,dimnames=list(1:N,1:N))
  x[rep(11,6),c(6,7,8,14,15,16)] <- 1
  x[c(6,7,8,14,15,16),rep(11,6)] <- 1
  return(x)
}

pentadec <- function(N=18) {
  x <- matrix(0,nrow=N,ncol=N,dimnames=list(1:N,1:N))
  x[c(8,9,10),c(4,4,4)] <- 1
  x[c(7,11),c(5,5)] <- 1
  x[c(7,11),c(6,6)] <- 1  
  x[c(8,9,10),c(7,7,7)] <- 1
  x[c(8,9,10),c(12,12,12)] <- 1
  x[c(7,11),c(13,13)] <- 1
  x[c(7,11),c(14,14)] <- 1  
  x[c(8,9,10),c(15,15,15)] <- 1
  return(x)
}

symmetric <- function(N=5) {
  # develops for about 180 steps until stability
  x <- matrix(0,nrow=N,ncol=N)
  x[c(1,2,4,5),c(4,4,4,4)] <- 1
  x[c(1,5),c(3,3)] <- 1
  x[c(2,3,4),c(2,2,2)] <- 1
  return(x)
}

random <- function(N=10) {
  dat <- rbinom((N * N),1,0.5)
  frame <- matrix(dat,nrow=N,ncol=N)
  return(frame)
}

makeblock <- function(N=21,
                      pattern=matrix(1,nrow=6,ncol=6)) {
  frame <- matrix(0,nrow=N,ncol=N)
  dim1 <- nrow(pattern)
  left <- trunc(N/2) - trunc(dim1/2)
  frame[left:(left+dim1-1),left:(left+dim1-1)] <- pattern
  return(frame)
}


canvasN <- function(N=100,pts=TRUE) {
  par(mfrow=c(1,1),mai=c(0.1,0.1,0.1,0.1),oma=c(0.0,0,0.0,0.0))
  par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
  plot(seq(0,N,length=101),seq(0,N,length=101),
       type="n",xaxt="n",yaxt="n",xlab="",ylab="", bty="n")
  makerect(1,N,N,(N))
  if (pts) for (i in 1:100) points(c(2:101),rep(i,100),pch=16,cex=0.2)
}



plotstep <- function(N,frame,col=1,cex=1.0,sleep=0.2) {
  for (i in N:1) {
   xs <- which(frame[i,] > 0)
   npts <- length(xs)
   if (npts > 0) points(rep(i,npts),xs,pch=16,
                        cex=cex,col=col)
  }
  Sys.sleep(sleep)
} # end of plotstep


plotcount <- function(cnt) {
  N <- countgtzero(cnt)
  plotprep(width=6,height=4)
  plot1(1:N,cnt[1:N],xlabel="trial","Population Size")
}


dostep <- function(xj,yi,frame) {
  ans <- 0  # assume answer is the cell dies
  view <- sum(frame[(xj-1):(xj+1),(yi+1):(yi-1)]) - frame[xj,yi]
  if (view > 0) {
    if (((frame[xj,yi] == 1) & ((view == 2) | (view == 3))) |
        ((frame[xj,yi] == 0) & (view == 3))) ans <- 1
  }
  return(ans)
}

dostep2 <- function(xj,yi,frame) {
  ans <- 0  # assume answer is the cell dies
  view <- sum(frame[(xj-1):(xj+1),(yi+1):(yi-1)]) - frame[xj,yi]
  if (view > 0) {
    if (((frame[xj,yi] == 1) & ((view == 2) | (view == 3))) |
        ((frame[xj,yi] == 0) & (view == 3))) {
      ans <- 1
    } else {
      if ((frame[(xj-1),(yi-1)] == 1) & (runif(1) > 0.999))  ans <- 1
      if ((frame[(xj+1),(yi+1)] == 0) & (runif(1) > 0.999))  ans <- 1
    }
  }
  return(ans)
}

dolife <- function(N,frame, infun) {
  step <- frame
  nm1 <- N - 1
  for (i in nm1:2) { # y axis
    for (j in 2:nm1) { # x axis
      step[j,i] <- infun(j,i,frame)
    }
  }
  return(step)
}

placematrix <- function(frame,mat) {
  nmat <- nrow(mat)
  nframe <- nrow(frame)
  if (nmat > nframe)
    stop("input object too big for arena  \n")
  halfframe <- trunc(nframe/2)
  halfmat <- trunc(nmat/2)
  leftbot <- halfframe - halfmat
  frame[leftbot:(leftbot+nmat-1),
        leftbot:(leftbot+nmat-1)] <- mat
  return(frame)
}

reorder <- function(seed) {
  seed <- as.character(seed)
  num <- nchar(seed)
  pick <- sample(1:num)
  ans <- NULL
  for (i in 1:num) ans <- paste0(ans,substr(seed,pick[i],pick[i]))
  return(as.numeric(ans))  
}


#  useglidergun --------------------------------------
num <- 100
arena <- glidergun(N=num)

#plotprep(height=5,newdev=FALSE)
canvas()
plotstep(num,arena)
for (iter in 1:300) {
  arena <- dolife(num, arena)
 # plotprep(height=5,newdev=FALSE)
  canvas()
  plotstep(num,arena,cex=1.5,sleep=0.1)
}

# solid block -------------------------------------
num <- 21
pat=matrix(1,nrow=7,ncol=7)
arena <- makeblock(N=num,pattern=pat)
canvasN(23)
plotstep(num,arena,cex=2.0)

for (iter in 1:20) {
  arena <- dolife(num, arena)
  canvasN(25)
  plotstep(num,arena,cex=2.0,sleep=0.2)
} 


# random block ------------------------------------


begin <- gettime()
seed <- MQMF::getseed()
seed <- reorder(seed)
set.seed(seed)
tmp <- random(17)
num <- 100
arena <- matrix(0,nrow=num,ncol=num)
arena <- placematrix(arena,tmp)
canvasN(101)
plotstep(num,arena,cex=2.0)

count <- numeric(2000)
count[1] <- sum(arena)
for (iter in 2:2000) {
  arena <- dolife(num, arena, dostep2)
  canvasN(101)
  plotstep(num,arena,cex=2.0,sleep=0.05)
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
  seed <- reorder(seed)
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




















