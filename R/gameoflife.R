
#' @title canvasN sets up a plotting area ready for subsequent plots
#' 
#' @description canvasN sets up an empty square plotting area of 
#'     dimension N. It defines an arena ont which to place plots and
#'     related graphics.
#'
#' @param N the scale of the plotting area, default=100
#' @param pts a true or false term that determines whether to plot fine
#'     points on every available integer place on the canvas.
#'
#' @return produces an empty plot and returns an empty square matrix
#'     of order N into which to run the dynamics; which will be plotted 
#'     inside the empty plot.
#' @export
#'
#' @examples
#' \dontrun{
#'   canvasN()
#'   plot(c(20:80),c(20:80),type="p")
#' }
canvasN <- function(N=100,pts=TRUE) {
  par(mfrow=c(1,1),mai=c(0.1,0.1,0.1,0.1),oma=c(0.0,0,0.0,0.0))
  par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
  plot(seq(0,(N+1),length=102),seq(0,(N+1),length=102),
       type="n",xaxt="n",yaxt="n",xlab="",ylab="", bty="n")
  arena <- matrix(0,nrow=N,ncol=N,dimnames=list(1:N,1:N))
  if (pts) {
    for (i in 1:100) points(c(2:101),rep(i,100),pch=16,cex=0.2,col="grey")
    makerect(1,N,N,(N))
  }
  return(invisible(arena))
}


#' @title dostep runs a single step for a single point in the GoL
#' 
#' @description dostep conducts the cellular automata algorithm
#'     developed by John Horton Conway called the Game of Life (GoL)
#'     on a single point in the arena defined by the xj, yi, values.
#'     This function is only called from within dolife but is a required
#'     input to dolife. In this way you can write a variant that can
#'     then change the implemented dynamics with the GoL (see dostep2).
#'     See <https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life>
#'
#' @param xj the x-axis position of the point
#' @param yi the y-axis position of the point
#' @param frame the matrix holding the arena of interacting cells 
#'
#' @return does the given point survive or die
#' @export
#'
#' @examples
#' \dontrun{
#'  num <- 100 
#'  arena <- glidergun(N=num)
#'  canvas()
#'  plotstep(num,arena)
#'  for (iter in 1:300) {
#'    arena <- dolife(num, arena, dostep)
#'    canvas()
#'    plotstep(num,arena,cex=1.5,sleep=0.1)
#'  }
#' } 
dostep <- function(xj,yi,frame) {
  ans <- 0  # assume answer is the cell dies
  view <- sum(frame[(xj-1):(xj+1),(yi+1):(yi-1)]) - frame[xj,yi]
  if (view > 0) {
    if (((frame[xj,yi] == 1) & ((view == 2) | (view == 3))) |
        ((frame[xj,yi] == 0) & (view == 3))) ans <- 1
  }
  return(ans)
}

#' @title dostep2 runs a single step for a single point in the GoL
#' 
#' @description dostep2 modifies Conway's GoL cellular automaton's
#'     algorithm by including stochasticity on whether a new constraint
#'     will operate to leading to a point surviving. The single point 
#'     in the arena is defined by the xj, yi, values.
#'     This function is called from within dolife but is a required
#'     input to dolife. By using this function as a template you can 
#'     write your own variant of the survive/die algorithm and thereby
#'     then change the implemented dynamics with the GoL (see dostep).
#'
#' @param xj the x-axis position of the point
#' @param yi the y-axis position of the point
#' @param frame the matrix holding the arena of interacting cells 
#'
#' @return does the given point survive or die
#' @export
#'
#' @examples
#' \dontrun{
#'  num <- 100 
#'  arena <- glidergun(N=num)
#'  canvas()
#'  plotstep(num,arena)
#'  for (iter in 1:300) {
#'    arena <- dolife(num, arena, dostep2)
#'    canvas()
#'    plotstep(num,arena,cex=1.5,sleep=0.1)
#'  }
#' } 
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

#' @title dolife runs the game of life using infun for dynamics
#' 
#' @description dolife is a wrapper function that calls an input
#'     function, default=dostep, which calculates the dynamics of each
#'     individual step for each potentially changing cell. The 
#'     algorithm expects to step sequentially through the 2:(N-1)
#'     columns and (N-1):2 rows imposing the cellular automatas
#'     rules, e=as expressed within infun#'     
#'
#' @param N the size of the arena
#' @param frame the arena matrix
#' @param infun the function defining the automatic rules default
#'     = dostep
#'
#' @return returns a revised copy of the arena
#' @export
#'
#' @examples
#' \dontrun{
#'  num <- 100 
#'  arena <- glidergun(N=num)
#'  canvas()
#'  plotstep(num,arena)
#'  for (iter in 1:300) {
#'    arena <- dolife(num, arena, dostep2)
#'    canvas()
#'    plotstep(num,arena,cex=1.5,sleep=0.1)
#'  }
#' }
dolife <- function(N,frame, infun=dostep) {
  step <- frame
  nm1 <- N - 1
  for (i in nm1:2) { # y axis
    for (j in 2:nm1) { # x axis
      step[j,i] <- infun(j,i,frame)
    }
  }
  return(step)
}

#' @title fourblink defines a matrix containing a simple blinker
#' 
#' @description fourblink defines a matrix that contains what is known
#'     as a blinker. This is an arrangement of points in the arena that
#'     persists and appears to blink at each timestep defined by 
#'     'dolife'. This is a two stage blinker that repeats every two
#'     time steps. repeats of three and more are also possible
#'
#' @param N the size of matrix containing the blinker, default=21
#'
#' @return a matrix that contains an arrangement of points that will 
#'     blink each 'dolife' timestep
#' @export
#'
#' @examples
#' \dontrun{
#'   num <- 100
#'   arena <- makearena(N=num)
#'   canvasN(101)
#'   mat <- fourblink()
#'   arena <- placematrix(arena,mat)
#'   plotstep(num,arena,cex=1.0)
#'   for (i in 1:30) {
#'     arena <- dolife(num, arena, dostep)
#'     canvasN(101)
#'     plotstep(num,arena,cex=1.0,sleep=0.05)
#'   }
#' }
fourblink <- function(N=21) {
  x <- matrix(0,nrow=N,ncol=N,dimnames=list(1:N,1:N))
  x[rep(11,6),c(6,7,8,14,15,16)] <- 1
  x[c(6,7,8,14,15,16),rep(11,6)] <- 1
  return(x)
} # end of fourblink

#' @title glidergun defines a structure that generates repeated gliders
#' 
#' @description glidergun defines a structure that will automatically
#'     generate a stream of gliders forever. It is best implemented 
#'     within a 101 x 101 canvas and needs an arena of at least 
#'     100 x 100. If you define a smaller arena it will default to 100.
#'     This is called a Godper's glider gun, I assume after the person
#'     who discovered it.
#'
#' @param N the size of the arena over which the glidergun will operate
#'     default=100, which is also the minimum dimension needed for an
#'     adequater representation of the glider stream
#'
#' @return it generates an N x N matrix containing the glider gun at
#'     the top 
#' @export
#'
#' @examples
#' \dontrun{
#'  num <- 100 
#'  arena <- glidergun(N=num)
#'  canvas()
#'  plotstep(num,arena)
#'  for (iter in 1:300) {
#'    arena <- dolife(num, arena, dostep)
#'    canvas()
#'    plotstep(num,arena,cex=1.5,sleep=0.1)
#'  }
#' }
glidergun <- function(N=100) {
  if (N < 100) N = 100
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


#' @title makeblock creates a solid matrix of points
#' 
#' @description makeblock creates a solid matrix of points
#'
#' @param r the number of rows of the solid matrix object
#' @param c the number of columns of the solid matrix object
#'
#' @return a matrix of shape r x c
#' @export
#'
#' @examples
#' \dontrun{ 
#'   num <- 100
#'   arena <- canvasN(100)
#'   mat <- makeblock(5,6)
#'   arena <- placematrix(arena,mat)
#'   plotstep(num,arena,cex=1.0)
#' }
makeblock <- function(r=6, c=6) {
  pattern=matrix(1,nrow=r,ncol=c)
  return(pattern)
}


#' @title pentadec a fifteen step blinker
#' 
#' @description pentadec is a blinker with a sequence of 15 steps to
#'     repeat. One could start the sequence at any point and it would 
#'     continue for ever.
#'
#' @return a matrix containing the pentadec object
#' @export
#'
#' @examples
#' \dontrun{
#'   num <- 100
#'   arena <- canvasN(num)
#'   mat <- pentadec()
#'   arena <- placematrix(arena,mat)
#'   plotstep(num,arena,cex=1.0)
#'   for (i in 1:60) {
#'     arena <- dolife(num, arena, dostep)
#'     canvasN(101)
#'     plotstep(num,arena,cex=1.0,sleep=0.5)
#'   }
#' }  
pentadec <- function() {
  N <- 15
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

#' @title placematrix puts a matrix object in the middle of an arena
#' 
#' @description placematrix places a matrix object inside a larger 
#'     matrix. This is used to place a pre-defined object inside an
#'     arena defined by canvasN
#'
#' @param frame the larger arena in which to place the smaller mat
#' @param mat the smaller matrix to be polaced in the middle of the 
#'     larger frame
#'
#' @return the frame object, usually the arena in GoL
#' @export
#'
#' @examples
#' \dontrun{
#'   num <- 100 
#'   arena <- canvasN(num)
#'   mat <- makeblock(9,6)
#'   arena <- placematrix(arena,mat)
#'   plotstep(num,arena,cex=1.0)
#' }
placematrix <- function(frame,mat) {
  rmat <- nrow(mat)
  cmat <- ncol(mat)
  nframe <- nrow(frame)
  if ((rmat > nrow(frame)) | (cmat > ncol(frame)))
    stop("input object too big for arena  \n")
  halfframe <- trunc(nframe/2)
  halfmat <- trunc(cmat/2)
  leftbot <- halfframe - halfmat
  frame[leftbot:(leftbot+cmat-1),
        leftbot:(leftbot+rmat-1)] <- mat
  return(frame)
}

#' @title plotcount plots the population count through time
#' 
#' @description plotcount takes a vector of population counts, 
#'     identifies whether the population goes extinct of not =0, and 
#'     then plots the populaiton size against the number of generations
#'
#' @param cnt a vector of the sum of the arena at each step
#'
#' @return nothiing but it does generate a plot.
#' @export
#'
#' @examples
#' \dontrun{
#'   num <- 100
#'   arena <- canvasN(num)
#'   mat <- pentadec()
#'   arena <- placematrix(arena,mat)
#'   plotstep(num,arena,cex=1.0)
#'   iter <- 100
#'   count <- numeric(iter)
#'   for (i in 1:iter) {
#'     arena <- dolife(num, arena, dostep)
#'     canvasN(num)
#'     plotstep(num,arena,cex=1.0,sleep=0.05)
#'     count[i] <- sum(arena)
#'   }
#'   plotcount(count)
#' }  
plotcount <- function(cnt) {
  N <- countgtzero(cnt)
  par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0)) 
  par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)  
  plot(1:N,cnt[1:N],type="l",xlab="trial",ylab="Population Size",
       panel.first=grid())
}


#' @title plotstep plots the arena with the current state of the GoL
#' 
#' @description plotstep plots a single generation within the GoL. It
#'     takes in the arena and plots it as points. This is designed to 
#'     work within RStudio's plot window
#'
#' @param frame the arena at a given time step or generation
#' @param prevframe the arena in the previous timestep. If is.null
#'     then nothing will happen, otherwise this is printed first 
#'     to overwrite the previous points in white.
#' @param col the colour of the points, default=1=black
#' @param cex the size of the points, default=1.0
#' @param sleep the interval left to enable the canvas to update and 
#'     remain visible
#'
#' @return the input frame to be placed into the following prevframe
#' @export
#'
#' @examples
#' \dontrun{
#'   num <- 100
#'   arena <- canvasN(num)
#'   mat <- pentadec()
#'   arena <- placematrix(arena,mat)
#'   prevstep <- plotstep(num,arena,prevframe=NULL,cex=1.0)
#'   iter <- 100
#'   count <- numeric(iter)
#'   for (i in 1:iter) {
#'     arena <- dolife(num, arena, dostep)
#'     prevstep <- plotstep(num,arena,prevstep,cex=1.0,sleep=0.05)
#'     count[i] <- sum(arena)
#'   }
#' }
plotstep <- function(frame,prevframe,col=1,cex=1.0,sleep=0.2) {
  N <- nrow(frame)
  if (!is.null(prevframe)) {  
    for (i in N:1) {
      xs <- which(prevframe[i,] > 0)
      npts <- length(xs)
      if (npts > 0) points(rep(i,npts),xs,pch=16,
                           cex=cex,col="white")
    } 
  }
  for (i in N:1) {  
    xs2 <- which(frame[i,] > 0)
    npts2 <- length(xs2)
    if (npts2 > 0) points(rep(i,npts2),xs2,pch=16,
                         cex=cex,col=col)    
  }
  Sys.sleep(sleep)
  return(invisible((frame)))
} # end of plotstep


#' @title random generates a square matrix filled randomly
#' 
#' @description random generates a square matrix of order N which is 
#'     filled randomly using binomial random numbers giving a 50:50
#'     chance of any single location bing filled.
#'
#' @param N the size of the square matrix
#'
#' @return a square matrix with randomly filled cells
#' @export
#'
#' @examples
#' \dontrun{
#'  set.seed(56173) # pick any random seed
#'  num <- 100
#'  arena <- canvasN(num)
#'  mat <- random(20)
#'  arena <- placematrix(arena,mat)
#'  plotstep(num,arena,cex=1.0)
#'  iter <- 100
#'  count <- numeric(iter)
#'  for (i in 1:iter) {
#'    arena <- dolife(num, arena, dostep)
#'    canvasN(num)
#'    plotstep(num,arena,cex=1.0,sleep=0.05)
#'    count[i] <- sum(arena)
#'  }
#' 
#'  plotcount(count)
#' }
random <- function(N=10) {
  dat <- rbinom((N * N),1,0.5)
  frame <- matrix(dat,nrow=N,ncol=N)
  return(frame)
}


#' @title mixuprand randomizes the order of digits in a number
#' 
#' @description mixuprand takes a large number and randomizes the order
#'     of its digitsd so as to add a further layer of randomness to a 
#'     random seed. This is useful if using the current time to generate
#'     random seeds, as, obviously, repeated requests can be close in 
#'     time and therefore similar. This should not matter but mixuprand
#'     should help give the appearance of randomness.
#'
#' @param seed a number to be used as a random seed 
#'
#' @return a number the order of whose digits have been randomized.
#' @export
#'
#' @examples
#' \dontrun{
#'   mixuprand(123456)
#'   mixuprand(123456) 
#' }
mixuprand <- function(seed) {
  seed <- as.character(seed)
  num <- nchar(seed)
  pick <- sample(1:num)
  ans <- NULL
  for (i in 1:num) ans <- paste0(ans,substr(seed,pick[i],pick[i]))
  return(as.numeric(ans))  
} # end of mixuprand

#' @title symmetric is a 5x5 matrix that generates a symmetric output
#' 
#' @description symmetric is a 5 x 5 matrix that progress to form a 
#'     complex set of generations that have a mirrored symmetry about 
#'     its  vertical center. It runs for 170 generations and then 
#'     stabilizes into various static and three 2-stage blinkers
#'
#' @return a 5 x 5 matrix of points for use in GoL
#' @export
#'
#' @examples
#' \dontrun{
#'  set.seed(56173) # pick any random seed
#'  num <- 100
#'  arena <- canvasN(num)
#'  mat <- symmetric()
#'  arena <- placematrix(arena,mat)
#'  plotstep(num,arena,cex=1.0)
#'  iter <- 100
#'  count <- numeric(iter)
#'  for (i in 1:iter) {
#'    arena <- dolife(num, arena, dostep)
#'    canvasN(num)
#'    plotstep(num,arena,cex=1.0,sleep=0.05)
#'    count[i] <- sum(arena)
#'  }
#' 
#'  plotcount(count)
#' }
symmetric <- function(N=5) {
  # develops for about 170 steps until stability
  x <- matrix(0,nrow=N,ncol=N)
  x[c(1,2,4,5),c(4,4,4,4)] <- 1
  x[c(1,5),c(3,3)] <- 1
  x[c(2,3,4),c(2,2,2)] <- 1
  return(x)
}

