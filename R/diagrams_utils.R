

#' @title canvas sets up a plotting area ready for the flowchart
#'
#' @description canvas sets up a plotting areas ready for a flowchart
#'     made up of shapes, circles, polygons, rectangles, text, and arrows
#'
#' @param xstart x-origin value defaults = 0
#' @param xfinish maximum of x axis defaults = 100
#' @param ystart y-origin value default = 0
#' @param yfinish y-axis maximum default = 100
#'
#' @return nothing but plots an empty graph ready for polygons and text
#' @export
#'
#' @examples
#' \dontrun{
#'   canvas(ystart=50,yfinish=93.5)
#'   polygon(makevx(2,27),makevy(90,6),col=0,lwd=1,border=1)
#' }
canvas <- function(xstart=0,xfinish=100,ystart=0,yfinish=100) {
  par(mfrow=c(1,1),mai=c(0.1,0.1,0.1,0.1),oma=c(0.0,0,0.0,0.0))
  par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
  plot(seq(xstart,xfinish,length=101),seq(ystart,yfinish,length=101),
       type="n",xaxt="n",yaxt="n",xlab="",ylab="", bty="n")
  N <- xfinish - xstart
  arena <- matrix(0,nrow=N,ncol=N,dimnames=list(1:N,1:N))
  return(arena)
} # end of canvas

#' @title circle draws a circle with a given origin and radius
#' 
#' @description circle provides the means of drawing a circle of a given
#'     radius and origin within a diagram ready for the addition of text.
#'
#' @param origx the final x origin
#' @param origy the final y origin
#' @param radius the radius of the circle
#' @param col the col of the circle
#' @param lwd the line width of the circle
#'
#' @return the matrix of x and y values invisibly  
#' @export
#'
#' @examples
#' \dontrun{
#'   canvas()
#'   circle(origx=35,origy=70,radius=30,lwd=2,col=1)
#'   circle(origx=65,origy=60,radius=30,lwd=2,col=2)
#'   circle(origx=45,origy=40,radius=30,lwd=2,col=4)
#' }
circle <- function(origx=50,origy=50,radius=10,col=1,lwd=1) {
  ans <- pol2cart(angle=seq(0,360,0.1),dist=radius,xorig=origx,yorig=origy)
  lines(ans[,"x"],ans[,"y"],lwd=lwd,col=col)
  return(invisible(ans))
} # end of circle

#' @title getangle converts cartesian coordinates into the polar angle
#' 
#' @description getangle as a step in converting cartesian coordinates into
#'     polar coordinates this calculates the angle, in degrees, from x y
#'     values
#'
#' @param x either a vector of two values of a matrix of pairs of values
#'
#' @return a single angle of vector of angles
#'
#' @examples
#' \dontrun{
#'   getangle(c(3,3))  # should be 45
#'   dat <- matrix(c(3,4,5,7),nrow=2,ncol=2,byrow=TRUE)
#'   print(dat)
#'   getangle(dat)     # should be 36.8699 twice.
#' }
getangle <- function(x){
  if (is.vector(x)) angle <- 180 * (atan2(x[1],x[2])) / pi
  if (is.matrix(x)) angle <- 180 * (atan2(x[,1],x[,2])) / pi
  return(angle=angle)
}

#' @title makevx make an x values vector
#'
#' @description makevx takes the left x value of a rectangle and the
#'     increment rightwards that defines a vector describing the four
#'     vertices of the rectangle topleft, topright, bottomright,
#'     bottomleft, topleft. when matched with makevy generates the
#'     descriptor for a complete rectangle.
#'
#' @param init x-value for the left-hand edge of a rectangle
#' @param inc the x-increment added to init to define the right-hand edge
#'
#' @return a vector of y-values
#' @export
#'
#' @examples
#' \dontrun{
#'  plot(0:100,seq(58,93.5,length=101),type="n",xaxt="n",yaxt="n",
#'  xlab="",ylab="", bty="n")
#'  polygon(makevx(2,27),makevy(90,6),col=0,lwd=1,border=1)
#' }
makevx <- function(init,inc) {
  return(c(init,init+inc,init+inc,init,init))
}

#' @title makevy make a y values vector
#'
#' @description makevy takes the top y value of a rectangle and the
#'     vertical increment downwards and defines a vector describing the four
#'     vertices of the rectangle topleft, topright, bottomright,
#'     bottomleft. topleft, when matched with makevx generates the
#'     descriptor for a complete rectangle.
#'
#' @param init y-value for the top edge of a rectangle
#' @param inc the y-increment subtracted from init to define the lower edge
#'
#' @return a vector of y-values
#' @export
#'
#' @examples
#' \dontrun{
#'  canvas(ystart=50,yfinish=93.5)
#'  polygon(makevx(2,27),makevy(90,6),col=0,lwd=1,border=1)
#' }
makevy <- function(init,inc) {
  return(c(init,init,init-inc,init-inc,init))
}

#' @title makerect draws a rectangle once a plot is available
#'
#' @description makerect draws a rectangle after canvas has been called
#'
#' @param left defines lefthand edge of rectangle
#' @param xinc left + xinc defines right-hand edge or rectangle
#' @param top defines top edge of rectangle
#' @param yinc top - yincdefines bottom edge of rectangle
#' @param linecol colour of line. default="grey"
#' @param lwd the width of the line, default=1
#'
#' @return a vector denoting the center (x,y) of the rectangle
#' @export
#'
#' @examples
#' \dontrun{
#'    canvas(ystart=50,yfinish=93.5)
#'    makerect(left=2,xinc=27,top=90,yinc=6)
#' }
makerect <- function(left,xinc,top,yinc,linecol="grey",lwd=1) {
  polygon(makevx(left,xinc),makevy(top,yinc),col=0,
          lwd=lwd,border=linecol)
  centerx <- (left * 2 + xinc)/2
  centery <- (top * 2 - yinc)/2
  return(invisible(c(centerx,centery)))
}

#' @title plotoblong generates an oblong from x0,x1,y0,y1
#' 
#' @description plotoblong generates an oblong from x0,x1,y0,y1
#'
#' @param x0 x-axis left
#' @param x1 x-axis right
#' @param y0 yaxis bottom
#' @param y1 yaxis top
#' @param border colour of the border, default=black=1
#' @param col colour of fill, default = 0 =  empty
#' @param lwd width of the line,default=1
#'
#' @return nothing but it plots a polygon
#' @export
#'
#' @examples
#' \dontrun{
#'   canvas()
#'   plotoblong(1,50,1,50,lwd=3,linecol=4)
#' }
plotoblong <- function(x0,x1,y0,y1,border=1,col=0,lwd=1) {
  x <- c(x0,x0,x1,x1,x0); y <- c(y0,y1,y1,y0,y0)
  polygon(x,y,lwd=lwd,border=border,col=col)
}

#' @title pol2cart polar to cartesian coordinates
#' 
#' @description pol2cart translate polar coordinates of angles (as degrees)
#'     and a distance = radius, into cartesian coordinates of x and y. The
#'     option of using arbitrary origin coordinates is included
#'
#' @param angle the angle in degrees, either a single number of a vector
#' @param dist the length of the line or radius, a single number
#' @param xorig the final xorigin
#' @param yorig the final yorigin
#'
#' @return a matrix of 1 or more rows depending on length of angle
#' @export
#'
#' @examples
#' \dontrun{
#'   ans <- pol2cart(angle=seq(0,360,15),dist=20,xorig=30,yorig=30)
#'   print(ans)
#' }
pol2cart <- function(angle,dist,xorig=0,yorig=0){
  #  angle=45:50; dist=10; xorig=0; yorig=0
  numang <- length(angle)
  coord <- matrix(0,nrow=numang,ncol=2,dimnames=list(1:numang,c("x","y")))
  angler <- angle*pi/180
  for (i in 1:numang) {
    coord[i,] <- c(xorig + dist * sin(angler[i]),
                   yorig + dist * cos(angler[i]))  
  }
  return(coord) #output the new x and y coordinates
} # end of pol2cart

#' @title pythag calculates Pythagorus' theorum on a vector of two values
#' 
#' @description pythag Pythagorus' theorum states that the length of the
#'     hypotheneuse between two lines at right angels to each other (that
#'     is in cartesian coordinates) is the sqrt of the sum of their squares.
#'
#' @param x a vector of two numbers or a matrix of pairs of numbers
#'
#' @return a single number or a vector depending on input
#' @export
#'
#' @examples
#' \dontrun{
#'  pythag(c(3,4))  # should be 5
#'  dat <- matrix(c(3,4,5,7),nrow=2,ncol=2,byrow=TRUE)
#'  print(dat)
#'  pythag(dat)     # should be 5 and 10
#' }
pythag <- function(x) {  # x = ans
  if (is.vector(x)) ans <- sqrt((x[1]^2 + x[2]^2))
  if (is.matrix(x)) ans <- sqrt((x[,1]^2 + x[,2]^2))
  return(ans) 
}



