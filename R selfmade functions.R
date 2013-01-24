# Utility functions

####################################################################################
# Change factor variable to numeric variable
factor.to.numeric <- function(var) { as.numeric(levels(var))[var] }

####################################################################################
# Standardize to 0-1 range
scale01 <- function(var) { 
  (var-min(var, na.rm=TRUE))/
  (max(var, na.rm=TRUE)-min(var, na.rm=TRUE))
  }

####################################################################################
# Transform var so that miny = min.out and maxy = max.out. miny defaults to the minimum transformed value and maxy defaluts to miny defaults to the maximum transformed value. min.out-max.out defaults to 0 - 1.

scale.range <- function(var, miny=min(var, na.rm=TRUE), maxy=max(var, na.rm=TRUE), min.out=0, max.out=1){
  (var-miny)/(maxy-miny)*(max.out-min.out) + min.out
  }

#scale.range(5:15)
#scale.range(5:15, 3, 15)
#scale.range(5:15, min.out=4, max.out=5)

####################################################################################
# Add shaded polygon made from x and two y vars (top and bottom)
polygon.lines <- function(x, y.lo, y.hi, col="grey", ...) { 
  polygon( c(x,rev(x)), c(y.lo, rev(y.hi)), col=col, ...)
  }

####################################################################################
# Adding line to scatter plot, with scale on right side
lines.r <- function(x, y2, y2lim, axis.dist=0, axis.title="", title.dist=2.5, ...){
  y2ran <- range(y2, na.rm=T)
  y2new <- (y2 - y2ran[1])*diff(y2lim)/diff(y2ran) + y2lim[1]
  #range(y2new, na.rm=T)
  lines(x, y2new, ...)
  labpos <- (pretty(y2) - y2ran[1])*diff(y2lim)/diff(y2ran) + y2lim[1]
  axis(4, at=labpos, labels=pretty(y2), line=axis.dist)
  mtext(axis.title, 4, at=(min(labpos)+max(labpos))/2,  line=title.dist)
  }


####################################################################################
# Adding "winter shading" in scatter plots (shading marking the winter months)
season.rect <- function(from="10/1", to="04/30", yrange=c(-20,20), col="grey") {
for (y in 1930:2012) {
   t1 <- paste(from, "/", y,   sep="")
   t2 <- paste(to, "/", y+1, sep="")
   rect(chron(t1), yrange[1], chron(t2), yrange[2], col=col, border=NA)
   }
 }
 
####################################################################################
# Make empty plot with text in it (for headings in a "layout" plot)
textplot <- function(text, cex=1.5, ...){
  opa <- par(mar=rep(0,4))
  plot(1,1,type="n",axes=F,bty="n")
  text(1,1,text, cex=cex, ...)
  par(opa)
  }

#  Example
#x11(8,12)
#layout(matrix(c(1,1,2,3,4,4,5,6,7,7,8,9),6,2,byrow=T), heights=c(1,6,1,6,1,6))
#par(mar=c(5,4,1,1))
#textplot("Temperature")
#plot(m1)
#textplot("Salinity")
#plot(m2)
#textplot("O2 equivalents")
#plot(m3)

####################################################################################
# Shorter GAM output
summary2 <- function(gam) {
  print( round(summary(gam)$s.table, 3) )
  cat("R-sq = ", round(summary(gam)$dev.expl, 3), "\n")
  }


####################################################################################
# Not selfmade but from http://wiki.stdout.org/rcookbook/Graphs/Multiple%20graphs%20on%20one%20page%20%28ggplot2%29/

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#################################################################
# lsos: ls() with size
# By Dirk Eddelbuettel and Tony Breyal
# From:
# http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session

# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
                           capture.output(print(object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}

# shorthand
lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

lsos()
