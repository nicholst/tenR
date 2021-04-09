#
#  Demonstration of different options for dealing with overplotting
#

addTrans <- function(color,trans) # Add transparency to any color specification
{
  # Drawn from code by Sacha Epskamp
  # https://stackoverflow.com/questions/12995683/any-way-to-make-plot-points-in-scatterplot-more-transparent-in-r

  trans=floor(trans*255)  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

N=10000
X=rnorm(N);
Y=rnorm(N);

# Base R, no transparency
dev.new()
plot(X,Y,pch=16,col="blue")  # symbol 19 has outer rim; symbol 20 is small

# Base R, transparency
dev.new()
plot(X,Y,pch=16,col=addTrans("blue",0.2))

# GGplot transparency
dev.new()
library(ggplot2)
print(ggplot(data = data.frame(X,Y), mapping = aes(x = X, y = Y)) +
    geom_point(shape = 16,color="blue",alpha = 0.2))

# GGplot density plot
dev.new()
library(ggpointdensity)
print(ggplot(data = data.frame(X,Y), mapping = aes(x = X, y = Y)) +
    geom_pointdensity(adjust = .1))
