#' Plotting a raster image with axis and color legend
#'
#' The function is a wrapper for the \code{image()} function, but with a comfortable
#' comtrol of the z-axis and its color legend. The wrapper also supports image
#' resizing (resolution) and png output for better export.
#'
#' @param x x-axis vector corresponding to the z-matrix
#' @param y y-axis vector corresponding to the z-matrix
#' @param z numeric matrix to be plotted
#' @param zlim sets the range of the color coded z-axis
#' @param dim.max defines the dimensions of z, if it needs to be rescaled.
#'                This parameter can improve plotting speed.
#' @param plot.zero.line logical, if a line at \code{x = 0} and \code{y = 0} is
#'                        to be plotted.
#' @param regularGrid logical, if \code{FALSE} then a vector plot is generated,
#'                    which is the slow and standard behaviour of \code{image}.
#'                    If this parameter is \code{TRUE} then a raster image is
#'                    generated, which can be processed much faster, compared to
#'                    the \code{FALSE} option.
#' @param zlab defines the z-label
#' @param z.cex cex value for the z-label. It sets the font size in relation to
#'              the global \code{par()$cex} value
#' @param z.adj a two component vector. It sets the left/right and top/bottom
#'              justification
#' @param z.format controls how the numbers besides the colorscale are comosed.
#'                 It works like the \code{format} option of \code{\link{formatC}}
#' @param ndz sets the axis breakes right to the colorscale
#' @param ncolors number of colors to use in the plot
#' @param palette defines the color palette to be used in the plot
#' @param palette.inv logical, if \code{TRUE} reverts the color palette
#' @param ... further arguments to the plot function, e.g. \code{'xlab'}
#' @inheritParams graphics::plot.default
#' @importFrom graphics abline image mtext par strheight
#' @importFrom stats spline
#'
#' @examples
#'  rasterImage2( z = volcano, palette = "spectral", dim.max = c(500,500)
#'              ,zlab = "Height", z.adj = c(0,1) ,z.cex = 1
#'              ,main = "Volcano Data Set"
#'            )
#' @export
'rasterImage2' <- function(x=NULL,y=NULL,z,zlim=NULL,xlim=NULL,ylim=NULL
                         ,dim.max=NULL,plot.zero.line=T,regularGrid=T
                         ,zlab=NULL,z.cex=0.5,z.adj=c(0.5,0.5),z.format="fg"
                         ,ndz=7,ncolors=256
                         ,palette="spectral",palette.inv = F
                         ,...)
{
  dim.z<-dim(z)

  if(is.null(x)) x<-1:dim.z[1]
  if(is.null(y)) y<-1:dim.z[2]

  if(is.null(xlim)) xlim<-range(x,na.rm=T)
  if(is.null(zlim)) zlim<-1.05*range(z,na.rm=T)
  if(is.null(ylim)) ylim<-range(y,na.rm=T)

  # Zuschneiden der Matrix auf den sichtbaren Bereich
  dx<-abs(mean(diff(x)))
  dy<-abs(mean(diff(y)))
  z<-z[which(x >= min(xlim) - 1.05*dx & x <= max(xlim) + 1.05*dx),which(y >= min(ylim) - 1.05*dy & y <= max(ylim) + 1.05*dy)]
  x<-x[which(x >= min(xlim) - 1.05*dx & x <= max(xlim) + 1.05*dx)]
  y<-y[which(y >= min(ylim) - 1.05*dy & y <= max(ylim) + 1.05*dy)]

  if(!is.null(dim.max) | regularGrid)
  {
    x.new <- NULL
    y.new <- NULL
    if(is.null(dim.max) & regularGrid)
    {
      x.new<-seq(min(xlim),max(xlim),length.out=max(dim(z)))
      y.new<-seq(min(ylim),max(ylim),length.out=max(dim(z)))
    }
    else
    {
      x.new<-seq(min(xlim),max(xlim),length.out=ifelse(regularGrid,max(dim.max),dim.max[1]))
      y.new<-seq(min(ylim),max(ylim),length.out=ifelse(regularGrid,max(dim.max),dim.max[2]))
    }

    ### resampling der Matrix im Plot-bereich ###

    if(!is.null(dim.max))
    {
      z <-   apply(z,2,function(y,x,xout) return(spline(x,y,xout=xout,method="natural")$y),x,x.new)
      z <- t(apply(z,1,function(y,x,xout) return(spline(x,y,xout=xout,method="natural")$y),y,y.new))
      x <- x.new
      y <- y.new
    }
  }


  ## Farbpalette berechnen ##
  dz<-max(abs(zlim),na.rm=T)/ndz
  #lev<-seq(min(zlim,na.rm=T), max(zlim,na.rm=T), length.out = 0.5*max(c(dim(z),ndz*100)))
  lev <- seq(min(zlim,na.rm=T), max(zlim,na.rm=T), length.out = ncolors+1)
  color <- colorPalette(n = length(lev)-1,type = palette, inv = palette.inv)

  z[is.na(z)]<-0
  z[!is.finite(z)]<-0
  z[z>max(zlim)]<-max(zlim)
  z[z<min(zlim)]<-min(zlim)


  ## Plotten ##
  op<-par(no.readonly=T)
  par(mar=op$mar+c(0,0,0,3))
  image(
    x,y,z
    ,col=color
    ,breaks=lev
    ,xlim=xlim
    ,ylim=ylim
    ,useRaster = regularGrid
    ,...
  )
  if(plot.zero.line)
    abline(h=0,v=0,lty=3,col="white")

  usr<-par()$usr
  pin<-par()$pin
  fin<-par()$fin
  cin<-par()$cin

  dx <- par()$cxy[1]*par("cex")
  dy <- par()$cxy[2]*par("cex")
  # Anzahl Zeilen von zlab
  zlab.height <- strheight(zlab,cex=z.cex)+strheight("\n",cex=z.cex)
  p<-c(usr[2]+1*dx,usr[2]+2*dx,usr[3],usr[4])

  txt <- formatC(seq(min(zlim), max(zlim), length.out=ndz),digits=2, format = z.format)
  txt[1] <- paste("<",txt[1],sep="")
  txt[length(txt)] <- paste(">",txt[length(txt)],sep="")

  color.legend( xl=p[1],yb=p[3],xr=p[2],yt=p[4] - zlab.height # zlab.height*dy*z.cex
                ,legend=txt
                ,rect.col=color
                ,cex=z.cex*par()$cex
                ,align="rb"
                ,gradient="y"
  )
  # mtext(text=zlab,at=p[1],cex=z.cex*par()$cex,adj=z.adj[1],line=z.adj[2]-zlab.height*z.cex)
  mtext(text=zlab,at=p[1],cex=z.cex*par()$cex,adj=z.adj[1],line=z.adj[2]-zlab.height/dy)
  par(mar=op$mar)

}
