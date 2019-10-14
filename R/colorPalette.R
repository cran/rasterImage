#' Defines a color palette
#'
#' This function defines a color palette and returns a vector of colors. The
#' palettes itself are adapted from the ColorBrewer project.
#'
#' The parameter \code{type} controls the output palette type as follows:
#' \describe{
#'        \item{"spectral"}{spectral colors from blue to red}
#'        \item{"specrtalHalf"}{spectral colors from green to red}
#'        \item{"green"}{MultiHue yellow - green}
#'        \item{"blue"}{MultiHue yellow - green blue}
#'        \item{"orange"}{MultiHue yellow - orange - brown}
#'        \item{"red"}{MultiHue yellow - orange red}
#'        \item{"red-white-bule","bwr"}{red - white - blue colors}
#'        \item{"rainbow"}{reproduces the rainbow color set}
#'        \item{"black-white","bw"}{gray scale colors}
#'        \item{"white-black","wb"}{gray scale colors from white to black}
#'        \item{"jet.colors","jc"}{dark blue to dark red}
#'        \item{"hzdr1"}{HZDR cooperate design colors}
#'        \item{"hzdr2"}{HZDR cooperate design colors}
#' }
#' If a vector of color names is supported, then a customized palette will be
#' calculated according to these colors.
#'
#' @param n number of colors to produce
#' @param inv revert the order of colors
#' @param type sets the type of color palette. See Details
#' @return returns a vector of colors to be passed to \code{image} or \code{rasterImage}
#' @references \url{http://colorbrewer2.org} by Cynthia A. Brewer, Geography, Pennsylvania State University
#' @importFrom grDevices colorRampPalette colors rainbow
#' @examples
#' # default "spectral" palette
#' barplot(rep(1,10), col = colorPalette(10))
#'
#' # custom color palette
#' barplot(rep(1,10), col = colorPalette(n = 10, type = c("red","blue","yellow")))
#' @export
colorPalette <- function(n=NULL,type="spectral",inv=F)
{
  type <- tolower(type)
  # Farbvector anlegen #
  color <- NULL
  col.pal <- NULL

  if(!is.character(type))
    stop("type must be a character \"color\" argument")

  ### wenn eine Liste aus Farben oben steht ###
  if(length(type) > 1 & is.character(type))
  {
    if(sum(type %in% grDevices::colors()) != length(type) & sum(grep("#",type)) == 0 )
      stop("incorrect color or palette definition")

    colfun <- grDevices::colorRampPalette(type,interpolate="spline")
    return(colfun(n))
  }

  ### ColorBrewer ###

  # Spectral #
  if(type == "spectral")      col.pal <- rev(c( "#9E0142","#D53E4F","#F46D43"
                                                ,"#FDAE61","#FEE08B","#FFFFBF"
                                                ,"#E6F598","#ABDDA4","#66C2A5"
                                                ,"#3288BD","#5E4FA2"))
  if(type == "spectralhalf")  col.pal <- rev(c( "#a50026","#d73027","#f46d43"
                                                ,"#fdae61","#fee08b","#ffffbf"
                                                ,"#d9ef8b","#a6d96a","#66bd63"
                                                ,"#1a9850","#006837"))
  if(type == "green")         col.pal <- rev(c( "#ffffe5","#f7fcb9","#d9f0a3"
                                                ,"#addd8e","#78c679","#41ab5d"
                                                ,"#238443","#006837","#004529"))
  if(type == "blue")          col.pal <- rev(c( "#ffffd9","#edf8b1","#c7e9b4"
                                                ,"#7fcdbb","#41b6c4","#1d91c0"
                                                ,"#225ea8","#253494","#081d58"))
  if(type == "orange")        col.pal <- rev(c( "#ffffe5","#fff7bc","#fee391"
                                                ,"#fec44f","#fe9929","#ec7014"
                                                ,"#cc4c02","#993404","#662506"))
  if(type == "red")           col.pal <- rev(c( "#ffffcc","#ffeda0","#fed976"
                                                ,"#feb24c","#fd8d3c","#fc4e2a"
                                                ,"#e31a1c","#bd0026","#800026"))

  if(type == "blue-white-red" | type == "bwr")
    col.pal <- c("blue","white","red")

  if(type == "red-white-blue" | type == "rwb")
    col.pal <- c("red","white","blue")

  if(type == "rainbow")
    col.pal <- rainbow(20)

  if(type == "black-white" | type == "bw")
    col.pal <- c("black","white")

  if(type == "white-black" | type == "wb")
    col.pal <- c("white","black")

  if(type == "jet.colors" | type == "jc")
    col.pal <- c("#00007F", "blue", "#007FFF", "cyan"
                 ,"#7FFF7F", "yellow", "#FF7F00"
                 , "red", "#7F0000")

  if(type == "hzdr1" | type =="hzdr")
    col.pal <- c("#CF6800","#00589C","#003E6E"
                 ,"#515151","#9C9C9C","#B9B9B9")
  if(type == "hzdr2")         col.pal <- c("#D42D12","#CF6800","#E6AF11"
                                           ,"#144D28","#A9B509","#00A2E0")

  if(is.null(col.pal))
    stop("No valid pallete definition")

  colfun <- grDevices::colorRampPalette(col.pal,interpolate="spline")
  color <- colfun(ifelse(n==0,length(col.pal),n))

  if(is.null(color))
    stop("No valid pallete definition")

  if(inv) color <- rev(color)

  return(color)

}
