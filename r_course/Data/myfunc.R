library(tidyverse)
library(FME)
library(plotly)
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

options(device="quartz")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
sumLor <- function(x,params){
    # sum of Lorentzian functions
    y0 <- as.vector(params[grep("y0",names(params))])
    A  <- as.vector(params[grep("A",names(params))])
    x0 <- as.vector(params[grep("x0",names(params))])
    FWHM <- as.vector(params[grep("FWHM",names(params))])
    if(length(x0)!=length(FWHM)) FWHM <- rep(FWHM, length.out=length(x0))
    if(length(x0)!=length(A))    A <- rep(A, length.out=length(x0))
    if(length(x0)!=length(y0))   y0 <- rep(y0, length.out=length(x0))
    rowSums(sapply(1:length(x0), function(i) {
          y0[i] + 2*A[i]/(pi*FWHM[i])/( 1 + ((x-x0[i])/(FWHM[i]/2))^2 )
        }))
}





