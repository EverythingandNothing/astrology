
# Useful functions to have 

binomsd <- function(n,p){
  sd <- (n*p*(1-p))^.5
  return(sd)
}

zscore <- function(n,p,x){
  z <- (x-n*p)/binomsd(n=n,p=p)
  return(z)
}

#libraries used

library(dplyr)
library(janitor)
library(ggplot2)
library(stringr)
library(stats)
library(gplots)
library(RColorBrewer)
