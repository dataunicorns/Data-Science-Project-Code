# This is a function for using using the packages that are nt installed in R previously

pkgActivate <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    library(x)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

#Please use the function to invoke the packages in your programs

pkgActivate("tidyverse")

