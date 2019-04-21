# simple functions for rounding up or down to desired number of decimal places
floor_dec <- function(x, digits=1) round(x - 5*10^(-digits-1), digits)
ceiling_dec <- function(x, digits=1) round(x + 5*10^(-digits-1), digits)
