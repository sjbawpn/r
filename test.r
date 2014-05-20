#!/usr/bin/Rscript

cube <- function(x, n) {
    x ^ 3
}

#x <- 1:10
#if (x < 5) {
#    x <- 0
#}

f <- function(x) {
    g <- function(y) {
        y + z
    }
    z <- 4
    x + g(x)

}

z <- 10
#print(f(3))
##############

x <- 5
y <- if(x < 3) {
    NA
} else {
    11
}
#print(y)
############

h <- function(x, y = NULL, d = 3L) {
    z <- cbind(x, d)
    if (!is.null(y))
        z <- z + y
    else
        z<- z + f
    g <- x + y / z
    if (d == 3L)
        return(g)
    g <- g + 10
    g
}
#
####print(h(1))
