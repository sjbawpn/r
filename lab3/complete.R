complete <- function(directory, id = 1:332) {

    x <- NULL
    n <- NULL
    for(i in id) {

        path <- sprintf("%s/%.3d.csv", directory, i)
        file <- read.csv(path)
        
        sna <- is.na(file[["sulfate"]])
        nna <- is.na(file[["nitrate"]])
        
        nu <- nrow(file[!sna & ! nna,])

        if ( is.null(x) ) {
            x <- i
            n <- nu 
        } else {
            x <- c(x, i)
            n <- c(n, nu)
        }
    }
    comp <- data.frame(id=x, nobs=n)
    return(comp)

}
