source("complete.R")

corr <- function(directory, threshold = 0) {

    x <- NULL
    comp <-complete(directory)
    L <- comp$nobs > threshold
    compt <- comp[L,]

    id <- compt$id
    for(i in id) {

        path <- sprintf("%s/%.3d.csv", directory, i)
        file <- read.csv(path)

        sna <- is.na(file[["sulfate"]])
        nna <- is.na(file[["nitrate"]])

        nu <- file[!sna & ! nna,]

        sulf <- nu$sulfate
        nit <- nu$nitrate
        corl <- cor(sulf, nit)
        if ( is.null(x) ) {
            x <- corl
        } else {
            x <- c(x, corl)
        }
    }

    return(x)

}

