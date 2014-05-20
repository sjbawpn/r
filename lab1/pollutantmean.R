pollutantmean <- function(directory, pollutant, id = 1:332) {
    x <- NULL
    for(i in id) {
        path <- sprintf("%s/%.3d.csv", directory, i)
        file <- read.csv(path)
        
        na <- !is.na(file[[pollutant]])
        if ( is.null(x) ) {
            x <- file[[pollutant]][na]
        } else {
            x <- c(x, file[[pollutant]][na])
        }
    }

    return(round(mean(x), 3))

}
