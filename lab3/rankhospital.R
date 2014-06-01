rankhospital <- function(state, outcome, num = "best" ) {
    ## Read outcome data
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    sub <- outcomes[,c(2,7,11,17,23)]
    colnames(sub) <- c("name","state","heart attack", "heart failure", "pneumonia")
    sub[, 3] <- suppressWarnings(as.numeric(sub[, 3]))
    sub[, 4] <- suppressWarnings(as.numeric(sub[, 4]))
    sub[, 5] <- suppressWarnings(as.numeric(sub[, 5]))
    if (!state%in% sub$state) stop("invalid state");
    if (!outcome %in% colnames(sub)[c(3:5)]) stop("invalid outcome");
    state_sub <- sub[sub$state==state,]

    outcome_sub <- state_sub[,c(1,2,which(colnames(sub)== outcome))]
    sub <- outcome_sub[!is.na(outcome_sub[[outcome]]),]
    result <- sub[with(sub, order(sub[,3],sub[,1])),]

    r <- NA;
    if (num == "best") r <-1
    else if (num == "worst") r <- nrow(result)
    else if (class(num) == "numeric" && num <= nrow(result) && num > 0) r <- num
    else return(NA)
    
    return(result[r,]$name)


    ## Check that state and outcome are valid

    ## Return hospital name in that state with lowest 30-day death
    
    ## rate

}
