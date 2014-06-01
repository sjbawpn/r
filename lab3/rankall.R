rankall <- function(outcome, num = "best" ) {
    ## Read outcome data
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    sub <- outcomes[,c(2,7,11,17,23)]
    colnames(sub) <- c("name","state","heart attack", "heart failure", "pneumonia")
    sub[, 3] <- suppressWarnings(as.numeric(sub[, 3]))
    sub[, 4] <- suppressWarnings(as.numeric(sub[, 4]))
    sub[, 5] <- suppressWarnings(as.numeric(sub[, 5]))
    #if (!state%in% sub$state) stop("invalid state");
    if (!outcome %in% colnames(sub)[c(3:5)]) stop("invalid outcome");

    df <- data.frame(hospital = character(),state = character()) 
    states <- sort(unique(sub$state))

    for (s in states) {
        state_sub <- sub[sub$state==s,]
        outcome_sub <- state_sub[,c(1,2,which(colnames(sub)== outcome))]
        outcome_sub <- outcome_sub[!is.na(outcome_sub[[outcome]]),]
        result <- outcome_sub[with(outcome_sub, order(outcome_sub[,3],outcome_sub[,1])),]
        #print(result)
        h <- NA;
        if (num == "best") {
            h <- result[1,]$name;
        }
        else if (num == "worst") {
            h <- result[nrow(result),]$name;
        }
        else if (class(num) == "numeric" && num <= nrow(result) && num > 0) {
            h <- result[num,]$name;
        }
        df <- rbind(df, data.frame(row.names = s, hospital = h, state = s))  
    }

    
    
    return(df)


    ## Check that state and outcome are valid

    ## Return hospital name in that state with lowest 30-day death
    
    ## rate

}
