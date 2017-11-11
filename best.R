best <- function(state,outcome) {
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv",colClasses="character")
    availableOutcomes <- c(11,17,23)
    names(availableOutcomes) <- c("heart attack","heart failure","pneumonia")
    ## 11, 17, 24
    ## Check that state and outcome are valid
    if(any(outcomeData$State == state)){
        if(any(names(availableOutcomes) == outcome)){
            ## Extract data for just the state
            stateData <- outcomeData[outcomeData$State==state,]
            ## Extract data for just the outcome given and the name of the hospital
            nameAndDeathRate <- data.frame(stateData[2],stateData[availableOutcomes[outcome]])
            ## Rename second column to make it easier to use
            names(nameAndDeathRate)[2] <- "deathrate"
            ## Make the death rates numeric, suppress the warnings about
            ## introducing NAs
            nameAndDeathRate[2] <- suppressWarnings(lapply(nameAndDeathRate[2],as.numeric))
            ## Remove NAs
            nameAndDeathRate <- nameAndDeathRate[complete.cases(nameAndDeathRate),]
            ## Get the minimum rate
            minRate <- sapply(nameAndDeathRate[2],min)
            ## Check for multiple hospitals with this rate and return the first in alphabetical order
            minima <- nameAndDeathRate[nameAndDeathRate$deathrate==minRate,]
            result <- sapply(minima[1],min)
            names(result) <- NULL
            return(result)
        }
        else{
            stop("invalid outcome")
        }
    }
    else{
        stop("invalid state")
    }
    ## Return hospital name in that state with lowest 30-day death
    ## rate
}

plotMortality <- function(){
    outcome <- read.csv("outcome-of-care-measures.csv",colClasses="character")
    head(outcome)
    outcome[,11] <- as.numeric(outcome[,11])
    hist(outcome[,11])
    
}