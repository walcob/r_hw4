rankhospital <- function(state,outcome,num){
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv",colClasses="character")
    availableOutcomes <- c(11,17,23)
    names(availableOutcomes) <- c("heart attack","heart failure","pneumonia")
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
            ## Check to see if num is usable
            ## best
            if (num == "best") num <- 1
            ## worst
            else if (num == "worst") num <- dim(nameAndDeathRate)[1]
            ## out of range, return NA
            else if (num > dim(nameAndDeathRate)[1] || num < 1) return(NA)
            ## get order of rows
            ranking <- order(nameAndDeathRate$deathrate,nameAndDeathRate$Hospital.Name)
            return(nameAndDeathRate[ranking[num],1])
        }
        else{
            stop("invalid outcome")
        }
    }
    else{
        stop("invalid state")
    }
}

