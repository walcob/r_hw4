rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv",colClasses="character")
    availableOutcomes <- c(11,17,23)
    names(availableOutcomes) <- c("heart attack","heart failure","pneumonia")
    ## Check that state and outcome are valid
    if(any(names(availableOutcomes)==outcome)){
        ## For each state, find the hospital of the given rank
        ## Remove extraneous data from outcomeData
        index <- availableOutcomes[outcome]
        names(outcomeData)[index] <- "deathrate"
        outcomeData <- data.frame(outcomeData[2],outcomeData[index],outcomeData[7])
        ## Make death rates numeric
        outcomeData[2] <- suppressWarnings(lapply(outcomeData[2],as.numeric))
        outcomeData <- outcomeData[complete.cases(outcomeData),]
        states = split(outcomeData,outcomeData$State)
        hospital <- sapply(states,getrank,num)
        result <- data.frame(hospital=hospital,state=names(hospital))
        # ## Return a data frame with the hospital names and the
        # ## (abbreviated) state name
        return(result)
    }
    else{
        stop("invalid outcome")
    }
}

getrank <- function(stateData,num){
    ## Helper function that copies some of the meat and bones of rankhospital
    # print(stateData)
    if (num == "best") num <- 1
    ## worst
    else if (num == "worst") num <- dim(stateData)[1]
    ## out of range, return NA
    else if (num > dim(stateData)[1] || num < 1) return(NA)
    ## get order of rows
    ranking <- order(stateData$deathrate,stateData$Hospital.Name)
    return(stateData[ranking[num],1])
}