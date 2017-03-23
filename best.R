source("support.R")

best <- function (state, outcome) {

    data <- readFile("outcome-of-care-measures.csv", colClass="character")
    stateList <- data$State
    outcomeList <- c("Heart.Attack", "Heart.Failure" , "Pneumonia")

    # converting outcome into format similar to col name (eg. heart attack -> Heart.Attack)
    outcome <- gsub(" ",".",polish(outcome),fixed=TRUE)

    # verifying state in stateList
    if (!(state %in% stateList)) {
        stop ("invalid state")
    }
    # verifying outcome in outcomeList
    if (!(outcome %in% outcomeList)) {
        stop ("invalid outcome")
    }
    
    # generating complete column name
    colName <- paste("Hospital.30.Day.Death..Mortality..Rates.from",outcome,sep=".")
    
    # fetching Data for the given state
    dataState <- data[data$State==state,c("Hospital.Name",colName)]
    
    # converting values in numeric class
    oldw <- getOption("warn")
    options(warn = -1)
    dataState[[colName]] <- as.numeric(dataState[[colName]])
    options(warn = oldw)
    
    # selecting non-NA rows from the data
    dataState <- dataState[complete.cases(dataState[c("Hospital.Name", colName)]), c("Hospital.Name",colName)]

    # fetching hospital name(s) for minimum mortality for the outcome
    hospNames <- dataState$Hospital.Name[dataState[[colName]]==min(dataState[[colName]])]
    
    # sorting hospital name(s)
    hospName <- sort(hospNames)[1]
    
    # returning first name in the list
    hospName

}




