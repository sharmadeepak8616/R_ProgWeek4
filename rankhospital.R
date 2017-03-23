source("support.R")

rankhospital <- function (state, outcome, num="best") {
    
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
    
    # calculating number of hospitals with non-NA values
    totalHospitals <- length(dataState$Hospital.Name)

    # verifying num and assigning value to num if its a string
    if (num == "best") {
        num <- 1
    } else if (num == "worst") {
        num <- totalHospitals
    } else if ((class(num) %in% c("integer" , "numeric")) && (num > length(dataState$Hospital.Name))) {
        result.df <- rbind(result.df, list(NA, state))
        next
    } else if (!(class(num) %in% c("integer" , "numeric")) || (num < 1)) {
        stop("invalid rank")
    }
    
    # naming column names of dataState
    names(dataState) <- c("Hospital.Name" , "Rate")
    
    # sorting (increasing order) hospital.names based on mortality rate.
    attach(dataState)
    dataState <- dataState[order(Rate, Hospital.Name) , ]
    detach(dataState)
    
    # adding rank column to dataset(dataState)
    dataState <- cbind(dataState, Rank<-c(1:totalHospitals))
    
    # returning hospital name for asked num
    dataState$Hospital.Name[dataState$Rank==num]
    
}