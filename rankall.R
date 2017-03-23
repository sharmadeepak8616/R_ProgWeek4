source("support.R")

rankall <- function (outcome, num="best") {
    
    data <- readFile("outcome-of-care-measures.csv", colClass="character")
    stateList <- sort(unique(data$State))
    outcomeList <- c("Heart.Attack", "Heart.Failure" , "Pneumonia")
    finalRes <- list()
    
    # converting outcome into format similar to col name (eg. heart attack -> Heart.Attack)
    outcome <- gsub(" ",".",polish(outcome),fixed=TRUE)

    # verifying outcome in outcomeList
    if (!(outcome %in% outcomeList)) {
        stop ("invalid outcome")
    }
    
    # generating complete column name
    colName <- paste("Hospital.30.Day.Death..Mortality..Rates.from",outcome,sep=".")
    
    for (state in stateList) {
        # fetching Data for the given state
        dataState <- data[data$State==state,c("Hospital.Name",colName,"State")]
        
        # converting values in numeric class
        oldw <- getOption("warn")
        options(warn = -1)
        dataState[[colName]] <- as.numeric(dataState[[colName]])
        options(warn = oldw)
        
        # selecting non-NA rows from the data
        dataState <- dataState[complete.cases(dataState[c("Hospital.Name", colName)]), ]
        
        # calculating number of hospitals with non-NA values
        totalHospitals <- length(dataState$Hospital.Name)
        
        # verifying num and assigning value to num if its a string
        if (num == "best") {
            num <- 1
        } else if (num == "worst") {
            num <- totalHospitals
        } else if ((class(num) %in% c("integer" , "numeric")) && (num > length(dataState$Hospital.Name))) {
            # appending finalRes with hospital name(as NA) and state,
            #if given num is greater than number of hospitals(with mortality rate) in the given state
            finalRes <- rbind(finalRes, list(NA, state))
            
            #moving to next state value or iteration
            next

        } else if (!(class(num) %in% c("integer" , "numeric")) || (num < 1)) {
            stop("invalid rank")
        }
        
        # sorting (increasing order) hospital.names based on mortality rate.
        # if martality rate is same then hospital name will be ordered alphabetically
        oldw <- getOption("warn")
        options(warn = -1)
        attach(dataState)
        dataState <- dataState[order(dataState[[colName]], dataState$Hospital.Name), ]
        detach(dataState)
        options(warn = oldw)

        # adding rank column to dataset(dataState)
        dataState <- cbind(dataState, Rank<-c(1:totalHospitals))
        
        # fetching hospital name for asked num
        hospName <- dataState$Hospital.Name[dataState$Rank==num]
        
        # appending finalRes with hospital name and state for given num
        finalRes <- rbind(finalRes,c(hospName,state))
    }
    
    finalRes <- as.data.frame(finalRes)
    colnames(finalRes) <- c("hospital", "state")
    rownames(finalRes) <- stateList
    
    finalRes
}

