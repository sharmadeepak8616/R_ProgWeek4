
readFile <- function (file, loc=getwd(), colClass=NA) {
    fileData<-read.csv(file, colClasses=colClass)
    fileData
}

verifyInput <- function (value, validList, message) {
    if (!(value %in% validList)) {
        stop (message)
    }
}

polish <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}