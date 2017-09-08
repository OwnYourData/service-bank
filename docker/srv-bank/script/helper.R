epochMS <- function(){
        currTimeMS <- as.numeric(as.POSIXct(Sys.time()))*1000
        ctStr <- as.character(currTimeMS)
        unlist(strsplit(ctStr, "[.]"))[1]
}

str2num <- function(val){
        cleanVal <- gsub(',', '.', gsub("[\\.\\+]", '', val))
        if(is.na(suppressWarnings(as.numeric(cleanVal)))){
                NA
        } else {
                as.numeric(cleanVal)
        }
}
