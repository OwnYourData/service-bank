raika_parser <- function(raw){
        # read attributes
        bookingDate <- strsplit(raw, ";")[[1]][1]
        desc        <- strsplit(raw, ";")[[1]][2]
        valutaDate  <- strsplit(raw, ";")[[1]][3]
        val         <- strsplit(raw, ";")[[1]][4]
        currency    <- strsplit(raw, ";")[[1]][5]
        timestamp   <- strsplit(raw, ";")[[1]][6]

        # parse amount
        amount <- as.numeric(gsub(',', '.', gsub("[\\.\\+]", '', val)))
        # remove quotes from desc
        desc <- gsub('"', '', desc)

        other <- list(bookingDate = bookingDate,
                      original    = desc,
                      currency    = currency)
        line <- list(date        = valutaDate,
                     description = trimws(gsub('\\s+', ' ', desc)),
                     amount      = amount,
                     other       = other)
        
        # return value
        line
        
}