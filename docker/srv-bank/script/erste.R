erste_parser <- function(data){
        data <- data[,c('Buchungsdatum', 'Buchungstext', 'Betrag')]
        data$Buchungsdatum <- as.character(data$Buchungsdatum)
        data$Betrag <- as.numeric(gsub(',', '.', gsub("[\\.\\+]", '', data$Betrag)))
        data$Buchungstext <- as.character(data$Buchungstext)
        colnames(data) <- c('date', 'description', 'amount')
        lapply(setNames(split(data, seq(nrow(data))), NULL), function(x){
                other <- list(
                        original = x$description,
                        currency = "EUR"
                )
                c(as.list(x), other=list(other))
        })
}
