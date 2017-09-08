idb_parser <- function(raw){
        # simple reading of attributes
        iban <- strsplit(raw, ";")[[1]][1]
        desc <- strsplit(raw, ";")[[1]][2]
        valutaDate <- strsplit(raw, ";")[[1]][3]
        currency <- strsplit(raw, ";")[[1]][4]
        soll <- strsplit(raw, ";")[[1]][5]
        haben <- strsplit(raw, ";")[[1]][6]
        
        # calc other fields
        amount <- str2num(soll) + -1*str2num(haben)
        
        other <- list(iban = iban,
                      currency = currency,
                      original = desc)
        line <- list(date        = valutaDate,
                     description = desc,
                     amount      = amount,
                     other       = other)
        
        # return value
        if(is.na(amount)){
                NULL
        } else {
                line
        }
}

# build curl string
# cmdStr <- paste0(
#         "curl '", downloadUrl, epochMS(), "' ",
#         "-H 'Pragma: no-cache' ",
#         "-H 'DNT: 1' ",
#         "-H 'Accept-Encoding: gzip, deflate, br' ",
#         "-H 'Accept-Language: en-US,en;q=0.8,de;q=0.6' ",
#         "-H 'Upgrade-Insecure-Requests: 1' ",
#         "-H 'User-Agent: ", userAgentStr, "' ",
#         "-H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' ",
#         "-H 'Cache-Control: no-cache' ",
#         "-H 'Referer: https://banking.ing-diba.at/online-banking/wicket/wicket/page?0' ",
#         "-H 'Cookie: ", paste(apply(dfc[, c('name', 'value')], 1,
#                                     paste, collapse='='), collapse='; '), "' ",
#         "-H 'Connection: keep-alive' ",
#         "--compressed")

