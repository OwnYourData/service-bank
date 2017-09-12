eb_parser <- function(raw){
        # init
        htype <- ''
        payee <- ''
        
        # simple reading of attributes
        accountNr <- strsplit(raw, ";")[[1]][1]
        valutaDate <- strsplit(raw, ";")[[1]][3]
        bookingDate <- strsplit(raw, ";")[[1]][4]
        currency <- strsplit(raw, ";")[[1]][6]
        
        # parse amount
        val <- strsplit(raw, ";")[[1]][5]
        amount <- as.numeric(gsub(',', '.', gsub("[\\.\\+]", '', val)))
        
        # parse the description field to get more detailed information
        # from https://github.com/BernhardDenner/EasyBankcsv2qif/blob/master/easyBankcsv2qif.py
        desc <- strsplit(raw, ";")[[1]][2]
        r <- regexpr('^(.*)\\W*([A-Z]{2})/([0-9]+)\\W*(.*)?$', desc, perl=T)
        if (attr(r, 'match.length') > 0){
                cs <- as.vector(attr(r, "capture.start"))
                cl <- as.vector(attr(r, "capture.length"))
                desc1 <- trimws(substr(desc, cs[1], cs[1]+cl[1]-1))
                type <- substr(desc, cs[2], cs[2]+cl[2]-1)
                id <- substr(desc, cs[3], cs[3]+cl[3]-1)
                desc2 <- trimws(substr(desc, cs[4], cs[4]+cl[4]-1))
                
                # transfer
                if((type == 'BG' | type == 'FE') &&
                   (nchar(desc2) > 0 && nchar(desc1) > 0)){
                        htype = 'transfer'
                        m <- regexpr('^(([A-Z0-9]+\\W)?[A-Z]*[0-9]+)\\W(\\w+\\W+\\w+)\\W*(.*)$', desc2, perl=T)
                        if (attr(r, 'match.length') > 0){
                                mcs <- as.vector(attr(m, "capture.start"))
                                mcl <- as.vector(attr(m, "capture.length"))
                                group1 <- substr(desc2, mcs[1], mcs[1]+mcl[1]-1)
                                group2 <- substr(desc2, mcs[2], mcs[2]+mcl[2]-1)
                                group3 <- substr(desc2, mcs[3], mcs[3]+mcl[3]-1)
                                group4 <- substr(desc2, mcs[4], mcs[4]+mcl[4]-1)
                                if(nchar(group1) > 0){
                                        payee <- paste0(group3, ' (', group1, ')')
                                } else {
                                        payee <- group3
                                }
                                desc2 <- group1
                                memo <- trimws(paste0(desc1, ' ', group4))
                        } 
                        
                        # BG can meen "Bankgebuehren" (bankfee), therefore the desc2 XOR desc1 is empty
                } else if((type == 'BG' | type == 'RI') &&
                          nchar(desc2) == 0){
                        memo <- desc1
                        htype = 'bankfee'
                } else if((type == 'BG' | type == 'RI') &&
                          nchar(desc1) == 0){
                        memo <- desc2
                        htype = 'bankfee'
                        
                        # not really a transfer, but use the information we have
                } else if((type == 'MC') &&
                          nchar(desc2) == 0){
                        memo <- desc1
                } else if((type == 'MC') &&
                          nchar(desc1) == 0){
                        memo <- desc2
                        
                        # Maestro card (cash card) things
                } else if((type == 'MC') &&
                          (nchar(desc1) > 0) && (nchar(desc2) > 0)){
                        # withdraw with cash card
                        m1 <- regexpr('^((Auszahlung)\\W+\\w+)\\W*(.*)$', desc1, perl=T)
                        # payment with cash card
                        m2 <- regexpr('^((Bezahlung)\\W+\\w+)\\W*(.*)$', desc1, perl=T)
                        if (attr(m1, 'match.length') > 0){
                                m1cs <- as.vector(attr(m1, "capture.start"))
                                m1cl <- as.vector(attr(m1, "capture.length"))
                                group1 <- substr(desc2, m1cs[1], m1cs[1]+m1cl[1]-1)
                                group2 <- substr(desc2, m1cs[2], m1cs[2]+m1cl[2]-1)
                                group3 <- substr(desc2, m1cs[3], m1cs[3]+m1cl[3]-1)
                                htype <- 'withdraw'
                                memo <- group1
                                if(nchar(group3) > 0){
                                        memo <- paste0(memo, ' (', group3, ')')
                                }
                                memo <- paste0(memo, ' ', desc2)
                        } else if (attr(m2, 'match.length') > 0){
                                m2cs <- as.vector(attr(m2, "capture.start"))
                                m2cl <- as.vector(attr(m2, "capture.length"))
                                group1 <- substr(desc2, m2cs[1], m2cs[1]+m2cl[1]-1)
                                group2 <- substr(desc2, m2cs[2], m2cs[2]+m2cl[2]-1)
                                group3 <- substr(desc2, m2cs[3], m2cs[3]+m2cl[3]-1)
                                htype <- 'payment'
                                memo <- group1
                                if(nchar(group3) > 0){
                                        memo <- paste0(memo, ' (', group3, ')')
                                }
                                memo <- paste0(memo, ' ', desc2)
                        } else {
                                # credit card bill and other infos
                                if(substr(desc2, 1, nchar('Abrechnung')) == 'Abrechnung'){
                                        htpye <- 'credit card bill'
                                } else {
                                        htpye <- 'unknown'
                                }
                                memo <- paste0(desc1, ' ', desc2)
                        }
                        
                        # mixture of transfer, cash card payments
                } else if(type == 'VD') {
                        # if we have a value for desc1 but not for desc2 it may be a cash card payment
                        if((nchar(desc1) > 0) && (nchar(desc2) == 0)){
                                htype <- 'payment'
                                memo <- desc1
                                
                                # if we have values for both desc fields it may be a transfer
                        } else if((nchar(desc1) > 0) && (nchar(desc2) > 0)) {
                                htype <- 'transfer'
                                memo <- desc1
                                m <- regexpr('^(([A-Z0-9]+\\W)?[A-Z]*[0-9]+)?\\W*(\\w+\\W+\\w+)\\W*(.*)$', desc2, perl=T)
                                if (attr(m, 'match.length') > 0){
                                        mcs <- as.vector(attr(m, "capture.start"))
                                        mcl <- as.vector(attr(m, "capture.length"))
                                        group1 <- substr(desc2, mcs[1], mcs[1]+mcl[1]-1)
                                        group2 <- substr(desc2, mcs[2], mcs[2]+mcl[2]-1)
                                        group3 <- substr(desc2, mcs[3], mcs[3]+mcl[3]-1)
                                        group4 <- substr(desc2, mcs[4], mcs[4]+mcl[4]-1)
                                        payee <- group3
                                        if(nchar(group1)>0){
                                                payee <- paste0(payee, ' (', group1, ')')
                                        }
                                        memo <- paste0(memo, ' ', group4)
                                }
                                
                                # OG also seems to be a payment transaction
                        } else if(type == 'VD') {
                                htype <- 'payment'
                                memo <- paste0(desc1, ' ', desc2)
                                
                                # seems to be an cash card payment, however, I don't have enough infos about it
                        } else {
                                memo <- paste0(desc1, ' ', desc2)
                        }
                } else {
                        memo <- desc
                }
        }
        
        if(nchar(htype) == 0){
                htype <- 'unknown'
        }
        
        description <- list(id    = id,
                            type  = htype,
                            typeShort = type,
                            payee = payee,
                            desc1 = trimws(gsub('\\s+', ' ', desc1)), 
                            desc2 = trimws(gsub('\\s+', ' ', desc2)),
                            memo  = trimws(gsub('\\s+', ' ', memo)),
                            original = desc)
        
        line <- list(date  = valutaDate,
                     bookingDate = bookingDate,
                     description = description,
                     amount      = amount,
                     currency    = currency)
        
        other <- list(account = accountNr,
                      bookingDate = bookingDate,
                      id    = id,
                      type  = htype,
                      typeShort = type,
                      payee = payee,
                      desc2 = trimws(gsub('\\s+', ' ', desc2)),
                      memo  = trimws(gsub('\\s+', ' ', memo)),
                      original = desc,
                      currency = currency)
        line <- list(date        = valutaDate,
                     description = trimws(gsub('\\s+', ' ', desc1)),
                     amount      = amount,
                     other       = other)
        line$`_oydRepoName` <- 'Easybank Umsatzliste'

        # return value
        line
}

# build curl string
# cmdStr <- paste0(
#         "curl ",
#         "'https://ebanking.easybank.at/InternetBanking/InternetBanking/",
#         pageID, "' ", 
#         "-H 'Pragma: no-cache' ", 
#         "-H 'Origin: https://ebanking.easybank.at' ", 
#         "-H 'Accept-Encoding: gzip, deflate, br' ", 
#         "-H 'Accept-Language: en-US,en;q=0.8,de;q=0.6' ",
#         "-H 'Upgrade-Insecure-Requests: 1' ",
#         "-H 'User-Agent: ", userAgentStr, "' ",
#         "-H 'Content-Type: application/x-www-form-urlencoded' ",
#         "-H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' ",
#         "-H 'Cache-Control: no-cache' ",
#         "-H 'Referer: https://ebanking.easybank.at/' ",
#         "-H 'Cookie: st=", stCookie, "' ",
#         "-H 'Connection: keep-alive' ",
#         "-H 'DNT: 1' ",
#         "--data 'svc=EASYBANK&d=transactions&pagenumber=1&submitflag=true&suppressOverlay=true&print=false&csv=true&accountChanged=false&newSearch=false&sortingColumn=BOOKING_DATE&sortingDirection=-1&lastviewed=&outstandingBalance=&searchPanelShown=false&initialRowsPerPage=30&konto=0&datefrom=&datetill=&betvon=&centsvon=&betbis=&centsbis=&buchungstext=&umsatzart=-1&enlargementOfTransaction=0&rowsPerPage=30' ",
#         "--compressed")
