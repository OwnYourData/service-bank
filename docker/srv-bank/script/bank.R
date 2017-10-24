# Infos:
# - https://ropensci.org/tutorials/rselenium_tutorial.html
# - http://rpubs.com/johndharrison/RSelenium-Docker
# - https://hub.docker.com/u/selenium/
#
# $ apt-get update
# $ apt-get install libssl-dev libcurl4-openssl-dev phantomjs libxml2-dev
# install.packages("devtools")
# devtools::install_github("ropensci/RSelenium")

# Usage:
# - passing account credentials:
#   $ Rscript bank.R '{"bank":"easybank","username":"123456","password":"12345geheim","account":"AT121420020010123456"}'
#   $ Rscript bank.R '{"bank":"ingdiba","username":"123456","password":"12345geheim"}'
# - passing downloaded file
#   $ cat download.csv | Rscript bank.R --bank=easybank

library(RSelenium)
library(methods)
library(jsonlite)
library(xml2)

bank <- ''
username <- ''
password <- ''
account <- ''
accountNo <- 0
download <- TRUE

# read credentials from command line or stdin
localExecute <- FALSE
args <- commandArgs(trailingOnly=TRUE)
if(length(args) > 0){
        if(args[1] == '-l'){
                localExecute <- TRUE
                args <- args[c(FALSE, rep(TRUE, length(args)-1))]
        }
}
if (length(args) == 0){
        myStdin <- file("stdin")
        input <- suppressWarnings(readLines(myStdin))
        close(myStdin)
} else {
        if(lengths(args) == 1){
                arg1 <- args[1]
                if(substr(arg1, 1, 7) == '--bank='){
                        bank <- substr(arg1, 8, nchar(arg1))
                        if(bank == 'erste'){
                                myStdin <- file('stdin', 'rb')
                                retVal <- readBin(myStdin, 
                                                  raw(), 
                                                  n = 10e6, 
                                                  endian = "little")
                                retVal <- rawToChar(retVal[c(TRUE,FALSE)][-1])
                                retVal <- iconv(retVal, 
                                                from="ISO-8859-1", to="UTF-8")
                                con <- textConnection(retVal)
                                result <- suppressWarnings(read.csv(con))
                                close(myStdin)
                        } else {
                                myStdin <- file('stdin')
                                result <- suppressWarnings(readLines(myStdin))
                                close(myStdin)
                                result <- iconv(result, 
                                                from="ISO-8859-1", to="UTF-8")
                        }
                        download <- FALSE
                } else {
                        input <- arg1
                }
        } else {
                stop('ungültige Argumente')
        }
}

if(localExecute){
        srcPath <- '/Users/christoph/oyd/service-bank/docker/srv-bank/script/'
} else {
        srcPath <- '/srv-bank/'
}
source(paste0(srcPath, 'helper.R'))
source(paste0(srcPath, 'easybank.R'))
source(paste0(srcPath, 'ingdiba.R'))
source(paste0(srcPath, 'erste.R'))

if(download){
        if(validate(input)){
                ji <- fromJSON(input)
                bank <- ji$bank
                username <- ji$username
                password <- ji$password
                account <- ji$account
        } else {
                stop('invalid JSON input')
        }
        if (is.null(bank)){
                stop('invalid bank')
        } else if (bank == ''){
                stop('invalid bank')
        }
        if (is.null(username)){
                stop('invalid username')
        } else if (username == ''){
                stop('invalid username')
        }
        if (is.null(password)){
                stop('invalid password')
        } else if (password == ''){
                stop('invalid password')
        }
        if(bank == 'easybank'){
                if (is.null(account)){
                        stop('invalid account')
                } else if (account == ''){
                        stop('missing account')
                } else if (nchar(account) != 20){
                        stop('invalid account format')
                }
                account <- paste(substring(account, seq(1, 20, 4), seq(4, 20, 4)), 
                                 collapse = ' ')
        }
        
        pageID <- ''
        bankURL <- ''
        bankLogin <- ''
        bankDownload <- ''
        switch(bank,
               easybank={
                       pageID <- ''
                       bankURL <- 'https://ebanking.easybank.at/InternetBanking/InternetBanking'
                       bankLogin <- paste0(bankURL, '?d=login&svc=EASYBANK&ui=html&lang=de')
               },
               ingdiba={
                       bankURL <- 'https://banking.ing-diba.at/online-banking/'
                       bankLogin <- bankURL
               },
               erste={
                       bankURL <- 'https://login.sparkasse.at/sts/oauth/authorize?response_type=token&client_id=georgeclient'
                       bankLogin <- bankURL
                       
               },
               {
                       stop('invalid bank')
               })
        
        # access website
        remDr <- NA
        if(localExecute){
                # run local selenium: docker run -d -p 4445:4444 selenium/standalone-firefox:3.4.0
                remDr <- remoteDriver(remoteServerAddr = "192.168.1.206", port = 4445L)
        } else {
                remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444L)
        }
        remDr$open(silent = TRUE)
        remDr$navigate(bankLogin)
        # remDr$screenshot(display = TRUE)
        
        # navigate to place preparing for download and get current balance
        switch(bank,
               easybank={
                       # navgiate to download
                       remDr$findElement("id", "lof5")$sendKeysToElement(list(username))
                       remDr$findElement("id", "lof9")$sendKeysToElement(list(password))
                       remDr$findElement("link text", "Login")$clickElement()
        
                       accountElement <- tryCatch({ 
                               suppressMessages({ 
                                       remDr$findElement("partial link text", account) 
                               }) }, error = function(e) { 'unknown' }) 
                       if(class(accountElement) != "webElement"){
                               stop('invalid credentials or account')
                       }
                       if(accountElement$getElementText()[[1]] != account){
                               stop('account for credentials missing')
                       }
                       
                       pageSource <- remDr$getPageSource()
                       pageDOM <- read_html(unlist(pageSource))
                       accountNode <- xml_find_first(
                               pageDOM, 
                               paste0('//*[text()[contains(.,"',
                                      account,
                                      '")]]'))
                       parentNode <- xml_parent(xml_parent(xml_parent(accountNode)))
                       for (item in 2:(length(xml_children(parentNode))-1)){
                               if(grepl(account, 
                                        as.character(xml_child(parentNode,item)))){
                                       accountNo <- item-2
                               }
                       }
                       
                       accountElement$clickElement()
                       
                       # download URL
                       currUrl <- remDr$getCurrentUrl()[[1]]
                       pageID <- tail(strsplit(currUrl, '/')[[1]],1)
                       bankDownload <- paste0(bankURL, '/', pageID)
                       
                       # get current balance
                       balanceXpath <- '//*[text()[contains(.,"Kontostand")]]/following::span'
                       balanceElement <- remDr$findElement('xpath', 
                                                           balanceXpath)$getElementText()
                       balanceList <- unlist(strsplit(unlist(balanceElement), ' '))
                       balanceValue <- str2num(balanceList[1])
                       balanceCurrency <- balanceList[2]
               },
               ingdiba={
                       # navgiate to download
                       remDr$findElement('id', 'number')$sendKeysToElement(list(username))
                       remDr$findElement('id', 'pin')$sendKeysToElement(list(password))
                       remDr$findElement('xpath', '//*[@title="Login"]')$clickElement()
                       remDr$findElement('xpath', '//*[@title="Als CSV / PDF Datei exportieren"]')$clickElement()
                       remDr$findElement('xpath', '//*[text()[contains(.,"als CSV-Datei")]]')$clickElement()
                       remDr$findElement('xpath', '//*[text()[contains(.,"Datei öffnen")]]')$clickElement()
                       
                       # download URL
                       bankDownload <- paste0(bankURL, 
                                              'wicket/wicket/page?0-1.IBehaviorListener.0-umsaetze&antiCache=',
                                              epochMS())
                       
                       # get current balance
                       balanceXpath <- '//div[h3/text()[contains(.,"Kontostand")]]/following::span'
                       balanceElement <- remDr$findElement('xpath', 
                                                           balanceXpath)$getElementText()
                       balanceValue <- str2num(unlist(balanceElement)[1])
                       balanceCurrency <- 'EUR'
               },
               erste={
                       waitFor(remDr, 'user')
                       remDr$findElement('id', 'user')$sendKeysToElement(list(username))
                       remDr$findElement('id', 'password')$sendKeysToElement(list(password))
                       remDr$findElement('id', 'submitButton')$clickElement()
                       #remDr$screenshot(display = TRUE)
                       retVal <- 'Error'
                       while(retVal == 'Error'){
                               element <- suppressMessages(try(
                                       unlist(remDr$findElement('id', 'submenu_search')$getElementAttribute('id')),
                                       silent = TRUE))
                               retVal <- substr(element[1], 1, 5)
                               if(retVal == 'Error'){
                                       element <- suppressMessages(try(
                                               unlist(remDr$findElement('xpath', '//*[@class="ico-delete-thin"]')$getElementAttribute('class')),
                                               silent = TRUE))
                                       retVal <- substr(element[1], 1, 5)
                                       if(retVal != 'Error'){
                                               remDr$findElement('xpath', '//*[@class="ico-delete-thin"]')$clickElement()
                                               retVal <- 'Error'
                                       }
                               }
                               Sys.sleep(2)
                       }
                       
                       waitFor(remDr, 'submenu_search')
                       
                       # get current balance
                       balanceXpath <- '//div[contains(@class, "balanceLabel")]'
                       balanceElement <- unlist(remDr$findElement('xpath', 
                                balanceXpath)$getElementAttribute('title'))
                       balanceValue <- str2num(tail(unlist(strsplit(
                               balanceElement, ' ')),1))
                       balanceCurrency <- 'EUR'
                       
                       
                       remDr$findElement('id', 'submenu_search')$clickElement()
                       remDr$findElement('id', 'exportLink')$clickElement()
                       remDr$findElement('xpath', '//*[text()[contains(.,"Download")]]')$clickElement()
                       bankDownload <- 'https://api.sparkasse.at/proxy/g/api/my/transactions/export.csv?'
               },
               {
                       stop('invalid bank')
               })
        
        # userAgentStr <- remDr$executeScript("return navigator.userAgent;")[[1]]
        userAgentStr <- "Mozilla/5.0 (X11; Linux x86_64; rv:54.0) Gecko/20100101 Firefox/54.0"
        cookies <- remDr$getAllCookies()
        # remDr$screenshot(display = TRUE)
        
        switch(bank,
               easybank={
                       h <- curl::new_handle()
                       curl::handle_setheaders(h,
                                'Pragma' = 'no-cache',
                                'Origin' = 'https://ebanking.easybank.at',
                                'Accept-Encoding' = 'gzip, deflate, br',
                                'Accept-Language' = 'en-US,en;q=0.8,de;q=0.6',
                                'Upgrade-Insecure-Requests' = '1',
                                'User-Agent' = userAgentStr,
                                'Content-Type' = 'application/x-www-form-urlencoded',
                                'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8',
                                'Cache-Control' = 'no-cache',
                                'Referer' = 'https://ebanking.easybank.at/',
                                'Cookie' = paste0('st=', cookies[[1]]$value),
                                'Connection' = 'keep-alive',
                                'DNT' = '1')
                       curl::handle_setopt(h, copypostfields = paste0(
                                'svc=EASYBANK&',
                                'd=transactions&',
                                'pagenumber=1&',
                                'submitflag=true&',
                                'suppressOverlay=true&',
                                'print=false&',
                                'csv=true&',
                                'accountChanged=false&',
                                'newSearch=false&',
                                'sortingColumn=BOOKING_DATE&',
                                'sortingDirection=-1&',
                                'lastviewed=&',
                                'outstandingBalance=&',
                                'searchPanelShown=false&',
                                'initialRowsPerPage=30&',
                                'konto=', accountNo, '&',
                                'datefrom=&',
                                'datetill=&',
                                'betvon=&',
                                'centsvon=&',
                                'betbis=&',
                                'centsbis=&',
                                'buchungstext=&',
                                'umsatzart=-1&',
                                'enlargementOfTransaction=0&',
                                'rowsPerPage=30'))
                       con <- curl::curl(bankDownload, handle = h)
                       result <- suppressWarnings(readLines(con))
                       close(con)
                       result <- iconv(result, from="ISO-8859-1", to="UTF-8")
               },
               ingdiba={
                       h <- curl::new_handle()
                       dfc <- do.call(rbind, 
                                      lapply(cookies, data.frame, 
                                             stringAsFactors=FALSE))
                       curl::handle_setheaders(h,
                               'Pragma' = 'no-cache',
                               'DNT' = '1',
                               'Accept-Encoding' = 'gzip, deflate, br',
                               'Accept-Language' = 'en-US,en;q=0.8,de;q=0.6',
                               'Upgrade-Insecure-Requests' = '1',
                               'User-Agent' = userAgentStr,
                               'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8',
                               'Cache-Control' = 'no-cache',
                               'Referer' = 'https://banking.ing-diba.at/online-banking/wicket/wicket/page?0',
                               'Cookie' = paste(apply(dfc[, c('name', 'value')], 1, 
                                                      paste, collapse='='), collapse='; '),
                               'Connection' = 'keep-alive')
                       con <- curl::curl(bankDownload, handle = h)
                       result <- suppressWarnings(readLines(con))
                       close(con)
                       result <- iconv(result, from="ISO-8859-1", to="UTF-8")
               },
               erste={
                       dfc <- do.call(rbind, 
                                      lapply(cookies, data.frame, 
                                             stringAsFactors=FALSE))
                       retVal <- httr::POST(paste0(bankDownload,
                                       'fields=booking,amount,reference,receiver,referenceNumber&',
                                       'from=', 
                                       as.numeric(substr(Sys.Date(), 1, 4))-2, # 2 years back
                                       '-01-01T00%3A00%3A00%2B00%3A00&',
                                       'to=', 
                                       as.numeric(substr(Sys.Date(), 1, 4))+1, # next year
                                       '-01-01T23%3A59%3A59%2B00%3A00&',
                                       'lang=de&',
                                       'separator=%2C&',
                                       'mark=%22'),
                                httr::add_headers(
                                       'Origin' = 'https://george.sparkasse.at',
                                       'Accept-Encoding' = 'gzip, deflate, br',
                                       'Accept-Language' = 'en-US,en;q=0.8,de;q=0.6',
                                       'Authorization' = paste('bearer',
                                                               as.character(dfc[dfc$name=='gt', 'value'])),
                                       # 'Cookie' = paste(apply(dfc[, c('name', 'value')], 1, 
                                       #                        paste, collapse='='), collapse='; '),
                                       'Connection' = 'keep-alive',
                                       'Content-Length' = '0',
                                       'Client-Accept-Language' = 'de',
                                       'X-REQUEST-ID' = system('cat /proc/sys/kernel/random/uuid',intern = TRUE),
                                       'Pragma' = 'no-cache',
                                       'User-Agent' = userAgentStr,
                                       'Accept' = '*/*',
                                       'Cache-Control' = 'no-cache',
                                       'Referer' = paste0('https://george.sparkasse.at/index.html?at=c&ts=',
                                                          epochMS()),
                                       'DNT' = '1'
                               ))
                       retVal <- rawToChar(retVal$content[c(TRUE,FALSE)][-1])
                       retVal <- iconv(retVal, from="ISO-8859-1", to="UTF-8")
                       con <- textConnection(retVal)
                       result <- read.csv(con)
               },
               {
                       stop('invalid bank')
               })
        
        # logout
        switch(bank,
               easybank={
                        remDr$findElement("link text", "Logout")$clickElement()
               },
               ingdiba={
                       remDr$findElement('xpath', '//*[@title="Logout"]')$clickElement()
               },
               erste={
                       remDr$findElement('id', 'submenu_settings')$clickElement()
                       remDr$findElement('id', 'menu_logout')$clickElement()
               },
               {
                       stop('invalid bank')
               })
        remDr$close()
  
        balance <- list(balance  = balanceValue,
                        date     = as.character(Sys.Date()),
                        currency = balanceCurrency)
}

# convert result to JSON
switch(bank,
       easybank={
               bookings <- lapply(result, eb_parser)
       },
       ingdiba={
               bookings <- lapply(result, idb_parser)
       },
       erste={
               bookings <- erste_parser(result)
       },
       {
               stop('invalid bank')
       })
jsonResult <- {}
bookings <- Filter(length, bookings)
if(download){
        jsonResult <- bookings
        jsonResult <- list(
                balance  = list(balance),
                bookings = bookings)
} else {
        jsonResult <- bookings
}
toJSON(jsonResult, pretty = TRUE, auto_unbox = TRUE)
