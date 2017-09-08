# Infos:
# - https://ropensci.org/tutorials/rselenium_tutorial.html
# - http://rpubs.com/johndharrison/RSelenium-Docker
# - https://hub.docker.com/u/selenium/
#
# $ apt-get update
# $ apt-get install libssl-dev libcurl4-openssl-dev phantomjs libxml2-dev
# install.packages("devtools")
# devtools::install_github("ropensci/RSelenium")

# usage
# Rscript autoLoad.R '{"bank":"easybank","username":"123456","password":"12345geheim","account":"AT121420020010123456"}'
# Rscript autoLoad.R '{"bank":"ingdiba","username":"123456","password":"12345geheim"}'

library(RSelenium)
library(methods)
library(jsonlite)

source('/srv-bank/helper.R')
source('/srv-bank/easybank.R')
source('/srv-bank/ingdiba.R')

# read credentials from command line or stdin
args <- commandArgs(trailingOnly=TRUE)
if (length(args) == 0){
        myStdin <- file("stdin")
        input <- suppressWarnings(readLines(myStdin))
        close(myStdin)
} else {
        input <- args[1]
}

bank <- ''
username <- ''
password <- ''
account <- ''

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
       {
               stop('invalid bank')
       })

# access website
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444L)
# run local selenium: docker run -d -p 4445:4444 selenium/standalone-firefox:3.4.0
#remDr <- remoteDriver(remoteServerAddr = "192.168.1.206", port = 4445L)
remDr$open(silent = TRUE)
remDr$navigate(bankLogin)
# remDr$screenshot(display = TRUE)
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
               accountElement$clickElement()
               
               # download URL
               currUrl <- remDr$getCurrentUrl()[[1]]
               pageID <- tail(strsplit(currUrl, '/')[[1]],1)
               bankDownload <- paste0(bankURL, '/', pageID)
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
               

       },
       {
               stop('invalid bank')
       })

# userAgentStr <- remDr$executeScript("return navigator.userAgent;")[[1]]
userAgentStr <- "Mozilla/5.0 (X11; Linux x86_64; rv:54.0) Gecko/20100101 Firefox/54.0"
cookies <- remDr$getAllCookies()
# remDr$screenshot(display = TRUE)

h <- curl::new_handle()
switch(bank,
       easybank={
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
               curl::handle_setopt(h, copypostfields = "svc=EASYBANK&d=transactions&pagenumber=1&submitflag=true&suppressOverlay=true&print=false&csv=true&accountChanged=false&newSearch=false&sortingColumn=BOOKING_DATE&sortingDirection=-1&lastviewed=&outstandingBalance=&searchPanelShown=false&initialRowsPerPage=30&konto=0&datefrom=&datetill=&betvon=&centsvon=&betbis=&centsbis=&buchungstext=&umsatzart=-1&enlargementOfTransaction=0&rowsPerPage=30")
       },
       ingdiba={
               dfc <- do.call(rbind, lapply(cookies, data.frame, stringAsFactors=FALSE))
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
       },
       {
               stop('invalid bank')
       })
con <- curl::curl(bankDownload, handle = h)
result <- suppressWarnings(readLines(con))
close(con)

# logout
switch(bank,
       easybank={
                remDr$findElement("link text", "Logout")$clickElement()
       },
       ingdiba={
               remDr$findElement('xpath', '//*[@title="Logout"]')$clickElement()
       },
       {
               stop('invalid bank')
       })
remDr$close()

# convert result to JSON
result <- iconv(result, from="ISO-8859-1", to="UTF-8")
jsonResult <- {}
switch(bank,
       easybank={
               jsonResult <- lapply(result, eb_parser)
       },
       ingdiba={
               jsonResult <- lapply(result, idb_parser)
       },
       {
               stop('invalid bank')
       })
toJSON(Filter(length, jsonResult), pretty = TRUE, auto_unbox = TRUE)
