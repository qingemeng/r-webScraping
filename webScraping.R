library(rvest)
library(magrittr)

#Eg. 1
URL<-"https://scistarter.com/finder?phrase=&lat=&lng=&activity=At%20the%20beach&topic=&search_filters=&search_audience=&page=1#view-projects"

scistarter_html <- read_html(URL)

b=scistarter_html%>%html_nodes("a")%>%head()

a=scistarter_html%>%html_nodes("div")

scistarter_html %>%
  html_nodes("table") %>%
  html_table() %>%
  "["(1) %>% str()

c=scistarter_html %>%
  html_nodes("table") %>%
  html_table()

title = scistarter_html %>%
  html_nodes("div#project-listing") %>% #filter to the projec-listing div
  html_nodes("h3") %>%                  # get the headings
  html_text() %>%                       #get the text, not the HTML tags
  gsub("^\\s+|\\s+$", "", .)

page_list <- scistarter_html %>%
  html_nodes("td") %>%                  # get the headings
  html_text() %>%                       #get the text, not the HTML tags
  gsub("^\\s+|\\s+$", "", .)

goals <- page_list[seq(1, 30, 3)]
tasks <- page_list[seq(2, 30, 3)]
locations = page_list[seq(3, 30, 3)]

scistarter_df <- data.frame(title, goals, tasks, locations)

#Eg. 2
URL2 <- "https://www.nis.gov.kh/cpi/Apr14.html"

# TIP: When debugging or building your scraper, assign a variable to the raw HTML.
# That way you only have to read it once
accounts <- read_html(URL2) 

table <- accounts %>%
  html_nodes("table") %>%
  html_table(header=T)

dict <- table[[1]][,1:2]
accounts_df <- table[[1]][6:18,-1]


#Eg. 3
library(lubridate)
years <- c(2016) # edit for the year(s) you want
months <- c(1:2)

for (y in years) {
  for (m in months) {
    if (m == 4 || m==6 || m==9 || m==11) {
      days <- c(1:30) # Apr, Jun, Sep, Nov have 30
    } else if (m == 2 && y %% 4 == 0 ) {
      days <- c(1:29) # leap year
    } else if (m == 2 && y %% 4 != 0 ) {
      days <- c(1:28) # non leap year Febs
    } else {
      days <- c(1:31) # All the rest have 31 days 
    }
    for (d in days) {
    #for (d in 1) {
      URL <- paste0("https://www.wunderground.com/history/airport/WSSS/", 
                    y, "/", 
                    m, "/",
                    d, "/DailyHistory.html?req_city=Singapore&req_statename=Singapore")
      print(URL) # try this to test before running the script
      
      raw <- read_html(URL)
      
      max <- raw %>% 
        html_nodes(xpath='//*[@id="historyTable"]/tbody/tr[3]/td[2]/span/span[1]')  %>%
        html_text() %>% as.numeric()
      min <- raw %>%
        html_nodes(xpath='//*[@id="historyTable"]/tbody/tr[4]/td[2]/span/span[1]') %>%
        html_text() %>% as.numeric()
      
      date <- ymd(paste(y,m,d, sep="/"))
      record <- data.frame(cbind(as.character(date), min, max))
      
      if ( date == "2016-01-01") {
        sing_temp <- record
      } else {
        sing_temp <- rbind(sing_temp, record)
      }
    }
  }
}


#Eg. 4
url <- "http://there100.org/companies"

x <- scan(url, what="", sep="\n")
start <- grep("The IKEA Group", x) # start of companies
end <- grep("S.p.A is an Italian", x) # end of companies 
sub <- x[start:end]
sel <- grep("jpg|png", sub)
co.names <- sub[sel+1]
# add back in bank of america
bofa <- grep("Bank of America", x)
co.names <- c(co.names, x[start], bofa)
co.names <- gsub("<.*?>", "", co.names)

write.csv(co.names, "RE100_2016.csv", row.names=F)


