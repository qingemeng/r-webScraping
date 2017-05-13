temp_day <- function(y, m, d, sing_temp){
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