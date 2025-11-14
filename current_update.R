#Loading libraries
library(tidyverse)
library(jsonlite)
library(rvest)
library(DatawRappr)

#Loading API key, chart keys
api_key <- Sys.getenv("API_KEY")
currentChart <- Sys.getenv("CURRENT_KEY")


datawrapper_auth(api_key =  api_key, overwrite=TRUE)

##Parse html function
parse_table_rows <- function(page) {
  rows <- page %>% html_elements("tr")
  table_data <- map(rows, function(row) {
    cells <- row %>% html_elements("td")
    td_text <- cells %>% html_text(trim = TRUE)
    hint_title5 <- cells[5] %>%
      html_element("span.hint") %>%
      html_attr("title")
    c(td_text, airport_name = hint_title5)
  })
  df <- map_dfr(table_data, ~as.data.frame(t(.x), stringsAsFactors = FALSE))
  names(df) <- c("Canceled", "% C", "Delayed", "% D", "Airport", "airport_name")
  df <- df[-c(1,2),]
  df <- df %>%
    select(-airport_name)
}

  
##Working on the Airline updates
airports <- c('Delta Air Lines|Southwest|American Airlines|United')

airportNow_html <- read_html('https://www.flightaware.com/ajax/airport/cancelled_count.rvt?type=airline&timePeriod=today&airportFilter=')
airportYest_html <- read_html('https://www.flightaware.com/ajax/airport/cancelled_count.rvt?type=airline&timePeriod=yesterday&airportFilter=')

  
airportNow <- parse_table_rows(airportNow_html)
airportYest <- parse_table_rows(airportYest_html)

  
airportNow <- airportNow %>%
  filter(grepl(airports, Airport)) %>%
  filter(!grepl('Rouge', Airport)) %>%
  arrange(Airport) %>%
  rename(Airline = Airport)

airportYest <- airportYest %>%
  filter(grepl(airports, Airport)) %>%
  filter(!grepl('Rouge', Airport)) %>%
  arrange(Airport) 

#Pulling today with time
today_now <- format(as.POSIXct(Sys.time(), tz = "America/New_York"), "%b. %d at %I:%M %p %Z")
today_now <- sub(" at 0", " at ", today_now)
today_now <- gsub("AM", "a.m.", today_now)
today_now <- gsub("PM", "p.m.", today_now)

today_head <- format(as.POSIXct(Sys.time(), tz = "America/New_York"), "%b. %d")


##Scraping
total_html <- read_html('https://www.flightaware.com/live/cancelled/today')

totals <- total_html %>%
  html_elements("div[style='float: left; display: inline-block; max-width: 74%'] h3") %>%
  html_text(trim = TRUE)

totals <- data.frame(totals) %>%
  separate(totals, into = c("key", "value"), sep = ":") %>%
  mutate(
    key = trimws(key),
    value = trimws(value)
  ) %>%
  pivot_wider(names_from = key, values_from = value)


##Datawrapper chart
dw_edit_chart(
  chart_id = currentChart,
  title = paste('Flight delays, cancellations in the U.S. on',today_head),
  intro = paste0("<span style='font-size: 18px;line-height: 26px;'><b>
Total Delays:</b>", totals$`Total delays within, into, or out of the United States today`,"<br><b>Total Cancellations:</b>",totals$`Total cancellations within, into, or out of the United States today`,"</span>"),
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'Flight Aware',
  source_url = 'flightaware.com',
  annotate = paste("<i>Data as of ",today_now,".</i> Total flight delays and cancellations include all flights within, into or out of the U.S.", sep='')
)

#Adding data to the chart
dw_data_to_chart(airportNow,
                 chart_id = currentChart
)

#Republishing the chart
dw_publish_chart(currentChart)


##Trying to scrape
