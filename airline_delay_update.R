#Loading libraries
library(tidyverse)
library(jsonlite)
library(rvest)
library(DatawRappr)

#Loading API key, chart keys
api_key <- Sys.getenv("API_KEY")
canceledChart <- Sys.getenv("CANCELED_KEY")
canceledPerChart <- Sys.getenv("CANCELEDPER_KEY")
delayedChart <- Sys.getenv("DELAYED_KEY")
delayedPerChart <- Sys.getenv("DELAYEDPER_KEY")


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
  arrange(Airport)

airportYest <- airportYest %>%
  filter(grepl(airports, Airport)) %>%
  filter(!grepl('Rouge', Airport)) %>%
  arrange(Airport)


##Uploading base data for datawrapper
canceled <- read_csv('toupdate/airline_canceled.csv')
canceled_per <- read_csv('toupdate/airline_canceled_per.csv')
delayed <- read_csv('toupdate/airline_delayed.csv')
delayed_per <- read_csv('toupdate/airline_delayed_per.csv')

##Pulling yesterday's date
yesterday <- format(as.Date(with_tz(Sys.time(), tz = 'America/New_York')) - 1, "%b. %d")
yesterday

##Pulling data as of
today <- format(as.POSIXct(round_date(Sys.time(), 'hour'), tz = "America/New_York"), "%b. %d at %I %p %Z")
today <- sub(" at 0", " at ", today)
today <- gsub("AM", "a.m.", today)
today <- gsub("PM", "p.m.", today)
today

##Adding yesterday's numbers
canceled <- canceled %>%
  left_join(
    airportYest %>%
      select(Airport, Canceled) %>%
      rename(!!yesterday := Canceled),
    by = "Airport"
  )

canceled_per <- canceled_per %>%
  left_join(
    airportYest %>%
      select(Airport, `% C`) %>%
      rename(!!yesterday := `% C`),
    by = "Airport"
  )

delayed <- delayed %>%
  left_join(
    airportYest %>%
      select(Airport, Delayed) %>%
      rename(!!yesterday := Delayed),
    by = "Airport"
  )

delayed_per <- delayed_per %>%
  left_join(
    airportYest %>%
      select(Airport, `% D`) %>%
      rename(!!yesterday := `% D`),
    by = "Airport"
  )

##Updating the data for tomorrow
write_csv(canceled, 'to_update/airline_canceled.csv')
write_csv(canceled_per, 'to_update/airline_canceled_per.csv')
write_csv(delayed, 'to_update/airline_delayed.csv')
write_csv(delayed_per, 'to_update/airline_delayed_per.csv')

##Updating the datawrapper chart

#####Canceled chart
#Editing the chart
dw_edit_chart(
  chart_id = canceledChart,
  title = paste('Canceled flights by airline each day'),
  intro = 'Click the icons below to see total flight delays/cancellations by each airline, and the percent of total airline flights affected each day.',
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'Flight Aware',
  source_url = 'flightaware.com',
  annotate = paste("<i>Data as of ",today,".</i>", sep='')
)

#Adding data to the chart
dw_data_to_chart(canceled,
                 chart_id = canceledChart
)

#Republishing the chart
dw_publish_chart(canceledChart)


####Canceled percent chart
#Editing the chart
dw_edit_chart(
  chart_id = canceledPerChart,
  title = paste('Percent of canceled flights by airline each day'),
  intro = 'Click the icons below to see total flight delays/cancellations by each airline, and the percent of total airline flights affected each day.',
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'Flight Aware',
  source_url = 'flightaware.com',
  annotate = paste("<i>Data as of ",today,".</i>", sep='')
)

#Adding data to the chart
dw_data_to_chart(canceled_per,
                 chart_id = canceledPerChart
)

#Republishing the chart
dw_publish_chart(canceledPerChart)


####Delayed chart
#Editing the chart
dw_edit_chart(
  chart_id = delayedChart,
  title = paste('Delayed flights by airline each day'),
  intro = 'Click the icons below to see total flight delays/cancellations by each airline, and the percent of total airline flights affected each day.',
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'Flight Aware',
  source_url = 'flightaware.com',
  annotate = paste("<i>Data as of ",today,".</i>", sep='')
)

#Adding data to the chart
dw_data_to_chart(delayed,
                 chart_id = delayedChart
)

#Republishing the chart
dw_publish_chart(delayedChart)


###Delayed percent chart
#Editing the chart
dw_edit_chart(
  chart_id =delayedPerChart,
  title = paste('Percent of delayed flights by airline each day'),
  intro = 'Click the icons below to see total flight delays/cancellations by each airline, and the percent of total airline flights affected each day.',
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'Flight Aware',
  source_url = 'flightaware.com',
  annotate = paste("<i>Data as of ",today,".</i>", sep='')
)

#Adding data to the chart
dw_data_to_chart(delayed_per,
                 chart_id = delayedPerChart
)

#Republishing the chart
dw_publish_chart(delayedPerChart)

