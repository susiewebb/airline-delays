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
totalChart <- Sys.getenv("TOTAL_KEY")


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
airports <- '(^Delta Air Lines$)|(^Southwest$)|(^American Airlines$)|(^United$)'

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


##Pulling data as of
today <- format(as.POSIXct(floor_date(Sys.time(), 'hour'), tz = "America/New_York"), "%b. %d at %I %p %Z")
today <- sub(" at 0", " at ", today)
today <- gsub("AM", "a.m.", today)
today <- gsub("PM", "p.m.", today)

##Adding yesterday's numbers

if (!yesterday %in% colnames(canceled)){
  
  canceled_new <- canceled %>%
    left_join(
      airportYest %>%
        select(Airport, Canceled) %>%
        rename(!!yesterday := Canceled),
      by = "Airport"
    )
  
  canceled_per_new <- canceled_per %>%
    left_join(
      airportYest %>%
        select(Airport, `% C`) %>%
        rename(!!yesterday := `% C`),
      by = "Airport"
    )
  
  delayed_new <- delayed %>%
    left_join(
      airportYest %>%
        select(Airport, Delayed) %>%
        rename(!!yesterday := Delayed),
      by = "Airport"
    )
  
  delayed_per_new <- delayed_per %>%
    left_join(
      airportYest %>%
        select(Airport, `% D`) %>%
        rename(!!yesterday := `% D`),
      by = "Airport"
    )

##Resaving the csv
  write_csv(canceled_new, 'toupdate/airline_canceled.csv')
  write_csv(canceled_per_new, 'toupdate/airline_canceled_per.csv')
  write_csv(delayed_new, 'toupdate/airline_delayed.csv')
  write_csv(delayed_per_new,'toupdate/airline_delayed_per.csv')
} else{
  canceled_new <- canceled
  canceled_per_new <- canceled_per
  delayed_new <- delayed
  delayed_per_new <- delayed_per
  }


##Updating the datawrapper chart

#####Canceled chart
#Editing the chart
dw_edit_chart(
  chart_id = canceledChart,
  title = paste('Canceled flights by airline each day'),
  intro = 'Click the icons below to see total flight delays/cancellations by each airline, and the percent of total airline flights affected each day.<br>
<a target="_self" href="https://datawrapper.dwcdn.net/O8gS2/" style="background-color:#3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:4px;  opacity: 100%; " rel="nofollow noopener"> Total Canceled</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/vCOr8/" style="background-color:#3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:4px;  opacity: 50%; " rel="nofollow noopener">% Canceled</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/YH2ZW/" style="background-color: #3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:12px;margin-bottom:9px;  opacity: 50%;rel="nofollow noopener">Total Delayed</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/wSryL/" style="background-color: #3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:12px;margin-bottom:9px; opacity: 50%; rel="nofollow noopener">% Delayed</a>',
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'Flight Aware',
  source_url = 'flightaware.com',
  annotate = paste("<i>Data as of ",today,". Any gaps in the data are because that airline was not among the worst 100 airlines for cancellations that day.</i>", sep='')
)

#Adding data to the chart
dw_data_to_chart(canceled_new,
                 chart_id = canceledChart
)

#Republishing the chart
dw_publish_chart(canceledChart)


####Canceled percent chart
#Editing the chart
dw_edit_chart(
  chart_id = canceledPerChart,
  title = paste('Percent of canceled flights by airline each day'),
intro = 'Click the icons below to see total flight delays/cancellations by each airline, and the percent of total airline flights affected each day.<br>
<a target="_self" href="https://datawrapper.dwcdn.net/O8gS2/" style="background-color:#3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:4px;  opacity: 50%; " rel="nofollow noopener"> Total Canceled</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/vCOr8/" style="background-color:#3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:4px;  opacity: 100%; " rel="nofollow noopener">% Canceled</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/YH2ZW/" style="background-color: #3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:12px;margin-bottom:9px;  opacity: 50%;rel="nofollow noopener">Total Delayed</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/wSryL/" style="background-color: #3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:12px;margin-bottom:9px; opacity: 50%; rel="nofollow noopener">% Delayed</a>',
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'Flight Aware',
  source_url = 'flightaware.com',
  annotate = paste("<i>Data as of ",today,". Any gaps in the data are because that airline was not among the worst 100 airlines for cancellations that day.</i>", sep='')
)

#Adding data to the chart
dw_data_to_chart(canceled_per_new,
                 chart_id = canceledPerChart
)

#Republishing the chart
dw_publish_chart(canceledPerChart)


####Delayed chart
#Editing the chart
dw_edit_chart(
  chart_id = delayedChart,
  title = paste('Delayed flights by airline each day'),
  intro = 'Click the icons below to see total flight delays/cancellations by each airline, and the percent of total airline flights affected each day.<br>
<a target="_self" href="https://datawrapper.dwcdn.net/O8gS2/" style="background-color:#3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:4px;  opacity: 50%; " rel="nofollow noopener"> Total Canceled</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/vCOr8/" style="background-color:#3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:4px;  opacity: 50%; " rel="nofollow noopener">% Canceled</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/YH2ZW/" style="background-color: #3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:12px;margin-bottom:9px;  opacity: 100%;rel="nofollow noopener">Total Delayed</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/wSryL/" style="background-color: #3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:12px;margin-bottom:9px; opacity: 50%; rel="nofollow noopener">% Delayed</a>',
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'Flight Aware',
  source_url = 'flightaware.com',
  annotate = paste("<i>Data as of ",today,". Any gaps in the data are because that airline was not among the worst 100 airlines for cancellations that day.</i>", sep='')
)

#Adding data to the chart
dw_data_to_chart(delayed_new,
                 chart_id = delayedChart
)

#Republishing the chart
dw_publish_chart(delayedChart)


###Delayed percent chart
#Editing the chart
dw_edit_chart(
  chart_id =delayedPerChart,
  title = paste('Percent of delayed flights by airline each day'),
  intro = 'Click the icons below to see total flight delays/cancellations by each airline, and the percent of total airline flights affected each day.<br>
<a target="_self" href="https://datawrapper.dwcdn.net/O8gS2/" style="background-color:#3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:4px;  opacity: 50%; " rel="nofollow noopener"> Total Canceled</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/vCOr8/" style="background-color:#3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:4px;  opacity: 50%; " rel="nofollow noopener">% Canceled</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/YH2ZW/" style="background-color: #3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:12px;margin-bottom:9px;  opacity: 50%;rel="nofollow noopener">Total Delayed</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/wSryL/" style="background-color: #3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:12px;margin-bottom:9px; opacity: 100%; rel="nofollow noopener">% Delayed</a>',
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'Flight Aware',
  source_url = 'flightaware.com',
  annotate = paste("<i>Data as of ",today,". Any gaps in the data are because that airline was not among the worst 100 airlines for cancellations that day.</i>", sep='')
)

#Adding data to the chart
dw_data_to_chart(delayed_per_new,
                 chart_id = delayedPerChart
)

#Republishing the chart
dw_publish_chart(delayedPerChart)


##Now pulling the total updates
totals_old <- read_csv('toupdate/totals.csv')


if (!(yesterday %in% totals_old$date)) {
  total_html <- read_html('https://www.flightaware.com/live/cancelled/yesterday')
  
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
    
  totals_new <- rbind(totals_old, 
                      data.frame(
                        date = yesterday, 
                        total_delays = totals$`Total delays within, into, or out of the United States yesterday`, 
                        total_cancellations = totals$`Total cancellations within, into, or out of the United States yesterday`))
  #Getting rid of any commas
  totals_new$total_delays <- trimws(gsub(",", "", totals_new$total_delays))
  totals_new$total_cancellations <- trimws(gsub(",", "", totals_new$total_cancellations))

  write_csv(totals_new, 'toupdate/totals.csv')

} else{
  totals_new <- totals_old
  }

##Datawrapper chart!!!
dw_edit_chart(
  chart_id = totalChart,
  title = 'U.S. air travel delays and cancellations',
  intro = 'Below are the total daily delays and cancellations for all flights within, into or out of the U.S.',
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'Flight Aware',
  source_url = 'flightaware.com',
  annotate = paste0("<i>Data as of ",today,".")
)

#Adding data to the chart
  dw_data_to_chart(totals_new,
                   chart_id = totalChart
  )

#Republishing the chart
dw_publish_chart(totalChart)

