library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(roll)
theme_set(theme_bw())


runs_dat <- read.csv('runs.csv')
#runs_dat <- read.csv(here::here('app', 'runs.csv'))

runs_dat <- runs_dat %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))

last_date <-  Sys.Date() %>% as.Date(format = "%m/%d/%Y")

runs_dates <- data.frame(Date = seq(min(runs_dat$Date), last_date, by = "day")) %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),
    Weekday = weekdays(Date),
    WeekNumber = isoweek(Date)
  ) %>%
  left_join(runs_dat) %>%
  mutate(Distance = replace_na(Distance, 0)) %>%
  mutate(LimitCalendar = 11 * (1.1 ^ (WeekNumber - 33))) %>%
  group_by(WeekNumber) %>%
  mutate(CalendarCumulative = cumsum(Distance)) %>%
  ungroup() %>%
  mutate(RollingCumulative = roll_sum(Distance, width = 7, min_obs = 1))



ui <- fluidPage(titlePanel("Cumulative weekly distance"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      inputId = "week_type",
                      label = "Week Start",
                      choices = c("7 days ago", "Calendar week"),
                      selected = "7 days ago"
                    )
                  ),
                  mainPanel(
                    plotOutput(outputId = "mileagePlot"),
                    textOutput(outputId = "mileageText")
                  )
                ))

server <- function(input, output, session) {
  makePlot <- reactive(if (input$week_type == "Calendar week") {

    ggplot(runs_dates, aes(Date, CalendarCumulative)) + 
      geom_col(fill = "blue") + 
       geom_col(aes(y = Distance), alpha = .5, fill = "lightblue") +
      geom_line(aes(y = LimitCalendar), color = "red")
  } else if (input$week_type == "7 days ago") {
    ggplot(runs_dates, aes(Date, RollingCumulative)) + 
      geom_col(fill = "blue") + 
      geom_col(aes(y = Distance), alpha = .5, fill = "lightblue") +
      geom_line(aes(y = LimitCalendar), color = "red")

  })
  
  makeText <- reactive(if (input$week_type == "Calendar week") {

    final_week <- filter(runs_dates, WeekNumber == max(runs_dates$WeekNumber))
    
    paste0(
      "For the week starting Monday ",
      min(final_week$Date),
      " cumulative mileage is ",
      sum(final_week$Distance),
      "."
    )
  } else if (input$week_type == "7 days ago") {
    paste0(
      "For the past 7 days counting backwards from ",
      runs_dates$Date[nrow(runs_dates)],
      " cumulative mileage is ",
      runs_dates$RollingCumulative[nrow(runs_dates)],
      "."
    )
    
  })
  
  
  
  
  output$mileagePlot <- renderPlot(makePlot())
  
  output$mileageText <- renderText(makeText())
  
}





shinyApp(ui, server)
