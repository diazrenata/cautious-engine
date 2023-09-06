library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)

#runs_dat <- read.csv('runs.csv')
runs_dat <- read.csv(here::here('app', 'runs.csv'))

runs_dates <- runs_dat %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         Weekday = weekdays(Date))

run_days <- unique(select(runs_dates, Date, Weekday))

week_starts <- data.frame(Date = seq(min(run_days$Date) - 6, max(run_days$Date), by = "days")) %>%
  mutate(Weekday = weekdays(Date)) %>% 
  filter(Weekday == "Monday") %>%
  mutate(Week = row_number())
week_ends <-  data.frame(Date = seq(min(run_days$Date), max(run_days$Date) + 6, by = "days")) %>%
  mutate(Weekday = weekdays(Date)) %>% 
  filter(Weekday == "Sunday") %>%
  mutate(Week = row_number()) 
week_numbers <- data.frame(
  Week = week_starts$Week,
  Week_start = week_starts$Date,
  Week_end  = week_ends$Date
)

run_week_numbers <- left_join(runs_dates, week_numbers, by = join_by(between(Date, Week_start, Week_end)))

cumulative_week_miles <- run_week_numbers %>%
  group_by(Week) %>%
  summarize(Miles = sum(Distance))


today <- as.Date(Sys.Date())

days_since_today <- data.frame(
  Date = seq(min(runs_dates$Date), today, by = "days")) %>%
  mutate(Week = ceiling(as.numeric(Date - today) / 7) + ceiling(nrow(.) / 7))

running_week_starts <- days_since_today %>% 
  group_by(Week) %>%
  summarize(Week_start = min(Date),
            Week_end = max(Date)) %>%
  ungroup()

running_week_numbers <- left_join(runs_dates, running_week_starts, by = join_by(between(Date, Week_start, Week_end)))

running_cumulative_week_miles <- running_week_numbers %>%
  group_by(Week) %>%
  summarize(Miles = sum(Distance))




ui <- fluidPage(
  titlePanel("Cumulative weekly distance"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "week_type", label = "Week Start", choices = c("7 days ago", "Calendar week"), selected = "7 days ago")
    ),
    mainPanel(
      plotOutput(outputId = "mileagePlot"),
      textOutput(outputId = "mileageText")
    )
  )
)

server <- function(input, output, session) {
  
  
  makePlot <- reactive(
    if(input$week_type == "Calendar week") {
      ggplot(cumulative_week_miles, aes(Week, Miles)) + geom_line() + geom_point()
    } else if(input$week_type == "7 days ago") {
      ggplot(running_cumulative_week_miles, aes(Week, Miles)) + geom_line() + geom_point()
      
    }
  )
  
  
  
  
  output$mileagePlot <- renderPlot(makePlot())
  
}





shinyApp(ui, server)

