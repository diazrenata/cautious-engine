library(shiny)

all_sims <- read.csv(here::here("app", "all_sims.csv"))

all_states <- sort(c("All", unique(all_sims$regionname)))
all_route_names <- sort(unique(all_sims$routename))

#all_routes <- unique(all_sims$matssname)

ui <- navbarPage(
  title = "Birds app",
  tabPanel(title = "Map of routes",
           sidebarPanel("All routes possible",
                        br(),
                        br(),
                        "Select a state from below:",
                        selectInput("state", "State:", all_states, multiple = T),
                        "Select a route from below:",
                        selectInput("routename", "Route name:", all_route_names, multiple = F))),
  tabPanel("Route-level results",
           sidebarPanel("All routes possible",
                        br(),
                        br(),
                        "Select a state from below:",
                        selectInput("state", "State:", all_states, multiple = T),
                        "Select a route from below:",
                        selectInput("routename", "Route name:", all_route_names, multiple = F),
                        "Variables to show",
                        br(),
                        br(),
                        "Show real (estimated) data?",
                        radioButtons("showEst", "Show estimated data?", choices = c("Yes", "No"), selected = "Yes"), 
                        "Show null model data?",
                        radioButtons("showNull", "Show null model data?", choices = c("Yes", "No"), selected = "No"), 
                        "Show real (estimated) data?",
                        radioButtons("showFitted", "Show fitted data?", choices = c("Yes", "No"), selected = "No"))
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)