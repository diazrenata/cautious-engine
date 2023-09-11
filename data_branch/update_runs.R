update_runs <- function(mileage = 10, date = format(Sys.Date(), "%m/%d/%Y"), minutes = NA, seconds = NA) {
  
  runs <- read.csv("runs.csv")
  
  new_run <- data.frame(Date = date, Minutes = minutes, Seconds = seconds, Distance = mileage)
  
  runs <- rbind(runs, new_run)
  
  write.csv(runs, "runs.csv", row.names = F)
}

#update_runs()
