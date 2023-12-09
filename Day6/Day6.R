library(stringr)

puzzle <- 2

connection <- file(description = "Day6Input.txt", open = "r")
races <- str_split(readLines(connection), "\\s+")
times <- as.integer(races[[1]][2:length(races[[1]])])
if (puzzle == 2) { times <- c(as.integer(paste(times, collapse = ""))) }
distances <- as.integer(races[[2]][2:length(races[[1]])])
if (puzzle == 2) { distances <- c(as.numeric(paste(distances, collapse = ""))) }
close(connection)

prod(sapply(1:length(times), function(x) {
  time <- times[x]
  distance <- distances[x]
  
  first_time <- 1
  while (T) {
    if ((time - first_time) * first_time > distance) { break }
    else { first_time <- first_time + 1 }
  }
  last_time <- time - 1
  while (T) {
    if ((time - last_time) * last_time > distance) { break }
    else { last_time <- last_time - 1 }
  }
  return(last_time - first_time + 1)
}))
