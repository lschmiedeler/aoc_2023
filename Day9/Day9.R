library(stringr)

puzzle <- 2

connection <- file(description = "Day9Input.txt", open = "r")
report <- readLines(connection)
close(connection)

sum(sapply(report, function(line) {
  history <- as.integer(str_split(line, "\\s+")[[1]])
  if (puzzle == 1) { value <- history[length(history)] }
  else { 
    value <- history[1]
    i <- 1
  }
  while (T) {
    history <- diff(history)
    if (puzzle == 1) { value <- value + history[length(history)] }
    else { 
      if (i %% 2 == 1) { value <- value - history[1]  }
      else { value <- value + history[1]  }
      i <- i + 1
    }
    if (sum(history == 0) == length(history)) { break }
  }
  return(value)
}))
