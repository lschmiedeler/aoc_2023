library(stringr)
library(dplyr)

puzzle <- 2

if (puzzle == 1) { limit <- 0 } else { limit <- -10 }

connection <- file(description = "Day3Input.txt", open = "r")
schematic <- unname(sapply(readLines(connection), function(x) { 
  row <- str_split(x, "")[[1]]
  row <- str_replace_all(row, pattern = "\\.", replacement = "10")
  row <- str_replace_all(row, pattern = "\\*", replacement = "-0")
  row <- str_replace_all(row, pattern = "[^[0-9]]", replacement = "-10")
  return(as.numeric(row))
}))
close(connection)

nrows <- nrow(schematic)
ncols <- ncol(schematic)

check_left <- function(x) { return(x %% nrows > 1) }
check_right <- function(x) { return(x %% nrows != 0) }
check_up <- function(x) { return(x > nrows) }
check_down <- function(x) { return(x <= ncols * nrows - nrows) }

check_for_symbol <- function(x) {
  if (check_left(x)) { if (schematic[x - 1] < limit) { return(x-1) } }
  if (check_right(x)) { if (schematic[x + 1] < limit) { return(x+1) } }
  if (check_up(x)) { if (schematic[x - nrows] < limit) { return(x - nrows) } }
  if (check_down(x)) { if (schematic[x + nrows] < limit) { return(x + nrows) } }
  if (check_left(x) & check_up(x)) { if (schematic[x - (nrows - 1)] < limit) { return(x - (nrows - 1)) } }
  if (check_left(x) & check_down(x)) { if (schematic[x + (nrows - 1)] < limit) { return(x + (nrows - 1)) } }
  if (check_right(x) & check_up(x)) { if (schematic[x - (nrows + 1)] < limit) { return(x - (nrows + 1)) } }
  if (check_right(x) & check_down(x)) { if (schematic[x + (nrows + 1)] < limit) { return(x + (nrows + 1)) } }
  return(0)
}

part_numbers <- sapply(seq(1, nrows * ncols), function(x) {
  if (schematic[x] >= 0 & schematic[x] < 10) { 
    num_start <- F
    if (x %% nrows == 1) { num_start <- T }
    else if (schematic[x-1] < 0 | schematic[x-1] > 9) { num_start <- T }
    
    if (num_start) {
      number <- c(schematic[x])
      i <- 1
      while (T) {
        if (schematic[x+i] >= 0 & schematic[x+i] < 10) { number <- append(number, schematic[x+i])}
        else { break }
        i <- i + 1
      }
      symbol_i <- sapply(seq(0, i - 1), function(y) { return(check_for_symbol(x+y)) })
      if (sum(symbol_i > 0)) { 
        info <- c(as.numeric(paste0(number, collapse = "")), max(symbol_i))
        return(info)
      }
    }
  }
  return(0)
})

part_numbers_df <- data.frame(unname(t(data.frame(part_numbers)))) %>% filter(X2 != 0)

if (puzzle == 1) { sum(part_numbers_df$X1) } else {
  sum(part_numbers_df %>% group_by(X2) %>% summarize(n = n(), gear_ratio = prod(X1)) %>% ungroup() %>%
        filter(n == 2) %>% select(gear_ratio))
}
