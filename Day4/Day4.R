library(stringr)

puzzle <- 2

connection <- file(description = "Day4Input.txt", open = "r")
cards <- readLines(connection)
close(connection)

find_copies <- function(num) {
  matches <- all_matches[[num]]
  if (length(matches) == 1) { return(num) }
  else {
    c(num, unlist(sapply(matches[2:length(matches)], function(x) { find_copies(x) })))
  }
}

all_matches <- unname(sapply(cards, function(card) {
  card_split <- str_split(card, ":|\\|")
  card_num <- as.integer(str_extract(card_split[[1]][1], "[0-9]+")[[1]])
  winning_numbers <- str_split(card_split[[1]][2], "\\s+")[[1]]
  winning_numbers <- winning_numbers[winning_numbers != ""]
  your_numbers <-  str_split(card_split[[1]][3], "\\s+")[[1]]
  your_numbers <- your_numbers[your_numbers != ""]    
  num_matches <- length(your_numbers[your_numbers %in% winning_numbers])
  if (puzzle == 1) {
    if (num_matches == 0) { return(0) }
    else { return(2**(num_matches - 1)) }
  }
  else {
    if (num_matches == 0) { copies <- c(card_num) }
    else { copies <- c(card_num, sapply(1:num_matches, function(x) { card_num + x })) }
    return(copies)
  }
}))

if (puzzle == 1) { sum(all_matches) } else {
  sum(table(unlist(sapply(all_matches, function(matches) {
    if (length(matches) == 1) { return(matches[1]) }
    else { return(c(matches[1], unlist(sapply(matches[2:length(matches)], 
                                              function(match) { find_copies(match) })))) }
  }))))
}
