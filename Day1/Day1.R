library(stringi)

puzzle <- 2

connection <- file(description = "Day1Input.txt", open = "r")
document <- readLines(connection)
close(connection)

find_number <- function(x) {
  if (x %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9")) { return(x) }
  return(ifelse(x == "one", "1", ifelse(x == "two", "2", 
         ifelse(x == "three", "3", ifelse(x == "four", "4", 
         ifelse(x == "five", "5", ifelse(x == "six", "6",
         ifelse(x == "seven", "7", ifelse(x == "eight", "8", "9")))))))))
}

numbers <- "[0-9]"
numbers_words <- "(one)|(two)|(three)|(four)|(five)|(six)|(seven)|(eight)|(nine)"
numbers_words_rev <- stri_reverse(numbers_words)
numbers_words_rev <- stri_replace_all(numbers_words_rev, ")", fixed = "(")
numbers_words_rev <- stri_replace_all(numbers_words_rev, "|(", fixed = "|)")
substr(numbers_words_rev, 1, 1) <- "("

sum(sapply(document, function(x) {
  if (puzzle == 1) { first_pattern <- numbers }
  else { first_pattern <- paste0(numbers, "|", numbers_words) }
  first <- stri_extract_first_regex(x, pattern = first_pattern)
  x_rev <- stri_reverse(x)
  if (puzzle == 1) { second_pattern <- first_pattern }
  else { second_pattern <- paste0(numbers, "|", numbers_words_rev) }
  last <- stri_extract_first_regex(x_rev, pattern = second_pattern)
  first <- find_number(first)
  last <- find_number(stri_reverse(last))
  return(as.integer(paste0(first, last)))
}))
