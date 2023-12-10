library(stringr)
library(dplyr)

puzzle <- 2

connection <- file(description = "Day7Input.txt", open = "r")
hands_df <- lapply(readLines(connection), function(x) {
  split <- str_split(x, "\\s+")[[1]]
  data.frame(hand = split[1], bid = as.integer(split[2])) 
}) %>% bind_rows()
close(connection)

find_hand_rank <- function(hand) {
  hand_table <- table(str_split(hand, "")[[1]])
  if (puzzle == 1 | length(hand_table) == 1) { 
    hand_table_no_j <- hand_table
    count_j <- 0 
  } else {
    hand_table_no_j <- hand_table[names(hand_table) != "J"]
    count_j <- unname(ifelse(is.na(hand_table["J"]), 0, hand_table["J"]))
  }
  if ((sum(hand_table == 5) == 1) | (max(hand_table_no_j) + count_j == 5)) { return(1) }
  if ((sum(hand_table == 4) == 1) | (max(hand_table_no_j) + count_j == 4)) { return(2) }
  if (((sum(hand_table == 3) == 1) & (sum(hand_table == 2) == 1)) |
      ((sum(sort(hand_table_no_j, decreasing = T)[1:2], na.rm = T) + count_j) == 5)) { return(3)}
  if ((sum(hand_table == 3) == 1) | (max(hand_table_no_j) + count_j == 3)) { return(4) }
  if ((sum(hand_table == 2) == 2) | 
      ((sum(sort(hand_table_no_j, decreasing = T)[1:2], na.rm = T) + count_j) == 4)) { return(5) }
  if ((sum(hand_table == 2) == 1) | (max(hand_table_no_j) + count_j == 2)) { return(6) }
  if (sum(hand_table == 1) == 5) { return(7) }
}

find_card_rank <- function(card) { 
  if (puzzle == 1) { cards <-  c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")}
  else { cards <-  c("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J")}
  return(match(card, cards))
}

winnings_df <- hands_df %>% 
  group_by(hand) %>% 
  mutate(
    hand_rank = find_hand_rank(hand),
    card_1_rank = find_card_rank(str_split(hand, "")[[1]][1]),
    card_2_rank = find_card_rank(str_split(hand, "")[[1]][2]),
    card_3_rank = find_card_rank(str_split(hand, "")[[1]][3]),
    card_4_rank = find_card_rank(str_split(hand, "")[[1]][4]),
    card_5_rank = find_card_rank(str_split(hand, "")[[1]][5])) %>% 
  ungroup() %>% 
  arrange(hand_rank, card_1_rank, card_2_rank, card_3_rank, card_4_rank, card_5_rank) %>%
  mutate(rank = seq(n(), 1, -1)) %>%
  mutate(winnings = bid * rank) 
winnings_df %>% select(winnings) %>% sum()
