library(stringr)

puzzle <- 2

connection <- file(description = "Day2Input.txt", open = "r")
games <- readLines(connection)
close(connection)

max_red <- 12
max_green <- 13 
max_blue <- 14

find_max_num_cubes <- function(cube_sets, color, return_max) {
  n <- max(as.integer(stringr::str_extract(cube_sets, pattern = paste("([0-9]+)", color), group = 1)), na.rm = T)
  if (return_max) { return(n) }
  if (color == "red") { return(n > max_red) }
  if (color == "green") { return(n > max_green) }
  if (color == "blue") { return(n > max_blue) }
}

sum(sapply(games, function(game) {
  cube_sets <- stringr::str_split(game, pattern = ":|;")[[1]]
  game_number <- as.integer(stringr::str_extract(game[1], pattern = "Game ([0-9]+)", group = 1))
  cube_sets <- cube_sets[2:length(cube_sets)]
  if (puzzle == 1) { return_value <- F }
  else { return_value <- T }
  max_num_cubes <- sapply(c("red", "green", "blue"), function(x) { 
    find_max_num_cubes(cube_sets, x, return_value)
  })
  if (puzzle == 1) { 
    if (sum(max_num_cubes) == 0) { return(game_number) }
    else { return(0) }
  }
  else { return(prod(max_num_cubes)) }
}))
