library(stringr)
library(sp)

puzzle <- 2

connection <- file(description = "Day10Input.txt", open = "r")
pipes <- lapply(readLines(connection), function(line) { str_split(line, "")[[1]] })
ncol <- length(pipes[[1]])
nrow <- length(pipes)
pipes <- unlist(pipes)
close(connection)

up_pipes <- c("|", "L", "J", "S")
down_pipes <- c("|", "7", "F", "S")
left_pipes <- c("-", "J", "7", "S")
right_pipes <- c("-", "L", "F", "S")
find_next_pipes <- function(i) {
  current_pipe <- pipes[i]
  next_pipes <- c()
  if (i > ncol) {
    next_i <- i - ncol
    if (current_pipe %in% up_pipes & pipes[next_i] %in% down_pipes) { next_pipes <- c(next_pipes, c(next_i)) }
  }
  if (i <= nrow * ncol - ncol) {
    next_i <- i + ncol
    if (current_pipe %in% down_pipes & pipes[next_i] %in% up_pipes) { next_pipes <- c(next_pipes, c(next_i)) }
  }
  if (i %% ncol != 0) {
    next_i <- i + 1
    if (current_pipe %in% right_pipes & pipes[next_i] %in% left_pipes) { next_pipes <- c(next_pipes, c(next_i)) }
  }
  if (i - 1 %% ncol != 0) {
    next_i <- i - 1
    if (current_pipe %in% left_pipes & pipes[next_i] %in% right_pipes) { next_pipes <- c(next_pipes, c(next_i)) }
  }
  return(next_pipes)
}

s_index <- which(pipes == "S")
visited_pipes_1 <- c(s_index, find_next_pipes(s_index)[1])
visited_pipes_2 <- c(find_next_pipes(s_index)[2])
d <- 1
while(T) {
  next_pipe_1 <- find_next_pipes(visited_pipes_1[length(visited_pipes_1)])
  next_pipe_2 <- find_next_pipes(visited_pipes_2[length(visited_pipes_2)])
  next_pipe_1 <- next_pipe_1[!(next_pipe_1 %in%  c(visited_pipes_1, visited_pipes_2))]
  if (length(next_pipe_1) != 0) { visited_pipes_1 <- c(visited_pipes_1, next_pipe_1) }
  else { break }
  d <- d + 1
  next_pipe_2 <- next_pipe_2[!(next_pipe_2 %in% c(visited_pipes_1, visited_pipes_2))]
  if  (length(next_pipe_2) != 0) { visited_pipes_2 <- c(visited_pipes_2, next_pipe_2) }
  else { break }
}

if (puzzle == 1) { d } else {
  pipes_in_order <- c(visited_pipes_1, rev(visited_pipes_2))
  x_coords <- pipes_in_order %% ncol
  x_coords[x_coords == 0] <- ncol
  y_coords <- sapply(pipes_in_order, function(p) { for (i in seq(nrow - 1, 0, -1)) { 
    if (p > ncol * i) { return(nrow - i) } }
  })
  sum(sapply(min(x_coords):max(x_coords), function(x) {
    sum(sapply(min(y_coords):max(y_coords), function(y) {
      if (point.in.polygon(x, y, x_coords, y_coords) == 1) { return(1) } else { return(0) }
    }))
  }))  
}
