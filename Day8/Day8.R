library(stringr)
library(collections)
library(numbers)

puzzle <- 2

connection <- file(description = "Day8Input.txt", open = "r")
map <- readLines(connection)
close(connection)

directions <- str_split(map[1], "")[[1]]
network <- map[3:length(map)]
network_dict <- dict()
for (n in network) {
  node <- str_extract(n, "([0-9A-Z]+) = \\(([0-9A-Z]+), ([0-9A-Z]+)\\)", group = c(1,2,3))
  network_dict$set(node[1], c(node[2], node[3]))
}

if (puzzle == 1) { current_nodes <- c("AAA") } else { 
  all_nodes <- unlist(network_dict$keys())
  current_nodes <- all_nodes[str_detect(all_nodes, "[0-9A-Z][0-9A-Z]A")]
}

steps <- sapply(current_nodes, function(n) {
  current_node <- n
  i <- 0
  n <- 1
  while (T) {
    next_nodes <- network_dict$get(current_node)
    if (directions[i + 1] == "L") { current_node <- next_nodes[1] } else { current_node <- next_nodes[2] }
    if (str_detect(current_node, "[A-Z][A-Z]Z")) { break }
    i <- (i + 1) %% length(directions)
    n <- n + 1
  }
  return(n)
})
if (puzzle == 1) { unname(steps) } else { unname(format(mLCM(steps), scientific = F)) }
