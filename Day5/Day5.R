library(stringr)
library(dplyr)

puzzle <- 2

connection <- file(description = "Day5Input.txt", open = "r")
maps <- str_split(paste(readLines(connection), collapse = "\n"), "\\n\\n")[[1]]
close(connection)

seeds <- as.numeric(str_extract_all(maps[1], "[0-9]+")[[1]])
maps_df <- lapply(maps[2:length(maps)], function(map) {
  map_split <- str_split(map, "\n")[[1]]
  map_type <- str_split(str_extract(map_split[1], "[A-z|-]+"), "-")[[1]]
  map_start <- map_type[1]
  map_end <- map_type[3]
  map_ranges <- str_extract_all(map_split[2:length(map_split)], "[0-9]+")
  df <- lapply(map_ranges, function(row) {
    row <- as.numeric(row)
    dest_start <- row[1]
    source_start <- row[2]
    range <- row[3]
    data.frame(dest_start = dest_start, dest_end = dest_start + range - 1,
               source_start = source_start, source_end = source_start + range - 1, 
               range = range, map_start = map_start, map_end = map_end)
  }) %>% bind_rows()
}) %>% bind_rows()
levels <- unique(maps_df$map_start)

find_dest <- function(start, type) {
  dest_row <- maps_df %>% filter(map_start == type & source_start <= start & source_end >= start)
  if (nrow(dest_row) == 0) { return(start) }
  else { return(dest_row$dest_start + (start - dest_row$source_start)) }
}

find_dest_range <- function(start, range, type) {
  dest_row <- maps_df %>% filter(map_start == type & source_start <= start & source_end >= start)
  if (nrow(dest_row) == 0) { 
    next_starts <- maps_df %>% filter(map_start == "soil" & source_start > start)
    if (nrow(next_starts) == 0) { return(c(start, range)) }
    else { 
      next_start <- min(next_starts$source_start)
      return(c(start, next_start - start)) 
    }
  }
  else {
    diff <- start - dest_row$source_start
    return(c(dest_row$dest_start + diff, min(dest_row$range - diff, range)))
  }
}

if (puzzle == 1) {
  min(sapply(seeds, function(seed) {
    start <- seed
    for (level in levels) { start <- find_dest(start, level) }
    return(start)
  }))
} else {
  starts <- seeds
  for (level in levels) {
    new_starts <- c()
    for (x in seq(1, length(starts), 2)) {
      start <- starts[x]
      range <- starts[x+1]
      current_range <- 0
      while (current_range < range) {
        dest_range <- find_dest_range(start + current_range, range - current_range, level)
        new_starts <- c(new_starts, dest_range)
        current_range <- current_range + dest_range[2]
      }
    }
    starts <- new_starts
  }
  min(starts[c(T, F)])
}
