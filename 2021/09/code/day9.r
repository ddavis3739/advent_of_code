# --- Day 9: Smoke Basin ---
#   These caves seem to be lava tubes. Parts are even still volcanically active; small hydrothermal vents release smoke into the caves that slowly settles like rain.
# 
# If you can model how the smoke flows through the caves, you might be able to avoid it and be that much safer. The submarine generates a heightmap of the floor of the nearby caves for you (your puzzle input).
# 
# Smoke flows to the lowest point of the area it's in. For example, consider the following heightmap:
# 
# 2199943210
# 3987894921
# 9856789892
# 8767896789
# 9899965678
# Each number corresponds to the height of a particular location, where 9 is the highest and 0 is the lowest a location can be.
# 
# Your first goal is to find the low points - the locations that are lower than any of its adjacent locations. Most locations have four adjacent locations (up, down, left, and right); locations on the edge or corner of the map have three or two adjacent locations, respectively. (Diagonal locations do not count as adjacent.)
# 
# In the above example, there are four low points, all highlighted: two are in the first row (a 1 and a 0), one is in the third row (a 5), and one is in the bottom row (also a 5). All other locations on the heightmap have some lower adjacent location, and so are not low points.
# 
# The risk level of a low point is 1 plus its height. In the above example, the risk levels of the low points are 2, 1, 6, and 6. The sum of the risk levels of all low points in the heightmap is therefore 15.
# 
# Find all of the low points on your heightmap. What is the sum of the risk levels of all low points on your heightmap?
# 
# Your puzzle answer was 580.
# 
# --- Part Two ---
# Next, you need to find the largest basins so you know what areas are most important to avoid.
# 
# A basin is all locations that eventually flow downward to a single low point. Therefore, every low point has a basin, although some basins are very small. Locations of height 9 do not count as being in any basin, and all other locations will always be part of exactly one basin.
# 
# The size of a basin is the number of locations within the basin, including the low point. The example above has four basins.
# 
# The top-left basin, size 3:
# 
# 2199943210
# 3987894921
# 9856789892
# 8767896789
# 9899965678
# The top-right basin, size 9:
# 
# 2199943210
# 3987894921
# 9856789892
# 8767896789
# 9899965678
# The middle basin, size 14:
# 
# 2199943210
# 3987894921
# 9856789892
# 8767896789
# 9899965678
# The bottom-right basin, size 9:
# 
# 2199943210
# 3987894921
# 9856789892
# 8767896789
# 9899965678
# Find the three largest basins and multiply their sizes together. In the above example, this is 9 * 14 * 9 = 1134.
# 
# What do you get if you multiply together the sizes of the three largest basins?
# 
# Your puzzle answer was 856716.

library(tidyverse)
library(readr)
library (plyr)

# d9 - asses risk level of smoke basins

d9_base <-  
  readLines("/Users/andrew.davis/Documents/personal_repos/advent_of_code_2021/data/day_9/day9_AoC.txt")

d9_input <- 
  sapply(
    d9_base,
    function(x){
      str_split(x, '')
    }) %>% 
  unlist() %>% 
  as.numeric()

d9_input <- 
  as.data.frame(
    matrix(d9_input,
           nrow=length(d9_base),
           byrow=TRUE)
  ) %>%
  as.data.frame()  

tube_tab <- 
  !(d9_input == d9_input)
  
for(row in 1:nrow(d9_input)){
  
  if (row==1) {
    x_col_full <- d9_input[row:(row + 1), ]
  } else if(row == nrow(d9_input)){
    x_col_full <- d9_input[(row-1):row, ]
  } else{
    x_col_full <- d9_input[(row-1):(row+1), ]
  }
  
  for(col in 1:ncol(d9_input)){
    
    x <- d9_input[row, col]
    
    if (col==1) {
      x_row <- d9_input[row, col:(col + 1) ]
    } else if(col == ncol(d9_input)){
      x_row <- d9_input[row,(col-1):col ]
    } else{
      x_row <- d9_input[row,(col-1):(col+1) ]
    }
    
    # print(x_col)
    # print(x_row)
    
    x_col <- x_col_full[, col]
    
    vert_check <- 
      (min(x_col) == x) &
      (length(x_col[x_col == min(x_col)]) == 1)
    row_check <- 
      (min(x_row) == x) &
      (length(x_col[x_col == min(x_col)]) == 1)
    
    if (vert_check & row_check) {
      tube_tab[row, col] <- TRUE
    }
  }
  
  x_col_full <- NULL
  x_row <- NULL
}

sum(d9_input[tube_tab] + 1)

## part 2 - find basin sizes 
# (cut to me realizing I have never done a Breadth First Search (BFS) Algorithm before and scraping other people's repos to get a workable answer) 
# Ignore peaks
indices <- which(d9_input < 9)
sink_counter <- matrix(0,
                       nrow = nrow(d9_input) * ncol(d9_input),
                       ncol=1)

dims <- dim(d9_input)

# Trace a starting point to a low point
low_point_finder <- 
  function(coord, input, dims){
    
    searching <- 1
    current <- 
      arrayInd(coord,
               dims)
    
    while(searching == 1){
      previous <- current
      adjacent_inds <- 
        rbind(
          current, 
          current + c(0,1), 
          current + c(1,0), 
          current - c(0,1), 
          current - c(1,0)
          )
      
      # account for area edges
      adjacent_inds[
        which(adjacent_inds == (dims[1] + 1) | 
                adjacent_inds == (dims[2] + 1))
        ] <- 0
      
      adjacent_inds <- 
        adjacent_inds[which(rowSums(sign(adjacent_inds))==2),]
      
      current <- 
        adjacent_inds[
          arrayInd(
              which(
              input[adjacent_inds] == 
                min(input[adjacent_inds])
              )[1],
              dim(adjacent_inds)
              )[1],
          ]
      
      if(setequal(current,previous) == TRUE){
        searching <- 0
        index <- dims[1]*(current[2]-1) + current[1]
        return(index)
      }
    }
}

# Loop over each possible starting point and trace to low point
for(i in 1:length(indices)){
  low_point <- low_point_finder(indices[i], d9_input, dims)
  sink_counter[low_point,1] <- sink_counter[low_point,1] + 1
}

# Compute final answer
print(prod(rev(sink_counter[order(sink_counter),])[1:3]))

sort(sink_counter, decreasing = T)[1:3] %>% 
  prod()


