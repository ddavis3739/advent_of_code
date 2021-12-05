# --- Day 5: Hydrothermal Venture ---
#   You come across a field of hydrothermal vents on the ocean floor! These vents constantly produce large, opaque clouds, so it would be best to avoid them if possible.
# 
# They tend to form in lines; the submarine helpfully produces a list of nearby lines of vents (your puzzle input) for you to review. For example:
#   
#   0,9 -> 5,9
# 8,0 -> 0,8
# 9,4 -> 3,4
# 2,2 -> 2,1
# 7,0 -> 7,4
# 6,4 -> 2,0
# 0,9 -> 2,9
# 3,4 -> 1,4
# 0,0 -> 8,8
# 5,5 -> 8,2
# Each line of vents is given as a line segment in the format x1,y1 -> x2,y2 where x1,y1 are the coordinates of one end the line segment and x2,y2 are the coordinates of the other end. These line segments include the points at both ends. In other words:
#   
#   An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
# An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.
# For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2.
# 
# So, the horizontal and vertical lines from the above list would produce the following diagram:
#   
#   .......1..
# ..1....1..
# ..1....1..
# .......1..
# .112111211
# ..........
# ..........
# ..........
# ..........
# 222111....
# In this diagram, the top left corner is 0,0 and the bottom right corner is 9,9. Each position is shown as the number of lines which cover that point or . if no line covers that point. The top-left pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.
# 
# To avoid the most dangerous areas, you need to determine the number of points where at least two lines overlap. In the above example, this is anywhere in the diagram with a 2 or larger - a total of 5 points.
# 
# Consider only horizontal and vertical lines. At how many points do at least two lines overlap?
#   
#   Your puzzle answer was 4745.
# 
# --- Part Two ---
#   Unfortunately, considering only horizontal and vertical lines doesn't give you the full picture; you need to also consider diagonal lines.
# 
# Because of the limits of the hydrothermal vent mapping system, the lines in your list will only ever be horizontal, vertical, or a diagonal line at exactly 45 degrees. In other words:
# 
# An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
# An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.
# Considering all lines from the above example would now produce the following diagram:
# 
# 1.1....11.
# .111...2..
# ..2.1.111.
# ...1.2.2..
# .112313211
# ...1.2....
# ..1...1...
# .1.....1..
# 1.......1.
# 222111....
# You still need to determine the number of points where at least two lines overlap. In the above example, this is still anywhere in the diagram with a 2 or larger - now a total of 12 points.
# 
# Consider all of the lines. At how many points do at least two lines overlap?
# 
# Your puzzle answer was 18442.


library(tidyverse)
library(readr)
library (plyr)
library(reshape2)
library(viridis)

# d5 - find heavy thermal vent areas for horz and vert lines
parse_board_nums <- 
  function(x){
    x = trimws(x)
    x = strsplit(x, "\\s+")
    return(x)
  }

d5_base <-  
  readLines("Downloads/AoC_day5.txt")

d5 <- 
  d5_base %>%
  strsplit(., '->') %>%
  unlist() %>%
  trimws() %>% 
  strsplit(., ',') %>%
  unlist() %>%
  trimws() %>%
  as.numeric()

d5_df <- 
  matrix(d5,
       nrow = length(d5_base),
       byrow = T) %>% 
  as.data.frame() 

names(d5_df) <- 
  c('x1', 'y1', 'x2', 'y2')

d5_df_line <- 
  d5_df[(d5_df$x1 == d5_df$x2) | 
          (d5_df$y1 == d5_df$y2), ]

base_mat <- 
  matrix(0, 
         nrow = max(c(d5_df$y1, d5_df$y2)),
         ncol = max(c(d5_df$x1, d5_df$x2))
  )

for (i in 1:nrow(d5_df_line)) {
  x_change <- d5_df_line$x1[i]:d5_df_line$x2[i]
  y_change <- d5_df_line$y1[i]:d5_df_line$y2[i]
  
  base_mat[y_change, x_change] <- 
    base_mat[y_change, x_change] + 1
}

base_mat[base_mat > 1] %>% length()

# d5 part 2 - include diagonal lines 

base_mat_diag <- base_mat

d5_df_diagline <- 
  d5_df[abs(d5_df$x1 - d5_df$x2) ==
          abs(d5_df$y1 - d5_df$y2), ]

for (i in 1:nrow(d5_df_diagline)) {
    x_change <- d5_df_diagline$x1[i]:d5_df_diagline$x2[i]
    y_change <- d5_df_diagline$y1[i]:d5_df_diagline$y2[i]
    
    diag <- data_frame(
      x_change,
      y_change
    )
    
    for(row in 1:nrow(diag)){
      base_mat_diag[as.numeric(diag[row, 2]), as.numeric(diag[row, 1])] <- 
        base_mat_diag[as.numeric(diag[row, 2]), as.numeric(diag[row, 1])] + 1
    }

  }

base_mat_diag[base_mat_diag > 1] %>% length()

# heat map for fun
p <- ggplot(melt(base_mat_diag), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_viridis(trans='log2', na.value="white") +
  theme_classic()

print(p)
