# setup 
rm(list = ls())
# set to repo wd
setwd('/Users/andrewdavis/Documents/git_repos/advent_of_code/2023')

# load packages
require(Hmisc)
library(data.table)
require(tidyverse)

std <- function(x) sd(x)/sqrt(length(x))

mytheme <- theme_bw() + theme(axis.line = element_line(colour = "black"),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              #panel.border = element_blank(),
                              #panel.background = element_blank(),
                              axis.text=element_text(size=11, color = 'black'),
                              axis.title=element_text(size=12),
                              legend.text=element_text(size=12))

# --- Day 3: Gear Ratios ---
#   You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up to the water source, but this is as far as he can bring you. You go inside.
# 
# It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.
# 
# "Aaah!"
# 
# You turn around to see a slightly-greasy Elf with a wrench and a look of surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it." You offer to help.
# 
# The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one. If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.
# 
# The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)
# 
# Here is an example engine schematic:
# 
# 467..114..
# ...*......
# ..35..633.
# ......#...
# 617*......
# .....+.58.
# ..592.....
# ......755.
# ...$.*....
# .664.598..
# In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.
# 
# Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?

 
input <- readLines('03/data/input.in') 
# input <- readLines('03/data/ex1.txt')


input_v <-  
  sapply(input, function(x){
    a <- str_split(x, '')
  })  

input_mat <-
  input_v %>% bind_rows() %>% t() %>% as.matrix()

rownames(input_mat) <- NULL

num_mat <- grepl('[0-9]', input_mat)
num_mat <- matrix(num_mat, nrow = nrow(input_mat))

chr_mat <- !num_mat
chr_mat <- matrix(chr_mat, nrow = nrow(input_mat))

part_mat <- 
  !grepl('[0-9]', input_mat) & !grepl('\\.', input_mat)
part_mat <- matrix(part_mat, nrow = nrow(input_mat))
 
# get part values and positions
part_mat_pos <- which(part_mat, arr.ind=TRUE)
part_mat_pos <- 
  part_mat_pos%>% 
  as.data.frame() %>% 
  arrange(row)

part_val <- vector()
a <- 1
for (i in 1:nrow(part_mat_pos)){ 
  part_val[i] <- input_mat[part_mat_pos[i,1], part_mat_pos[i, 2]]
}

part_mat_pos$part_val <- part_val
part_mat_pos$part_index <- 1:nrow(part_mat_pos)

# get number values and positions
num_mat_pos <- which(num_mat, arr.ind=TRUE)
num_mat_pos <- 
  num_mat_pos%>% 
  as.data.frame() %>% 
  arrange(row)

num_index <- vector()
num_val <- vector()
a <- 1
for (i in 1:nrow(num_mat_pos)){ 
  if(i == 1){
    num_index[i] <- a
  } else{
    rowCheck <- num_mat_pos[i,1] == num_mat_pos[i-1,1]
    colCheck <- (num_mat_pos[i, 2] - 1) == num_mat_pos[i-1, 2]
    a <- ifelse(rowCheck & colCheck, a, a+1)
    num_index[i] <- a
  }
  num_val[i] <- input_mat[num_mat_pos[i,1], num_mat_pos[i, 2]]
}

num_mat_pos$num_index <- as.factor(num_index)

num_mat_pos$num_val <- num_val

num_mat_pos <- num_mat_pos %>% 
  group_by(num_index) %>% 
  mutate(num_val_full = paste0(num_val, collapse = "")) %>% 
  mutate(num_val_full = as.numeric(num_val_full)) %>% 
  ungroup() %>% 
  as.data.frame()

# id nums next to part vals 
adj_bool <- vector()
for (i in 1:nrow(num_mat_pos)) {
  # print(i)
  
  r1 <- num_mat_pos[i, 1]
  r2 <- num_mat_pos[i, 2]
  
  test_df <- tibble(
    row = c(rep(r1, 3), rep(r1+1, 3), rep(r1-1, 3)),
    col = rep(c(r2-1, r2, r2+1), 3)
    )
  
  a <- 
    test_df %>% left_join(part_mat_pos,
                          by = join_by(row, col)) %>%
    filter(!is.na(part_val))
  if (nrow(a) == 0) {
    adj_bool[i] <- F
  } else{
    adj_bool[i] <- a$part_index
  }
  
}

num_mat_pos$adj_bool <- as.character(adj_bool)
num_mat_pos$part_index <- num_mat_pos$adj_bool

part_mat_pos$part_index <- as.character(part_mat_pos$part_index)
part_nums <- 
  part_mat_pos %>% 
  left_join(num_mat_pos %>% 
              select(-row, -col))

part_nums_uni <- 
  part_nums %>% 
  select(part_index, num_index, num_val_full) %>% 
  mutate(
    part_index = as.character(part_index), 
    num_index = as.character(num_index)
  ) %>% 
  distinct()

p1 <- sum(part_nums_uni$num_val_full)

# --- Part Two ---
#   The Elf says they've stopped producing snow because they aren't getting any water! He isn't sure why the water stopped; however, he can show you how to get to the water source to check it out for yourself. It's just up ahead!
#   
#   As you continue your walk, the Elf poses a second question: in each game you played, what is the fewest number of cubes of each color that could have been in the bag to make the game possible?
#   
#   Again consider the example games from earlier:
#   
#   Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
# Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
# Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
# Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
# Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
# In game 1, the game could have been played with as few as 4 red, 2 green, and 6 blue cubes. If any color had even one fewer cube, the game would have been impossible.
# Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue cubes.
# Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
# Game 4 required at least 14 red, 3 green, and 15 blue cubes.
# Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.
# The power of a set of cubes is equal to the numbers of red, green, and blue cubes multiplied together. The power of the minimum set of cubes in game 1 is 48. In games 2-5 it was 12, 1560, 630, and 36, respectively. Adding up these five powers produces the sum 2286.
# 
# For each game, find the minimum set of cubes that must have been present. What is the sum of the power of these sets?
  
part_nums_gear <- 
  part_nums %>% 
  filter(part_val == '*')

part_nums_gear <- 
  part_nums_gear %>%
  select(part_index, num_index, num_val_full) %>% 
  distinct()

part_nums_gear <- 
  part_nums_gear %>%
  group_by(part_index) %>% 
  mutate(rowCount = n())

part_nums_gear <- 
  part_nums_gear %>% 
  filter(rowCount == 2) %>%
  group_by(part_index) %>% 
  mutate(gearRatio = prod(num_val_full)) %>%
  select(-num_index, -num_val_full) %>%
  distinct()


p2 <- sum(part_nums_gear$gearRatio)

ans <-  matrix(c(p1, p2)) 
write_lines(ans, '03/data/input.ans')
