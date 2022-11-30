# --- Day 4: Giant Squid ---
#   You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you can't see any sunlight. What you can see, however, is a giant squid that has attached itself to the outside of your submarine.
# 
# Maybe it wants to play bingo?
#   
#   Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen at random, and the chosen number is marked on all boards on which it appears. (Numbers may not appear on all boards.) If all numbers in any row or any column of a board are marked, that board wins. (Diagonals don't count.)
# 
# The submarine has a bingo subsystem to help passengers (currently, you and the giant squid) pass the time. It automatically generates a random order in which to draw numbers and a random set of boards (your puzzle input). For example:
# 
# 7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
# 
# 22 13 17 11  0
#  8  2 23  4 24
# 21  9 14 16  7
#  6 10  3 18  5
#  1 12 20 15 19
# 
#  3 15  0  2 22
#  9 18 13 17  5
# 19  8  7 25 23
# 20 11 10 24  4
# 14 21 16 12  6
# 
# 14 21 17 24  4
# 10 16 15  9 19
# 18  8 23 26 20
# 22 11 13  6  5
#  2  0 12  3  7
# After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no winners, but the boards are marked as follows (shown here adjacent to each other to save space):
# 
# 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
# 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
# After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there are still no winners:
# 
# 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
# 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
# Finally, 24 is drawn:
# 
# 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
# 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
# At this point, the third board wins because it has at least one complete row or column of marked numbers (in this case, the entire top row is marked: 14 21 17 24 4).
# 
# The score of the winning board can now be calculated. Start by finding the sum of all unmarked numbers on that board; in this case, the sum is 188. Then, multiply that sum by the number that was just called when the board won, 24, to get the final score, 188 * 24 = 4512.
# 
# To guarantee victory against the giant squid, figure out which board will win first. What will your final score be if you choose that board?
# 
# Your puzzle answer was 44088.
# 
# --- Part Two ---
# On the other hand, it might be wise to try a different strategy: let the giant squid win.
# 
# You aren't sure how many bingo boards a giant squid could play at once, so rather than waste time counting its arms, the safe thing to do is to figure out which board will win last and choose that one. That way, no matter which boards it picks, it will win for sure.
# 
# In the above example, the second board is the last to win, which happens after 13 is eventually called and its middle column is completely marked. If you were to keep playing until this point, the second board would have a sum of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.
# 
# Figure out which board will win last. Once it wins, what would its final score be?
# 
# Your puzzle answer was 23670.
                                                                                                                                                                                                                                                                                                         
library(tidyverse)
library(readr)
library (plyr)

# d4 - find matching bingo board
parse_board_nums <- 
  function(x){
    x = trimws(x)
    x = strsplit(x, "\\s+")
    return(x)
  }

d4_nums <-  
  readLines("Documents/personal_repos/advent_of_code_2021/data/day4_AoC.txt", n = 1) %>% 
  strsplit(., ",") %>% 
  unlist()

d4_boards <-  
  readLines("Downloads//day4_AoC.txt") 

d4_boards <-  
  d4_boards[-1]

d4_boards <- 
  d4_boards[d4_boards != ""]

d4_boards <- 
  sapply(d4_boards, 
         parse_board_nums) 

d4_boards <- 
  matrix(d4_boards %>% unlist(),
         nrow = length(d4_boards),
         byrow = T) %>% 
  as.data.frame()

names(d4_boards) <- LETTERS[1:5]

d4_boards_list <- list()
for (i in 
     (1:nrow(d4_boards)/5) %>% 
     ceiling() %>% 
     unique()) {
  d4_boards_list[[i]] <- 
    d4_boards[(i*5-4):(i*5), ]
}

# find matches 
d4_nums <-  as.numeric(d4_nums)

find_bingo <- 
  function(x){
    row_check <- 
      apply(x, 1, all)
    col_check <- 
      apply(x, 2, all)
    
    if (any(row_check)) {
      print("Row winner!!!")
      x <- 1
      return(x)
    }
    if (any(col_check)) {
      print("Col winner!!!")
      x <- 1
      return(x)
    }
    x <- 0
    return(x)
  }

# base logical
d4_boards_list_check <- 
  lapply(d4_boards_list, 
         function(x){
           x != x
         })

bingo_check <- list()
for(i in 1:length(d4_nums)){
  
  pos <- i
  i <- d4_nums[i]
  
  print(pos)
  print(i)
  
  y <- 
    lapply(d4_boards_list, 
           function(x){
             x == i
           }) 
  
  for(j in 1:length(y)){
    d4_boards_list_check[[j]] <- 
      d4_boards_list_check[[j]] | 
      y[[j]]
  }
  
  bingo_check_itr <- 
    lapply(d4_boards_list_check, 
         find_bingo) %>% 
    unlist()
  
  bingo_check[[pos]] <- 
    bingo_check_itr
  
  if (any(bingo_check_itr == 1)) {
    break
  }
}

# sum of final board
winning_board <- 
  d4_boards_list[[
  bingo_check[[length(bingo_check)]] %>%
    grep(1, .)
  ]] 

# unchosen numbers
winning_board_nums <- 
  winning_board[
  !(
    d4_boards_list_check[[
    bingo_check[[length(bingo_check)]] %>%
      grep(1, .)
    ]]
  )
]

winning_board_nums %>% 
  as.numeric() %>%
  sum() *
  # last number called
  i

# d4 part 2 - last board to win
# base logical
d4_boards_list_check <- 
  lapply(d4_boards_list, 
         function(x){
           x != x
         })

bingo_check <- list()
bingo_check_cum <- rep(F, 100)
for(i in 1:length(d4_nums)){
  
  pos <- i
  i <- d4_nums[i]
  
  print(pos)
  print(i)
  
  y <- 
    lapply(d4_boards_list, 
           function(x){
             x == i
           }) 
  
  for(j in 1:length(y)){
    d4_boards_list_check[[j]] <- 
      d4_boards_list_check[[j]] | 
      y[[j]]
  }
  
  bingo_check_itr <- 
    lapply(d4_boards_list_check, 
           find_bingo) %>% 
    unlist()
  
  bingo_check_cum <- 
    bingo_check_cum | bingo_check_itr
  
  bingo_check[[pos]] <- 
    bingo_check_itr
  
  if (all(bingo_check_cum)) {
    break
  }
  
  last_board_check <- 
    bingo_check[[length(bingo_check)]] %>%
    grep(0, .)
}


# sum of final board to win
winning_board <- 
  d4_boards_list[[
    10
  ]] 

# unchosen numbers
winning_board_nums <- 
  winning_board[
    !(
      d4_boards_list_check[[
        10
      ]]
    )
  ]

winning_board_nums %>% 
  as.numeric() %>%
  sum() *
  # last number called
  i
