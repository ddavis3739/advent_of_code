# setup 
rm(list = ls())
# set to repo wd
setwd('/Users/andrewdavis/Documents/git_repos/advent_of_code/2015')

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

# --- Day 1: Not Quite Lisp ---
#   Santa was hoping for a white Christmas, but his weather machine's "snow" function is powered by stars, and he's fresh out! To save Christmas, he needs you to collect fifty stars by December 25th.
# 
# Collect stars by helping Santa solve puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
#   
#   Here's an easy puzzle to warm you up.
# 
# Santa is trying to deliver presents in a large apartment building, but he can't find the right floor - the directions he got are a little confusing. He starts on the ground floor (floor 0) and then follows the instructions one character at a time.
# 
# An opening parenthesis, (, means he should go up one floor, and a closing parenthesis, ), means he should go down one floor.
# 
# The apartment building is very tall, and the basement is very deep; he will never find the top or bottom floors.
# 
# For example:
#   
#   (()) and ()() both result in floor 0.
# ((( and (()(()( both result in floor 3.
# ))((((( also results in floor 3.
#         ()) and ))( both result in floor -1 (the first basement level).
#         ))) and )())()) both result in floor -3.
#   To what floor do the instructions take Santa?

input <- readLines('01/data/input.in') 

input_v <-  
  strsplit(input, '') %>% unlist()

floor_val <- 0
for(i in 1:length(input_v)){
  a <- input_v[i]
  if (a == '(') {
    floor_val <- floor_val + 1 
  } else if(a == ')'){
    floor_val <- floor_val - 1 
  } else{
    print('bad character')
    break
  }
}
floor_val

p1 <- floor_val
# --- Part Two ---
#   Now, given the same instructions, find the position of the first character that causes him to enter the basement (floor -1). The first character in the instructions has position 1, the second character has position 2, and so on.
# 
# For example:
#   
#   ) causes him to enter the basement at character position 1.
# ()()) causes him to enter the basement at character position 5.
# What is the position of the character that causes Santa to first enter the basement?

floor_val <- 0
for(i in 1:length(input_v)){
  a <- input_v[i]
  if (a == '(') {
    floor_val <- floor_val + 1 
  } else if(a == ')'){
    floor_val <- floor_val - 1 
    if(floor_val < 0) break
  } else{
    print('bad character')
    break
  }
}
floor_val
i
p2 <- i

ans <-  matrix(c(p1, p2)) 
write_lines(ans, '01/data/input.ans')
