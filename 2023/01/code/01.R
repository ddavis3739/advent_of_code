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

# --- Day 1: Trebuchet?! ---
#   Something is wrong with global snow production, and you've been selected to take a look. The Elves have even given you a map; on it, they've used stars to mark the top fifty locations that are likely to be having problems.
# 
# You've been doing this long enough to know that to restore snow operations, you need to check all fifty stars by December 25th.
# 
# Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
# 
# You try to ask why they can't just use a weather machine ("not powerful enough") and where they're even sending you ("the sky") and why your map looks mostly blank ("you sure ask a lot of questions") and hang on did you just say the sky ("of course, where do you think snow comes from") when you realize that the Elves are already loading you into a trebuchet ("please hold still, we need to strap you in").
# 
# As they're making the final adjustments, they discover that their calibration document (your puzzle input) has been amended by a very young Elf who was apparently just excited to show off her art skills. Consequently, the Elves are having trouble reading the values on the document.
# 
# The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value that the Elves now need to recover. On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.
# 
# For example:
#   
#   1abc2
# pqr3stu8vwx
# a1b2c3d4e5f
# treb7uchet
# In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.
# 
# Consider your entire calibration document. What is the sum of all of the calibration values?
#   
  
input <- readLines('01/data/input.in') 

input_v <-  
  sapply(input, function(x){
    gsub('[a-zA-Z]', '', x, fixed = F) 
  })

cals <- 
  sapply(input_v, function(x){
    a <- str_sub(x, 1, 1)
    b <- str_sub(x, -1, -1)
    paste0(a, b)
  }
  ) %>%
  as.numeric()

p1 <- sum(cals)

# --- Part Two ---
#   Your calculation isn't quite right. It looks like some of the digits are actually spelled out with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".
# 
# Equipped with this new information, you now need to find the real first and last digit on each line. For example:
# 
# two1nine
# eightwothree
# abcone2threexyz
# xtwone3four
# 4nineeightseven2
# zoneight234
# 7pqrstsixteen
# In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.
# 
# What is the sum of all of the calibration values?

  
numbers <- 0:9 %>% as.character()
# bit hacky but need to account for the "eightwo" situation that results in "82"
numbers <- c('ze0ro', 'o1ne', 't2wo', 't3hree', 'f4our',
           'f5ive', 's6ix', 's7even', 'e8ight', 'n9ine')
words <- c('zero', 'one', 'two', 'three', 'four',
           'five', 'six', 'seven', 'eight', 'nine')

input <- readLines('01/data/input.in') 
ex2 <- readLines('01/data/example2.txt') 

input_v1 <- input
# input_v1 <- ex2

a <- data_frame(numbers, words)
for(i in words){
  print(i)
  input_v1 <- gsub(i, a[a$words == i, 'numbers'] %>% unlist, input_v1)
}

input_v2 <-  
  sapply(input_v1, function(x){
    gsub('[a-zA-Z]', '', x, fixed = F) 
  })
  
cals <- 
  sapply(input_v2, function(x){
    a <- str_sub(x, 1, 1)
    b <- str_sub(x, -1, -1)
    paste0(a, b)
  }
  ) %>%
  as.numeric()

p2 <- sum(cals)

ans <-  matrix(c(p1, p2)) 
write_lines(ans, '01/data/input.ans')
