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

# --- Day 8: Haunted Wasteland ---
#   You're still riding a camel across Desert Island when you spot a sandstorm quickly approaching. When you turn to warn the Elf, she disappears before your eyes! To be fair, she had just finished warning you about ghosts a few minutes ago.
# 
# One of the camel's pouches is labeled "maps" - sure enough, it's full of documents (your puzzle input) about how to navigate the desert. At least, you're pretty sure that's what they are; one of the documents contains a list of left/right instructions, and the rest of the documents seem to describe some kind of network of labeled nodes.
# 
# It seems like you're meant to use the left/right instructions to navigate the network. Perhaps if you have the camel follow the same instructions, you can escape the haunted wasteland!
#   
#   After examining the maps for a bit, two nodes stick out: AAA and ZZZ. You feel like AAA is where you are now, and you have to follow the left/right instructions until you reach ZZZ.
# 
# This format defines each node of the network individually. For example:
#   
#   RL
# 
# AAA = (BBB, CCC)
# BBB = (DDD, EEE)
# CCC = (ZZZ, GGG)
# DDD = (DDD, DDD)
# EEE = (EEE, EEE)
# GGG = (GGG, GGG)
# ZZZ = (ZZZ, ZZZ)
# Starting with AAA, you need to look up the next element based on the next left/right instruction in your input. In this example, start with AAA and go right (R) by choosing the right element of AAA, CCC. Then, L means to choose the left element of CCC, ZZZ. By following the left/right instructions, you reach ZZZ in 2 steps.
# 
# Of course, you might not find ZZZ right away. If you run out of left/right instructions, repeat the whole sequence of instructions as necessary: RL really means RLRLRLRLRLRLRLRL... and so on. For example, here is a situation that takes 6 steps to reach ZZZ:
#   
#   LLR
# 
# AAA = (BBB, BBB)
# BBB = (AAA, ZZZ)
# ZZZ = (ZZZ, ZZZ)
# Starting at AAA, follow the left/right instructions. How many steps are required to reach ZZZ?
  
input <- readLines('08/data/input.in') 
# input <- readLines('08/data/ex1.txt')
# input <- readLines('08/data/ex2.txt')

# one millisecond holding down the button --> boat's speed increases by one millimeter per millisecond

steps <- input[1]
steps <- str_split(steps, '') %>% unlist()
input_map <- input[-c(1:2)]

input_map <- 
  gsub('\\=|\\(|\\,|\\)', '', input_map)

input_map <- 
  lapply(input_map, function(x){
  x <- str_split(x, ' +')
  x <- x[x != '+ ']
}) %>% unlist()

input_map <- 
  matrix(input_map, ncol = 3, byrow = TRUE) %>%
  as.data.frame()
names(input_map) <- c('start', 'L', 'R')
input_map <- 
  input_map %>% 
  arrange(start)

i = 1
interations <- 0
step_count = 0
next_step <- 'AAA'
while(i <= length(steps)){
  # print(i)
  a <- 
    input_map %>% filter(start == next_step)
  if(steps[i] == 'R'){
    next_step <- a$R[1]  
  } else{
    next_step <- a$L[1]  
  }
  
  step_count <- step_count +1
  # print(step_count)
  # print(next_step)
  if(next_step == 'ZZZ') break
  
  i = i + 1
  if(i > length(steps)){
    i = 1
    
    interations <- interations +1
    print(paste('reset', interations))
  }
  
}

p1 <- step_count

p1

# --- Part Two ---
#   The sandstorm is upon you and you aren't any closer to escaping the wasteland. You had the camel follow the instructions, but you've barely left your starting position. It's going to take significantly more steps to escape!
# 
# What if the map isn't for people - what if the map is for ghosts? Are ghosts even bound by the laws of spacetime? Only one way to find out.
# 
# After examining the maps a bit longer, your attention is drawn to a curious fact: the number of nodes with names ending in A is equal to the number ending in Z! If you were a ghost, you'd probably just start at every node that ends with A and follow all of the paths at the same time until they all simultaneously end up at nodes that end with Z.
# 
# For example:
# 
# LR
# 
# 11A = (11B, XXX)
# 11B = (XXX, 11Z)
# 11Z = (11B, XXX)
# 22A = (22B, XXX)
# 22B = (22C, 22C)
# 22C = (22Z, 22Z)
# 22Z = (22B, 22B)
# XXX = (XXX, XXX)
# Here, there are two starting nodes, 11A and 22A (because they both end with A). As you follow each left/right instruction, use that instruction to simultaneously navigate away from both nodes you're currently on. Repeat this process until all of the nodes you're currently on end with Z. (If only some of the nodes you're on end with Z, they act like any other node and you continue as normal.) In this example, you would proceed as follows:
#   
#   Step 0: You are at 11A and 22A.
# Step 1: You choose all of the left paths, leading you to 11B and 22B.
# Step 2: You choose all of the right paths, leading you to 11Z and 22C.
# Step 3: You choose all of the left paths, leading you to 11B and 22Z.
# Step 4: You choose all of the right paths, leading you to 11Z and 22B.
# Step 5: You choose all of the left paths, leading you to 11B and 22C.
# Step 6: You choose all of the right paths, leading you to 11Z and 22Z.
# So, in this example, you end up entirely on nodes that end in Z after 6 steps.
# 
# Simultaneously start on every node that ends with A. How many steps does it take before you're only on nodes that end with Z?

next_step_vec <- 
  input_map %>% filter(substring(start, 3, 3) == 'A') %>% select(start) %>% unlist
next_step_vec <- next_step_vec[next_step_vec != 'AAA']
i_vec <- rep(1, length(next_step_vec))
step_count_vec <- rep(0, length(next_step_vec))
interations_vec <- rep(0, length(next_step_vec))

for(b in seq_along(next_step_vec)){
  
  i <- i_vec[b]
  
  step_count <- step_count_vec[b]
  interations <- interations_vec[b]
  next_step <- next_step_vec[b]
  
  print(b)
  print(step_count)
  print(interations)
  print(next_step)
  
  while(i <= length(steps)){
    # print(i)
    a <- 
      input_map %>% filter(start == next_step) 
    
    if(steps[i] == 'R'){
      next_step <- a$R[1]  
    } else{
      next_step <- a$L[1]  
    }
    
    step_count <- step_count +1
    
    # print(step_count)
    # print(next_step)
    
    i = i + 1
    if(i > length(steps)){
      i = 1
      interations <- interations + 1
      doneies <- sum(substring(next_step, 3, 3) == 'Z')
      print(paste('reset', interations, 'done', doneies))
    }
    
    if(substring(next_step, 3, 3) == 'Z') {
      step_count_vec[b] <- step_count
      interations_vec[b] <- interations
      # break up one layer of loop
      i <- length(steps) + 1
    }
    
  }
}


step_count_vec <- append(step_count_vec, p1)

# find the LCM
p2 <- numbers::mLCM(step_count_vec)
options(digits = 22)
p2

ans <-  matrix(c(p1, p2)) 
write_lines(ans, '08/data/input.ans')

