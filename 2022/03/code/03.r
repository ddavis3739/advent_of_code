# setup 
rm(list = ls())
# set to repo wd
setwd('/Users/andrewdavis/Documents/git_repos/advent_of_code/2022')

# load packages
require(Hmisc)
library(data.table)
require(tidyverse)
require(Rlibstree)

std <- function(x) sd(x)/sqrt(length(x))

mytheme <- theme_bw() + theme(axis.line = element_line(colour = "black"),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              #panel.border = element_blank(),
                              #panel.background = element_blank(),
                              axis.text=element_text(size=11, color = 'black'),
                              axis.title=element_text(size=12),
                              legend.text=element_text(size=12))

# --- Day 3: Rucksack Reorganization ---
#   One Elf has the important job of loading all of the rucksacks with supplies for the jungle journey. Unfortunately, that Elf didn't quite follow the packing instructions, and so a few items now need to be rearranged.
# 
# Each rucksack has two large compartments. All items of a given type are meant to go into exactly one of the two compartments. The Elf that did the packing failed to follow this rule for exactly one item type per rucksack.
# 
# The Elves have made a list of all of the items currently in each rucksack (your puzzle input), but they need your help finding the errors. Every item type is identified by a single lowercase or uppercase letter (that is, a and A refer to different types of items).
# 
# The list of items for each rucksack is given as characters all on a single line. A given rucksack always has the same number of items in each of its two compartments, so the first half of the characters represent items in the first compartment, while the second half of the characters represent items in the second compartment.
# 
# For example, suppose you have the following list of contents from six rucksacks:
# 
# vJrwpWtwJgWrhcsFMMfFFhFp
# jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
# PmmdzqPrVvPwwTWBwg
# wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
# ttgJtRGJQctTZtZT
# CrZsJsPPZsGzwwsLwLmpwMDw
# The first rucksack contains the items vJrwpWtwJgWrhcsFMMfFFhFp, which means its first compartment contains the items vJrwpWtwJgWr, while the second compartment contains the items hcsFMMfFFhFp. The only item type that appears in both compartments is lowercase p.
# The second rucksack's compartments contain jqHRNqRjqzjGDLGL and rsFMfFZSrLrFZsSL. The only item type that appears in both compartments is uppercase L.
# The third rucksack's compartments contain PmmdzqPrV and vPwwTWBwg; the only common item type is uppercase P.
# The fourth rucksack's compartments only share item type v.
# The fifth rucksack's compartments only share item type t.
# The sixth rucksack's compartments only share item type s.
# To help prioritize item rearrangement, every item type can be converted to a priority:
#   
#   Lowercase item types a through z have priorities 1 through 26.
# Uppercase item types A through Z have priorities 27 through 52.
# In the above example, the priority of the item type that appears in both compartments of each rucksack is 16 (p), 38 (L), 42 (P), 22 (v), 20 (t), and 19 (s); the sum of these is 157.
# 
# Find the item type that appears in both compartments of each rucksack. What is the sum of the priorities of those item types?

input <- read_table('03/data/input.txt', col_names = F)

input_df <- 
  sapply(input[,1], 
       function(x){
         nchar(x)
         substring(x, 1, nchar(x)/2)
         matrix(
           c(substring(x, 1, nchar(x)/2),
             substring(x, nchar(x)/2+1, nchar(x))),
           nrow = 1)
       }) %>% 
  matrix(.,
         nrow = 2, 
         byrow = T) %>%
  t(.) %>% 
  as.data.frame()

names(input_df) <- c('f', 'b')

common <- 
  apply(input_df, 1,
      function(x){
        a <- x[1] %>% str_split('') %>% unlist()
        b <- x[2] %>% str_split('') %>% unlist()
        
        intersect(a, b)
      })

common_num <- 
  sapply(common, 
       function(x){
         if(x %in% letters){
           match(x, letters)
         } else{
           match(x, LETTERS) + 26
         }
       })

sum(common_num)

# --- Part Two ---
#   As you finish identifying the misplaced items, the Elves come to you with another issue.
# 
# For safety, the Elves are divided into groups of three. Every Elf carries a badge that identifies their group. For efficiency, within each group of three Elves, the badge is the only item type carried by all three Elves. That is, if a group's badge is item type B, then all three Elves will have item type B somewhere in their rucksack, and at most two of the Elves will be carrying any other item type.
# 
# The problem is that someone forgot to put this year's updated authenticity sticker on the badges. All of the badges need to be pulled out of the rucksacks so the new authenticity stickers can be attached.
# 
# Additionally, nobody wrote down which item type corresponds to each group's badges. The only way to tell which item type is the right one is by finding the one item type that is common between all three Elves in each group.
# 
# Every set of three lines in your list corresponds to a single group, but each group can have a different badge item type. So, in the above example, the first group's rucksacks are the first three lines:
#   
#   vJrwpWtwJgWrhcsFMMfFFhFp
# jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
# PmmdzqPrVvPwwTWBwg
# And the second group's rucksacks are the next three lines:
# 
# wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
# ttgJtRGJQctTZtZT
# CrZsJsPPZsGzwwsLwLmpwMDw
# In the first group, the only item type that appears in all three rucksacks is lowercase r; this must be their badges. In the second group, their badge item type must be Z.
# 
# Priorities for these items must still be found to organize the sticker attachment efforts: here, they are 18 (r) for the first group and 52 (Z) for the second group. The sum of these is 70.
# 
# Find the item type that corresponds to the badges of each three-Elf group. What is the sum of the priorities of those item types?

input_df <- 
  input[,1] %>% 
  as.data.frame() 

input_df <- 
  matrix(input_df[,1])

input_df <- 
  matrix(input_df, ncol = 3, byrow = T) %>%
  as.data.frame()

names(input_df) <- c('e1', 'e2', 'e3')

common <- 
  apply(input_df, 1,
        function(x){
          a <- x[1] %>% str_split('') %>% unlist()
          b <- x[2] %>% str_split('') %>% unlist()
          c <- x[3] %>% str_split('') %>% unlist()
          
          Reduce(intersect, list(a,b,c))
        })

common_num <- 
  sapply(common, 
         function(x){
           if(x %in% letters){
             match(x, letters)
           } else{
             match(x, LETTERS) + 26
           }
         })

sum(common_num)




