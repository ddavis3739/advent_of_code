# setup 
rm(list = ls())
# set to repo wd
setwd('/Users/andrewdavis/Documents/git_repos/advent_of_code/2022')

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

# --- Day 6: Tuning Trouble ---
#   The preparations are finally complete; you and the Elves leave camp on foot and begin to make your way toward the star fruit grove.
# 
# As you move through the dense undergrowth, one of the Elves gives you a handheld device. He says that it has many fancy features, but the most important one to set up right now is the communication system.
# 
# However, because he's heard you have significant experience dealing with signal-based systems, he convinced the other Elves that it would be okay to give you their one malfunctioning device - surely you'll have no problem fixing it.
# 
# As if inspired by comedic timing, the device emits a few colorful sparks.
# 
# To be able to communicate with the Elves, the device needs to lock on to their signal. The signal is a series of seemingly-random characters that the device receives one at a time.
# 
# To fix the communication system, you need to add a subroutine to the device that detects a start-of-packet marker in the datastream. In the protocol being used by the Elves, the start of a packet is indicated by a sequence of four characters that are all different.
# 
# The device will send your subroutine a datastream buffer (your puzzle input); your subroutine needs to identify the first position where the four most recently received characters were all different. Specifically, it needs to report the number of characters from the beginning of the buffer to the end of the first such four-character marker.
# 
# For example, suppose you receive the following datastream buffer:
#   
#   mjqjpqmgbljsphdztnvjfqwrcgsmlb
# After the first three characters (mjq) have been received, there haven't been enough characters received yet to find the marker. The first time a marker could occur is after the fourth character is received, making the most recent four characters mjqj. Because j is repeated, this isn't a marker.
# 
# The first time a marker appears is after the seventh character arrives. Once it does, the last four characters received are jpqm, which are all different. In this case, your subroutine should report the value 7, because the first start-of-packet marker is complete after 7 characters have been processed.
# 
# Here are a few more examples:
#   
#   bvwbjplbgvbhsrlpgdmjqwftvncz: first marker after character 5
# nppdvjthqldpwncqszvftbrmjlhg: first marker after character 6
# nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg: first marker after character 10
# zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw: first marker after character 11
# How many characters need to be processed before the first start-of-packet marker is detected?

input <- readLines('06/data/input.txt')

input_split <- 
  input %>% 
  str_split(., '') %>% 
  unlist()

input_split_check4 <- 
  sapply(1:length(input_split),
         function(x){
           if ((x-3) >= 1) {
             length(
               unique(input_split[(x-3):x])
             )
           } else {
             FALSE
           }
         })

ans <- (which(input_split_check4==4)[1]-3):(which(input_split_check4==4)[1])
ans[4]

# --- Part Two ---
#   Your device's communication system is correctly detecting packets, but still isn't working. It looks like it also needs to look for messages.
# 
# A start-of-message marker is just like a start-of-packet marker, except it consists of 14 distinct characters rather than 4.
# 
# Here are the first positions of start-of-message markers for all of the above examples:
#   
#   mjqjpqmgbljsphdztnvjfqwrcgsmlb: first marker after character 19
# bvwbjplbgvbhsrlpgdmjqwftvncz: first marker after character 23
# nppdvjthqldpwncqszvftbrmjlhg: first marker after character 23
# nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg: first marker after character 29
# zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw: first marker after character 26
# How many characters need to be processed before the first start-of-message marker is detected?

input_split_check14 <- 
  sapply(1:length(input_split),
         function(x){
           if ((x-13) >= 1) {
             length(
               unique(input_split[(x-13):x])
             )
           } else {
             FALSE
           }
         })

ans <- (which(input_split_check14==14)[1]-13):(which(input_split_check14==14)[1])
ans[14]


# function version
# S/O to reddit user u/Minigodzz

library(magrittr)
library(zoo, include.only = "rollapply")

data <- scan("06/data/input.txt", "raw")

is_unique_roll <- function(vec, window){
  vec %>%
    rollapply(width = window,
              function(x){
                length(unique(x)) == length(x)
              })
}

find_marker <- function(signal, window){
  signal %>%
    strsplit("") %>%
    unlist %>%
    is_unique_roll(window) %>%
    match(TRUE, .) %>%
    `+`(window - 1)
}

data %>%
  strsplit("") %>%
  unlist %>%
  is_unique_roll(4) %>%
  match(TRUE, .) %>%
  `+`(4 - 1)

data %>%
  find_marker(4)
data %>%
  find_marker(14)
