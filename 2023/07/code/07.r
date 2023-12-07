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

# --- Day 7: Camel Cards ---
#   Your all-expenses-paid trip turns out to be a one-way, five-minute ride in an airship. (At least it's a cool airship!) It drops you off at the edge of a vast desert and descends back to Island Island.
# 
# "Did you bring the parts?"
# 
# You turn around to see an Elf completely covered in white clothing, wearing goggles, and riding a large camel.
# 
# "Did you bring the parts?" she asks again, louder this time. You aren't sure what parts she's looking for; you're here to figure out why the sand stopped.
#                                                                                           
#                                                                                           "The parts! For the sand, yes! Come with me; I will show you." She beckons you onto the camel.
#                                                                                           
#                                                                                           After riding a bit across the sands of Desert Island, you can see what look like very large rocks covering half of the horizon. The Elf explains that the rocks are all along the part of Desert Island that is directly above Island Island, making it hard to even get there. Normally, they use big machines to move the rocks and filter the sand, but the machines have broken down because Desert Island recently stopped receiving the parts they need to fix the machines.
#                                                                                           
#                                                                                           You've already assumed it'll be your job to figure out why the parts stopped when she asks if you can help. You agree automatically.
#                                                                                           
#                                                                                           Because the journey will take a few days, she offers to teach you the game of Camel Cards. Camel Cards is sort of similar to poker except it's designed to be easier to play while riding a camel.
# 
# In Camel Cards, you get a list of hands, and your goal is to order them based on the strength of each hand. A hand consists of five cards labeled one of A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2. The relative strength of each card follows this order, where A is the highest and 2 is the lowest.
# 
# Every hand is exactly one type. From strongest to weakest, they are:
# 
# Five of a kind, where all five cards have the same label: AAAAA
# Four of a kind, where four cards have the same label and one card has a different label: AA8AA
# Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
# Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
# Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
# One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
# High card, where all cards' labels are distinct: 23456
#                                                                                           Hands are primarily ordered based on type; for example, every full house is stronger than any three of a kind.
#                                                                                           
#                                                                                           If two hands have the same type, a second ordering rule takes effect. Start by comparing the first card in each hand. If these cards are different, the hand with the stronger first card is considered stronger. If the first card in each hand have the same label, however, then move on to considering the second card in each hand. If they differ, the hand with the higher second card wins; otherwise, continue with the third card in each hand, then the fourth, then the fifth.
#                                                                                           
#                                                                                           So, 33332 and 2AAAA are both four of a kind hands, but 33332 is stronger because its first card is stronger. Similarly, 77888 and 77788 are both a full house, but 77888 is stronger because its third card is stronger (and both hands have the same first and second card).
#                                                                                           
#                                                                                           To play Camel Cards, you are given a list of hands and their corresponding bid (your puzzle input). For example:
#                                                                                             
#                                                                                             32T3K 765
#                                                                                           T55J5 684
#                                                                                           KK677 28
#                                                                                           KTJJT 220
#                                                                                           QQQJA 483
#                                                                                           This example shows five hands; each hand is followed by its bid amount. Each hand wins an amount equal to its bid multiplied by its rank, where the weakest hand gets rank 1, the second-weakest hand gets rank 2, and so on up to the strongest hand. Because there are five hands in this example, the strongest hand will have rank 5 and its bid will be multiplied by 5.
#                                                                                           
#                                                                                           So, the first step is to put the hands in order of strength:
#                                                                                             
#                                                                                             32T3K is the only one pair and the other hands are all a stronger type, so it gets rank 1.
#                                                                                           KK677 and KTJJT are both two pair. Their first cards both have the same label, but the second card of KK677 is stronger (K vs T), so KTJJT gets rank 2 and KK677 gets rank 3.
#                                                                                           T55J5 and QQQJA are both three of a kind. QQQJA has a stronger first card, so it gets rank 5 and T55J5 gets rank 4.
#                                                                                           Now, you can determine the total winnings of this set of hands by adding up the result of multiplying each hand's bid with its rank (765 * 1 + 220 * 2 + 28 * 3 + 684 * 4 + 483 * 5). So the total winnings in this example are 6440.
# 
# Find the rank of every hand in your set. What are the total winnings?

input <- readLines('07/data/input.in') 
# input <- readLines('07/data/ex1.txt')

# one millisecond holding down the button --> boat's speed increases by one millimeter per millisecond


card <- c('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3',  '2')
strength <- length(card):1
strength_df <- 
  tibble(
    card,
    strength
  )
hand_str <- c('5oaK', '4oaK','fullHouse','3oaK','2pair','1pair', 'high card')
hand_rank <- length(hand_str):1
m1 <- c(5, 4, 3, 3, 2, 2, 1)
m2 <- c(NA, NA, 2, NA, 2, NA, NA)

hand_val <- 
  tibble(
    hand_str, 
    hand_rank = hand_rank,
    match1_freq = as.integer(m1), 
    match2_freq = as.integer(m2)
  )
# tied hands are broken by strongest lead card

input_v <- 
  lapply(input, function(x){
    str_split(x, ' ')
  }) %>% unlist() 

input_df <- 
  matrix(input_v, ncol = 2, byrow = T) %>% 
  as_tibble()
names(input_df) <- c('hand', 'bid')

hand_res <- 
  lapply(input_df$hand, 
       function(x){
         freq_table <- 
           x %>% 
           str_split("") %>% 
           table() %>% 
           as.data.frame()
         
         names(freq_table)[1] <- 'card'
         
         freq_table <- freq_table %>% 
           left_join(strength_df, by = join_by(card)) %>% 
           arrange(desc(strength))

         # print(freq_table)
         
         match1 = (freq_table %>% filter(Freq == max(Freq)))[1,1] %>% as.character()
         match1_freq = (freq_table %>% filter(Freq == max(Freq)))[1,2] 
         
         match2 = NA
         match2_freq = NA
         
        if((nrow(freq_table) == 2) & 
                   all(freq_table$Freq != 4)
                   ){
         match2 = (freq_table %>% filter(Freq != max(Freq)))[1,1]
         match2_freq = (freq_table %>% filter(Freq != max(Freq)))[1,2]
         } else if(
         nrow(freq_table %>% filter(Freq == max(Freq))) == 2
         ){
           match2 = (freq_table %>% filter(Freq == max(Freq)))[2,1]
           match2_freq = (freq_table %>% filter(Freq == max(Freq)))[2,2]
         } else if(max(freq_table$Freq) == 1){
           match1 = (freq_table %>% arrange(desc(strength)))[1,1]
           match1_freq = 1
           
           match2 = NA
           match2_freq = NA
         }
         
         a <- 
           tibble(
             match1 = match1, 
             match1_freq = match1_freq, 
             match2 = match2,
             match2_freq = match2_freq
           )
         # print(a)
       }) %>% 
  bind_rows()

input_df_res <- 
  input_df %>% 
  bind_cols(hand_res) %>% 
  left_join(hand_val)

input_df_res$c1_str = strength_df$strength[match(substring(input_df_res$hand, 1, 1), strength_df$card)]
input_df_res$c2_str = strength_df$strength[match(substring(input_df_res$hand, 2, 2), strength_df$card)]
input_df_res$c3_str = strength_df$strength[match(substring(input_df_res$hand, 3, 3), strength_df$card)]
input_df_res$c4_str = strength_df$strength[match(substring(input_df_res$hand, 4, 4), strength_df$card)]
input_df_res$c5_str = strength_df$strength[match(substring(input_df_res$hand, 5, 5), strength_df$card)]

input_df_res <- 
  input_df_res %>% 
  arrange(desc(hand_rank), 
          desc(c1_str),
          desc(c2_str),
          desc(c3_str),
          desc(c4_str),
          desc(c5_str)
  ) %>% 
  mutate(hand_rank_final = n():1) %>% 
  mutate(bid = as.numeric(bid)) %>% 
  mutate(winnings = bid * hand_rank_final)

p1 <- sum(input_df_res$winnings)

p1



# --- Part Two ---
#   To make things a little more interesting, the Elf introduces one additional rule. Now, J cards are jokers - wildcards that can act like whatever card would make the hand the strongest type possible.
# 
# To balance this, J cards are now the weakest individual cards, weaker even than 2. The other cards stay in the same order: A, K, Q, T, 9, 8, 7, 6, 5, 4, 3, 2, J.
# 
# J cards can pretend to be whatever card is best for the purpose of determining hand type; for example, QJJQ2 is now considered four of a kind. However, for the purpose of breaking ties between two hands of the same type, J is always treated as J, not the card it's pretending to be: JKKK2 is weaker than QQQQ2 because J is weaker than Q.
# 
# Now, the above example goes very differently:
# 
# 32T3K 765
# T55J5 684
# KK677 28
# KTJJT 220
# QQQJA 483
# 32T3K is still the only one pair; it doesn't contain any jokers, so its strength doesn't increase.
# KK677 is now the only two pair, making it the second-weakest hand.
# T55J5, KTJJT, and QQQJA are now all four of a kind! T55J5 gets rank 3, QQQJA gets rank 4, and KTJJT gets rank 5.
# With the new joker rule, the total winnings in this example are 5905.
# 
# Using the new joker rule, find the rank of every hand in your set. What are the new total winnings?


card <- c('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3',  '2', 'J')
strength <- length(card):1
strength_df <- 
  tibble(
    card,
    strength
  )
# tied hands are broken by strongest lead card
hand_res <- 
  lapply(input_df$hand, 
         function(x){
           freq_table <- 
             x %>% 
             str_split("") %>% 
             table() %>% 
             as.data.frame() %>% 
             arrange(desc(Freq)) 
           
           names(freq_table)[1] <- 'card'
           a <- freq_table$card[freq_table$card != 'J'][1] %>% as.character()
           
           freq_table <- freq_table %>% 
             mutate(card = as.character(card)) %>%
             mutate(card = ifelse(card == 'J', a, card)) %>% 
             group_by(card) %>% 
             summarise(Freq = sum(Freq)) %>%
             left_join(strength_df, by = join_by(card)) %>% 
             arrange(desc(strength)) %>% 
             as.data.frame()
           
           # print(freq_table)
           
           match1 = (freq_table %>% filter(Freq == max(Freq)))[1,1] %>% as.character()
           match1_freq = (freq_table %>% filter(Freq == max(Freq)))[1,2] 
           
           match2 = NA
           match2_freq = NA
           
           if((nrow(freq_table) == 2) & 
              all(freq_table$Freq != 4)
           ){
             match2 = (freq_table %>% filter(Freq != max(Freq)))[1,1]
             match2_freq = (freq_table %>% filter(Freq != max(Freq)))[1,2]
           } else if(
             nrow(freq_table %>% filter(Freq == max(Freq))) == 2
           ){
             match2 = (freq_table %>% filter(Freq == max(Freq)))[2,1]
             match2_freq = (freq_table %>% filter(Freq == max(Freq)))[2,2]
           } else if(max(freq_table$Freq) == 1){
             match1 = (freq_table %>% arrange(desc(strength)))[1,1]
             match1_freq = 1
             
             match2 = NA
             match2_freq = NA
           }
           
           a <- 
             tibble(
               match1 = match1, 
               match1_freq = match1_freq, 
               match2 = match2,
               match2_freq = match2_freq
             )
           # print(a)
         }) %>% 
  bind_rows()

input_df_res <- 
  input_df %>% 
  bind_cols(hand_res) %>% 
  left_join(hand_val)

input_df_res$c1_str = strength_df$strength[match(substring(input_df_res$hand, 1, 1), strength_df$card)]
input_df_res$c2_str = strength_df$strength[match(substring(input_df_res$hand, 2, 2), strength_df$card)]
input_df_res$c3_str = strength_df$strength[match(substring(input_df_res$hand, 3, 3), strength_df$card)]
input_df_res$c4_str = strength_df$strength[match(substring(input_df_res$hand, 4, 4), strength_df$card)]
input_df_res$c5_str = strength_df$strength[match(substring(input_df_res$hand, 5, 5), strength_df$card)]

input_df_res <- 
  input_df_res %>% 
  arrange(desc(hand_rank), 
          desc(c1_str),
          desc(c2_str),
          desc(c3_str),
          desc(c4_str),
          desc(c5_str)
  ) %>% 
  mutate(hand_rank_final = n():1) %>% 
  mutate(bid = as.numeric(bid)) %>% 
  mutate(winnings = bid * hand_rank_final)



p2 <- sum(input_df_res$winnings)
p2

ans <-  matrix(c(p1, p2)) 
write_lines(ans, '07/data/input.ans')

