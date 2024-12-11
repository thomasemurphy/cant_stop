library(tidyverse)

die <- 1:6

n_dice <- 4

my_numbers <- c(4, 5, 8)

n_sims <- 10000

hit_count <- 0

for (sim_count in 1: n_sims) {
  
  dice_roll <- sample(
    x = die,
    size = n_dice,
    replace = TRUE
  )
  
  dice_roll_sums <- c()
  for (counter_1 in 1 : n_dice - 1) {
    for (counter_2 in counter_1 + 1 : n_dice) {
      dice_roll_sums <- append(
        dice_roll_sums,
        dice_roll[counter_1] + dice_roll[counter_2]
      )
    }
  }
  
  unique_sums <- unique(dice_roll_sums)
  
  # not sure where the NAs are coming from
  unique_sums <- unique_sums[!is.na(unique_sums)]
  
  hit <- sum(my_numbers %in% unique_sums) > 0
  
  if (hit) {
    hit_count <- hit_count + 1
  }
  
}

hit_pct <- hit_count / n_sims

hit_pct
