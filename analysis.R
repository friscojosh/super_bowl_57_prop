library(tidyverse)
library(nflreadr)
library(httr)
library(odds.converter)
# prop to analyze: 3 straight scores by either team. 
# note I am assuming this does not require scores on three straight POSSESSIONS.
# i.e. team A could score, then team B could have two consecutive drives where they do not score,
# then team A scores, team B does not score, and finally team A scores a third time.
# odds: yes -201 no +150
# https://www.betonline.ag/sportsbook/props

# first we'll calculate what it will take to make this a plus expected value wager...
# convert the odds on the bet to implied probability and decimal form
odds <- c(-201, 150)
probs <- odds.us2prob(odds)
dec_odds <- odds.us2dec(odds)

# add vig -- the profit or tax imposed by the sports book -- and overround
# This assumes no draws and that the overround is even distributed between the yes and no bet.
# vig  and the overrorund are related mathematically by the following 2 equations:
# 1) overround = (2vig - 1)/(1 - vig) 
# 2) vig = (overround - 1)/overround
# explainer here: https://www.matterofstats.com/what-is-vig-and-overround
prop_odds <- tibble(vig_prob1 = probs[1],
                    vig_prob2 = probs[2]) |>
  mutate(overround = ((1/ dec_odds[1]) + (1 / dec_odds[2])),
         vig = (overround - 1) / overround)
view(prop_odds)

# So, for this bet to be positive expected value, we would need to estimate that
# the true probability of either team scoring three times in a row is +/- 6.3%
# from the implied probability of 66.8% to break even on the wager

# get play by play data to calculate % of drives ending in a score
# goal is to get a global base rate for all teams. Afterward we'll calculate it
# using only teams with similar moneyline odds as the Chiefs and Eagles, which is currently 
# Eagles -1.5 (-110) Chiefs 1.5 (-110)

# grab the last three years of data
nflreadr_pbp <- load_pbp(2020:2022)

# clean up some of the plays with no posteam and defteam
# these are rows indicating the game started, timeout was taken, two min warning, etc.
not_needed <- c("GAME|END QUARTER|Two-Minute Warning|Timeout|END GAME")
nflreadr_pbp_clean <- nflreadr_pbp |> dplyr::filter(!str_detect(desc, not_needed))

# identify scoring drives. any score will do. drives are numbered in the order in which they occurred in the game.
# special teams scores (punt returns for TDs etc.) are captured. See 2020_03_KC_BAL, drive #5 for an example.
scoring_drives <- nflreadr_pbp_clean |>
  group_by(game_id, old_game_id, posteam, defteam, drive) |> # roll up the data to the team-drive level
  summarise(drive_ended_with_score = max(drive_ended_with_score),
            week = max(week),
            spread = max(spread_line), # just taking max to keep these in the result
            total = max(total_line)) |>  # just taking max to keep these in the result
  ungroup() |>
  arrange(game_id, drive) # ensure drives are in the order they occur so we can do lag/lead stuff

# identify instances where three consecutive scores occur in a game.
three_straight_scores <- scoring_drives |>
  group_by(game_id) |>
  # to make this intuitive in the results, we have to code it in a way that is slightly confusing.
  # instead of starting with a drive and counting forwards, we find a drive and then count backwards in time.
  mutate(three_in_a_row = ifelse(lag(drive_ended_with_score, n = 2) == 1 & 
                                 lag(drive_ended_with_score, n = 1) == 1 & 
                                 drive_ended_with_score == 1, 1, 0)) |> 
  dplyr::select(game_id, old_game_id, week, posteam, defteam, drive, drive_ended_with_score, three_in_a_row, spread, total)
view(three_straight_scores)

three_straight_scores |> 
  ungroup() |> 
  filter(!is.na(three_in_a_row)) |> 
  summarize(three_in_a_row = sum(three_in_a_row))
# 875 occurrences
