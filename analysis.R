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
# goal is to get a global base rate for all teams for reg and post-season.
# Then we'll look at just the post-season.
# And finally we'll calculate it using only teams with similar odds as 
# the Chiefs and Eagles, which is currently Eagles -1.5 (-110) Chiefs 1.5 (-110)
# with an over/under of 50 points scored

# grab the last three years of data
nflreadr_pbp <- load_pbp(2010:2022)

# clean up some of the plays with no posteam and defteam
# these are rows indicating the game started, timeout was taken, two min warning, etc.
not_needed <- c("GAME|END QUARTER|Two-Minute Warning|Timeout|END GAME")
nflreadr_pbp_clean <- nflreadr_pbp |> dplyr::filter(!str_detect(desc, not_needed))

# identify scoring drives. any score will do. drives are numbered in the order in which they occurred in the game.
# special teams scores (punt returns for TDs etc.) are captured. See 2020_03_KC_BAL, drive #5 for an example.
scoring_drives <- nflreadr_pbp_clean |>
  dplyr::mutate(playoff = ifelse(season <= 2020 & week > 17, 1,
                                 ifelse(season > 2020 & week > 18, 1, 0))) |> 
  dplyr::group_by(game_id, old_game_id, posteam, defteam, drive) |> # roll up the data to the team-drive level
  dplyr::summarise(drive_ended_with_score = max(drive_ended_with_score),
            week = max(week),
            spread = max(spread_line), # just taking max to keep these in the result
            total = max(total_line),
            playoff = max(playoff),
            season = max(season)) |>  # just taking max to keep these in the result
  ungroup() |>
  dplyr::arrange(game_id, drive) # ensure drives are in the order they occur so we can do lag/lead stuff

# identify instances where three consecutive scores occur in a game.
three_straight_scores <- scoring_drives |>
  group_by(game_id) |>
  # to make this intuitive in the results, we have to code it in a way that is slightly confusing.
  # instead of starting with a drive and counting forwards, we find a drive and then count backwards in time.
  mutate(three_in_a_row = ifelse(lag(drive_ended_with_score, n = 2) == 1 & 
                                 lag(drive_ended_with_score, n = 1) == 1 & 
                                 drive_ended_with_score == 1, 1, 0)) |> 
  dplyr::select(game_id, old_game_id, week, posteam, defteam, drive, drive_ended_with_score, three_in_a_row, spread, total, playoff, season)
view(three_straight_scores)

three_straight_scores |> 
  ungroup() |> 
  filter(!is.na(three_in_a_row)) |> 
  summarize(three_in_a_row = sum(three_in_a_row))
# 2968 occurrences

as_tibble(unique(three_straight_scores$game_id)) |> nrow()
# 3507 games

three_straight_scores |>
  ungroup() |>
  filter(!is.na(three_in_a_row)) |>
  group_by(game_id) |>
  summarize(three_in_a_row = max(three_in_a_row)) |>
  mutate(three_in_a_row = ifelse(three_in_a_row > 0, 1, 0)) |> # just to ensure we didn't get a value over 1 with the max() function for some reason
  summarise(pct = mean(three_in_a_row))
# 45.2% of games had at least one "three scores in a row" event, well below the 66.8% implied probability

# but is there a relationship between Vegas team total or spread and the likelihood of three consecutive scores in a game?
# the super bowl has a relatively high total of 50. Also the scoring environment has increased since 2010, so we need to account for season
model_data <- three_straight_scores |>
  ungroup() |>
  filter(!is.na(three_in_a_row)) |>
  group_by(game_id) |>
  summarize(three_in_a_row = max(three_in_a_row), # after checking, max() did the job in the last operation, so we'll just use it alone
            total = max(total),
            spread = max(spread),
            playoff = as.factor(max(playoff)),
            season = as.factor(season))

library(brms)
library(tidybayes)

# Housekeeping...
# Use the cmdstanr backend for Stan because it's faster and more modern than the
# default rstan. Future Josh or Holly: You need to install the cmdstanr package first.
# (https://mc-stan.org/cmdstanr/) and then run cmdstanr::install_cmdstan()

# I have 64 cores at the moment.
options(mc.cores = 64, # change if you need to
         brms.backend = "cmdstanr")

# the multi-level binary logistic regression model
sb_model <- brm(formula = three_in_a_row ~ (1 | season) + total + spread + playoff,  # let the intercept vary by season
                          data = model_data, 
                          family = bernoulli(link = "logit"), # do binary logistic regression with brms
                          warmup = 500, 
                          iter = 2000, 
                          chains = 6, # change if you need to
                          cores = 12, # change if you need to
                          seed = 57)
# NOTE: Total execution time: 395.5 seconds.

# save the model
write_rds(sb_model, "sb_model.rds")
# read the model
sb_model <- read_rds("sb_model.rds")

# some general checks
summary(sb_model)
plot(sb_model)
pp_check(sb_model, ndraws = 500) # very nice fit

# make a custom theme for the super bowl
theme_sb <- function() {
  theme_minimal(base_family = "LibreFranklin") +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(family = "LibreFranklin", face = "bold"),
          axis.title = element_text(family = "LibreFranklin"),
          strip.text = element_text(family = "LibreFranklin",
                                    size = rel(1), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA))
}

# check the conditional effects for total and spread
ce1 <- conditional_effects(sb_model, effects = "total:spread")
plot(ce1, plot = FALSE)[[1]] + # so we can treat it like a ggplot object
  theme_sb() +
  labs(x = '"Vegas" total', y = "Probability of 3 scores in a row",
       title = "HIGHER TOTALS MEAN A HIGHER CHANCE FOR THREE SCORES",
       subtitle = "Meanwhile the spread doesn't seem to matter")

# let's look at the indicator for playoff games
ce2 <- conditional_effects(sb_model, effects = "total:playoff")
plot(ce2, plot = FALSE)[[1]] + # so we can treat it like a ggplot object
  theme_sb() +
  labs(x = "Total", y = "Probability of 3 scores in a row",
       title = "Games feature three straight scores less often in the playoffs",
       subtitle = "")

# grab a bunch of draws from the model
predicted_values <- model_data |> 
  add_predicted_draws(sb_model, allow_new_levels = TRUE,
                      ndraws = 100) |> 
  ungroup()

# plot 'em faceted on season
predicted_values |>
  ggplot(aes(x = .prediction, group = .draw)) +
  geom_line(stat = 'density',
            alpha = 0.1,
            colour = 'blue') +
  labs(x = "",
       title = "") +
  facet_wrap(~ season) +
  theme_sb()

# a simple table or chart (bar chart?) of these means is probably best
# for communicating the change over time
predicted_values |>
  group_by(season) |>
  summarise(three_in_a_row_prob = mean(.prediction))
# A tibble: 13 × 2
# season three_in_a_row_prob
# <fct>                <dbl>
#  1 2010                 0.376
#  2 2011                 0.360
#  3 2012                 0.358
#  4 2013                 0.403
#  5 2014                 0.388
#  6 2015                 0.437
#  7 2016                 0.433
#  8 2017                 0.437
#  9 2018                 0.498
# 10 2019                 0.474
# 11 2020                 0.518
# 12 2021                 0.549
# 13 2022                 0.463

# let's input the bet we're actually interested in
the_super_bowl <- tibble("season" = 2022, "total" = 50, "spread" = 1.5, "playoff" = 1)

# predict it
the_super_bowl <- the_super_bowl |>
  add_predicted_draws(sb_model, allow_new_levels = TRUE,
                      ndraws = 1000)

mean(the_super_bowl$.prediction)
# 55.4% is the model's guess at the true probability of three scores in a row in the super bowl

# the sportsbook's implied probability is:
prop_odds$vig_prob1
# 66.8%

# the vig on the bet is:
prop_odds$vig
# 6.3%

# putting it all together
prop_odds$vig_prob1 - mean(the_super_bowl$.prediction) - + prop_odds$vig
# we still have a positive expected value bet. Our edge is 5 percentage points, which is pretty large. We should bet "No"

