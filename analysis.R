library(tidyverse)
library(nflreadr)
library(odds.converter)
# prop to analyze: 3 straight scores by either team. 
# odds: yes -201 no +150
# https://www.betonline.ag/sportsbook/props

# first we'll calculate what it will take to make this a plus expected value wager...
# convert the odds on the bet to implied probability and decimal form
odds <- c(-201, 150)
probs <- odds.us2prob(odds)
dec_odds <- odds.us2dec(odds)
# add vig -- the profit or tax imposed by the sports book -- and overround
# This assumes no draws and that the overround is evenly distributed between the yes and no bet.
# vig and the overrorund are related mathematically by the following 2 equations:
# 1) overround = (2vig - 1)/(1 - vig) 
# 2) vig = (overround - 1)/overround
# explainer here: https://www.matterofstats.com/what-is-vig-and-overround
prop_odds <- dplyr::tibble(vig_prob1 = probs[1],
                    vig_prob2 = probs[2]) |>
  dplyr::mutate(overround = ((1/ dec_odds[1]) + (1 / dec_odds[2])),
         vig = (overround - 1) / overround)

view(prop_odds)
# So, for this bet to be positive expected value, we would need to estimate that
# the true probability of either team scoring three times in a row is +/- 6.3%
# from the implied probability of 66.8% to break even on the wager

# get play by play data
# goal is to get a global base rate for all teams for reg and post-season.
# Then we'll look at just the post-season.
# And finally we'll calculate it using only teams with similar odds as 
# the Chiefs and Eagles, which is currently Eagles -1.5 (-110) Chiefs 1.5 (-110)
# with an over/under of 50 points scored

# grab the last 23 years of data
nflreadr_pbp <- load_pbp(2000:2022)

# clean up some of the plays with no posteam and defteam
# these are rows indicating the game started, timeout was taken, two min warning, etc.
not_needed <- c("GAME|END QUARTER|Two-Minute Warning|Timeout|END GAME")
nflreadr_pbp_clean <- nflreadr_pbp |> dplyr::filter(!str_detect(desc, not_needed))
# identify scoring drives. any score will do. drives are numbered in the order in which they occurred in the game.
# special teams scores (punt returns for TDs etc.) are captured. See 2020_03_KC_BAL, drive #5 for an example.
scoring_drives <- nflreadr_pbp_clean |>
  dplyr::mutate(playoff = ifelse(season <= 2020 & week > 17, 1,
                                 ifelse(season > 2020 & week > 18, 1, 0)),
                # create a binary variable that indicates a drive ended with a score.
                drive_ended_with_score = ifelse(fixed_drive_result %in% c("Touchdown", "Field goal", "Opp touchdown", "Safety"), 1, 0)) |> 
  dplyr::group_by(game_id, old_game_id, posteam, defteam, fixed_drive) |> # roll up the data to the team-drive level. fixed drive is needed for 2013 playoff data
  dplyr::summarise(drive_ended_with_score = max(drive_ended_with_score),
            week = max(week),
            spread = max(spread_line), # just taking max to keep these in the result
            total = max(total_line),
            playoff = max(playoff),
            season = max(season)) |>  # just taking max to keep these in the result
  ungroup() |>
  dplyr::arrange(game_id, fixed_drive) # ensure drives are in the order they occur so we can do lag/lead stuff

# identify instances where three consecutive scores occur in a game.
three_straight_scores <- scoring_drives |>
  dplyr::group_by(game_id) |>
  # to make this intuitive in the results, we have to code it in a way that is slightly confusing.
  # instead of starting with a drive and counting forwards, we find a drive and then count backwards in time.
  dplyr::mutate(three_in_a_row = ifelse(lag(drive_ended_with_score, n = 2) == 1 & 
                                 lag(drive_ended_with_score, n = 1) == 1 & 
                                 drive_ended_with_score == 1, 1, 0)) |> 
  dplyr::select(game_id, old_game_id, week, posteam, defteam, fixed_drive, drive_ended_with_score, three_in_a_row, spread, total, playoff, season)

view(three_straight_scores)

# find the number of games with at least one instance of three scores in a row
three_straight_scores |> 
  dplyr::ungroup() |> 
  dplyr::mutate(three_in_a_row = ifelse(is.na(three_in_a_row), 0, three_in_a_row),
         three_in_a_row = ifelse(three_in_a_row > 0, 1, 0)) |>
  dplyr::group_by(game_id) |>
  dplyr::summarize(three_in_a_row = max(three_in_a_row), # max() will always return 1 (or 0) at this grouping level
            season = max(season)) |> 
  # now group by season to we can count the number of games that had at least one three in a row scoring event
  dplyr::summarize(three_in_a_row = sum(three_in_a_row))
# 3317 occurrences

# find the number of games
as_tibble(unique(three_straight_scores$game_id)) |> nrow()
# 6159 games

# find the share of games with an instance of three scores in a row
three_straight_scores |>
  dplyr::ungroup() |>
  dplyr::mutate(three_in_a_row = ifelse(is.na(three_in_a_row), 0, three_in_a_row),
         three_in_a_row = ifelse(three_in_a_row > 0, 1, 0)) |>
  # group by game first to make make sure we only count one three in a row event per game, since that's all we need
  dplyr::group_by(game_id) |>
  dplyr::summarize(three_in_a_row = max(three_in_a_row)) |>
  dplyr::summarise(pct = mean(three_in_a_row))
# 53.9% of games had at least one "three scores in a row" event, well below the 66.8% implied probability

# let's make a table for the article
occurrences <- three_straight_scores |> 
  dplyr::ungroup() |>
  # there are NA generated because of the lead() function. make them 0
  dplyr::mutate(three_in_a_row = ifelse(is.na(three_in_a_row), 0, three_in_a_row)) |>
  # group by game first to make make sure we only count one three in a row event per game, since that's all we need
  dplyr::group_by(game_id) |>
  dplyr::summarize(three_in_a_row = max(three_in_a_row), # max() will always return 1 at this grouping level
            season = max(season)) |> 
  # now group by season to we can count the number of games that had at least one three in a row scoring event
  dplyr::group_by(season) |> 
  dplyr::summarize(three_in_a_row = sum(three_in_a_row))

pct_of_games <- three_straight_scores |>
  dplyr::ungroup() |>
  # there are NA generated because of the lead() function. make them 0
  dplyr::mutate(three_in_a_row = ifelse(is.na(three_in_a_row), 0, three_in_a_row)) |>
  # group by game first to make make sure we only count one three in a row event per game, since that's all we need
  dplyr::group_by(game_id) |>
  dplyr::summarize(three_in_a_row = max(three_in_a_row), # max() will always return 1 at this grouping level
            season = max(season)) |> 
  # now group by season to we can count the number of games that had at least one three in a row scoring event
  dplyr::group_by(season) |> 
  dplyr::summarise(pct = mean(three_in_a_row))

# I'd also like to see how many times this has happened in a Super Bowl
super_bowls <- three_straight_scores |>
  dplyr::ungroup() |>
  # there are NA generated because of the lead() function. make them 0
  dplyr::mutate(three_in_a_row = ifelse(is.na(three_in_a_row), 0, three_in_a_row)) |>
  # group by game first to make make sure we only count one three in a row event per game, since that's all we need
  dplyr::group_by(game_id) |>
  dplyr::summarize(sb_three_in_a_row = max(three_in_a_row), # max() will always return 1 at this grouping level
            season = max(season),
            week = max(week)) |>
  # now group by season to we can count the number of games that had at least one three in a row scoring event
  dplyr::group_by(season) |> 
  dplyr::filter(week == max(week), # this will give us the final game of the year, the Super Bowl
         season != 2022) |> # this year's SB has not happened yet!
  dplyr::summarize(sb_three_in_a_row = max(sb_three_in_a_row),
            sb_game_check = n()) # this needs to be one. if it isn't there is a problem

mean(super_bowls$sb_three_in_a_row)
# 64% of super bowls had three scores in a row -- 0.6363636

table <- occurrences |>
  dplyr::inner_join(pct_of_games, by = "season") |>
  dplyr::left_join(super_bowls, by = "season") |>
  dplyr::select(-sb_game_check)
# export for carpenter
readr::write_csv(table, "table.csv")

# is there a relationship between Vegas team total or spread and the likelihood of three consecutive scores in a game?
# the super bowl has a relatively high total of 50. Also the scoring environment has increased since 2010, so we need to account for season
model_data <- three_straight_scores |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(three_in_a_row), # lead() creates NAs that we don't need
         season >= 2010) |> # we don't need all the data for this model
  dplyr::group_by(game_id) |>
  dplyr::summarize(three_in_a_row = max(three_in_a_row), # after checking, max() did the job in the last operation, so we'll just use it alone
            total = max(total),
            spread = max(spread),
            playoff = as.factor(max(playoff)),
            season = as.factor(season))

library(brms)
library(tidybayes)
library(scales)

# Housekeeping...
# Use the cmdstanr backend for Stan because it's faster and more modern than the
# default rstan. Future Josh or Holly: You need to install the cmdstanr package first.
# (https://mc-stan.org/cmdstanr/) and then run cmdstanr::install_cmdstan()

options(mc.cores = 64, # change if you need to
         brms.backend = "cmdstanr")

# the multi-level binary logistic regression model
sb_model <- brms::brm(formula = three_in_a_row ~ (1 | season) + total + spread + playoff,  # let the intercept vary by season
                          data = model_data, 
                          family = bernoulli(link = "logit"), # do binary logistic regression with brms
                          warmup = 500, 
                          iter = 2000, 
                          chains = 6, # change if you need to
                          cores = 12, # change if you need to
                          seed = 57)
# NOTE: Total execution time: 395.5 seconds.

sb_model2 <- brms::brm(formula = three_in_a_row ~ (1 | season) + total + spread + playoff,  # let the intercept vary by season
                      data = model_data, 
                      family = bernoulli(link = "logit"), # do binary logistic regression with brms
                      warmup = 500, 
                      iter = 2000, 
                      chains = 6, # change if you need to
                      cores = 12, # change if you need to
                      seed = 6969)

sb_model3 <- brms::brm(formula = three_in_a_row ~ (1 | season) + total + spread + playoff,  # let the intercept vary by season
                       data = model_data, 
                       family = bernoulli(link = "logit"), # do binary logistic regression with brms
                       warmup = 500, 
                       iter = 2000, 
                       chains = 6, # change if you need to
                       cores = 12, # change if you need to
                       seed = 58)

# some general checks
summary(sb_model)
plot(sb_model)
pp_check(sb_model, ndraws = 500) # very nice fit

summary(sb_model2)
plot(sb_model2)
pp_check(sb_model2, ndraws = 500) # very nice fit

summary(sb_model3)
plot(sb_model3)
pp_check(sb_model3, ndraws = 500) # very nice fit

# save the model
write_rds(sb_model, "sb_model.rds")
write_rds(sb_model2, "sb_model2.rds")
write_rds(sb_model3, "sb_model3.rds")
# read the model
sb_model <- read_rds("sb_model.rds")
sb_model2 <- read_rds("sb_model2.rds")
sb_model3 <- read_rds("sb_model3.rds")


# make a custom theme for the super bowl
theme_sb <- function() {
  theme_minimal(base_family = "FiraSans") +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(family = "LibreFranklin", face = "bold"),
          axis.title = element_text(family = "FiraSans"),
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


wager <- c(50, .668) # the details of the wager
# conditional effects of the total
ce2 <- conditional_effects(sb_model, effects = "total")

# make a plot
plot(ce2, plot = FALSE)[[1]] + # so we can treat it like a ggplot object
  geom_point(aes(x = wager[1], y = wager[2]), size = 3, shape = 21, color = "gray", fill = "orange") +
  geom_line(size = 1, color = "orange") +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0, color = "gray") +
  theme_sb() +
  labs(x = '"Vegas" total', y = "Probability of 3 scores in a row",
       title = "The liklihood of three straight scores is mispriced",
       subtitle = "Conditional effects of point total and on the liklihood of three\nconsecutive scores",
       caption = "SOURCE: NFLFASTR") +
  scale_y_continuous(labels = scales::percent_format()) +
  annotate(
    geom = "curve", x = 45, y = .73, xend = 49.4, yend = .668, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 41, y = .75, label = "Super Bowl prop", hjust = "left")

# grab a bunch of draws from the model
predicted_values <- model_data |> 
  add_predicted_draws(sb_model, allow_new_levels = TRUE,
                      ndraws = 100,
                      seed = 12) |> 
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
  dplyr::group_by(season) |>
  dplyr::summarise(three_in_a_row_prob = mean(.prediction))
# A tibble: 13 ?? 2
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

# let's input the bet we're interested in and predict it
the_super_bowl_bet <- tibble("season" = 2022, "total" = 50, "spread" = 1.5, "playoff" = 1)
# predict it
the_super_bowl_bet <- the_super_bowl_bet |>
  tidybayes::add_predicted_draws(sb_model2, allow_new_levels = TRUE,
                      ndraws = 1000,
                      seed = 1580)

mean(the_super_bowl_bet$.prediction)
# 55% is the model's guess at the true probability of three scores in a row in the super bowl
# the sportsbook's implied probability is:
prop_odds$vig_prob1
# 66.8%
# the vig on the bet is:
prop_odds$vig
# 6.3%
# putting it all together
prop_odds$vig_prob1 - mean(the_super_bowl_bet$.prediction) - prop_odds$vig
# we still have a positive expected value bet. Our edge is -5.1 percentage points, which is pretty large. We can't overcome the vig
