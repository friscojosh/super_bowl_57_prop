# Super Bowl 57 Prop Analysis

The prop for this analysis came from [BetOnline.ag](https://www.betonline.ag/sportsbook/props). At the time I began the analysis (Monday at 11 am PST) prop odds for "3 straight scores by either team" was: yes -201, no: 150

3 straight scores by either team is somewhat ambiguous, but I am taking it to mean that
we are betting on if there will be three consecutive scores, with either team doing the scoring. There just must be three straight scores.

Play by play data is from `nflreadr`. The columns I used were:
- `game_id`: alpha-numeric game id
- `old_game_id`: gsis_id used by the NFL, just in case
- `week`: the week of the season
- `season`: the season
- `posteam`: the team possessing the ball
- `defteam`: the team on defense
- `drive`: Numeric drive number in the game.
- `fixed_drive_result`: How a drive ended. Possible values: Punt, Touchdown, Turnover, Field goal, Turnover on downs, End of half, Missed field goal, Opp touchdown, Safety, NA
- `drive_ended_with_score`: Binary indicator the drive ended with a score. Inferred from `fixed_drive_result`
- `spread_line`: The closing spread line for the game. A positive number means the home team was favored by that many points, a negative number means the away team was favored by that many points. (Source: Pro-Football-Reference)
- `total_line`: The closing total line for the game. (Source: Pro-Football-Reference)
- `desc`: Detailed string description for the given play.

I ended up running a multi-level binary logistic regression model using `brms` to predict the probability that a game will have three straight scores or not. I used Vegas total, spread, an indicator for playoffs vs regular season and the season as predictors. Spread was pretty useless. There appear to be slightly fewer "3 straight score" events in the playoffs vs the regular season, even when accounting for season and Vegas total. 

Even after accounting for the sportsbook's profit -- or vig -- a bet on "No" has positive expected value. The edge is **5** percentage points, which is large. 

Overall the fit was very nice, the result is interesting and says something important about offensive football both over time and in this season. I'm happy with outcome. 
