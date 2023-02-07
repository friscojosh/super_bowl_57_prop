# Super Bowl 57 Prop Analysis

The prop for this analysis came from [BetOnline.ag](https://www.betonline.ag/sportsbook/props). At the time I began the analysis (Monday at 11 am PST) prop odds for "3 straight scores by either team" was: yes -201, no: 150

3 straight scores by either team is somewhat ambiguous, but I am taking it to mean that
we are betting on if there will be three consecutive scores, with either team doing the scoring.

So it could be TeamA-TeamA-TeamB, TeamA-TeamB-TeamB, TeamB-TeamB-TeamB, etc. any of which would win a bet on yes.

Play by play data is from `nflreadr`. The columns I used were:
- `game_id`: alpha-numeric game id
- `old_game_id`: gsis_id used by the NFL, just in case
- `week`: the week of the season
- `posteam`: the team possessing the ball
- `defteam`: the team on defense
- `drive`: Numeric drive number in the game.
- `drive_ended_with_score`: Binary indicator the drive ended with a score.
- `spread_line`: The closing spread line for the game. A positive number means the home team was favored by that many points, a negative number means the away team was favored by that many points. (Source: Pro-Football-Reference)
- `total_line`: The closing total line for the game. (Source: Pro-Football-Reference)
- `desc`: Detailed string description for the given play.

