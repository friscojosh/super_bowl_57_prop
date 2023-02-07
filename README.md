# Super Bowl 57 Prop Analysis
---

The prop for this analysis came from [BetOnline.ag](https://www.betonline.ag/sportsbook/props). At the time I began the analysis (Monday at 11 am PST) prop odds for "3 straight scores by either team" was: yes -201, no: 150

3 straight scores by either team is somewhat ambiguous. Can the team that scores three straight times do so
over, say 5 drives so long as the other team scores no points in between? The prop does not say.

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

---

Probably the trickiest part of this analysis is coming up with the combinations 
that a team can score three times, but not on consecutive drives, since we're unsure
exactly what the prop rules are. The permutations I coded for 2020-23 were:

- alternating. score, no score, score, no score, score (1-0-1-0-1). *This happened 365 times*
- two one one: score, score, no score, score (1-1-0-1). *This happened 12 times*
- one one two: (1-0-1-1). 
- three straight: (1-1-1)
- one two two: (1-0-0-1-1)
- two two one: (1-1-0-0-1)
- one two one one one: (1-0-0-1-0-1)
- one one two one: (1-0-1-0-0-1)