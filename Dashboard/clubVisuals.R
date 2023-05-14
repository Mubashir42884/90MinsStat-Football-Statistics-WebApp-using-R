club_data <- read.csv("Clubs.csv")

# create new data frame with selected columns
top_clubs <- club_data[, c("Club", "LeagueNameLong", "Position", "Points", "MatchPlayed", "Win", "Draw", "Lost", "GoalScored", "Conceded", "GoalDiff.", "totalShots")]

# filter rows where totalShots is greater than 0
top_clubs <- top_clubs[top_clubs$totalShots > 0, ]

# Add maxPoints column
top_clubs$maxPoints <- top_clubs$MatchPlayed * 3

# Add pointsRatio column
top_clubs$pointsRatio <-
  ((
    1 - (top_clubs$maxPoints - top_clubs$Points) / top_clubs$maxPoints
  )) * 100

# Round 'pointsRatio' column to 2 decimal places
top_clubs$pointsRatio <- round(top_clubs$pointsRatio, 2)

############ Success Rate Factorization

labels <-
  c(
    "Unsuccessful",
    "Less Successful",
    "Moderately Successful",
    "Successful",
    "Very Successful",
    "Most Successful",
    "Extremely Successful"
  )
breakpoints <- c(-Inf, 20, 35, 60, 70, 85, 95, 100)

top_clubs$successFactor <-
  cut(
    top_clubs$pointsRatio,
    breaks = breakpoints,
    labels = labels,
    include.lowest = TRUE
  )

View(top_clubs)

write.csv(top_clubs, "Top Clubs.csv", row.names = FALSE)
