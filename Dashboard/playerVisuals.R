### Merging Top players and All players

players_df <- read.csv("Players.csv")

# merge the two data frames
merged_df <- merge(players_df, top_players_df[c("Player", "Pictures")], 
                   by = "Player", all.x = TRUE)

# replace missing picture links with the default link
merged_df$Pictures[is.na(merged_df$Pictures)] <- "https://www.onlygfx.com/wp-content/uploads/2020/02/football-soccer-silhouette-4.png"

# add the "Pictures" column to "players_df"
players_df$Pictures <- merged_df$Pictures


# Define a dictionary to map the old values to new values
position_dict <- c("Fwd" = "Forward",
                   "Mid" = "Midfielder",
                   "Def" = "Defender",
                   "GK" = "Goalkeeper")

# Replace the old values in the "Position" column with the new values
players_df$Position <- replace(players_df$Position, players_df$Position %in% names(position_dict), position_dict)

##############   MERGE Picture Links

# Merge the two data frames on the 'Players' column
merged_df <- merge(players_df, top_players_df, by = "Player", all.x = TRUE)

# Replace the 'Pictures' column in 'players_df' with corresponding values from 'top_players_df'
players_df$Pictures <- ifelse(!is.na(merged_df$Pictures.y), merged_df$Pictures.y, players_df$Pictures)

write.csv(players_df, "Players.csv", row.names = FALSE)
players_df <- read.csv("Players.csv")
View(players_df)
