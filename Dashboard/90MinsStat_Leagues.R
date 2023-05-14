#############################  EPL CLUBS #####################################
epl.url <- "https://www.espn.co.uk/football/table/_/league/eng.1"

eplClubs <- read_html(epl.url) %>%
  html_nodes(".Table") %>%
  html_table()
eplClubs <- data.frame(eplClubs)

# Extract position from the first column
eplClubs$Position <- gsub("[^0-9]+", "", eplClubs$X2022.2023)
eplClubs$X2022.2023 <- gsub("\\d+", "", eplClubs$X2022.2023)

# Remove the first 3 characters from the new 'Position' column
eplClubs$X2022.2023 <- substr(eplClubs$X2022.2023, 4, nchar(eplClubs$X2022.2023))

# Rename columns
eplClubs <- rename(eplClubs, Club = "X2022.2023", 
                   MatchPlayed = "GP",
                   Win = "W",
                   Draw = "D",
                   Lost = "L",
                   GoalScored = "F",
                   Conceded = "A",
                   GoalDiff. = "GD",
                   Points = "P")

# Mutate new Columns
eplClubs <- mutate(eplClubs,
                   LeagueNameLong = "English Premier League",
                   LeagueNameShort = "EPL",
                   LeagueSeason = "2022/23")

# Rearrange columns
eplClubs <- eplClubs[c(11, 10, 1, 9, 2:8, 12, 13)]


##View(eplClubs)

################################################################################


#############################  La Liga CLUBS #####################################
laliga.url <- "https://www.espn.co.uk/football/table/_/league/esp.1"

laligaClubs <- read_html(laliga.url) %>%
  html_nodes(".Table") %>%
  html_table()
laligaClubs <- data.frame(laligaClubs)

# Extract position from the first column
laligaClubs$Position <- gsub("[^0-9]+", "", laligaClubs$X2022.2023)
laligaClubs$X2022.2023 <- gsub("\\d+", "", laligaClubs$X2022.2023)

# Remove the first 3 characters from the new 'Position' column
laligaClubs$X2022.2023 <- substr(laligaClubs$X2022.2023, 4, nchar(laligaClubs$X2022.2023))

# Rename columns
laligaClubs <- rename(laligaClubs, Club = "X2022.2023", 
                      MatchPlayed = "GP",
                      Win = "W",
                      Draw = "D",
                      Lost = "L",
                      GoalScored = "F",
                      Conceded = "A",
                      GoalDiff. = "GD",
                      Points = "P")

# Mutate new Columns
laligaClubs <- mutate(laligaClubs,
                      LeagueNameLong = "Spanish La Liga",
                      LeagueNameShort = "La Liga",
                      LeagueSeason = "2022/23")

# Rearrange columns
laligaClubs <- laligaClubs[c(11, 10, 1, 9, 2:8, 12, 13)]


##View(laligaClubs)

################################################################################


#############################  Serie-A CLUBS #####################################
seria.url <- "https://www.espn.co.uk/football/table/_/league/ita.1"

seriaClubs <- read_html(seria.url) %>%
  html_nodes(".Table") %>%
  html_table()
seriaClubs <- data.frame(seriaClubs)

# Extract position from the first column
seriaClubs$Position <- gsub("[^0-9]+", "", seriaClubs$X2022.2023)
seriaClubs$X2022.2023 <- gsub("\\d+", "", seriaClubs$X2022.2023)

# Remove the first 3 characters from the new 'Position' column
seriaClubs$X2022.2023 <- substr(seriaClubs$X2022.2023, 4, nchar(seriaClubs$X2022.2023))

# Edit false club name
seriaClubs <- seriaClubs %>%
  mutate(X2022.2023 = ifelse(X2022.2023 == "AAS Roma", "AS Roma", X2022.2023))

# Rename columns
seriaClubs <- rename(seriaClubs, Club = "X2022.2023", 
                     MatchPlayed = "GP",
                     Win = "W",
                     Draw = "D",
                     Lost = "L",
                     GoalScored = "F",
                     Conceded = "A",
                     GoalDiff. = "GD",
                     Points = "P")

# Mutate new Columns
seriaClubs <- mutate(seriaClubs,
                     LeagueNameLong = "Italian Serie-A",
                     LeagueNameShort = "Serie-A",
                     LeagueSeason = "2022/23")

# Rearrange columns
seriaClubs <- seriaClubs[c(11, 10, 1, 9, 2:8, 12, 13)]


##View(seriaClubs)

################################################################################

##########################  Bundesliga CLUBS ###################################
bundliga.url <- "https://www.espn.co.uk/football/table/_/league/ger.1"

bundligaClubs <- read_html(bundliga.url) %>%
  html_nodes(".Table") %>%
  html_table()
bundligaClubs <- data.frame(bundligaClubs)


# Extract position from the first column
# Extract first 1 or 2 numbers before any characters come
bundligaClubs$Position <- gsub("([0-9]{1,2}).*", "\\1", bundligaClubs$X2022.23)
bundligaClubs$X2022.23 <- gsub("^\\d{1,2}", "", bundligaClubs$X2022.23)

# Remove the first 3 characters from the new 'Position' column
bundligaClubs$X2022.23 <- substr(bundligaClubs$X2022.23, 4, nchar(bundligaClubs$X2022.23))

# Rename columns
bundligaClubs <- rename(bundligaClubs, Club = "X2022.23", 
                        MatchPlayed = "GP",
                        Win = "W",
                        Draw = "D",
                        Lost = "L",
                        GoalScored = "F",
                        Conceded = "A",
                        GoalDiff. = "GD",
                        Points = "P")

# Mutate new Columns
bundligaClubs <- mutate(bundligaClubs,
                        LeagueNameLong = "German Bundesliga",
                        LeagueNameShort = "Bundesliga",
                        LeagueSeason = "2022/23")

# Rearrange columns
bundligaClubs <- bundligaClubs[c(11, 10, 1, 9, 2:8, 12, 13)]


##View(bundligaClubs)

################################################################################

#############################  Ligue-1 CLUBS #####################################
lig1.url <- "https://www.espn.co.uk/football/table/_/league/fra.1"

lig1Clubs <- read_html(lig1.url) %>%
  html_nodes(".Table") %>%
  html_table()
lig1Clubs <- data.frame(lig1Clubs)

# Extract position from the first column
lig1Clubs$Position <- gsub("[^0-9]+", "", lig1Clubs$X2022.23)
lig1Clubs$X2022.23 <- gsub("\\d+", "", lig1Clubs$X2022.23)

# Remove the first 3 characters from the new 'Position' column
lig1Clubs$X2022.23 <- substr(lig1Clubs$X2022.23, 4, nchar(lig1Clubs$X2022.23))

# Edit false club name
lig1Clubs <- lig1Clubs %>%
  mutate(X2022.23 = ifelse(X2022.23 == "LLille", "Lille", X2022.23)) %>%
  mutate(X2022.23 = ifelse(X2022.23 == "ENice", "Nice", X2022.23)) %>%
  mutate(X2022.23 = ifelse(X2022.23 == "NLyon", "Lyon", X2022.23)) %>%
  mutate(X2022.23 = ifelse(X2022.23 == "SBrest", "Brest", X2022.23)) %>%
  mutate(X2022.23 = ifelse(X2022.23 == "MStade de Reims", "Stade de Reims", X2022.23))




# Rename columns
lig1Clubs <- rename(lig1Clubs, Club = "X2022.23", 
                    MatchPlayed = "GP",
                    Win = "W",
                    Draw = "D",
                    Lost = "L",
                    GoalScored = "F",
                    Conceded = "A",
                    GoalDiff. = "GD",
                    Points = "P")

# Mutate new Columns
lig1Clubs <- mutate(lig1Clubs,
                    LeagueNameLong = "French Ligue 1",
                    LeagueNameShort = "Ligue-1",
                    LeagueSeason = "2022/23")

# Rearrange columns
lig1Clubs <- lig1Clubs[c(11, 10, 1, 9, 2:8, 12, 13)]


##View(lig1Clubs)

################################################################################

#############################  Primeira Liga CLUBS #############################
prima.url <- "https://www.espn.co.uk/football/table/_/league/por.1"

primaClubs <- read_html(prima.url) %>%
  html_nodes(".Table") %>%
  html_table()
primaClubs <- data.frame(primaClubs)

# Extract position from the first column
primaClubs$Position <- gsub("[^0-9]+", "", primaClubs$X2022.2023)
primaClubs$X2022.2023 <- gsub("\\d+", "", primaClubs$X2022.2023)

# Remove the first 3 characters from the new 'Position' column
primaClubs$X2022.2023 <- substr(primaClubs$X2022.2023, 4, nchar(primaClubs$X2022.2023))

# Edit false club name
primaClubs <- primaClubs %>%
  mutate(X2022.2023 = ifelse(X2022.2023 == "GBraga", "Braga", X2022.2023)) %>%
  mutate(X2022.2023 = ifelse(X2022.2023 == "CGil Vicente", "Gil Vicente", X2022.2023)) %>%
  mutate(X2022.2023 = ifelse(X2022.2023 == "OPortimonense", "Portimonense", X2022.2023))

# Rename columns
primaClubs <- rename(primaClubs, Club = "X2022.2023", 
                     MatchPlayed = "GP",
                     Win = "W",
                     Draw = "D",
                     Lost = "L",
                     GoalScored = "F",
                     Conceded = "A",
                     GoalDiff. = "GD",
                     Points = "P")

# Mutate new Columns
primaClubs <- mutate(primaClubs,
                     LeagueNameLong = "Portuguese Primeira Liga",
                     LeagueNameShort = "Primeira Liga",
                     LeagueSeason = "2022/23")

# Rearrange columns
primaClubs <- primaClubs[c(11, 10, 1, 9, 2:8, 12, 13)]


##View(primaClubs)

################################################################################

#############################  Dutch Eredivisie CLUBS #############################
erdvsi.url <- "https://www.espn.co.uk/football/table/_/league/ned.1"

erdvsiClubs <- read_html(erdvsi.url) %>%
  html_nodes(".Table") %>%
  html_table()
erdvsiClubs <- data.frame(erdvsiClubs)

# Extract position from the first column
erdvsiClubs$Position <- gsub("[^0-9]+", "", erdvsiClubs$X2022.2023)
erdvsiClubs$X2022.2023 <- gsub("\\d+", "", erdvsiClubs$X2022.2023)

# Remove the first 3 characters from the new 'Position' column
erdvsiClubs$X2022.2023 <- substr(erdvsiClubs$X2022.2023, 4, nchar(erdvsiClubs$X2022.2023))

# Edit false club name
erdvsiClubs <- erdvsiClubs %>%
  mutate(X2022.2023 = ifelse(X2022.2023 == "Z Alkmaar", "AZ Alkmaar", X2022.2023))

# Rename columns
erdvsiClubs <- rename(erdvsiClubs, Club = "X2022.2023", 
                      MatchPlayed = "GP",
                      Win = "W",
                      Draw = "D",
                      Lost = "L",
                      GoalScored = "F",
                      Conceded = "A",
                      GoalDiff. = "GD",
                      Points = "P")

# Mutate new Columns
erdvsiClubs <- mutate(erdvsiClubs,
                      LeagueNameLong = "Dutch Eredivisie",
                      LeagueNameShort = "Eredivisie",
                      LeagueSeason = "2022/23")

# Rearrange columns
erdvsiClubs <- erdvsiClubs[c(11, 10, 1, 9, 2:8, 12, 13)]


##View(erdvsiClubs)

################################################################################



# rbind() to add all the clubs to a new dataframe

allClubs_df <- rbind(eplClubs, 
                  laligaClubs,
                  seriaClubs, 
                  bundligaClubs, 
                  erdvsiClubs, 
                  lig1Clubs, 
                  primaClubs)


View(allClubs_df)

write.csv(allClubs_df, "All Clubs Data.csv", row.names = FALSE)

