#############################  TOTAL SHOTS #####################################

# load the data
allClubs_df <- read.csv("All Clubs Data.csv")
allSquads_df <- read.csv("All Squad Data.csv")

# Convert Shots column to numeric and replace NAs with 0 in allSquads_df
allSquads_df$Shots <- as.numeric(as.character(allSquads_df$Shots))
allSquads_df$Shots[is.na(allSquads_df$Shots)] <- 0

# Add a column 'League' to allSquads_df from allClubs_df based on the common column 'Club'
allSquads_df <-
  merge(allSquads_df, allClubs_df[, c("Club", "LeagueNameLong")], by = "Club", all.x = TRUE)
#View(allSquads_df)

# Aggregate shots by club in allSquads_df
clubShots_df <-
  aggregate(allSquads_df["Shots"],
            by = list(Club = allSquads_df$Club),
            FUN = sum)
names(clubShots_df)[2] <- "totalShots"

# Merge total shots with allClubs_df
allClubs_df <- merge(allClubs_df, clubShots_df, by = "Club", all = TRUE)

# Replace any NAs with 0 in totalShots column
allClubs_df$totalShots[is.na(allClubs_df$totalShots)] <- 0

#View(allClubs_df)
write.csv(allClubs_df, "Clubs.csv", row.names = FALSE)
write.csv(allSquads_df, "Players.csv", row.names = FALSE)


########################   TOP PLAYERS  #######################################

# Read Players.csv into a dataframe
players_df <- read.csv("Players.csv")

# Replace NA and "--" values with 0
players_df[is.na(players_df)] <- 0
players_df[players_df == "--"] <- 0

# Group by league and player name and calculate total goals, assists, and shots on target
top_players_leagues <- players_df %>%
  group_by(LeagueNameLong, Player, Position) %>%
  mutate(Goals = as.numeric(Goals)) %>%
  mutate(Assist = as.numeric(Assist)) %>% # convert Assist column to numeric
  mutate(OnTarget = as.numeric(OnTarget)) %>% # convert OnTarget column to numeric
  summarise(
    Club = first(Club),
    NationalTeam = first(NT),
    Appearance = first(APP),
    TotalGoals = sum(Goals),
    TotalAssists = sum(Assist),
    ShotsOnTarget = sum(OnTarget)
  ) %>%
  ungroup() %>%
  
  # Select top 5 players by total goals in each league
  group_by(LeagueNameLong) %>%
  arrange(desc(TotalGoals)) %>%
  top_n(5, TotalGoals) %>%
  ungroup()

# Use the mutate() and case_when() functions to rename the values in the 'Position' column
top_players_leagues <- top_players_leagues %>% 
  mutate(Position = case_when(
    Position == "Fwd" ~ "Forward",
    Position == "Mid" ~ "Midfielder",
    Position == "Def" ~ "Defender",
    Position == "GK" ~ "Goalkeeper",
    TRUE ~ Position # Keep the original value if it's not one of the above
  ))

##View(top_players_leagues)
# Write top players by league to a new csv file
write.csv(top_players_leagues, "Top Players.csv", row.names = FALSE)

################################################################################



########################  ADDING PLAYER PICTURES TO CSV ########################

top_players_df <- read.csv("Top Players.csv")

top_players_df$Pictures <- NA

player_names <- unique(top_players_df$Player)

player_images <- list(
  "Rafa" = "https://www.ligaportugal.pt/liga/clube/20222023/ligaportugalbwin/278/jogador/67905/foto",
  "Evanilson" = "https://www.ligaportugal.pt/liga/clube/20222023/ligaportugalbwin/157/jogador/77101/foto",
  "David Neres" = "https://www.ligaportugal.pt/liga/clube/20222023/ligaportugalbwin/278/jogador/79105/foto",
  "Galeno" = "https://www.dragaoautentico.pt/media/13galeno.png",
  "Timo Werner" = "https://i.bundesliga.com/player/dfl-obj-00021u-dfl-clu-000017-dfl-sea-0001k6.png",
  "Serge Gnabry" = "https://i.bundesliga.com/player/dfl-obj-0027g6-dfl-clu-00000g-dfl-sea-0001k6.png",
  "Moussa Diaby" = "https://i.bundesliga.com/player/dfl-obj-002g02-dfl-clu-00000b-dfl-sea-0001k6.png",
  "Steven Berghuis" = "https://d3et0fncpz2hhr.cloudfront.net/players/2022/_550x776_fit_center-center_80_none/ajax-s-berghuis-7joybllup4h9oe6zfhcepjy6t-62f261fdecf05.png",
  "Dusan Tadic" = "https://d3et0fncpz2hhr.cloudfront.net/players/2022/_550x776_fit_center-center_80_none/ajax-d-tadic-6m2pk6odyb3atayt6u8cifahh-62f261fad8d1e.png",
  "Eric Maxim Choupo-Moting" = "https://i.bundesliga.com/player/dfl-obj-0000rp-dfl-clu-00000g-dfl-sea-0001k6.png",
  "Álvaro Morata" = "https://img-estaticos.atleticodemadrid.com/system/fotos/8240/destacado_600x600/BUSTOS_MORATA.png?1565972794",
  "Antoine Griezmann" = "https://assets.laliga.com/squad/2022/t175/p76650/2048x2048/p76650_t175_2022_1_003_000.png",
  "Paulo Dybala" = "https://media.asroma.com/prod/images/square_medium_fill/78711975b458-sito0020dybala-paulo-5646.png",
  "Jamal Musiala" = "https://i.bundesliga.com/player/dfl-obj-002gcr-dfl-clu-00000g-dfl-sea-0001k6.png",
  "Steven Bergwijn" = "https://d3et0fncpz2hhr.cloudfront.net/players/2022/_550x776_fit_center-center_80_none/ajax-s-bergwijn-6hrp4zuxh40i3tccmk5s0j5xx-62f261f934ebb.png",
  "Mohammed Kudus" = "https://d3et0fncpz2hhr.cloudfront.net/players/2022/_550x776_fit_center-center_80_none/ajax-m-kudus-p7vw4thx5wzo49h6hfabcq5m-62f26209f20ba.png",
  "Brian Brobbey" = "https://www.playmakerstats.com/img/jogadores/54/685854_ori__20200717082351_brian_brobbey.png",
  "Borja Iglesias" = "https://assets.laliga.com/squad/2022/t185/p202044/2048x2225/p202044_t185_2022_1_001_000.png",
  "Rafael Leão" = "https://assets-eu-01.kc-usercontent.com/1293c890-579f-01b7-8480-902cca7de55e/0403d37e-f8ab-4900-a269-b1bf4bafefae/Leao-Large.png",
  "Khvicha Kvaratskhelia" = "https://sortitoutsi.net/uploads/media/UVfGIrDkpA7OzNvII4EvEbH76lP1Py6cz0UxpFEG.png",
  "Christopher Nkunku" = "https://i.bundesliga.com/player/dfl-obj-002g3d-dfl-clu-000017-dfl-sea-0001k5.png",
  "Alexis Sánchez" = "https://www.footyrenders.com/render/alexis-sanchez-54.png",
  "Neymar" = "https://images.psg.media/media/27051/card-22-23_neymar.png?center=0.5,0.5&mode=crop&width=400&height=600&quality=75",
  "Mehdi Taremi" = "https://www.footyrenders.com/render/mehdi-taremi-3.png",
  "Karim Benzema" = "https://www.realmadrid.com/img/vertical_380px/380x501_benzema_20230215094340.jpg",
  "Lautaro Martínez" = "https://intermilan.bynder.com/transform/Horizontal/bdbf14ed-34c5-4faa-90e4-df12e084e3aa/L_Martinez_2x",
  "Lionel Messi" = "https://www.footyrenders.com/render/lionel-messi-403.png",
  "Gabriel Martinelli" = "https://resources.premierleague.com/premierleague/photos/players/250x250/p444145.png",
  "Mohamed Salah" = "https://resources.premierleague.com/premierleague/photos/players/250x250/p118748.png",
  "Marcus Rashford" = "https://resources.premierleague.com/premierleague/photos/players/250x250/p176297.png",
  "João Mário" = "https://www.ligaportugal.pt/media/33315/joaomario.png",
  "Goncalo Ramos" = "https://sportsmanheight.com/wp-content/uploads/2022/11/9-3.png",
  "Robert Lewandowski" = "https://assets.laliga.com/squad/2022/t178/p56764/2048x2225/p56764_t178_2022_1_001_000.png",
  "Alexandre Lacazette" = "https://b.fssta.com/uploads/application/soccer/headshots/4781.png",
  "Victor Osimhen" = "https://m.footballdatabase.eu/images/photos/players/a_282/282707.jpg",
  "Kylian Mbappé" = "https://www.pngmart.com/files/22/Kylian-Mbappe-PNG.png",
  "Harry Kane" = "https://resources.premierleague.com/premierleague/photos/players/250x250/p78830.png",
  "Erling Haaland" = "https://www.mancity.com/meta/media/233jlh2j/microsoftteams-image-127.png?width=600"
)

#View(player_images)

for (i in 1:nrow(top_players_df)) {
  player_name <- top_players_df$Player[i]
  picture_url <- player_images[[player_name]]
  if (!is.null(picture_url) && nchar(picture_url) > 0) {
    top_players_df$Pictures[i] <- picture_url
  }
}


#View(top_players_df)

write.csv(top_players_df, "Top Players.csv", row.names = FALSE)
write.csv(players_df, "Players.csv", row.names = FALSE)

################################################################################