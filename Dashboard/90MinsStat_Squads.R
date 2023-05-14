##################  LIST OF LINKS TO SCRAP

teamList <- list("https://www.espn.co.uk/football/team/squad/_/id/83/esp.barcelona",
                 "https://www.espn.co.uk/football/team/squad/_/id/86/esp.real_madrid",
                 "https://www.espn.co.uk/football/team/squad/_/id/1068/esp.atletico_madrid",
                 "https://www.espn.co.uk/football/team/squad/_/id/89/esp.real_sociedad",
                 "https://www.espn.co.uk/football/team/squad/_/id/244/esp.betis_sevilla",
                 "https://www.espn.co.uk/football/team/squad/_/id/359/eng.arsenal",
                 "https://www.espn.co.uk/football/team/squad/_/id/382/eng.man_city",
                 "https://www.espn.co.uk/football/team/squad/_/id/361/eng.newcastle",
                 "https://www.espn.co.uk/football/team/squad/_/id/360/eng.man_utd",
                 "https://www.espn.co.uk/football/team/squad/_/id/367/eng.tottenham",
                 "https://www.espn.co.uk/football/team/squad/_/id/364/eng.liverpool",
                 "https://www.espn.co.uk/football/team/squad/_/id/363/eng.chelsea",
                 "https://www.espn.co.uk/football/team/squad/_/id/114/ita.napoli",
                 "https://www.espn.co.uk/football/team/squad/_/id/103/ita.ac_milan",
                 "https://www.espn.co.uk/football/team/squad/_/id/110/ita.inter_milan",
                 "https://www.espn.co.uk/football/team/squad/_/id/104/ita.as_roma",
                 "https://www.espn.co.uk/football/team/squad/_/id/111/ita.juventus",
                 "https://www.espn.co.uk/football/team/squad/_/id/132/ger.bayern_munich",
                 "https://www.espn.co.uk/football/team/squad/_/id/11420/ger.rb_leipzig",
                 "https://www.espn.co.uk/football/team/squad/_/id/124/ger.dortmund",
                 "https://www.espn.co.uk/football/team/squad/_/id/131/ger.leverkusen",
                 "https://www.espn.co.uk/football/team/squad/_/id/160/fra.psg",
                 "https://www.espn.co.uk/football/team/squad/_/id/176/fra.marseille",
                 "https://www.espn.co.uk/football/team/squad/_/id/167/fra.lyon",
                 "https://www.espn.co.uk/football/team/squad/_/id/1929/por.benfica",
                 "https://www.espn.co.uk/football/team/squad/_/id/437/por.porto",
                 "https://www.espn.co.uk/football/team/squad/_/id/139/ned.ajax"
)


##################  LIST OF TEAM NAMES THAT ARE BEING SCRAPPED

team_name <- list(
  "Barcelona",
  "Real_Madrid",
  "Atletico_Madrid",
  "Real_Sociedad",
  "Real Betis",
  "Arsenal",
  "Manchester_City",
  "Newcastle_United",
  "Manchester_United",
  "Tottenham_Hotspur",
  "Liverpool",
  "Chelsea",
  "Napoli",
  "AC_Milan",
  "Internazionale",
  "AS_Roma",
  "Juventus",
  "Bayern_Munich",
  "RB_Leipzig",
  "Borussia_Dortmund",
  "Bayer_Leverkusen",
  "Paris_Saint-Germain",
  "Marseille",
  "Lyon",
  "Benfica",
  "FC_Porto",
  "Ajax_Amsterdam"
)

##################  AN EMPTY LIST FOR STORING ALL THE TEAM SQUAD'S DATA FRAME LATER

team_data_list <- list()


##################  THE WHOLE PROCESS OF SCRAPPING

### Loop through each team link in teamList
for (i in seq_along(teamList)) {
  
  ###  Read the HTML content from the team link
  team_html <- read_html(teamList[[i]])
  
  ###  Extract the table from the HTML content
  ###  There are 2 tables, need to clean those and join both together before assigning to a df
  
  tables <- team_html %>% html_nodes(".Table")
  
  table1_df <- tables[[1]] %>% html_table()
  table1_df <- table1_df[, -ncol(table1_df)]
  
  table1_df <- table1_df %>%
    mutate(G = NA_real_,
           SH = NA_real_,
           ST = NA_real_
    )
  table2_df <- tables[[2]] %>% html_table()
  table2_df <- table2_df %>%
    mutate(SV = NA_real_,
           GA = NA_real_
    )
  
  ###  rbind() used to bind the two tables
  team_Table <- rbind(table1_df,table2_df)
  #View(team_Table)
  
  
  # Store the team data in a dataframe with the team name as the dataframe name
  
  # to remove punctuation and spaces
  teamName <- gsub("[[:punct:][:space:]]+", "_", team_name[i]) 
  assign(teamName, team_Table)
  
  # Add the team data to the list of all team dataframes
  team_data_list[[i]] <- team_Table
}


##################  THE WHOLE PROCESS OF CLEANING THE RETRIVED DATA


### assign team names to each team data frame
team_data_list <- setNames(team_data_list, team_name)

### Separating the kit number from Player name and assigned to a new column
for (i in seq_along(team_data_list)) {
  # extract the kit number from the Name column
  team_data_list[[i]]$KitNo <-
    gsub("\\D", "", team_data_list[[i]]$Name)
  # move the KitNo column to the appropriate location
  team_data_list[[i]] <-
    team_data_list[[i]][c("Name", "KitNo", names(team_data_list[[i]])[-c(1, 3)])]
  team_data_list[[i]] <-
    # to remove the extra column added in the end
    team_data_list[[i]][,-ncol(team_data_list[[i]])]  
}

### assigning the team name for each squad in a new column 'Club'
for (i in seq_along(team_data_list)) {
  team_data_list[[i]]$Club <- team_name[i]
}


#View(team_data_list[["PSG"]])
#View(team_data_[["FC_Barcelona"]])
#names(team_data_list)

### rbind() used to bind all the squads df to a single df named 'allSquads_df'
allSquads_df <- do.call(rbind, team_data_list)

### removing extra characters from the df names to rename 'Club' column
allSquads_df$Club <- gsub("_", " ", allSquads_df$Club)

### renaming columns names
allSquads_df <- allSquads_df %>%
  rename(Player = Name,
         Kit_No = KitNo,
         Position = POS,
         Height = HT,
         Weight = WT,
         NT = NAT,
         Saves = SV,
         Conceded = GA,
         Assist = A,
         Foul = FC,
         Fouled = FA,
         YC = YC,
         RC = RC,
         Goals = G,
         Shots = SH,
         OnTarget = ST)

### recoding 'Position' column values
allSquads_df <- allSquads_df %>%
  mutate(Position = recode(Position, 
                           "G" = "GK", 
                           "D" = "Def", 
                           "M" = "Mid", 
                           "F" = "Fwd")
         )

# Remove all numbers from the "Player" column
allSquads_df$Player <- str_replace_all(allSquads_df$Player, "[[:digit:]]+", "")


View(allSquads_df)

# Write data frame to CSV file


write.csv(allSquads_df, "All Squad Data.csv", row.names = FALSE)

