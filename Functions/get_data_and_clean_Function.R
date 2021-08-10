
################# GET DATA AND CLEAN FUNCTIONS #################
 
# This script reads data from fitzRoy and manupulates in into a clean dataset from 2002 onwards

############## 
#LOAD PACKAGES
##############

library(tidyverse)
library(dplyr)
library(elo)
library(lubridate)
library(fitzRoy)
library(RMySQL)
library(lubridate)




#######################
# GET DATA FROM FITZROY
#######################



GetDataCleanFitzRoy <- function() {

  # get current year for data retriveal end point
  current_date <- Sys.Date()
  current_year <- year(current_date)
  
  # get player stats from fitzRoy
  dat <- fitzRoy::get_fryzigg_stats(start = 2002, end = current_year)
  
  # connect to mySQL db and get player info
  mydb <- dbConnect(MySQL(), user = "root", password = "yourpassword", dbname = "dfs_db", host = "localhost")
  player_info           <- dbReadTable(mydb, "player_id")
  
glimpse(dat)
 # Clean data
  
  player_stats <- 
    dat %>%
    
  # formats Round
    mutate(round  = case_when(match_round == "Grand Final" ~ 28,                              
                              match_round == "Preliminary Final" ~ 27,
                              match_round == "Preliminary Finals" ~ 27,
                              match_round == "Semi Final" ~ 26,
                              match_round == "Semi Finals" ~ 26,
                              match_round == "Elimination Final" ~ 25,
                              match_round == "Qualifying Final" ~ 25,
                              match_round == "Finals Week 1" ~ 25,
                              match_round == "24" ~ 24,
                              match_round == "23" ~ 23,
                              match_round == "22" ~ 22,
                              match_round == "21" ~ 21,
                              match_round == "20" ~ 20,
                              match_round == "19" ~ 19,
                              match_round == "18" ~ 18,
                              match_round == "17" ~ 17,
                              match_round == "16" ~ 16,
                              match_round == "15" ~ 15,
                              match_round == "14" ~ 14,
                              match_round == "13" ~ 13,
                              match_round == "12" ~ 12,
                              match_round == "11" ~ 11,
                              match_round == "10" ~ 20,
                              match_round == "9" ~ 9,
                              match_round == "8" ~ 8,
                              match_round == "7" ~ 7,
                              match_round == "6" ~ 6,
                              match_round == "5" ~ 5,
                              match_round == "4" ~ 4,
                              match_round == "3" ~ 3,
                              match_round == "2" ~ 2,
                              match_round == "1" ~ 1),
           
      # format player name     
            player = paste0(player_first_name, " ", player_last_name),
            player = case_when(player == "Kamdyn Mcintosh" ~ "Kamdyn McIntosh",
                                     player == "Daniel Butler" ~ "Dan Butler",
                                     player == "Edward Curnow" ~ "Ed Curnow",
                                     player == "Sam P-Seton" ~ "Sam Petrevski-Seton",
                                     player == "Lachlan Plowman" ~ "Lachie Plowman",
                                     player == "Anthony M-Tipungwuti" ~ "Anthony McDonald-Tipungwuti",
                                     player == "Mitchell Brown" ~ "Mitch Brown",
                                     player == "Matthew Dea" ~ "Matt Dea",
                                     player == "Joshua Green" ~ "Josh Green",
                                     player == "Joshua Begley" ~ "Josh Begley",
                                     player == "Zachary Merrett" ~ "Zach Merrett",
                                     player == "David MacKay" ~ "David Mackay",
                                     player == "Cameron E-Yolmen" ~ "Cam Ellis-Yolmen",
                                     player == "Patrick McCartin" ~ "Paddy McCartin",
                                     player == "Nicholas Robertson" ~ "Nick Robertson",
                                     player == "Joshua Walker" ~ "Josh Walker",
                                     player == "Cameron Rayner" ~ "Cam Rayner",
                                     player == "Oliver Wines" ~ "Ollie Wines",
                                     player == "Darcy B-Jones" ~ "Darcy Byrne-Jones",
                                     player == "Sam P-Pepper" ~ "Sam Powell-Pepper",
                                     player == "Thomas Jonas" ~ "Tom Jonas",
                                     player == "Nathan Fyfe" ~ "Nat Fyfe",
                                     player == "Cameron McCarthy" ~ "Cam McCarthy",
                                     player == "Matthew Rosa" ~ "Matt Rosa",
                                     player == "Lachlan Weller" ~ "Lachie Weller",
                                     player == "Darcy Macpherson" ~ "Darcy MacPherson",
                                     player == "Nicholas Holman" ~ "Nick Holman",
                                     player == "Jamie MacMillan" ~ "Jamie Macmillan",
                                     player == "Ed V-Willis" ~ "Ed Vickers-Willis",
                                     player == "Luke D-Uniacke" ~ "Luke Davies-Uniacke",
                                     player == "Jaeger O'Meara" ~ "Jaeger OMeara",
                                     player == "Tim O'Brien" ~ "Tim OBrien",
                                     player == "Benjamin Stratton" ~ "Ben Stratton",
                                     player == "Samuel Murray" ~ "Sam Murray",
                                     player == "Will H-Elliott" ~ "Will Hoskin-Elliott",
                                     player == "Joshua Kelly" ~ "Josh Kelly",
                                     player == "Matthew De Boer" ~ "Matthew de Boer",
                                     player == "Samuel Reid" ~ "Sam Reid",
                                     player == "Lachlan Hunter" ~ "Lachie Hunter",
                                     player == "Jackson Macrae" ~ "Jack Macrae",
                                     player == "Matthew Suckling" ~ "Matt Suckling",
                                     player == "Timothy English" ~ "Tim English",
                                     player == "Thomas Liberatore" ~ "Tom Liberatore",
                                     player == "Joshua Wagner" ~ "Josh Wagner",
                                     player == "Alex N-Bullen" ~ "Alex Neal-Bullen",
                                     player == "Mitchell Hannan" ~ "Mitch Hannan",
                                     player == "Gary Jnr Ablett" ~ "Gary Ablett",
                                     player == "Mitchell Duncan" ~ "Mitch Duncan",
                                     player == "Lachlan Fogarty" ~ "Lachie Fogarty",
                                     player == "Thomas Stewart" ~ "Tom Stewart",
                                     player == "Dominic Sheed" ~ "Dom Sheed",
                                     player == "Mark Lecras" ~ "Mark LeCras",
                                     player == "Bradley Sheppard" ~ "Brad Sheppard",
                                     player == "Nicholas Naitanui" ~ "Nic Naitanui",
                                     player == "Josh P. Kennedy" ~ "Josh Kennedy",
                                     player == "Cameron O'Shea" ~ "Cameron OShea",
                                     player == "Matthew Scharenberg" ~ "Matt Scharenberg",
                                     player == "Timothy Broomhead" ~ "Tim Broomhead",
                                     player == "Mitchell Crowden" ~ "Mitch Crowden",
                                     player == "Daniel Hannebery" ~ "Dan Hannebery",
                                     player == "Mark O'Connor" ~ "Mark OConnor",
                                     player == "Thomas Sheridan" ~ "Tom Sheridan",
                                     player == "Nicholas Coffield" ~ "Nick Coffield",
                                     player == "William Langford" ~ "Will Langford",
                                     player == "Mitchell Wallis" ~ "Mitch Wallis",
                                     player == "Jordan De Goey" ~ "Jordan de Goey",
                                     player == "Nicholas Graham" ~ "Nick Graham",
                                     player == "Lochie O'Brien" ~ "Lochie OBrien",
                                     player == "Joshua Kennedy" ~ "Josh Kennedy",
                                     player == "George H-Smith" ~ "George Horlin-Smith",
                                     player == "Thomas Boyd" ~ "Tom Boyd",
                                     player == "Daniel Robinson" ~ "Dan Robinson",
                                     player == "Nicholas Shipley" ~ "Nick Shipley",
                                     player == "Christopher Mayne" ~ "Chris Mayne",
                                     player == "Samuel Wright" ~ "Sam Wright",
                                     player == "Edward Phillips" ~ "Ed Phillips",
                                     player == "Mitchell Lewis" ~ "Mitch Lewis",
                                     player == "Matthew Buntine" ~ "Matt Buntine",
                                     player == "Thomas Murphy" ~ "Tom Murphy",
                                     player == "Joshua Schoenfeld" ~ "Josh Schoenfeld",
                                     player == "Bradley Lynch" ~ "Brad Lynch",
                                     player == "Harrison Petty" ~ "Harry Petty",
                                     player == "Jonathan O'Rourke" ~ "Jonathan ORourke",
                                     player == "Jay K-Harris" ~ "Jay Kennedy-Harris",
                                     player == "Colin O'Riordan" ~ "Colin ORiordan",
                                     player == "Lachlan Henderson" ~ "Lachie Henderson",
                                     player == "Zachary Williams" ~ "Zac Williams",
                                     player == "Samuel Collins" ~ "Sam Collins",
                                     player == "Lachlan Schultz" ~ "Lachie Schultz",
                                     player == "Reilly O'Brien" ~ "Reilly OBrien",
                                     player == "Zachary Clarke" ~ "Zac Clarke",
                                     player == "Justin Mcinerney" ~ "Justin McInerney",
                                     player == "Robert Young" ~ "Robbie Young",
                                     player == "Mitchell Hinge" ~ "Mitch Hinge",
                                     player == "Callum C-Jones" ~ "Callum Coleman-Jones",
                                     player == "Bradley Scheer" ~ "Brad Scheer",
                                     player == "Derek E-Smith" ~ "Derek Eggmolesse-Smith",
                                     player == "Ian Hill" ~ "Bobby Hill",
                                     player == "Joshua Deluca" ~ "Josh Deluca",
                                     player == "Brandon Z-Thatcher" ~ "Brandon Zerk-Thatcher",
                                     player == "Zachary Sproule" ~ "Zach Sproule",
                                     player == "Thomas Berry" ~ "Tom Berry",
                                     player == "Lachlan Ash" ~ "Lachie Ash",
                                     player == "Harrison Jones" ~ "Harry Jones",
                                     player == "Minairo Frederick" ~ "Michael Frederick",
                                     player == "Matthew Ling" ~ "Matt Ling",
                                     player == "Bradley Close" ~ "Brad Close",
                                     player == "Xavier O'Halloran" ~ "Xavier OHalloran",
                                     player == "Matt Cottrell" ~ "Matthew Cottrell",
                                     player == "Thomas Hutchesson" ~ "Tom Hutchesson",
                                     player == "Xavier O'Neill" ~ "Xavier ONeill",
                                     player == "Matt Owies" ~ "Matthew Owies",
                                     TRUE ~ player),
      # # format team name
      # player_team = case_when(player_team == "Geelong"                ~ "GEE",
      #                         player_team == "Hawthorn"               ~ "HAW",
      #                         player_team == "Brisbane Lions"         ~ "BRI",
      #                         player_team == "Collingwood"            ~ "COL",
      #                         player_team == "North Melbourne"        ~ "NME",
      #                         player_team == "Fremantle"              ~ "FRE",
      #                         player_team == "Essendon"               ~ "ESS",
      #                         player_team == "Carlton"                ~ "CAR",
      #                         player_team == "Sydney"                 ~ "SYD",
      #                         player_team == "Gold Coast"             ~ "GCS",
      #                         player_team == "Greater Western Sydney" ~ "GWS",
      #                         player_team == "Melbourne"              ~ "MEL",
      #                         player_team == "Western Bulldogs"       ~ "WBD",
      #                         player_team == "West Coast"             ~ "WCE",
      #                         player_team == "St Kilda"               ~ "STK",
      #                         player_team == "Adelaide"               ~ "ADE",
      #                         player_team == "Port Adelaide"          ~ "POR",
      #                         player_team == "Richmond"               ~ "RIC") ,
      # create season and helper variables 
         season = year(match_date),
         helper = paste0(player,"_", season,"_", round,"_",player_team)) %>%
      
      # format variable names           
         select(helper, 
           id = player_id,
           player,
           first_name = player_first_name,
           surname = player_last_name,
           specific_position = player_position,
           height = player_height_cm,
           weight = player_weight_kg,
           jumper_number = guernsey_number,
           season,
           round,
           team = player_team,
           match_id,
           date = match_date,
           start_time = match_local_time,
           venue = venue_name,
           attendance = match_home_team,
           away_team = match_away_team,
           home_score = match_home_team_score,
           away_score = match_away_team_score,
           weather_type = match_weather_temp_c,
           temp = match_weather_temp_c, 
           dfs_score = afl_fantasy_score,
           supercoach = supercoach_score,
           rating_points,
           kicks,
           handballs,
           marks,
           tackles,
           hitouts,
           goals,
           behinds,
           FF = free_kicks_for,
           FA = free_kicks_against,
           disposals,
           effective_disposals,
           effective_kicks,
           disposal_efficiency = disposal_efficiency_percentage,
           CP = contested_possessions,
           UP = uncontested_possessions, 
           f50_ground_ball_gets,
           ground_ball_gets,
           clangers,
           turnovers,
           intercepts,
           ruck_contests,
           hitouts_to_advantage,
           hitout_win_percentage,
           contested_marks,
           marks_I50 = marks_inside_fifty,
           intercept_marks,
           marks_on_lead,
           clearances,
           centre_clearances,
           stoppage_clearances,
           R50 = rebounds,
           I50 = inside_fifties,
           score_involvements,
           score_launches,
           shots_at_goal,
           goal_assists,
           metres_gained,
           one_percenters, 
           bounces,
           spoils,
           TOG = time_on_ground_percentage,
           pressure_acts,
           tackles_inside_fifty,
           contest_def_losses,
           contest_def_one_on_ones,
           contest_off_one_on_ones,
           contest_off_wins,
           def_half_pressure_acts,
           brownlow_votes, 
           everything())
  
  
  # Filter out player name and surname form player_info dataframe, only need position and id
  player_info <- player_info %>% select(id = player_id, position)
  
  # merge player_info into player_stats_merged
  player_stats_merged <- merge(player_info, player_stats, by = "id")
                                                     

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  }

#############################
# GET DATA FROM MYSQL DATABSE
#############################

GetDataCleanMySQL <- function() {
 
  
  # Connect to MySQL dfs_db
  mydb <- dbConnect(MySQL(), user = "root", password = "yourpassword", dbname = "dfs_db", host = "localhost")
  
  # Get required tables
  advanced_player_stats <- dbReadTable(mydb, "advanced_player_stats") # advanced_player_stats
  player_stats          <- dbReadTable(mydb, "player_stats")          # player_stats
  player_info           <- dbReadTable(mydb, "player_id")
  
  # Combine advanced_player_stats and player_stats
  
  # create unique helper column to merge columns with: use a space separator between first and surname to be able to
  # connect with advanced_player_stats which has first and surname as one combined column
  
  # CLEAN PLAYER_STATS
  
  
  player_stats <- 
    player_stats %>%
    mutate(Round  = case_when(Round == "GF" ~ 28,                              # formats Round and converts to numeric
                              Round == "PF" ~ 27,
                              Round == "SF" ~ 26,
                              Round == "EF" ~ 25,
                              Round == "QF" ~ 25,
                              Round == "24" ~ 24,
                              Round == "23" ~ 23,
                              Round == "22" ~ 22,
                              Round == "21" ~ 21,
                              Round == "20" ~ 20,
                              Round == "19" ~ 19,
                              Round == "18" ~ 18,
                              Round == "17" ~ 17,
                              Round == "16" ~ 16,
                              Round == "15" ~ 15,
                              Round == "14" ~ 14,
                              Round == "13" ~ 13,
                              Round == "12" ~ 12,
                              Round == "11" ~ 11,
                              Round == "10" ~ 20,
                              Round == "9" ~ 9,
                              Round == "8" ~ 8,
                              Round == "7" ~ 7,
                              Round == "6" ~ 6,
                              Round == "5" ~ 5,
                              Round == "4" ~ 4,
                              Round == "3" ~ 3,
                              Round == "2" ~ 2,
                              Round == "1" ~ 1),
           helper = paste0(First.name," ",Surname,"_",Season,"_",Round,"_",Playing.for)) %>%   # creates helper to bind to player_stats
    select(helper, everything())
  
  
  
  # CLEAN ADVANCED_PLAYER_STATS
  advanced_player_stats <- 
    advanced_player_stats %>% 
    # formats Round and converts to numeric
    mutate(Round  = case_when(Round == "Grand Final" ~ 28,                     
                              Round == "Preliminary Final" ~ 27,
                              Round == "Semi Final" ~ 26,
                              Round == "Elimination Final" ~ 25,
                              Round == "Qualifying Final" ~ 25,
                              Round == "Round 24" ~ 24,
                              Round == "Round 23" ~ 23,
                              Round == "Round 22" ~ 22,
                              Round == "Round 21" ~ 21,
                              Round == "Round 20" ~ 20,
                              Round == "Round 19" ~ 19,
                              Round == "Round 18" ~ 18,
                              Round == "Round 17" ~ 17,
                              Round == "Round 16" ~ 16,
                              Round == "Round 15" ~ 15,
                              Round == "Round 14" ~ 14,
                              Round == "Round 13" ~ 13,
                              Round == "Round 12" ~ 12,
                              Round == "Round 11" ~ 11,
                              Round == "Round 10" ~ 20,
                              Round == "Round 9" ~ 9,
                              Round == "Round 8" ~ 8,
                              Round == "Round 7" ~ 7,
                              Round == "Round 6" ~ 6,
                              Round == "Round 5" ~ 5,
                              Round == "Round 4" ~ 4,
                              Round == "Round 3" ~ 3,
                              Round == "Round 2" ~ 2,
                              Round == "Round 1" ~ 1),
           # formats player name to be same as player_stats
           Player = case_when(Player == "Kamdyn Mcintosh" ~ "Kamdyn McIntosh",
                              Player == "Daniel Butler" ~ "Dan Butler",
                              Player == "Edward Curnow" ~ "Ed Curnow",
                              Player == "Sam P-Seton" ~ "Sam Petrevski-Seton",
                              Player == "Lachlan Plowman" ~ "Lachie Plowman",
                              Player == "Anthony M-Tipungwuti" ~ "Anthony McDonald-Tipungwuti",
                              Player == "Mitchell Brown" ~ "Mitch Brown",
                              Player == "Matthew Dea" ~ "Matt Dea",
                              Player == "Joshua Green" ~ "Josh Green",
                              Player == "Joshua Begley" ~ "Josh Begley",
                              Player == "Zachary Merrett" ~ "Zach Merrett",
                              Player == "David MacKay" ~ "David Mackay",
                              Player == "Cameron E-Yolmen" ~ "Cam Ellis-Yolmen",
                              Player == "Patrick McCartin" ~ "Paddy McCartin",
                              Player == "Nicholas Robertson" ~ "Nick Robertson",
                              Player == "Joshua Walker" ~ "Josh Walker",
                              Player == "Cameron Rayner" ~ "Cam Rayner",
                              Player == "Oliver Wines" ~ "Ollie Wines",
                              Player == "Darcy B-Jones" ~ "Darcy Byrne-Jones",
                              Player == "Sam P-Pepper" ~ "Sam Powell-Pepper",
                              Player == "Thomas Jonas" ~ "Tom Jonas",
                              Player == "Nathan Fyfe" ~ "Nat Fyfe",
                              Player == "Cameron McCarthy" ~ "Cam McCarthy",
                              Player == "Matthew Rosa" ~ "Matt Rosa",
                              Player == "Lachlan Weller" ~ "Lachie Weller",
                              Player == "Darcy Macpherson" ~ "Darcy MacPherson",
                              Player == "Nicholas Holman" ~ "Nick Holman",
                              Player == "Jamie MacMillan" ~ "Jamie Macmillan",
                              Player == "Ed V-Willis" ~ "Ed Vickers-Willis",
                              Player == "Luke D-Uniacke" ~ "Luke Davies-Uniacke",
                              Player == "Jaeger O'Meara" ~ "Jaeger OMeara",
                              Player == "Tim O'Brien" ~ "Tim OBrien",
                              Player == "Benjamin Stratton" ~ "Ben Stratton",
                              Player == "Samuel Murray" ~ "Sam Murray",
                              Player == "Will H-Elliott" ~ "Will Hoskin-Elliott",
                              Player == "Joshua Kelly" ~ "Josh Kelly",
                              Player == "Matthew De Boer" ~ "Matthew de Boer",
                              Player == "Samuel Reid" ~ "Sam Reid",
                              Player == "Lachlan Hunter" ~ "Lachie Hunter",
                              Player == "Jackson Macrae" ~ "Jack Macrae",
                              Player == "Matthew Suckling" ~ "Matt Suckling",
                              Player == "Timothy English" ~ "Tim English",
                              Player == "Thomas Liberatore" ~ "Tom Liberatore",
                              Player == "Joshua Wagner" ~ "Josh Wagner",
                              Player == "Alex N-Bullen" ~ "Alex Neal-Bullen",
                              Player == "Mitchell Hannan" ~ "Mitch Hannan",
                              Player == "Gary Jnr Ablett" ~ "Gary Ablett",
                              Player == "Mitchell Duncan" ~ "Mitch Duncan",
                              Player == "Lachlan Fogarty" ~ "Lachie Fogarty",
                              Player == "Thomas Stewart" ~ "Tom Stewart",
                              Player == "Dominic Sheed" ~ "Dom Sheed",
                              Player == "Mark Lecras" ~ "Mark LeCras",
                              Player == "Bradley Sheppard" ~ "Brad Sheppard",
                              Player == "Nicholas Naitanui" ~ "Nic Naitanui",
                              Player == "Josh P. Kennedy" ~ "Josh Kennedy",
                              Player == "Cameron O'Shea" ~ "Cameron OShea",
                              Player == "Matthew Scharenberg" ~ "Matt Scharenberg",
                              Player == "Timothy Broomhead" ~ "Tim Broomhead",
                              Player == "Mitchell Crowden" ~ "Mitch Crowden",
                              Player == "Daniel Hannebery" ~ "Dan Hannebery",
                              Player == "Mark O'Connor" ~ "Mark OConnor",
                              Player == "Thomas Sheridan" ~ "Tom Sheridan",
                              Player == "Nicholas Coffield" ~ "Nick Coffield",
                              Player == "William Langford" ~ "Will Langford",
                              Player == "Mitchell Wallis" ~ "Mitch Wallis",
                              Player == "Jordan De Goey" ~ "Jordan de Goey",
                              Player == "Nicholas Graham" ~ "Nick Graham",
                              Player == "Lochie O'Brien" ~ "Lochie OBrien",
                              Player == "Joshua Kennedy" ~ "Josh Kennedy",
                              Player == "George H-Smith" ~ "George Horlin-Smith",
                              Player == "Thomas Boyd" ~ "Tom Boyd",
                              Player == "Daniel Robinson" ~ "Dan Robinson",
                              Player == "Nicholas Shipley" ~ "Nick Shipley",
                              Player == "Christopher Mayne" ~ "Chris Mayne",
                              Player == "Samuel Wright" ~ "Sam Wright",
                              Player == "Edward Phillips" ~ "Ed Phillips",
                              Player == "Mitchell Lewis" ~ "Mitch Lewis",
                              Player == "Matthew Buntine" ~ "Matt Buntine",
                              Player == "Thomas Murphy" ~ "Tom Murphy",
                              Player == "Joshua Schoenfeld" ~ "Josh Schoenfeld",
                              Player == "Bradley Lynch" ~ "Brad Lynch",
                              Player == "Harrison Petty" ~ "Harry Petty",
                              Player == "Jonathan O'Rourke" ~ "Jonathan ORourke",
                              Player == "Jay K-Harris" ~ "Jay Kennedy-Harris",
                              Player == "Colin O'Riordan" ~ "Colin ORiordan",
                              Player == "Lachlan Henderson" ~ "Lachie Henderson",
                              Player == "Zachary Williams" ~ "Zac Williams",
                              Player == "Samuel Collins" ~ "Sam Collins",
                              Player == "Lachlan Schultz" ~ "Lachie Schultz",
                              Player == "Reilly O'Brien" ~ "Reilly OBrien",
                              Player == "Zachary Clarke" ~ "Zac Clarke",
                              Player == "Justin Mcinerney" ~ "Justin McInerney",
                              Player == "Robert Young" ~ "Robbie Young",
                              Player == "Mitchell Hinge" ~ "Mitch Hinge",
                              Player == "Callum C-Jones" ~ "Callum Coleman-Jones",
                              Player == "Bradley Scheer" ~ "Brad Scheer",
                              Player == "Derek E-Smith" ~ "Derek Eggmolesse-Smith",
                              Player == "Ian Hill" ~ "Bobby Hill",
                              Player == "Joshua Deluca" ~ "Josh Deluca",
                              Player == "Brandon Z-Thatcher" ~ "Brandon Zerk-Thatcher",
                              Player == "Zachary Sproule" ~ "Zach Sproule",
                              Player == "Thomas Berry" ~ "Tom Berry",
                              Player == "Lachlan Ash" ~ "Lachie Ash",
                              Player == "Harrison Jones" ~ "Harry Jones",
                              Player == "Minairo Frederick" ~ "Michael Frederick",
                              Player == "Matthew Ling" ~ "Matt Ling",
                              Player == "Bradley Close" ~ "Brad Close",
                              Player == "Xavier O'Halloran" ~ "Xavier OHalloran",
                              Player == "Matt Cottrell" ~ "Matthew Cottrell",
                              Player == "Thomas Hutchesson" ~ "Tom Hutchesson",
                              Player == "Xavier O'Neill" ~ "Xavier ONeill",
                              Player == "Matt Owies" ~ "Matthew Owies",
                              TRUE ~ Player),
           Team = case_when(Team == "GWS" ~ "Greater Western Sydney",
                            Team == "Brisbane" ~ "Brisbane Lions",
                            TRUE ~ Team),
           # creates helper to bind to player_stats
           helper = paste0(Player,"_",Season,"_",Round,"_",Team)) %>%      
    select(helper,                                                # select on variables that are not duplicates in player_stats
           Status, 
           Match_id, 
           ED, 
           DE, 
           GA...15, 
           D, 
           AF, 
           SC, 
           CCL, 
           SCL, 
           SI, 
           MG, 
           ITC, 
           T5)
  
  
  
  # This checks for players that naming conventions are not lining up - use player_stats as primary table
  #anti_join <- anti_join(advanced_player_stats, player_stats, by = "helper")
  #anti_join <- unique(anti_join$Player)
  
  # MERGE PLAYER_STATS AND ADVANCDED_PLAYER_STATS
  
  player_stats_merged <- merge(advanced_player_stats, player_stats, by = "helper") %>%    # merges by helper column
    select(helper,                                                   # reorder and rename variables    
           ID, 
           first_name = First.name,
           surname = Surname,
           jumper_number = Jumper.No.,
           season = Season,
           round = Round,
           team = Playing.for,
           status = Status,
           match_id = Match_id,
           date = Date,
           start_time = Local.start.time,
           venue = Venue,
           attendance = Attendance,
           home_team = Home.team,
           away_team = Away.team,
           home_score = Home.score,
           away_score = Away.score,
           dfs_score = AF,
           supercoach = SC,
           kicks = Kicks,
           handballs = Handballs,
           marks = Marks,
           tackles = Tackles,
           hit_outs = Hit.Outs,
           goals = Goals,
           behinds = Behinds,
           FF = Frees.For,
           FA = Frees.Against,
           disposals = D,
           effective_disposals = ED,
           disposal_efficiency = DE,
           CP = Contested.Possessions,
           UP = Uncontested.Possessions, 
           clangers = Clangers,
           contested_marks = Contested.Marks,
           marks_I50 = Marks.Inside.50,
           tackles_I50 = T5,
           clearances = Clearances,
           centre_clearances = CCL,
           stoppage_clearances = SCL,
           R50 = Rebounds,
           I50 = Inside.50s,
           score_involvements = SI,
           metres_gained = MG,
           intercepts = ITC,
           goal_assists = Goal.Assists,
           one_percenters = One.Percenters, 
           bounces = Bounces,
           TOG = Time.on.Ground..,
           brownlow_votes = Brownlow.Votes, 
           everything())
  
  # Filter out player name and surname form player_info dataframe, only need position and id
  player_info <- player_info %>% select(ID = player_id, position)
  
  # merge player_info into player_stats_merged
  position_merged <- merge(player_stats_merged, player_info, by = "ID")
  
  # change column positio and rename a few columns
  position_merged <- 
    position_merged %>%
    relocate(position, .after = surname) %>%
    select(-c(HQ1G, HQ1B, HQ2G, HQ2B, HQ3G, HQ3B, HQ4G, HQ4B, AQ1G, AQ1B, AQ2G,AQ2B, AQ3G, AQ3B, AQ4G, AQ4B)) # Removes home and away quarter scores
}

