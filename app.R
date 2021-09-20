
library(shiny)
library(tidyverse)
library(DT)
library(readxl)
library(lpSolve)
library(gdata)
library(Hmisc)
library(shinydashboard)
library(gfonts)
library(reactable)

# ----------------------------------------------------------------------------------------------------------------------

# Load and create data
dataset <- read.csv("brownlow_model_2021.csv") %>%
  mutate("Predicted Votes" = round(predicted_votes,1),
         "Top 1 Chance"    = top_1_chance/100,
         "Top 2 Chance"    = top_2_chance/100,
         "Top 3 Chance"    = top_3_chance/100,
         "Top 5 Chance"    = top_5_chance/100,
         "Top 10 Chance"   = top_10_chance/100,
         "Top 20 Chance"   = top_20_chance/100,
         "Team Top Chance" = team_top_chance/100,
         "Votes SD"        = sd_votes) %>%
  select(c(Name = helper2, Team = team, "Predicted Votes", "Votes SD", "Top 1 Chance", "Top 2 Chance", "Top 3 Chance", 
           "Top 5 Chance", "Top 10 Chance", "Top 20 Chance", "Team Top Chance")) 

brownlow_df <- dataset %>% select(-"Team Top Chance")
teams_df    <- dataset %>% select(Name, Team, "Predicted Votes", "Votes SD", "Team Top Chance")

first_five <- read.csv("first_5_rounds.csv") %>%
  mutate("Predicted Votes" = round(predicted_votes,1),
         "Top 1 Chance"    = top_1_chance/100,
         "Votes SD"        = sd_votes) %>%
  select(c(Name = helper2, Team = team, "Predicted Votes", "Votes SD", "Top 1 Chance")) 
         
first_ten  <- read.csv("first_10_rounds.csv") %>%
  mutate("Predicted Votes" = round(predicted_votes,1),
         "Top 1 Chance"    = top_1_chance/100,
         "Votes SD"        = sd_votes) %>%
  select(c(Name = helper2, Team = team, "Predicted Votes", "Votes SD", "Top 1 Chance")) 
         
last_eight <- read.csv("last_8_rounds.csv") %>%
  mutate("Predicted Votes" = round(predicted_votes,1),
         "Top 1 Chance"    = top_1_chance/100,
         "Votes SD"        = sd_votes) %>%
  select(c(Name = helper2, Team = team, "Predicted Votes", "Votes SD", "Top 1 Chance")) 
         

match_match <- read.csv("match_match.csv") %>%
  mutate("Predicted Votes" = round(predicted_votes,1),
         "3 Votes Chance"         = match_1_chance/100,
         "2 Votes Chance"         = match_2_chance,
         "1 Vote Chance"          = match_3_chance,
         "Votes SD"        = sd_votes) %>%
  select(c(MatchID = match_id, Round = round, Name = helper2, Team = team, "Predicted Votes", "Votes SD", 
           "3 Votes Chance", "Fantasy Score" = dfs_score, Disposals = disposals, Goals = goals, 
           "Score Involvements" = score_involvements, "Winning Team" = winning_team, "Coach Votes" = coach_votes)) 










# ----------------------------------------------------------------------------------------------------------------------

ui <-  dashboardPage(skin = "blue",
    dashboardHeader(title = "2021 Brownlow Model"),                                             # App Title
    dashboardSidebar(
      
      br(),
      br(),
      
      img(src = "logo.png", height = 130, width = 113.75, style="display: block; margin-left: auto; margin-right: auto;"),
    
      br(),
     
    sidebarMenu(style = "position: fixed; overflow: visible;",
                id = "sidebarmenu",
                
                menuItem("Brownlow Predictions",     tabName = "a", icon = icon("medal")),
                menuItem(" Leading After Round 5",    tabName = "b", icon = icon("html5")),
                menuItem("Leading After Round 10",   tabName = "c", icon = icon("hourglass-start")),
                menuItem("Most Votes Last 8 Rounds", tabName = "d", icon = icon("hourglass-end")),
                menuItem("Team Votes",               tabName = "e", icon = icon("users")),
                menuItem("Per Match Votes",          tabName = "f", icon = icon("users")),
                menuItem("Model Specifications",     tabName = "z", icon = icon("calander-day")),
                br(),
                br()
    )
  ),
    dashboardBody(                                                                              # Main Panel 
        tabItems(
          
          
        tabItem(tabName = "a",
                reactableOutput("table1")),
        tabItem(tabName = "b",
                reactableOutput("table2")),
        tabItem(tabName = "c",
                reactableOutput("table3")),
        tabItem(tabName = "d",
                reactableOutput("table4")),
        tabItem(tabName = "e",
                reactableOutput("table5")),
        tabItem(tabName = "f",
                reactableOutput("table6")),
        
        tabItem(tabName = "z",
                p(strong("Brownlow model")),
                p("This model uses BRMS package to run an ordinal multilevel logistic (adjacent category) model, trainied on 2018, 2019 and 2020 seasons data. Using the following predictor variables to classify a player’s game as either 0, 1, 
                  2, or 3 Brownlow votes:"),
                br(),
                p("Winning team, Goals, Relative Brownlow Votes, Coaches Votes, AFL Fantasy Score, Disposals & Score Involvements"),
                br(),
                p("Variables: Winning Team, Goals and Relative Brownlow Votes where modelled as Category Specific and 
                  provide separate slopes for positions. The final model was created from iterative modelling process
                  whereby variables were added in stepwise fashion resulting in 13 models, with the final model 
                  displaying the greatest accuracy (LOC), 10,000 predictions were then sampled from the model posterior which 
                  provides probabilistic inference for betting markets."),
                br(),
                p("20+ other variables were analysed in initial exploratory data analysis but showed either low 
                  predictive power of Brownlow votes or high multicollinearity which was assessed via VIF analysis.")
            )
        )
    )
)  

server <- function(input, output) {       
  
  
  # ----------------------------------------------------------------------------
  
  # THEME
  # options(reactable.theme = reactableTheme(
  #   color = "hsl(233, 9%, 87%)",
  #   backgroundColor = "hsl(233, 9%, 19%)",
  #   borderColor = "hsl(233, 9%, 22%)",
  #   stripedColor = "hsl(233, 12%, 22%)",
  #   highlightColor = "hsl(233, 12%, 24%)",
  #   inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  #   selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  #   pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  #   pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
  #))
  
  # ----------------------------------------------------------------------------
  
  # Define server logic required
  output$table1 <- renderReactable({
    reactable(brownlow_df,
              searchable = TRUE,                               
              columns = list(Team              = colDef(filterable = TRUE),                                   # make team and name filterable only
                             Name              = colDef(minWidth = 200, filterable = TRUE),
                             "Predicted Votes" = colDef(style = function(value) {list(fontWeight = "bold")}),  
                             "Top 1 Chance"    = colDef(format = colFormat(percent = TRUE, digits = 1)),
                             "Top 2 Chance"    = colDef(format = colFormat(percent = TRUE, digits = 1)),
                             "Top 3 Chance"    = colDef(format = colFormat(percent = TRUE, digits = 1)),
                             "Top 5 Chance"    = colDef(format = colFormat(percent = TRUE, digits = 1)),
                             "Top 10 Chance"   = colDef(format = colFormat(percent = TRUE, digits = 1)),
                             "Top 20 Chance"   = colDef(format = colFormat(percent = TRUE, digits = 1))),
              sortable = TRUE,                                                                   # make all columns sortable
              # fullWidth = FALSE,
              defaultSortOrder = "desc",
              defaultSorted = c("Predicted Votes"),                                              # sort table intially by predicted votes
              showPageSizeOptions = TRUE, pageSizeOptions = c(5, 10, 20), defaultPageSize = 20, # Add in page size options and     # how many results per page          
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "14px")
    )
  })
  
  
  output$table2 <- renderReactable({
    reactable(first_five,
              searchable = TRUE,                               
              columns = list(Team           = colDef(filterable = TRUE),                                   # make team and name filterable only
                             Name           = colDef(minWidth = 200, filterable = TRUE),
                             "Predicted Votes" = colDef(style = function(value) {list(fontWeight = "bold")}),
                             "Top 1 Chance" = colDef(format = colFormat(percent = TRUE, digits = 1))),
              sortable = TRUE,                                                                   # make all columns sortable
              # fullWidth = FALSE,
              defaultSortOrder = "desc",
              defaultSorted = c("Predicted Votes"),                                              # sort table intially by predicted votes
              showPageSizeOptions = TRUE, pageSizeOptions = c(5, 10, 20), defaultPageSize = 20, # Add in page size options and     # how many results per page          
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "14px"))
  })
  
  
  
  output$table3 <- renderReactable({
    reactable(first_ten,
              searchable = TRUE,                               
              columns = list(Team           = colDef(filterable = TRUE),                                   # make team and name filterable only
                             Name           = colDef(minWidth = 200, filterable = TRUE),
                             "Predicted Votes" = colDef(style = function(value) {list(fontWeight = "bold")}),
                             "Top 1 Chance" = colDef(format = colFormat(percent = TRUE, digits = 1))),
              sortable = TRUE,                                                                   # make all columns sortable
              defaultSortOrder = "desc",
              # fullWidth = FALSE,
              defaultSorted = c("Predicted Votes"),                                              # sort table intially by predicted votes
              showPageSizeOptions = TRUE, pageSizeOptions = c(5, 10, 20), defaultPageSize = 20, # Add in page size options and     # how many results per page          
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "14px"))
  })
  
  
  
  output$table4 <- renderReactable({
    reactable(last_eight,
              searchable = TRUE,                               
              columns = list(Team           = colDef(filterable = TRUE),                                   # make team and name filterable only
                             Name           = colDef(minWidth = 200, filterable = TRUE),
                             "Predicted Votes" = colDef(style = function(value) {list(fontWeight = "bold")}),
                             "Top 1 Chance" = colDef(format = colFormat(percent = TRUE, digits = 1))),
              sortable = TRUE,                                                                   # make all columns sortable
              defaultSortOrder = "desc",
              # fullWidth = FALSE,
              defaultSorted = c("Predicted Votes"),                                              # sort table intially by predicted votes
              showPageSizeOptions = TRUE, pageSizeOptions = c(5, 10, 20), defaultPageSize = 20, # Add in page size options and     # how many results per page          
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "14px"))
  })
  
  
  
  output$table5 <- renderReactable({
    reactable(teams_df,
              searchable = TRUE,                               
              columns = list(Team              = colDef(filterable = TRUE),                                   # make team and name filterable only
                             Name              = colDef(minWidth = 200, filterable = TRUE),
                             "Predicted Votes" = colDef(style = function(value) {list(fontWeight = "bold")}),
                             "Team Top Chance" = colDef(format = colFormat(percent = TRUE, digits = 1))),
              sortable = TRUE,                                                                   # make all columns sortable
              defaultSortOrder = "desc",
              # fullWidth = FALSE,
              defaultSorted = c("Team", "Team Top Chance"),                                              # sort table intially by predicted votes
              showPageSizeOptions = TRUE, pageSizeOptions = c(5, 10, 20), defaultPageSize = 20, # Add in page size options and     # how many results per page          
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "14px"))
  })
  
  
  
  output$table6 <- renderReactable({
    reactable(match_match,
              searchable = TRUE,                               
              columns = list(MatchID           = colDef(filterable = TRUE),
                             Round             = colDef(filterable = TRUE),
                             Team              = colDef(filterable = TRUE),                                   # make team and name filterable only
                             Name              = colDef(minWidth = 200, filterable = TRUE),
                             "Predicted Votes" = colDef(style = function(value) {list(fontWeight = "bold")}),
                             "3 Votes Chance"  = colDef(format = colFormat(percent = TRUE, digits = 1))),
              sortable = TRUE,                                                                   # make all columns sortable
              defaultSortOrder = "desc",
              # fullWidth = FALSE,
              defaultSorted = c("MatchID", "3 Votes Chance"),                                              # sort table intially by predicted votes
              showPageSizeOptions = TRUE, pageSizeOptions = c(5, 10, 20), defaultPageSize = 20, # Add in page size options and     # how many results per page          
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "14px"))
  })
  

}

shinyApp(ui, server)


   
