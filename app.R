library(tidyverse)
library(shiny)
library(gtools)
library(rintrojs)
library(plotly)
library(viridis)


# # Step 1: Let's get Rank players by court region-Step 3 in empirical-lpl-demo.Rmd
load("shot_data_step_2.rda")
load("full_lineups_w_mins.rda")

# For plotting court
source("discrete_court_regions-ggplot2.R")

# For Player Photos
available_players <- read_csv("available_players.csv")

find_player_by_name = function(n) {
  filter(available_players, lower_name == tolower(n))
}

player_photo_url = function(player_id) {
  paste0("https://stats.nba.com/media/players/230x185/", player_id, ".png")
}

# Notes for player_photo.R
# find_player_by_name() function outputs df of person_id for each player, which you'll use for 
# grabbing NBA photos using urls

# Find unique lineup codes
lineup_code <- shot_data_step_2 %>% 
  select(lineup_code) %>% 
  filter(!is.na(lineup_code)) %>% 
  unique() %>% 
  pull()


# Western Teams
lineup_code_western <- c("NOP", "DAL", "DEN", "GSW", "HOU", "LAC", "LAL", "MIN", "OKC", "OKC", "UTA", "MEM", "SAC", "SAS", "POR", "PHX")

# Eastern Teams
lineup_code_eastern <- c("ATL", "BOS", "CLE", "CHI", "MIA", "MIL", "BKN", "NYK", "ORL", "IND", "PHI", "TOR", "DET", "CHA", "WAS")

# Divide Teams into Conferences
lineup_code_df <- data.frame(lineup_code = as.character(lineup_code))

# Eastern
east <- lineup_code_df %>% 
  mutate(lineup_code_team = sub("^([[:alpha:]]*).*", "\\1", lineup_code)) %>% 
  mutate(conference = if_else(lineup_code_team %in% lineup_code_western, "WEST", "EAST")) %>% 
  filter(conference == "EAST") %>% 
  pull(lineup_code) %>% 
  as.character()
# Sort
east <- mixedsort(east)

# Western
west <- lineup_code_df %>% 
  mutate(lineup_code_team = sub("^([[:alpha:]]*).*", "\\1", lineup_code)) %>% 
  mutate(conference = if_else(lineup_code_team %in% lineup_code_western, "WEST", "EAST")) %>% 
  filter(conference == "WEST") %>% 
  pull(lineup_code) %>% 
  as.character()
# Sort 
west <- mixedsort(west)


# Beginning of Shiny App----

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  introjsUI(),
  
  # Application title
  titlePanel(tagList(
    img(src = "nba-logo.png", height = 60, width = 100),
    
    span("CHUCKERS: 16/17 LINEUP SHOT DISTRIBUTION EFFICIENCY",
         span(
           introBox(
             actionButton("help", 
                          label = "Help",
                          icon = icon("question"),
                          style="color: #fff; background-color: #B21212; border-color: #B21212"),
             actionButton("Twitter", 
                          label = "Twitter",
                          icon = icon("twitter"),
                          width = "80px",
                          onclick ="window.open(`https://twitter.com/howard_baek`, '_blank')",
                          style="color: #fff; background-color: #00acee; border-color: #00acee"),
             actionButton("github",
                          label = "Code",
                          icon = icon("github"),
                          width = "80px",
                          onclick ="window.open(`https://github.com/howardbaek/nba-chuckers`, '_blank')",
                          style="color: #fff; background-color: #767676; border-color: #767676"),
             data.step = 4,
             data.intro = "Read more about these plots on the GitHub README by clicking `Code`"),
           style = "position:absolute;right:2em;"
         )
    )
  ),
  windowTitle = "NBA Chuckers App"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(2,
           # Lineup Code
           introBox(
             selectInput(inputId = "choose_lineup_code",
                         label = "LINEUP CODE",
                         choices = list(
                           Eastern = east,
                           Western = west
                         ),
                         selected = "SAC_1"),
             data.step = 1,
             data.intro = "Search for Lineup Combination"),
           # Num of Shots Taken
           h4(div(img(src="shot.png", width = 30), "NUM OF SHOTS")),
           textOutput("num_shot"),
           tags$head(tags$style("#num_shot{color: black;
                                font-size: 20px;
                                font-style: italic;
                                }"
                         )
           ),
           
           # % of Shots Made
           h4(div(img(src="basketball.png", width = 30), "FG%")),
           textOutput("p_shot_made"),
           tags$head(tags$style("#p_shot_made{color: red;
                                font-size: 18px;
                                font-style: italic;
                                }"
                         )
           ),
           br(),
           # Information about App
           uiOutput("app_info"),
           textOutput("app_info_3"),
           uiOutput("app_info_4"),
           uiOutput("app_info_5"),
           
           h5("Built with",
              img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
              "by",
              img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px")
           ),
           
           
           introBox(
             h4("GLOSSARY"),
             data.step = 3,
             data.intro = "Brief Description of Metrics used"
           )
           ),
    
    column(10,
           tabsetPanel(
             tabPanel("Rank", plotlyOutput("rank_plot")),
             tabPanel("Rank Correspondence", plotOutput("rank_corr")),
             tabPanel("Lineup Points Lost (LPL)", plotOutput("lpl")),
             tabPanel("Player LPL (PLC)", plotOutput("plc")))
    )
    
    
    
           ),
  
  fluidRow(
    column(2,
           textOutput("tab_info_1"),
           textOutput("tab_info_2"),
           textOutput("tab_info_3"),
           textOutput("tab_info_4"),
           textOutput("tab_info_5")),
    column(2,
           introBox(
             uiOutput("player_photo_first"),
             data.step = 2,
             data.intro = "Visualize Lineup Combination with Player Photos")
    ),
    column(2,
           uiOutput("player_photo_second")),
    column(2,
           uiOutput("player_photo_third")),
    column(2,
           uiOutput("player_photo_fourth")),
    column(2,
           uiOutput("player_photo_fifth"))
  )
  
  
  
           )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Choose Lineup Code
  my_lineup_code <- reactive({input$choose_lineup_code})
  
  # NUM of Shots Made
  output$num_shot <- renderText({
    
    # Filter out for lineup shot data with Lineup Code
    lineup_shot_data <- shot_data_step_2 %>%
      filter(lineup_code == my_lineup_code())
    
    lineup_shot_data %>% 
      nrow()
    
  })
  
  # Help
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Previous",
                                               "skipLabel"="Skip")
               )
  )
  
  
  # % of Shots Made
  output$p_shot_made <- renderText({
    
    # Filter out for lineup shot data with Lineup Code
    lineup_shot_data <- shot_data_step_2 %>%
      filter(lineup_code == my_lineup_code())
    
    # P Shot Made
    p_shot_made <- lineup_shot_data %>% 
      summarise(isShotMade = mean(isShotMade)) %>% 
      pull(isShotMade)
    
    p_shot_made <- 100 * round(p_shot_made, 3)
    
    paste0(p_shot_made, "%")
    
  })
  
  # App Info
  howard_baek <- a("Howard Baek", href="http://insidethetv.rbind.io/")
  paper_url <- a("Chuckers: Measuring Lineup Shot Distribution 
                 Optimality Using Spatial Allocative Efficiency Models",
                 href = "http://www.sloansportsconference.com/wp-content/uploads/2019/02/Chuckers-1.pdf")
  ballr_url <- a("Todd Schneider's BallR", href = "https://github.com/toddwschneider/ballr")
  
  output$app_info <- renderUI({
    
    tagList("- App Developed by ", howard_baek)
    
  })
  
  
  output$app_info_3 <- renderText({
    
    "- As Used in MIT Sloan Sports Analytics Conference Paper:"
    
  })
  
  output$app_info_4 <- renderUI({
    
    tagList(paper_url)
    
  })
  
  output$app_info_5 <- renderUI({
    
    tagList("- Player Photos Grabbed with ", ballr_url)
    
  })
  
  output$tab_info_1 <- renderText({
    
    "- FG% = % of shots that player X made."
  })
  
  output$tab_info_2 <- renderText({
    
    "- FGA = the number of shot attempts per 36 minutes by player X"
    
  })
  
  output$tab_info_3 <- renderText({
    
    "- Rank Correspondence = Rank of FGA - Rank of FG%"
    
  })
  
  output$tab_info_4 <- renderText({
    
    "- LPL is defined as the difference in expected points between the actual distribution of shot attempts from a given lineup and the expected points had those same shots been taken according to the optimal redistribution."
    
  })
  
  output$tab_info_5 <- renderText({
    
    "- PLC is each player's contribution to LPL"
    
  })
  
  output$tab_info_2 <- renderText({
    
    "- FGA = the number of shot attempts per 36 minutes by player X"
    
  })
  
  output$tab_info_2 <- renderText({
    
    "- FGA = the number of shot attempts per 36 minutes by player X"
    
  })
  
  
  
  # Rank Plot-------------------------------------------------- 
  output$rank_plot <- renderPlotly({
    
    # Filter out for lineup shot data with Lineup Code
    lineup_shot_data <- shot_data_step_2 %>%
      filter(lineup_code == my_lineup_code())
    
    player_codes <- c(lineup_shot_data$lineup_player_1[1],
                      lineup_shot_data$lineup_player_2[1],
                      lineup_shot_data$lineup_player_3[1],
                      lineup_shot_data$lineup_player_4[1],
                      lineup_shot_data$lineup_player_5[1])
    player_names <- lineup_shot_data$namePlayer[match(player_codes,
                                                      lineup_shot_data$idPlayer)]
    
    lineup_fgp <- matrix(0,10,5)
    rownames(lineup_fgp) <- colnames(shot_data_step_2)[19:28]
    colnames(lineup_fgp) <- player_codes
    
    for(j in 1:10) {
      for(i in 1:length(player_codes)) {
        player_region_data <- shot_data_step_2[shot_data_step_2$idPlayer == player_codes[i] & 
                                                 shot_data_step_2[,18+j] == 1, ]
        lineup_fgp[j,i] <- sum(player_region_data$isShotMade)/nrow(player_region_data)
      }
    }
    # Impute a FG% of 0 for regions where no shots were taken:
    lineup_fgp[is.na(lineup_fgp)] <- 0
    
    # Output: lineup_fgp
    
    
    
    # FGA
    lineup_mins <- full_lineups_w_mins$MIN[which(full_lineups_w_mins$lineup_code == my_lineup_code())]
    
    lineup_fga <- matrix(0,10,5)
    rownames(lineup_fga) <- colnames(shot_data_step_2)[19:28]
    colnames(lineup_fga) <- player_codes
    
    for(j in 1:10) {
      for(i in 1:length(player_codes)) {
        player_region_lineup_data <- shot_data_step_2[shot_data_step_2$idPlayer == player_codes[i] & 
                                                        shot_data_step_2[,18+j] == 1 & 
                                                        shot_data_step_2$lineup_code == my_lineup_code(), ]
        lineup_fga[j,i] <- nrow(player_region_lineup_data) / lineup_mins * 36
      }
    }
    
    
    # Output: lineup_fga
    
    
    # Rank fgp and fga of 5 players for each basis
    fgp_rank = t(apply(-1 * lineup_fgp, 1, rank, ties.method = "random"))
    fga_rank = t(apply(-1 * lineup_fga, 1, rank, ties.method = "random"))
    
    # Plot
    rank_colors <- RColorBrewer::brewer.pal(5, "RdYlBu")
    
    gg_court <- ggplot2::fortify(court_regions())
    gg_lineup_fgp <- NULL
    gg_lineup_fga <- NULL
    for(i in 1:dim(fgp_rank)[2]){
      gg_player_fgp <- gg_court
      gg_player_fgp$rank <- NA
      gg_player_fga <- gg_court
      gg_player_fga$rank <- NA
      for (j in 1:nrow(fgp_rank)){
        gg_player_fgp$rank[gg_player_fgp$id == row.names(fgp_rank)[j]] <- fgp_rank[j,i]
        gg_player_fga$rank[gg_player_fga$id == row.names(fga_rank)[j]] <- fga_rank[j,i]
      }
      gg_player_fgp$rank <- factor(gg_player_fgp$rank, levels = 1:5)
      gg_player_fgp$player <- player_names[i]
      gg_lineup_fgp <- rbind(gg_lineup_fgp, gg_player_fgp)
      
      gg_player_fga$rank <- factor(gg_player_fga$rank, levels = 1:5)
      gg_player_fga$player <- player_names[i]
      gg_lineup_fga <- rbind(gg_lineup_fga, gg_player_fga)
    }
    
    # FGP Plotly Graph-------------------------------
    lineup_fgp <- lineup_fgp %>% 
      as.data.frame()
    
    lineup_fgp_colnames <- lineup_fgp %>% 
      as.data.frame() %>% 
      colnames()
    
    # First player name
    first_player <- lineup_shot_data %>% 
      filter(idPlayer == lineup_fgp_colnames[1]) %>% 
      pull(namePlayer) %>% 
      unique()
    
    # Second player
    second_player <- lineup_shot_data %>% 
      filter(idPlayer == lineup_fgp_colnames[2]) %>% 
      pull(namePlayer) %>% 
      unique()
    
    # Third player
    third_player <- lineup_shot_data %>% 
      filter(idPlayer == lineup_fgp_colnames[3]) %>% 
      pull(namePlayer) %>% 
      unique()
    
    # Fourth player
    fourth_player <- lineup_shot_data %>% 
      filter(idPlayer == lineup_fgp_colnames[4]) %>% 
      pull(namePlayer) %>% 
      unique()
    
    # Fifth player
    fifth_player <- lineup_shot_data %>% 
      filter(idPlayer == lineup_fgp_colnames[5]) %>% 
      pull(namePlayer) %>% 
      unique()
    
    # Change column names to player names
    lineup_fgp_processed <- lineup_fgp %>% 
      rownames_to_column() %>% 
      rename(basis = 1) %>% 
      magrittr::set_colnames(c("basis", first_player, second_player, third_player, fourth_player, fifth_player)) %>% 
      gather(first_player, second_player, third_player, fourth_player, fifth_player, key = "player", value = "fgp") %>% 
      spread(key = basis, value = fgp)
    
    # First Player FGP Basis 10
    first_player_fgp_10 <- lineup_fgp_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_10)
    
    # First Player FGP Basis 11
    first_player_fgp_11 <- lineup_fgp_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_11)
    
    # First Player FGP Basis 20
    first_player_fgp_20 <- lineup_fgp_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_20)
    
    # First Player FGP Basis 21
    first_player_fgp_21 <- lineup_fgp_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_21)
    
    # First Player FGP Basis 22
    first_player_fgp_22 <- lineup_fgp_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_22)
    
    # First Player FGP Basis 30
    first_player_fgp_30 <- lineup_fgp_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_30)
    
    # First Player FGP Basis 31
    first_player_fgp_31 <- lineup_fgp_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_31)
    
    # First Player FGP Basis 32
    first_player_fgp_32 <- lineup_fgp_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_32)
    
    # First Player FGP Basis 33
    first_player_fgp_33 <- lineup_fgp_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_33)
    
    # First Player FGP Basis 34
    first_player_fgp_34 <- lineup_fgp_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_34)
    
    
    # Second Player FGP Basis 10
    second_player_fgp_10 <- lineup_fgp_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_10)
    
    # Second Player FGP Basis 11
    second_player_fgp_11 <- lineup_fgp_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_11)
    
    # Second Player FGP Basis 20
    second_player_fgp_20 <- lineup_fgp_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_20)
    
    # Second Player FGP Basis 21
    second_player_fgp_21 <- lineup_fgp_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_21)
    
    # Second Player FGP Basis 22
    second_player_fgp_22 <- lineup_fgp_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_22)
    
    # Second Player FGP Basis 30
    second_player_fgp_30 <- lineup_fgp_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_30)
    
    # Second Player FGP Basis 31
    second_player_fgp_31 <- lineup_fgp_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_31)
    
    # Second Player FGP Basis 32
    second_player_fgp_32 <- lineup_fgp_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_32)
    
    # Second Player FGP Basis 33
    second_player_fgp_33 <- lineup_fgp_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_33)
    
    # Second Player FGP Basis 34
    second_player_fgp_34 <- lineup_fgp_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_34)
    
    
    # Third Player FGP Basis 10
    third_player_fgp_10 <- lineup_fgp_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_10)
    
    # Third Player FGP Basis 11
    third_player_fgp_11 <- lineup_fgp_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_11)
    
    # Third Player FGP Basis 20
    third_player_fgp_20 <- lineup_fgp_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_20)
    
    # Third Player FGP Basis 21
    third_player_fgp_21 <- lineup_fgp_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_21)
    
    # Third Player FGP Basis 22
    third_player_fgp_22 <- lineup_fgp_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_22)
    
    # Third Player FGP Basis 30
    third_player_fgp_30 <- lineup_fgp_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_30)
    
    # Third Player FGP Basis 31
    third_player_fgp_31 <- lineup_fgp_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_31)
    
    # Third Player FGP Basis 32
    third_player_fgp_32 <- lineup_fgp_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_32)
    
    # Third Player FGP Basis 33
    third_player_fgp_33 <- lineup_fgp_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_33)
    
    # Third Player FGP Basis 34
    third_player_fgp_34 <- lineup_fgp_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_34)
    
    
    # Fourth Player FGP Basis 10
    fourth_player_fgp_10 <- lineup_fgp_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_10)
    
    # Fourth Player FGP Basis 11
    fourth_player_fgp_11 <- lineup_fgp_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_11)
    
    # Fourth Player FGP Basis 20
    fourth_player_fgp_20 <- lineup_fgp_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_20)
    
    # Fourth Player FGP Basis 21
    fourth_player_fgp_21 <- lineup_fgp_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_21)
    
    # Fourth Player FGP Basis 22
    fourth_player_fgp_22 <- lineup_fgp_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_22)
    
    # Fourth Player FGP Basis 30
    fourth_player_fgp_30 <- lineup_fgp_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_30)
    
    # Fourth Player FGP Basis 31
    fourth_player_fgp_31 <- lineup_fgp_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_31)
    
    # Fourth Player FGP Basis 32
    fourth_player_fgp_32 <- lineup_fgp_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_32)
    
    # Fourth Player FGP Basis 33
    fourth_player_fgp_33 <- lineup_fgp_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_33)
    
    # Fourth Player FGP Basis 34
    fourth_player_fgp_34 <- lineup_fgp_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_34)
    
    
    # Fifth Player FGP Basis 10
    fifth_player_fgp_10 <- lineup_fgp_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_10)
    
    # Fifth Player FGP Basis 11
    fifth_player_fgp_11 <- lineup_fgp_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_11)
    
    # Fifth Player FGP Basis 20
    fifth_player_fgp_20 <- lineup_fgp_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_20)
    
    # Fifth Player FGP Basis 21
    fifth_player_fgp_21 <- lineup_fgp_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_21)
    
    # Fifth Player FGP Basis 22
    fifth_player_fgp_22 <- lineup_fgp_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_22)
    
    # Fifth Player FGP Basis 30
    fifth_player_fgp_30 <- lineup_fgp_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_30)
    
    # Fifth Player FGP Basis 31
    fifth_player_fgp_31 <- lineup_fgp_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_31)
    
    # Fifth Player FGP Basis 32
    fifth_player_fgp_32 <- lineup_fgp_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_32)
    
    # Fifth Player FGP Basis 33
    fifth_player_fgp_33 <- lineup_fgp_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_33)
    
    # Fifth Player FGP Basis 34
    fifth_player_fgp_34 <- lineup_fgp_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_34)
    
    
    # Add FGP column----------------------------------
    gg_lineup_fgp <- gg_lineup_fgp %>% 
      mutate(fgp = case_when(
        player == first_player & id == "basis_10" ~ first_player_fgp_10,
        player == first_player & id == "basis_11" ~ first_player_fgp_11,
        player == first_player & id == "basis_20" ~ first_player_fgp_20,
        player == first_player & id == "basis_21" ~ first_player_fgp_21,
        player == first_player & id == "basis_22" ~ first_player_fgp_22,
        player == first_player & id == "basis_30" ~ first_player_fgp_30,
        player == first_player & id == "basis_31" ~ first_player_fgp_31,
        player == first_player & id == "basis_32" ~ first_player_fgp_32,
        player == first_player & id == "basis_33" ~ first_player_fgp_33,
        player == first_player & id == "basis_34" ~ first_player_fgp_34,
        player == second_player & id == "basis_10" ~second_player_fgp_10,
        player == second_player & id == "basis_11" ~second_player_fgp_11,
        player == second_player & id == "basis_20" ~second_player_fgp_20,
        player == second_player & id == "basis_21" ~second_player_fgp_21,
        player == second_player & id == "basis_22" ~second_player_fgp_22,
        player == second_player & id == "basis_30" ~second_player_fgp_30,
        player == second_player & id == "basis_31" ~second_player_fgp_31,
        player == second_player & id == "basis_32" ~second_player_fgp_32,
        player == second_player & id == "basis_33" ~second_player_fgp_33,
        player == second_player & id == "basis_34" ~second_player_fgp_34,
        player == third_player & id == "basis_10" ~ third_player_fgp_10,
        player == third_player & id == "basis_11" ~ third_player_fgp_11,
        player == third_player & id == "basis_20" ~ third_player_fgp_20,
        player == third_player & id == "basis_21" ~ third_player_fgp_21,
        player == third_player & id == "basis_22" ~ third_player_fgp_22,
        player == third_player & id == "basis_30" ~ third_player_fgp_30,
        player == third_player & id == "basis_31" ~ third_player_fgp_31,
        player == third_player & id == "basis_32" ~ third_player_fgp_32,
        player == third_player & id == "basis_33" ~ third_player_fgp_33,
        player == third_player & id == "basis_34" ~ third_player_fgp_34,
        player == fourth_player & id == "basis_10" ~fourth_player_fgp_10,
        player == fourth_player & id == "basis_11" ~fourth_player_fgp_11,
        player == fourth_player & id == "basis_20" ~fourth_player_fgp_20,
        player == fourth_player & id == "basis_21" ~fourth_player_fgp_21,
        player == fourth_player & id == "basis_22" ~fourth_player_fgp_22,
        player == fourth_player & id == "basis_30" ~fourth_player_fgp_30,
        player == fourth_player & id == "basis_31" ~fourth_player_fgp_31,
        player == fourth_player & id == "basis_32" ~fourth_player_fgp_32,
        player == fourth_player & id == "basis_33" ~fourth_player_fgp_33,
        player == fourth_player & id == "basis_34" ~fourth_player_fgp_34,
        player == fifth_player & id == "basis_10" ~ fifth_player_fgp_10,
        player == fifth_player & id == "basis_11" ~ fifth_player_fgp_11,
        player == fifth_player & id == "basis_20" ~ fifth_player_fgp_20,
        player == fifth_player & id == "basis_21" ~ fifth_player_fgp_21,
        player == fifth_player & id == "basis_22" ~ fifth_player_fgp_22,
        player == fifth_player & id == "basis_30" ~ fifth_player_fgp_30,
        player == fifth_player & id == "basis_31" ~ fifth_player_fgp_31,
        player == fifth_player & id == "basis_32" ~ fifth_player_fgp_32,
        player == fifth_player & id == "basis_33" ~ fifth_player_fgp_33,
        player == fifth_player & id == "basis_34" ~ fifth_player_fgp_34,
        TRUE ~ 0
      ))
    
    
    
    gg_lineup_fgp <- gg_lineup_fgp %>% 
      mutate(player = factor(player, levels=c(player_names[1], 
                                              player_names[2],
                                              player_names[3],
                                              player_names[4],
                                              player_names[5]))
      )
    
    # FGP Rank Plot---------------------------
    fgp_rank_plot <- ggplot2::ggplot(data = gg_lineup_fgp, ggplot2::aes(x=long, 
                                                                        y=lat, 
                                                                        group = group,
                                                                        fill = rank,
                                                                        text = paste0("Rank : ", rank,
                                                                                      "<br>FG%: ", round(100 * fgp, 2), "%"))) +
      ggplot2::facet_grid(. ~ player) +
      ggplot2::geom_polygon(color = "black")  +
      ggplot2::coord_equal() +
      scale_fill_viridis(discrete = TRUE, direction = -1) +
      ggplot2::theme(axis.line=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_blank(),
                     axis.text.y=ggplot2::element_blank(),
                     axis.ticks=ggplot2::element_blank(),
                     axis.title.x=ggplot2::element_blank(),
                     axis.title.y=ggplot2::element_blank(),
                     panel.background=ggplot2::element_blank(),
                     panel.border=ggplot2::element_blank(),
                     panel.grid.major=ggplot2::element_blank(),
                     panel.grid.minor=ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(size = 16), 
                     strip.background = ggplot2::element_blank(),
                     legend.position = "none",
                     # legend.margin = ggplot2::margin(0.1, 0.1, 0.1, 0, "npc"),
                     plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
                     panel.spacing.y = ggplot2::unit(0, "cm"),
                     text=element_text(size=16, family="Avenir"))
    
    
    # Plotly Version
    p1 <- ggplotly(fgp_rank_plot, tooltip = "text")
    
    
    
    # FGA Plotly Graph --------------------------------------
    lineup_fga <- lineup_fga %>% 
      as.data.frame()
    
    lineup_fga_colnames <- lineup_fga %>% 
      as.data.frame() %>% 
      colnames()
    
    # Change column names to player names
    lineup_fga_processed <- lineup_fga %>% 
      rownames_to_column() %>% 
      rename(basis = 1) %>% 
      magrittr::set_colnames(c("basis", first_player, second_player, third_player, fourth_player, fifth_player)) %>% 
      gather(first_player, second_player, third_player, fourth_player, fifth_player, key = "player", value = "fga") %>% 
      spread(key = basis, value = fga)
    
    
    first_player_fga_10 <- lineup_fga_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_10)
    
    
    first_player_fga_11 <- lineup_fga_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_11)
    
    
    first_player_fga_20 <- lineup_fga_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_20)
    
    
    first_player_fga_21 <- lineup_fga_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_21)
    
    
    first_player_fga_22 <- lineup_fga_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_22)
    
    
    first_player_fga_30 <- lineup_fga_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_30)
    
    
    first_player_fga_31 <- lineup_fga_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_31)
    
    
    first_player_fga_32 <- lineup_fga_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_32)
    
    
    first_player_fga_33 <- lineup_fga_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_33)
    
    
    first_player_fga_34 <- lineup_fga_processed %>% 
      filter(player == first_player) %>% 
      pull(basis_34)
    
    
    
    
    
    second_player_fga_10 <- lineup_fga_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_10)
    
    
    second_player_fga_11 <- lineup_fga_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_11)
    
    
    second_player_fga_20 <- lineup_fga_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_20)
    
    
    second_player_fga_21 <- lineup_fga_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_21)
    
    
    second_player_fga_22 <- lineup_fga_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_22)
    
    
    second_player_fga_30 <- lineup_fga_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_30)
    
    
    second_player_fga_31 <- lineup_fga_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_31)
    
    
    second_player_fga_32 <- lineup_fga_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_32)
    
    
    second_player_fga_33 <- lineup_fga_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_33)
    
    
    second_player_fga_34 <- lineup_fga_processed %>% 
      filter(player == second_player) %>% 
      pull(basis_34)
    
    
    
    
    
    
    
    third_player_fga_10 <- lineup_fga_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_10)
    
    
    third_player_fga_11 <- lineup_fga_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_11)
    
    third_player_fga_20 <- lineup_fga_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_20)
    
    third_player_fga_21 <- lineup_fga_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_21)
    
    third_player_fga_22 <- lineup_fga_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_22)
    
    third_player_fga_30 <- lineup_fga_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_30)
    
    third_player_fga_31 <- lineup_fga_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_31)
    
    third_player_fga_32 <- lineup_fga_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_32)
    
    third_player_fga_33 <- lineup_fga_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_33)
    
    
    third_player_fga_34 <- lineup_fga_processed %>% 
      filter(player == third_player) %>% 
      pull(basis_34)
    
    
    
    
    
    fourth_player_fga_10 <- lineup_fga_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_10)
    
    
    fourth_player_fga_11 <- lineup_fga_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_11)
    
    
    fourth_player_fga_20 <- lineup_fga_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_20)
    
    
    fourth_player_fga_21 <- lineup_fga_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_21)
    
    
    fourth_player_fga_22 <- lineup_fga_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_22)
    
    
    fourth_player_fga_30 <- lineup_fga_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_30)
    
    
    fourth_player_fga_31 <- lineup_fga_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_31)
    
    
    fourth_player_fga_32 <- lineup_fga_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_32)
    
    
    fourth_player_fga_33 <- lineup_fga_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_33)
    
    
    fourth_player_fga_34 <- lineup_fga_processed %>% 
      filter(player == fourth_player) %>% 
      pull(basis_34)
    
    
    
    
    
    fifth_player_fga_10 <- lineup_fga_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_10)
    
    fifth_player_fga_11 <- lineup_fga_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_11)
    
    
    fifth_player_fga_20 <- lineup_fga_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_20)
    
    fifth_player_fga_21 <- lineup_fga_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_21)
    
    fifth_player_fga_22 <- lineup_fga_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_22)
    
    fifth_player_fga_30 <- lineup_fga_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_30)
    
    fifth_player_fga_31 <- lineup_fga_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_31)
    
    
    fifth_player_fga_32 <- lineup_fga_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_32)
    
    
    fifth_player_fga_33 <- lineup_fga_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_33)
    
    
    fifth_player_fga_34 <- lineup_fga_processed %>% 
      filter(player == fifth_player) %>% 
      pull(basis_34)
    
    
    # Add FGA Column -----------------------------------------------
    gg_lineup_fga <- gg_lineup_fga %>% 
      mutate(fga = case_when(
        player == first_player & id == "basis_10" ~ first_player_fga_10,
        player == first_player & id == "basis_11" ~ first_player_fga_11,
        player == first_player & id == "basis_20" ~ first_player_fga_20,
        player == first_player & id == "basis_21" ~ first_player_fga_21,
        player == first_player & id == "basis_22" ~ first_player_fga_22,
        player == first_player & id == "basis_30" ~ first_player_fga_30,
        player == first_player & id == "basis_31" ~ first_player_fga_31,
        player == first_player & id == "basis_32" ~ first_player_fga_32,
        player == first_player & id == "basis_33" ~ first_player_fga_33,
        player == first_player & id == "basis_34" ~  first_player_fga_34,
        player == second_player & id == "basis_10" ~ second_player_fga_10,
        player == second_player & id == "basis_11" ~ second_player_fga_11,
        player == second_player & id == "basis_20" ~ second_player_fga_20,
        player == second_player & id == "basis_21" ~ second_player_fga_21,
        player == second_player & id == "basis_22" ~ second_player_fga_22,
        player == second_player & id == "basis_30" ~ second_player_fga_30,
        player == second_player & id == "basis_31" ~ second_player_fga_31,
        player == second_player & id == "basis_32" ~ second_player_fga_32,
        player == second_player & id == "basis_33" ~ second_player_fga_33,
        player == second_player & id == "basis_34" ~ second_player_fga_34,
        player == third_player & id == "basis_10" ~ third_player_fga_10,
        player == third_player & id == "basis_11" ~ third_player_fga_11,
        player == third_player & id == "basis_20" ~ third_player_fga_20,
        player == third_player & id == "basis_21" ~ third_player_fga_21,
        player == third_player & id == "basis_22" ~ third_player_fga_22,
        player == third_player & id == "basis_30" ~ third_player_fga_30,
        player == third_player & id == "basis_31" ~ third_player_fga_31,
        player == third_player & id == "basis_32" ~ third_player_fga_32,
        player == third_player & id == "basis_33" ~ third_player_fga_33,
        player == third_player & id == "basis_34" ~  third_player_fga_34,
        player == fourth_player & id == "basis_10" ~ fourth_player_fga_10,
        player == fourth_player & id == "basis_11" ~ fourth_player_fga_11,
        player == fourth_player & id == "basis_20" ~ fourth_player_fga_20,
        player == fourth_player & id == "basis_21" ~ fourth_player_fga_21,
        player == fourth_player & id == "basis_22" ~ fourth_player_fga_22,
        player == fourth_player & id == "basis_30" ~ fourth_player_fga_30,
        player == fourth_player & id == "basis_31" ~ fourth_player_fga_31,
        player == fourth_player & id == "basis_32" ~ fourth_player_fga_32,
        player == fourth_player & id == "basis_33" ~ fourth_player_fga_33,
        player == fourth_player & id == "basis_34" ~ fourth_player_fga_34,
        player == fifth_player & id == "basis_10" ~ fifth_player_fga_10,
        player == fifth_player & id == "basis_11" ~ fifth_player_fga_11,
        player == fifth_player & id == "basis_20" ~ fifth_player_fga_20,
        player == fifth_player & id == "basis_21" ~ fifth_player_fga_21,
        player == fifth_player & id == "basis_22" ~ fifth_player_fga_22,
        player == fifth_player & id == "basis_30" ~ fifth_player_fga_30,
        player == fifth_player & id == "basis_31" ~ fifth_player_fga_31,
        player == fifth_player & id == "basis_32" ~ fifth_player_fga_32,
        player == fifth_player & id == "basis_33" ~ fifth_player_fga_33,
        player == fifth_player & id == "basis_34" ~ fifth_player_fga_34,
        TRUE ~ 0))
    
    
    gg_lineup_fga <- gg_lineup_fga %>% 
      mutate(player = factor(player, levels=c(player_names[1], 
                                              player_names[2],
                                              player_names[3],
                                              player_names[4],
                                              player_names[5]))
      )
    
    # FGA Rank Plot ------------------------
    fga_rank_plot <- ggplot2::ggplot(data = gg_lineup_fga, ggplot2::aes(x=long, 
                                                                        y=lat, 
                                                                        group = group,
                                                                        fill = rank,
                                                                        text = paste0("Rank: ", rank,
                                                                                      "<br>FGA : ", round(fga, 2)))) +
      ggplot2::facet_grid(. ~ player) +
      ggplot2::geom_polygon(color = "black")  +
      ggplot2::coord_equal() +
      scale_fill_viridis(discrete = TRUE, direction = -1) +
      ggplot2::theme(axis.line=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_blank(),
                     axis.text.y=ggplot2::element_blank(),
                     axis.ticks=ggplot2::element_blank(),
                     axis.title.x=ggplot2::element_blank(),
                     axis.title.y=ggplot2::element_blank(),
                     panel.background=ggplot2::element_blank(),
                     panel.border=ggplot2::element_blank(),
                     panel.grid.major=ggplot2::element_blank(),
                     panel.grid.minor=ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(size = 16), 
                     strip.background = ggplot2::element_blank(),
                     legend.position = "none",
                     # legend.margin = ggplot2::margin(0.1, 0.1, 0.1, 0, "npc"),
                     plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
                     panel.spacing.y = ggplot2::unit(0, "cm"),
                     text=element_text(size=16, family="Avenir"))
    
    # FGA Plotly Version------------
    p2 <- ggplotly(fga_rank_plot, tooltip = "text")
    
    # Combine p1 and p2
    plotly::subplot(p1, p2,
                    nrows = 2,
                    margin = 0.05)
    
  })
  
  # Rank Correspondence Plot------------- 
  output$rank_corr <- renderPlot({
    
    # Filter out for lineup shot data with Lineup Code
    lineup_shot_data <- shot_data_step_2 %>%
      filter(lineup_code == my_lineup_code())
    
    player_codes <- c(lineup_shot_data$lineup_player_1[1],
                      lineup_shot_data$lineup_player_2[1],
                      lineup_shot_data$lineup_player_3[1],
                      lineup_shot_data$lineup_player_4[1],
                      lineup_shot_data$lineup_player_5[1])
    player_names <- lineup_shot_data$namePlayer[match(player_codes,
                                                      lineup_shot_data$idPlayer)]
    
    lineup_fgp <- matrix(0,10,5)
    rownames(lineup_fgp) <- colnames(shot_data_step_2)[19:28]
    colnames(lineup_fgp) <- player_codes
    
    for(j in 1:10) {
      for(i in 1:length(player_codes)) {
        player_region_data <- shot_data_step_2[shot_data_step_2$idPlayer == player_codes[i] & 
                                                 shot_data_step_2[,18+j] == 1, ]
        lineup_fgp[j,i] <- sum(player_region_data$isShotMade)/nrow(player_region_data)
      }
    }
    # Impute a FG% of 0 for regions where no shots were taken:
    lineup_fgp[is.na(lineup_fgp)] <- 0
    
    # Output: lineup_fgp
    
    
    
    # FGA
    lineup_mins <- full_lineups_w_mins$MIN[which(full_lineups_w_mins$lineup_code == my_lineup_code())]
    
    lineup_fga <- matrix(0,10,5)
    rownames(lineup_fga) <- colnames(shot_data_step_2)[19:28]
    colnames(lineup_fga) <- player_codes
    
    for(j in 1:10) {
      for(i in 1:length(player_codes)) {
        player_region_lineup_data <- shot_data_step_2[shot_data_step_2$idPlayer == player_codes[i] & 
                                                        shot_data_step_2[,18+j] == 1 & 
                                                        shot_data_step_2$lineup_code == my_lineup_code(), ]
        lineup_fga[j,i] <- nrow(player_region_lineup_data) / lineup_mins * 36
      }
    }
    
    
    # Output: lineup_fga
    
    
    # Rank fgp and fga of 5 players for each basis
    fgp_rank = t(apply(-1 * lineup_fgp, 1, rank, ties.method = "random"))
    fga_rank = t(apply(-1 * lineup_fga, 1, rank, ties.method = "random"))
    
    
    # Plot Rank Correspondence
    rank_correspondence <- fga_rank - fgp_rank
    rank_corr_colors <- RColorBrewer::brewer.pal(9, "RdBu")
    
    
    gg_court <- ggplot2::fortify(court_regions())
    gg_lineup_corr <- NULL
    gg_lineup_fgp <- NULL
    gg_lineup_fga <- NULL
    
    for(i in 1:dim(fgp_rank)[2]){
      gg_player_fgp <- gg_court
      gg_player_fgp$rank <- NA
      gg_player_fga <- gg_court
      gg_player_fga$rank <- NA
      for (j in 1:nrow(fgp_rank)){
        gg_player_fgp$rank[gg_player_fgp$id == row.names(fgp_rank)[j]] <- fgp_rank[j,i]
        gg_player_fga$rank[gg_player_fga$id == row.names(fga_rank)[j]] <- fga_rank[j,i]
      }
      gg_player_fgp$rank <- factor(gg_player_fgp$rank, levels = 1:5)
      gg_player_fgp$player <- player_names[i]
      gg_lineup_fgp <- rbind(gg_lineup_fgp, gg_player_fgp)
      
      gg_player_fga$rank <- factor(gg_player_fga$rank, levels = 1:5)
      gg_player_fga$player <- player_names[i]
      gg_lineup_fga <- rbind(gg_lineup_fga, gg_player_fga)
    }
    
    
    
    for(i in 1:dim(fgp_rank)[2]){
      gg_player_corr <- gg_court
      gg_player_corr$rank <- NA
      for (j in 1:nrow(fgp_rank)){
        gg_player_corr$rank[gg_player_fgp$id == row.names(fgp_rank)[j]] <- fga_rank[j,i] - fgp_rank[j,i]
      }
      gg_player_corr$rank <- factor(gg_player_corr$rank, levels = 4:(-4))
      levels(gg_player_corr$rank)[1] = "4 (Under-Use)"
      levels(gg_player_corr$rank)[2] = "3 (Under-Use)"
      levels(gg_player_corr$rank)[3] = "2 (Under-Use)"
      levels(gg_player_corr$rank)[4] = "1 (Under-Use)"
      levels(gg_player_corr$rank)[6] = "-1 (Over-Use)"
      levels(gg_player_corr$rank)[7] = "-2 (Over-Use)"
      levels(gg_player_corr$rank)[8] = "-3 (Over-Use)"
      levels(gg_player_corr$rank)[9] = "-4 (Over-Use)"
      gg_player_corr$player <- player_names[i]
      gg_lineup_corr <- rbind(gg_lineup_corr, gg_player_corr)
    }
    
    gg_lineup_corr <- gg_lineup_corr %>% 
      mutate(player = factor(player, levels=c(player_names[1], 
                                              player_names[2],
                                              player_names[3],
                                              player_names[4],
                                              player_names[5]))
      )
    
    rank_corr_plot <- ggplot2::ggplot(data = gg_lineup_corr, ggplot2::aes(x=long, 
                                                                          y=lat, 
                                                                          group = group,
                                                                          fill = rank)) +
      ggplot2::facet_grid(. ~ player) +
      ggplot2::geom_polygon()  +
      ggplot2::geom_path(color = "black") +
      ggplot2::coord_equal() +
      ggplot2::scale_fill_brewer(type = "div",
                                 palette = "RdBu",
                                 direction = 1,
                                 na.value = "grey60",
                                 name = "Rank\nCorrespondence",
                                 drop = FALSE) +
      ggplot2::theme(axis.line=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_blank(),
                     axis.text.y=ggplot2::element_blank(),
                     axis.ticks=ggplot2::element_blank(),
                     axis.title.x=ggplot2::element_blank(),
                     axis.title.y=ggplot2::element_blank(),
                     panel.background=ggplot2::element_blank(),
                     panel.border=ggplot2::element_blank(),
                     panel.grid.major=ggplot2::element_blank(),
                     panel.grid.minor=ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(size = 16), 
                     strip.background = ggplot2::element_blank(),
                     legend.text = ggplot2::element_text(size = 14),
                     legend.title = ggplot2::element_text(size = 16),
                     legend.justification = "left",
                     legend.position = "right",
                     # legend.margin = ggplot2::margin(0.1, 0.1, 0.1, 0, "npc"),
                     plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
                     panel.spacing.y = ggplot2::unit(0, "cm"),
                     text=element_text(family="Avenir"))
    
    rank_corr_plot
    
    
    
    
    
  })
  
  
  
  # LPL Plot------------------------ 
  output$lpl <- renderPlot({
    
    
    # Filter out for lineup shot data with Lineup Code
    lineup_shot_data <- shot_data_step_2 %>%
      filter(lineup_code == my_lineup_code())
    
    player_codes <- c(lineup_shot_data$lineup_player_1[1],
                      lineup_shot_data$lineup_player_2[1],
                      lineup_shot_data$lineup_player_3[1],
                      lineup_shot_data$lineup_player_4[1],
                      lineup_shot_data$lineup_player_5[1])
    player_names <- lineup_shot_data$namePlayer[match(player_codes,
                                                      lineup_shot_data$idPlayer)]
    
    lineup_fgp <- matrix(0,10,5)
    rownames(lineup_fgp) <- colnames(shot_data_step_2)[19:28]
    colnames(lineup_fgp) <- player_codes
    
    for(j in 1:10) {
      for(i in 1:length(player_codes)) {
        player_region_data <- shot_data_step_2[shot_data_step_2$idPlayer == player_codes[i] & 
                                                 shot_data_step_2[,18+j] == 1, ]
        lineup_fgp[j,i] <- sum(player_region_data$isShotMade)/nrow(player_region_data)
      }
    }
    # Impute a FG% of 0 for regions where no shots were taken:
    lineup_fgp[is.na(lineup_fgp)] <- 0
    
    # Output: lineup_fgp
    
    
    
    # FGA
    lineup_mins <- full_lineups_w_mins$MIN[which(full_lineups_w_mins$lineup_code == my_lineup_code())]
    
    lineup_fga <- matrix(0,10,5)
    rownames(lineup_fga) <- colnames(shot_data_step_2)[19:28]
    colnames(lineup_fga) <- player_codes
    
    for(j in 1:10) {
      for(i in 1:length(player_codes)) {
        player_region_lineup_data <- shot_data_step_2[shot_data_step_2$idPlayer == player_codes[i] & 
                                                        shot_data_step_2[,18+j] == 1 & 
                                                        shot_data_step_2$lineup_code == my_lineup_code(), ]
        lineup_fga[j,i] <- nrow(player_region_lineup_data) / lineup_mins * 36
      }
    }
    
    
    # Output: lineup_fga
    
    
    # Rank fgp and fga of 5 players for each basis
    fgp_rank = t(apply(-1 * lineup_fgp, 1, rank, ties.method = "random"))
    fga_rank = t(apply(-1 * lineup_fga, 1, rank, ties.method = "random"))
    
    #LPL
    lineup_estimated_makes <- apply(lineup_fgp * lineup_fga,1,sum)
    lineup_potential_makes <- NA
    location_point_vals <- rep(2:3, each = 5)
    old_shooting_order <- colnames(lineup_fgp)
    player_lpl <- lineup_fga
    
    for(i in 1:nrow(player_lpl)){
      new_shooting_order <- names(lineup_fgp[i,order(fgp_rank[i,])])
      actual_expected_makes = lineup_fgp[i, ] * lineup_fga[i, ] 
      optimum_expected_makes = (lineup_fgp[i,order(fgp_rank[i,])] *
                                  lineup_fga[i,order(fga_rank[i,])])[match(old_shooting_order,new_shooting_order)]
      player_lpl[i,] = (optimum_expected_makes - actual_expected_makes) * location_point_vals[i]
    }
    
    LPL_per_shot <- apply(player_lpl,1,sum)/apply(lineup_fga,1,sum)
    LPL_per_36 <- apply(player_lpl,1,sum)
    
    
    
    # Plot
    gg_LPL <- ggplot2::fortify(court_regions())
    gg_LPL$lpl_36 <- NA
    gg_LPL$lpl_shot <- NA
    for (j in 1:length(LPL_per_36)){
      gg_LPL$lpl_36[gg_LPL$id == names(LPL_per_36)[j]] <- LPL_per_36[j]
      gg_LPL$lpl_shot[gg_LPL$id == names(LPL_per_shot)[j]] <- LPL_per_shot[j]
    }
    
    gg_LPL_36_plot <- ggplot2::ggplot(data = gg_LPL, ggplot2::aes(x=long, 
                                                                  y=lat, 
                                                                  group = group,
                                                                  fill = lpl_36)) +
      ggplot2::geom_polygon()  +
      ggplot2::geom_path(color = "black") +
      ggplot2::coord_equal() +
      scale_fill_viridis(name = "LPL per 36") +
      ggplot2::theme(axis.line=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_blank(),
                     axis.text.y=ggplot2::element_blank(),
                     axis.ticks=ggplot2::element_blank(),
                     axis.title.x=ggplot2::element_blank(),
                     axis.title.y=ggplot2::element_blank(),
                     panel.background=ggplot2::element_blank(),
                     panel.border=ggplot2::element_blank(),
                     panel.grid.major=ggplot2::element_blank(),
                     panel.grid.minor=ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(size = 16), 
                     strip.background = ggplot2::element_blank(),
                     legend.text = ggplot2::element_text(size = 14),
                     legend.title = ggplot2::element_text(size = 16),
                     legend.justification = "left",
                     legend.position = "right",
                     plot.title = ggplot2::element_text(hjust = 0.5,
                                                        vjust = 0,
                                                        size = 16),
                     plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
                     panel.spacing.y = ggplot2::unit(0, "cm"),
                     text=element_text(family="Avenir")) + 
      ggplot2::ggtitle(label = "LPL per 36")
    
    gg_LPL_shot_plot <- ggplot2::ggplot(data = gg_LPL, ggplot2::aes(x=long, 
                                                                    y=lat, 
                                                                    group = group,
                                                                    fill = lpl_shot)) +
      ggplot2::geom_polygon()  +
      ggplot2::geom_path(color = "black") +
      ggplot2::coord_equal() +
      scale_fill_viridis(name = "LPL per Shot") +
      ggplot2::theme(axis.line=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_blank(),
                     axis.text.y=ggplot2::element_blank(),
                     axis.ticks=ggplot2::element_blank(),
                     axis.title.x=ggplot2::element_blank(),
                     axis.title.y=ggplot2::element_blank(),
                     panel.background=ggplot2::element_blank(),
                     panel.border=ggplot2::element_blank(),
                     panel.grid.major=ggplot2::element_blank(),
                     panel.grid.minor=ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5, 
                                                        vjust = 0,
                                                        size = 16),
                     strip.text = ggplot2::element_text(size = 16), 
                     strip.background = ggplot2::element_blank(),
                     legend.text = ggplot2::element_text(size = 14),
                     legend.title = ggplot2::element_text(size = 16),
                     legend.justification = "left",
                     legend.position = "right",
                     plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
                     panel.spacing.y = ggplot2::unit(0, "cm"),
                     text=element_text(family="Avenir")) + 
      ggplot2::ggtitle(label = "LPL per Shot")
    
    cowplot::plot_grid(gg_LPL_36_plot, 
                       gg_LPL_shot_plot, 
                       nrow = 1, 
                       align = "h")
    
    
    
  })
  
  
  
  # Player LPL (PLC) Plot--------------------------------
  output$plc <- renderPlot({
    
    # Filter out for lineup shot data with Lineup Code
    lineup_shot_data <- shot_data_step_2 %>%
      filter(lineup_code == my_lineup_code())
    
    player_codes <- c(lineup_shot_data$lineup_player_1[1],
                      lineup_shot_data$lineup_player_2[1],
                      lineup_shot_data$lineup_player_3[1],
                      lineup_shot_data$lineup_player_4[1],
                      lineup_shot_data$lineup_player_5[1])
    player_names <- lineup_shot_data$namePlayer[match(player_codes,
                                                      lineup_shot_data$idPlayer)]
    
    lineup_fgp <- matrix(0,10,5)
    rownames(lineup_fgp) <- colnames(shot_data_step_2)[19:28]
    colnames(lineup_fgp) <- player_codes
    
    for(j in 1:10) {
      for(i in 1:length(player_codes)) {
        player_region_data <- shot_data_step_2[shot_data_step_2$idPlayer == player_codes[i] & 
                                                 shot_data_step_2[,18+j] == 1, ]
        lineup_fgp[j,i] <- sum(player_region_data$isShotMade)/nrow(player_region_data)
      }
    }
    # Impute a FG% of 0 for regions where no shots were taken:
    lineup_fgp[is.na(lineup_fgp)] <- 0
    
    # Output: lineup_fgp
    
    
    
    # FGA
    lineup_mins <- full_lineups_w_mins$MIN[which(full_lineups_w_mins$lineup_code == my_lineup_code())]
    
    lineup_fga <- matrix(0,10,5)
    rownames(lineup_fga) <- colnames(shot_data_step_2)[19:28]
    colnames(lineup_fga) <- player_codes
    
    for(j in 1:10) {
      for(i in 1:length(player_codes)) {
        player_region_lineup_data <- shot_data_step_2[shot_data_step_2$idPlayer == player_codes[i] & 
                                                        shot_data_step_2[,18+j] == 1 & 
                                                        shot_data_step_2$lineup_code == my_lineup_code(), ]
        lineup_fga[j,i] <- nrow(player_region_lineup_data) / lineup_mins * 36
      }
    }
    
    
    # Output: lineup_fga
    
    
    
    # Rank fgp and fga of 5 players for each basis
    fgp_rank = t(apply(-1 * lineup_fgp, 1, rank, ties.method = "random"))
    fga_rank = t(apply(-1 * lineup_fga, 1, rank, ties.method = "random"))
    
    
    # Player LPL
    player_lpl <- lineup_fga # Initialize matrix
    player_plc <- lineup_fga # Initialize matrix
    
    lineup_estimated_makes <- apply(lineup_fgp * lineup_fga,1,sum)
    lineup_potential_makes <- NA
    location_point_vals <- rep(2:3, each = 5)
    old_shooting_order <- colnames(lineup_fgp)
    player_lpl <- lineup_fga
    
    for(i in 1:nrow(player_lpl)){
      new_shooting_order <- names(lineup_fgp[i,order(fgp_rank[i,])])
      actual_expected_makes = lineup_fgp[i, ] * lineup_fga[i, ] 
      optimum_expected_makes = (lineup_fgp[i,order(fgp_rank[i,])] *
                                  lineup_fga[i,order(fga_rank[i,])])[match(old_shooting_order,new_shooting_order)]
      player_lpl[i,] = (optimum_expected_makes - actual_expected_makes) * location_point_vals[i]
    }
    
    
    for(i in 1:nrow(player_plc)) {
      new_shooting_order <- names(lineup_fgp[i,order(fgp_rank[i,])])
      actual_expected_makes = lineup_fgp[i, ] * lineup_fga[i, ] 
      optimum_expected_makes = (lineup_fgp[i,order(fgp_rank[i,])] *
                                  lineup_fga[i,order(fga_rank[i,])])[match(old_shooting_order,new_shooting_order)]
      player_lpl[i,] = (optimum_expected_makes - actual_expected_makes) * location_point_vals[i]
      under_over <- ifelse(player_lpl[i, ] < 0, -1, 1)
      player_plc[i,] <- (abs(player_lpl[i,])/sum(abs(player_lpl[i,])))*sum(player_lpl[i,])*under_over
    }
    
    player_plc[is.na(player_plc)] <- 0
    player_plc_ps <- player_plc
    for(i in 1:5){
      player_plc_ps[,i] <- player_plc[,i]/(apply(lineup_fga,1,sum))
    }
    
    
    # Plotting
    
    gg_court <- ggplot2::fortify(court_regions())
    gg_lineup_PLC <- NULL
    for(i in 1:dim(fgp_rank)[2]){
      gg_player_PLC <- gg_court
      gg_player_PLC$plc_36 <- NA
      gg_player_PLC$plc_shot <- NA
      for (j in 1:nrow(fgp_rank)){
        gg_player_PLC$plc_36[gg_player_PLC$id == row.names(player_plc)[j]] <- player_plc[j,i] 
        gg_player_PLC$plc_shot[gg_player_PLC$id == row.names(player_plc_ps)[j]] <- player_plc_ps[j,i] 
      }
      gg_player_PLC$player <- player_names[i]
      gg_lineup_PLC <- rbind(gg_lineup_PLC, gg_player_PLC)
    }
    
    
    gg_lineup_PLC <- gg_lineup_PLC %>% 
      mutate(player = factor(player, levels=c(player_names[1], 
                                              player_names[2],
                                              player_names[3],
                                              player_names[4],
                                              player_names[5]))
      )
    
    PLC_per_36_plot <- ggplot2::ggplot(data = gg_lineup_PLC, ggplot2::aes(x=long, 
                                                                          y=lat, 
                                                                          group = group,
                                                                          fill = plc_36)) +
      ggplot2::facet_grid(. ~ player) +
      ggplot2::geom_polygon()  +
      ggplot2::geom_path(color = "black") +
      ggplot2::coord_equal() +
      ggplot2::scale_fill_gradient2(low = "blue",
                                    high = "red",
                                    mid = "white", midpoint = 0,
                                    name = "PLC per 36") +
      ggplot2::theme(axis.line=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_blank(),
                     axis.text.y=ggplot2::element_blank(),
                     axis.ticks=ggplot2::element_blank(),
                     axis.title.x=ggplot2::element_blank(),
                     axis.title.y=ggplot2::element_blank(),
                     panel.background=ggplot2::element_blank(),
                     panel.border=ggplot2::element_blank(),
                     panel.grid.major=ggplot2::element_blank(),
                     panel.grid.minor=ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(size = 16), 
                     strip.background = ggplot2::element_blank(),
                     legend.text = ggplot2::element_text(size = 14),
                     legend.title = ggplot2::element_text(size = 16),
                     legend.justification = "left",
                     legend.position = "right",
                     # legend.margin = ggplot2::margin(0.1, 0.1, 0.1, 0, "npc"),
                     plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
                     panel.spacing.y = ggplot2::unit(0, "cm"),
                     text=element_text(family="Avenir"))
    
    PLC_per_shot_plot <- ggplot2::ggplot(data = gg_lineup_PLC, ggplot2::aes(x=long, 
                                                                            y=lat, 
                                                                            group = group,
                                                                            fill = plc_shot)) +
      ggplot2::facet_grid(. ~ player) +
      ggplot2::geom_polygon()  +
      ggplot2::geom_path(color = "black") +
      ggplot2::coord_equal() +
      ggplot2::scale_fill_gradient2(low = "blue",
                                    high = "red",
                                    mid = "white", midpoint = 0,
                                    name = "PLC per Shot") +
      ggplot2::theme(axis.line=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_blank(),
                     axis.text.y=ggplot2::element_blank(),
                     axis.ticks=ggplot2::element_blank(),
                     axis.title.x=ggplot2::element_blank(),
                     axis.title.y=ggplot2::element_blank(),
                     panel.background=ggplot2::element_blank(),
                     panel.border=ggplot2::element_blank(),
                     panel.grid.major=ggplot2::element_blank(),
                     panel.grid.minor=ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(size = 16), 
                     strip.background = ggplot2::element_blank(),
                     legend.text = ggplot2::element_text(size = 14),
                     legend.title = ggplot2::element_text(size = 16),
                     legend.justification = "left",
                     legend.position = "right",
                     # legend.margin = ggplot2::margin(0.1, 0.1, 0.1, 0, "npc"),
                     plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
                     panel.spacing.y = ggplot2::unit(0, "cm"),
                     text=element_text(family="Avenir"))
    
    cowplot::plot_grid(PLC_per_36_plot, 
                       PLC_per_shot_plot, 
                       nrow = 2, 
                       align = "v")
  })
  
  
  
  
  # Player Photos------------------------------------------------------------
  # First Player
  output$player_photo_first = renderUI({
    
    # Filter out for lineup shot data with Lineup Code
    lineup_shot_data <- shot_data_step_2 %>%
      filter(lineup_code == my_lineup_code())
    
    # Find Player Codes
    player_codes <- c(lineup_shot_data$lineup_player_1[1],
                      lineup_shot_data$lineup_player_2[1],
                      lineup_shot_data$lineup_player_3[1],
                      lineup_shot_data$lineup_player_4[1],
                      lineup_shot_data$lineup_player_5[1])
    # Find Player Names from Player Codes
    player_names <- lineup_shot_data$namePlayer[match(player_codes,
                                                      lineup_shot_data$idPlayer)]
    
    # First Player Name & Image
    player_names_first <- player_names[1]
    
    # The below functions are sourced from player_photo.R
    # Using player_names_first as input to find_player_by_name, find person_id from available_players df
    # Then, use player_photo_url() function to paste correct url to acquire player photo
    tags$img(src = player_photo_url(find_player_by_name(player_names_first)$person_id), alt = "photo")
    
  })
  
  # Second Player
  output$player_photo_second = renderUI({
    
    # Filter out for lineup shot data with Lineup Code
    lineup_shot_data <- shot_data_step_2 %>%
      filter(lineup_code == my_lineup_code())
    
    # Find Player Codes
    player_codes <- c(lineup_shot_data$lineup_player_1[1],
                      lineup_shot_data$lineup_player_2[1],
                      lineup_shot_data$lineup_player_3[1],
                      lineup_shot_data$lineup_player_4[1],
                      lineup_shot_data$lineup_player_5[1])
    # Find Player Names from Player Codes
    player_names <- lineup_shot_data$namePlayer[match(player_codes,
                                                      lineup_shot_data$idPlayer)]
    
    # Second Player Name & Image
    player_names_second <- player_names[2]
    
    
    tags$img(src = player_photo_url(find_player_by_name(player_names_second)$person_id), alt = "photo")
    
  })
  
  
  # Third Player
  output$player_photo_third = renderUI({
    
    # Filter out for lineup shot data with Lineup Code
    lineup_shot_data <- shot_data_step_2 %>%
      filter(lineup_code == my_lineup_code())
    
    # Find Player Codes
    player_codes <- c(lineup_shot_data$lineup_player_1[1],
                      lineup_shot_data$lineup_player_2[1],
                      lineup_shot_data$lineup_player_3[1],
                      lineup_shot_data$lineup_player_4[1],
                      lineup_shot_data$lineup_player_5[1])
    # Find Player Names from Player Codes
    player_names <- lineup_shot_data$namePlayer[match(player_codes,
                                                      lineup_shot_data$idPlayer)]
    
    # Third Player Name & Image
    player_names_third <- player_names[3]
    
    
    tags$img(src = player_photo_url(find_player_by_name(player_names_third)$person_id), alt = "photo")
    
  })
  
  
  # Fourth Player
  output$player_photo_fourth = renderUI({
    
    # Filter out for lineup shot data with Lineup Code
    lineup_shot_data <- shot_data_step_2 %>%
      filter(lineup_code == my_lineup_code())
    
    # Find Player Codes
    player_codes <- c(lineup_shot_data$lineup_player_1[1],
                      lineup_shot_data$lineup_player_2[1],
                      lineup_shot_data$lineup_player_3[1],
                      lineup_shot_data$lineup_player_4[1],
                      lineup_shot_data$lineup_player_5[1])
    # Find Player Names from Player Codes
    player_names <- lineup_shot_data$namePlayer[match(player_codes,
                                                      lineup_shot_data$idPlayer)]
    
    # Second Player Name & Image
    player_names_fourth <- player_names[4]
    
    
    tags$img(src = player_photo_url(find_player_by_name(player_names_fourth)$person_id), alt = "photo")
    
  })
  
  
  # Fifth Player
  output$player_photo_fifth = renderUI({
    
    # Filter out for lineup shot data with Lineup Code
    lineup_shot_data <- shot_data_step_2 %>%
      filter(lineup_code == my_lineup_code())
    
    # Find Player Codes
    player_codes <- c(lineup_shot_data$lineup_player_1[1],
                      lineup_shot_data$lineup_player_2[1],
                      lineup_shot_data$lineup_player_3[1],
                      lineup_shot_data$lineup_player_4[1],
                      lineup_shot_data$lineup_player_5[1])
    # Find Player Names from Player Codes
    player_names <- lineup_shot_data$namePlayer[match(player_codes,
                                                      lineup_shot_data$idPlayer)]
    
    # Fifth Player Name & Image
    player_names_fifth <- player_names[5]
    
    
    tags$img(src = player_photo_url(find_player_by_name(player_names_fifth)$person_id), alt = "photo")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)