library(tidyverse)
library(shiny)
library(gtools)


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
  
  # Application title
  titlePanel(tagList(
    img(src = "nba-logo.png", height = 60, width = 100),
    
    span("CHUCKERS: 16/17 LINEUP SHOT DISTRIBUTION EFFICIENCY",
         span(
           actionButton("Twitter", 
                        label = "Twitter",
                        icon = icon("twitter"),
                        width = "80px",
                        onclick ="window.open(`https://twitter.com/jsonbaik`, '_blank')",
                        style="color: #fff; background-color: #00acee; border-color: #00acee"),
           actionButton("github",
                        label = "Code",
                        icon = icon("github"),
                        width = "80px",
                        onclick ="window.open(`https://github.com/jasonbaik94/nba-chuckers`, '_blank')",
                        style="color: #fff; background-color: #767676; border-color: #767676"),
           style = "position:absolute;right:2em;"
         )
    )
  ),
  windowTitle = "NBA Chuckers App"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(2,
           # Lineup Code
           selectInput(inputId = "choose_lineup_code",
                       label = "LINEUP CODE",
                       choices = list(
                         Eastern = east,
                         Western = west
                       ),
                       selected = "SAC_1"),
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
           br(),
           # Information about App
           uiOutput("app_info"),
           uiOutput("app_info_2"),
           textOutput("app_info_3"),
           uiOutput("app_info_4"),
           uiOutput("app_info_5"),
           
           h5("Built with",
              img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
              "by",
              img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px")
           )
           ),
    
    
    column(10,
           tabsetPanel(
             tabPanel("Rank", plotOutput("rank_plot")),
             tabPanel("Rank Correspondence", plotOutput("rank_corr")),
             tabPanel("Lineup Points Lost (LPL)", plotOutput("lpl")),
             tabPanel("Player LPL (PLC)", plotOutput("plc"))
             
           )
           
           
           
    )
           ),
  fluidRow(
    column(2,
           uiOutput("random_white_space")),
    column(2,
           uiOutput("player_photo_first")),
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
server <- function(input, output) {
  
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
  jason_baik <- a("Jason Baik", href="http://jsonbaik.rbind.io/about/")
  lpl_url <- a("Nathan	Sandholtz", href = "https://github.com/nsandholtz/lpl")
  paper_url <- a("Chuckers: Measuring Lineup Shot Distribution 
                 Optimality Using Spatial Allocative Efficiency Models",
                 href = "http://www.sloansportsconference.com/wp-content/uploads/2019/02/Chuckers-1.pdf")
  ballr_url <- a("Todd Schneider's BallR", href = "https://github.com/toddwschneider/ballr")
  
  output$app_info <- renderUI({
    
    tagList("- App Developed by ", jason_baik)
    
  })
  
  output$app_info_2 <- renderUI({
    
    tagList("- Plots Generated by ", lpl_url)
    
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
  
  # Rank Plot-------------------------------------------------- 
  output$rank_plot <- renderPlot({
    
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
    
    gg_lineup_fgp <- gg_lineup_fgp %>% 
      mutate(player = factor(player, levels=c(player_names[1], 
                                              player_names[2],
                                              player_names[3],
                                              player_names[4],
                                              player_names[5]))
      )
    
    
    fgp_rank_plot <- ggplot2::ggplot(data = gg_lineup_fgp, ggplot2::aes(x=long, 
                                                                        y=lat, 
                                                                        group = group,
                                                                        fill = rank)) +
      ggplot2::facet_grid(. ~ player) +
      ggplot2::geom_polygon()  +
      ggplot2::geom_path(color = "black") +
      ggplot2::coord_equal() +
      ggplot2::scale_fill_manual(values = rank_colors,
                                 limits = 1:5,
                                 name = "FG% Rank") +
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
    
    
    gg_lineup_fga <- gg_lineup_fga %>% 
      mutate(player = factor(player, levels=c(player_names[1], 
                                              player_names[2],
                                              player_names[3],
                                              player_names[4],
                                              player_names[5]))
      )
    
    fga_rank_plot <- ggplot2::ggplot(data = gg_lineup_fga, ggplot2::aes(x=long, 
                                                                        y=lat, 
                                                                        group = group,
                                                                        fill = rank)) +
      ggplot2::facet_grid(. ~ player) +
      ggplot2::geom_polygon()  +
      ggplot2::geom_path(color = "black") +
      ggplot2::coord_equal() +
      ggplot2::scale_fill_manual(values = rank_colors,
                                 limits = 1:5,
                                 name = "FGA Rank") +
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
    
    cowplot::plot_grid(fgp_rank_plot, 
                       fga_rank_plot, 
                       nrow = 2, 
                       align = "v")    
    
    
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
      ggplot2::scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(11, "RdYlBu")),
                                    name = "LPL per 36") +
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
                                                        size = 16,
                                                        face = "bold"),
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
      ggplot2::scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(11, "RdYlBu")),
                                    name = "LPL per Shot") +
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
                                                        size = 16,
                                                        face = "bold"),
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
  
  
  
  # Player LPL Plot--------------------------------
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

