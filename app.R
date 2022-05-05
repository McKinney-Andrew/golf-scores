# load relevant packages
library(tidyverse)
library(lubridate)
library(shiny)
library(reactable)

# load mckinney analytics colors
darkblue <- rgb(15,25,100, maxColorValue = 255, alpha = 255)
lightblue <- rgb(205,225,250, maxColorValue = 255, alpha = 255)
teal <- rgb(175,215,215, maxColorValue = 255, alpha = 255)
darkgray <- rgb(175,175,175, maxColorValue = 255, alpha = 255)
lightgray <- rgb(225,225,225, maxColorValue = 255, alpha = 255)

# global reactable theme
options(reactable.theme = reactableTheme(
  headerStyle = list(
    background = darkblue, 
    color = "white", 
    fontSize = "16px",
    paddingTop = "15px",
    paddingBottom = "15px",
    position = "sticky"
  )
))

# load the data
adjusted_scores <- read_csv("adjusted_scores.csv")
tournament_results <- read_csv("tournament_scores.csv")
rankings <- read_csv("complete_rankings.csv")

# User Interface
# ------------------------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  
  navbarPage(
    
    "McKinney Analytics",
    
    tabPanel(
      "Rankings",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          fluidRow(
            column(
              width = 6,
              selectInput(
                "rankings_as_of",
                label = "Rankings As Of:",
                choices = unique(rankings$as_of_date),
                selected = max(unique(rankings$as_of_date))
              )            
            )
          ),
          fluidRow(
            column(
              width = 3,
              selectInput(
                "rankings_n",
                label = "Top n:",
                choices = c("10", "25", "50", "100", "All" = max(rankings$mckinney_rank, na.rm = TRUE)),
                selected = "50"
              )            
            )
          ),
          fluidRow(
            column(
              width = 12,
              HTML(r"(
                Our rankings are based on a player's 80-round moving average of adjusted scores.
                Following the methods of <a href="https://datagolfblogs.ca/a-predictive-model-of-tournament-outcomes-on-the-pga-tour/"> Data Golf (2017)</a> 
                  and <a href="http://www.columbia.edu/~mnb2/broadie/Assets/owgr_20120507_broadie_rendleman.pdf"> Broadie and Rendelman (2012)</a>, 
                  scores are adjusted for tournament-round difficulties. To obtain estimates of tournament-round difficulties, we run the following fixed effects regression model:
                <br>
                <br>
                <center><em>Score<sub>ij</sub> = &mu;<sub>i</sub> + &delta;<sub>j</sub> + &epsilon;<sub>ij</sub></em></center> 
                <br>
                where <em>i</em> indexes a player and <em>j</em> indexes a tournament round. 
                We then collect the <em>&delta;<sub>j</sub></em> coefficients from the model and define the adjusted score as <em>Score<sub>ij</sub> - &delta;<sub>j</sub></em>. 
                <br>
                <br>
              )")
            )
          )
        ),
        mainPanel(
          fluidRow(
            column(
              width = 9,
              offset = 1,
              reactableOutput("table1")
            )
          )
        )
      )
    ),
    
    tabPanel(
      "Tournament Results",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          fluidRow(
            column(
              width = 6,
              selectInput(
                "tour",
                label = "Select Tour:",
                choices = unique(tournament_results$tour),
                selected = "USA"
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              selectInput(
                "year",
                label = "Select Year:",
                choices = NULL
              )
            )
          ),
          fluidRow(
            column(
              width = 10,
              selectInput(
                "event",
                label = "Select Tournament:",
                choices = NULL
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              downloadButton(
                "download_tournament_results",
                "Download Results"
              )
            )
          )
        ),
        mainPanel(
          fluidRow(
            column(
              width = 9,
              offset = 1,
              reactableOutput("table2")
            )
          )
        )
      )
    ),
    
    tabPanel(
      "Player Stats",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          fluidRow(
            column(
              width = 12,
              HTML(r"(
                To be included on this page, players must have played at least 50 rounds in the last 2 years. 
                <br>
              )")
            )
          ),
          br(),
          fluidRow(
            column(
              width = 8,
              selectInput(
                "player_1",
                label = "Player 1:",
                choices = sort(
                  adjusted_scores %>% 
                    filter(date >= max(date) - years(2)) %>% 
                    group_by(player) %>% filter(n() > 50) %>% 
                    distinct(player) %>% 
                    .$player
                  ),
                selected = rankings %>%
                  filter(mckinney_rank == 1) %>%
                  .$player
              )
            )
          ),
          fluidRow(
            column(
              width = 8,
              selectInput(
                "player_2",
                label = "Player 2:",
                choices = sort(append("-- None --", 
                  adjusted_scores %>% 
                    filter(date >= max(date) - years(2)) %>% 
                    group_by(player) %>% filter(n() > 50) %>% 
                    distinct(player) %>% 
                    .$player
                )),
                selected = "-- None --"
              )
            )
          ),
          fluidRow(
            column(
              width = 8,
              shinyWidgets::setSliderColor(darkblue, 1),
              sliderInput(
                "date_slider_1",
                label = "Date Range:",
                min = min(adjusted_scores$date),
                max = max(adjusted_scores$date),
                value = c(min(adjusted_scores$date), max(adjusted_scores$date)),
                ticks = FALSE
              )
            )
          )
        ),
        mainPanel(
          column(
            width = 6,
            fluidRow(
              column(
                width = 12,
                plotOutput("plot1")
              )
            )
          ),
          column(
            width = 6,
            fluidRow(
              column(
                width = 12,
                plotOutput("plot2")
              )
            )
          )
        )
      )
    )
    
  )
  
)

# ------------------------------------------------------------------------------------------------------------------------


# Server
# ------------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  ## Rankings Tab ##
  
  output$table1 <- renderReactable({
    rankings %>%
      group_by(player) %>%
      arrange(player, desc(as_of_date)) %>%
      mutate(previous_rank = lead(mckinney_rank)) %>%
      ungroup() %>%
      mutate(change = mckinney_rank - previous_rank) %>%
      arrange(mckinney_rank) %>%
      filter(as_of_date == input$rankings_as_of) %>%
      filter(mckinney_rank <= as.numeric(input$rankings_n)) %>%
      select(country, player, sbse, mckinney_rank, change, owgr_rank) %>%
      reactable(
        pagination = FALSE,
        searchable = TRUE,
        striped = FALSE,
        outlined = TRUE,
        sortable = TRUE,
        showSortable = TRUE,
        defaultColDef = colDef(
          align = "center",
          sortNALast = TRUE
        ),
        columns = list(
          country = colDef(name = "Country"),
          player = colDef(
            name = "Player",
            width = 250,
            align = "left",
            sortable = FALSE
          ),
          sbse = colDef(
            name = "Adj. Scoring Average",
            format = colFormat(digits = 3),
            width = 200
          ),
          mckinney_rank = colDef(name = "M.A. Rank"),
          change = colDef(
            name = "Trend",
            html = TRUE,
            cell = function(x) {
              if (x == 0 | is.na(x)) {
                "&bull;"
              } else if (x > 0) {
                paste("&#8595;", x)
              } else {
                paste("&#8593;", -1*x)
              }
            },
            style = function(x) {
              if (x == 0 | is.na(x)) {
                color <- "black"
              } else if (x < 0) {
                color <- "green"
              } else {
                color <- "red"
              }
              list(color = color, fontWeight = "bold")
            }
          ),
          owgr_rank = colDef(name = "OWGR")
        )
      )
  })
  
  ## Tournament Results Tab ##
  
  tour_results <- reactive({
    tournament_results %>%
      filter(tour == input$tour)
  })
  observeEvent(tour_results(), {
    updateSelectInput(
      session,
      "year",
      choices = unique(year(tour_results()$date)),
      selected = max(year(tour_results()$date))
    )
  })
  
  year_results <- reactive({
    req(input$year)
    filter(tour_results(), year(date) == input$year)
  })
  observeEvent(year_results(), {
    updateSelectInput(
      session,
      "event",
      choices = unique(year_results()$event),
      selected = tail(year_results(), n = 1)
    )
  })
  
  output$table2 <- renderReactable({
    req(input$event)
    year_results() %>%
      filter(event == input$event) %>%
      mutate(
        position = ifelse((position == lead(position) | position == lag(position)) & position != "MC" & position != "WD" & position != "DQ", paste0("T", position), position),
        position = ifelse(row_number() == 1, "1", position)
      ) %>%
      select(position, player, r1_score, r2_score, r3_score, r4_score, total_score) %>%
      select(where(~!all(is.na(.)))) %>%
      reactable(
        pagination = FALSE,
        searchable = TRUE,
        striped = FALSE,
        outlined = TRUE,
        sortable = TRUE,
        showSortable = TRUE,
        defaultColDef = colDef(
          align = "center",
          sortNALast = TRUE
        ),
        columns = list(
          position = colDef(
            name = "Position",
            sortable = FALSE
          ),
          player = colDef(
            name = "Player",
            width = 250,
            align = "left",
            sortable = FALSE
          ),
          r1_score = colDef(
            name = "R1",
            html = TRUE,
            cell = function(x) {
              if (x == 0 | is.na(x)) {
                "&bull;"
              } else {
                paste(x)
              }
            }
          ),
          r2_score = colDef(
            name = "R2",
            html = TRUE,
            cell = function(x) {
              if (x == 0 | is.na(x)) {
                "&bull;"
              } else {
                paste(x)
              }
            }
          ),
          r3_score = colDef(
            name = "R3",
            html = TRUE,
            cell = function(x) {
              if (x == 0 | is.na(x)) {
                "&bull;"
              } else {
                paste(x)
              }
            }
          ),
          r4_score = colDef(
            name = "R4",
            html = TRUE,
            cell = function(x) {
              if (x == 0 | is.na(x)) {
                "&bull;"
              } else {
                paste(x)
              }
            }
          ),
          total_score = colDef(
            name = "Total",
            html = TRUE,
            cell = function(x) {
              if (x == 0 | is.na(x)) {
                "&bull;"
              } else {
                paste(x)
              }
            }
          )
        )
      )
  })
  
  table_for_download <- reactive({
    req(input$event)
    year_results() %>%
      filter(event == input$event) %>%
      mutate(
        position = ifelse((position == lead(position) | position == lag(position)) & position != "MC" & position != "WD" & position != "DQ", paste0("T", position), position),
        position = ifelse(row_number() == 1, "1", position)
      ) %>%
      select(position, player, r1_score, r2_score, r3_score, r4_score, total_score) %>%
      select(where(~!all(is.na(.)))) %>%
      mutate(
        r1_score = ifelse(is.na(r1_score), "", r1_score),
        r2_score = ifelse(is.na(r2_score), "", r2_score),
        r3_score = ifelse(is.na(r3_score), "", r3_score),
        r4_score = ifelse(is.na(r4_score), "", r4_score),
        total_score = ifelse(is.na(total_score), "", total_score)
      )
  })
  
  output$download_tournament_results <- downloadHandler(
    filename = function() {
      paste(input$year, " ", input$event, ".csv", sep = "")
    },
    content = function(file) {
      write_csv(table_for_download(), file)
    }
  )
  
  ## Player Stats Tab ##
  
  output$plot1 <- renderPlot({
    
    adjusted_scores %>%
      filter(player == input$player_1 | player == input$player_2) %>%
      arrange(player, desc(date)) %>%
      group_by(player) %>%
      mutate(x80_round_avg = zoo::rollmean(adjusted_score, k = 80, fill = NA, align = "left")) %>%
      ungroup() %>%
      filter(between(date, input$date_slider_1[1], input$date_slider_1[2])) %>%
      ggplot() +
      geom_line(aes(x = date, y = x80_round_avg, color = player), size = 1) +
      scale_color_manual(
        breaks = c(input$player_1, input$player_2), 
        values = c(darkblue, teal)
      ) +
      labs(
        title = "Adjusted Scoring Average",
        subtitle = "",
        x = "",
        y = "80-Round Moving Average",
        color = ""
      ) +
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 5, face = "italic", hjust = 0.5),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        axis.line = element_line(color = "black", size = 1),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = "top",
        legend.key = element_rect(fill = "white")
      )
    
  })
  
  output$plot2 <- renderPlot({
    
    adjusted_scores %>%
      filter(player == input$player_1 | player == input$player_2) %>%
      filter(between(date, input$date_slider_1[1], input$date_slider_1[2])) %>%
      ggplot() +
      geom_histogram(aes(x = adjusted_score, fill = player)) +
      scale_x_continuous(
        limits = c(60,80),
        breaks = c(60,65,70,75,80)
      ) +
      scale_fill_manual(
        breaks = c(input$player_1, input$player_2), 
        values = c(darkblue, teal)
      ) +
      facet_wrap(~factor(player, levels=c(input$player_1, input$player_2))) +
      labs(
        title = "Adjusted Scoring Distribution",
        subtitle = "",
        x = "",
        y = "# of Occurrences",
        fill = ""
      ) +
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 5, face = "italic", hjust = 0.5),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        axis.line = element_line(color = "black", size = 1),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = "top",
        legend.key = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text.x = element_blank()
      )
    
  })
  
}

# ------------------------------------------------------------------------------------------------------------------------


shinyApp(ui, server)
