library(shiny)
library(tidyverse)
library(Lahman)

names = People |>
  mutate(name = paste(nameFirst, nameLast, sep = " "))

batting_5000 = Batting |>
  group_by(playerID) |>
  summarize(AB_career = sum(AB, na.rm = TRUE)) |>
  inner_join(Batting, by = "playerID") |>
  filter(AB_career >= 5000)

batting_5000 = batting_5000 |>
  inner_join(names, by = "playerID") |>
  mutate(
    Birthyear = if_else(
      birthMonth >= 7, birthYear + 1, birthYear
    ),
    Age = yearID - Birthyear,
    X1B = H - (X2B + X3B + HR),
    OBP = (H + BB + HBP)/(AB + BB + HBP + SF),
    SLG = (X1B + 2*X2B + 3*X3B + 4*HR)/AB,
    OPS = OBP + SLG
  )

View(batting_5000)
View(pitching_2000)

pitching_2000 = Pitching |>
  mutate(IP = IPouts / 3) |>
  group_by(playerID) |>
  summarize(IP_career = sum(IP, na.rm=TRUE)) |>
  inner_join(Pitching, by = "playerID") |>
  filter(IP_career >= 2000)

pitching_2000 = pitching_2000 |>
  inner_join(names, by = "playerID") |>
  mutate(
    Birthyear = if_else(
      birthMonth >= 7, birthYear + 1, birthYear
    ),
    Age = yearID - Birthyear,
    WHIP = (H + BB)* 3/IPouts
  )

batting_5000 = batting_5000 |> drop_na(Age)
pitching_2000 = pitching_2000 |> drop_na(Age)

## for batters show: HR, SLG, OBP, OPS

## for pitchers show: ERA, WHIP, ER



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Career Trajectories of MLB Players"),
  
  print("Group: Ken Ogata, Alex Kim "),
  
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "positionSelected",
        label = "Choose a Position:",
        choices = c("Batter", "Pitcher")
      ),
      selectInput(
        inputId = "playerSelected", ## fix
        label = "Choose a Player",
        choices = unique(batting_5000$name) ## fix
      ),
      selectInput(
        inputId = "playerSelected2",
        label = "Choose another Player",
        choices = unique(batting_5000$name)
      ),
      selectInput(
        inputId = "statSelected",
        label = "Select a stat:",
        choices = c("H", "HR", "SLG", "OBP", "OPS")
      )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  find_position = reactive({ ## which position did user select
    if(input$positionSelected == "Batter") {
      batting_5000
    } else {
      pitching_2000
    }
  })
  
  showStatistics = reactive({
    if (input$positionSelected == "Batter") {
      c("H", "HR", "SLG", "OBP", "OPS")
    } else {
      c("ER", "SO", "ERA", "WHIP")
    }
  })
  
  observeEvent( ## update dropdown
    eventExpr = input$positionSelected,
    handlerExpr = {
      updateSelectInput(inputId = "playerSelected", choices = unique(find_position()$name))
    }
  )
  
  observeEvent(
    eventExpr = input$positionSelected,
    handlerExpr = {
      updateSelectInput(inputId = "playerSelected2", choices = unique(find_position()$name))
    }
  )
  
  observeEvent(
    eventExpr = input$positionSelected,
    handlerExpr = {
      updateSelectInput(inputId = "statSelected", choices = showStatistics())
    }
  )
  
  
  output$distPlot <- renderPlot({
    
    data = find_position() ## update to positional dataset
    
    ## comp
    leagueAvg = data |>
      group_by(Age) |>
      summarize(AvgStat = mean(.data[[input$statSelected]], na.rm = TRUE), .groups = "drop")
    
    View(head(leagueAvg))
    
    data = data |>
      filter(name %in% c(input$playerSelected, input$playerSelected2))
    
    
    ggplot(data = data, aes_string(x = "Age", y = input$statSelected, color = "name")) +
      geom_point() +
      geom_smooth(
        method = "lm", se = FALSE, linewidth = 1.5,
        formula = y ~ poly(x, 2, raw = TRUE)
      ) +
      geom_line(data = leagueAvg, aes(x = Age, y = AvgStat, group = 1),
                linetype = "dashed", color = "Green 4", linewidth = 1.3)
    
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
