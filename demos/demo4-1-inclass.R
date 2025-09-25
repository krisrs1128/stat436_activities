library(tidyverse)
library(shiny)
theme_set(theme_bw())

###############################################################################
## Setup the data
###############################################################################

fires <- read_csv("https://uwmadison.box.com/shared/static/k5vvekf1bhh9e16qb9s66owygc70t7dm.csv") |>
  mutate(quantile = rank(AcresBurned) / n())
counties_order <- fires |>
  group_by(Counties) |>
  summarise(total = sum(AcresBurned, na.rm = T)) |>
  arrange(total) |>
  pull(Counties)

fires <- fires |>
  mutate(Counties = factor(Counties, levels = counties_order))
dotplot <- function(df) {
  ggplot(df) +
    geom_point(aes(day_of_year, Counties, size = AcresBurned,  col = selected)) +
    scale_color_manual(values = c("orange", "#e3e3e3"))
}

###############################################################################
## Write the Shiny App
###############################################################################

ui <- fluidPage(
  sliderInput("year", "Year", 2013, 2019, c(2013, 2019), sep = ""),
  plotOutput("dotplot", width = 800, height = 500)
)

server <- function(input, output) {

  fires |>
    mutate(selected = FALSE, min_year = input$year[1])

  output$dotplot <- renderPlot({
    fires |>
      mutate(
          selected = between(year, input$year[1], input$year[2]),
          selected = factor(selected, levels = c("TRUE", "FALSE"))
      ) |>
      dotplot()
  })
}

shinyApp(ui, server)