library(shiny)
library(googlesheets4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)

ui <- fillPage(
  tags$head(tags$style(HTML('.progress-bar {background-color: black;}'))),
  plotOutput("plot", height = "100%"),
)

server <- function(input, output, session) {
  gs4_deauth()
  
  withProgress(message = "Loading data", value = 0, {
    # Pollen counting sheet
    incProgress(0.25, detail = "    Pollen counts")
    pollen_counting <- read_sheet("10_lG9N0wGvgOmxDGuX5PXILB7QwC7m6CuYXzi78Qe3Q") %>%
    drop_na() %>%
    filter(count %in% c("g", "h", "l"))
  
    # Plate worksheets
    incProgress(0.25, detail = "    Plate worksheets")
    plate_worksheets <- read_sheet("1yQ5yAKiL6BzwZ-wH-Q44RoUEwMZztTYafzdvVylq6fo") %>%
      select(date, run, well, temp_target, accession)
    
    joined <- left_join(pollen_counting, plate_worksheets, by = c("date", "run", "well"))
    
    # Summarizing
    summarized <- joined %>%
      filter(count == "g") %>%
      group_by(accession, temp_target) %>%
      count(count) %>%
      filter(n >= 8)
    
    # Getting progress percentage
    progress <- nrow(summarized) / 460
    
    # Getting weather data
    yesterday <- as.character(Sys.Date() -1)
    
    # This is the sensor outside my house
    incProgress(0.25, detail = "    Weather")
    home_weather <- read_sheet("1ELcNVhti0JHUYfMLyu9pRZ_hoGN8uQ-Y8WViw9TLz8Q") %>%
      filter(date == yesterday) %>%
      summarize(max_temp = max(temp_f))
  })
  
  # Making the plot for output
  output$plot <- renderPlot({
    ggplot() +
      geom_rect(aes(xmin = 0, xmax = 1,
                    ymin = 0, ymax = progress,
                    fill = home_weather[[1]])) +
      scale_fill_distiller(limits = c(55, 105),
                           palette = "Spectral") +
      scale_x_continuous(limits = c(0, 1),
                         expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 1),
                         expand = c(0, 0)) +
      theme_nothing() 
  })
}


shinyApp(ui, server)