#####################
### Load packages ###
#####################

library(shiny) #For creating the shiny web app
library(ggplot2) #For graphing
library(scales) #To help graphing
library(plotly) #To help interactive graphing

#################
### Load data ###
#################

read_df_shiny_2 <- readRDS("data/df_shiny_2.Rda")

#################################
### Define UI for application ###
#################################

ui <- fluidPage(
  titlePanel("Animals by State and Species"),
  
  # Dropdown menu for selecting states
  selectInput("state", "Select a State:", choices = unique(read_df_shiny_2$state)),
  
  # Plot output area
  plotlyOutput("columnGraph")
)

#####################
### Define server ###
#####################

server <- function(input, output) {
  output$columnGraph <- renderPlotly({
    # Filter data based on the selected state
    filtered_data <- read_df_shiny_2[read_df_shiny_2$state == input$state, ]
    
    # Generate the plot
    g2 <- ggplot(filtered_data, aes(x=reorder(Species, Value), y=Value, text=ValueF)) +
      geom_col() +
      xlab(NULL) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                         name=paste0("Number killed* annually in ",input$state)) +
      coord_flip()# +
      #geom_text(aes(label = scales::comma(Value)))
    
    print(g2)
    
    # Convert ggplot to plotly and specify tooltip
    ggplotly(g2, tooltip = "text")
  })
}

###########################
### Run the application ###
###########################

shinyApp(ui = ui, server = server)

