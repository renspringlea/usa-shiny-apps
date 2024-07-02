#####################
### Load packages ###
#####################

library(shiny) #For creating the shiny web app
library(sf) #For spatial data
library(ggplot2) #For graphing
library(scales) #To help graphing
library(viridis) #To help graphing
library(plotly) #For interactive graphing

#################
### Load data ###
#################

read_df_shiny_1 <- readRDS("data/read_df_shiny_1.Rda")

#################################
### Define UI for application ###
#################################

ui <- fluidPage(
  titlePanel("Animals by Species and State"),
  
  
  
  selectInput("column", "Select Animal:",
              choices = colnames(read_df_shiny_1)[c(9:21)], 
              selected = colnames(read_df_shiny_1)[c(9:21)]),
  
  mainPanel(
    plotlyOutput("map")
  )
)

#####################
### Define server ###
#####################

server <- function(input, output) {
  output$map <- renderPlotly({
    # Filter data based on selected column
    #filtered_data <- read_df_shiny_1[,read_df_shiny_1[[input$column]]!= NA ]
    
    colid <- which(names(read_df_shiny_1)==input$column)
    
    # Create ggplot object
    p <- ggplot(data = read_df_shiny_1, 
                aes(fill = get(names(read_df_shiny_1)[colid]),
                    text=paste0(state,":\n",formatC((get(names(read_df_shiny_1)[colid])/1000000),
                                                    format="fg", big.mark=",",digits=3)," million"))) +
                    
      geom_sf(colour="white") +
      theme_void() +
      geom_sf_text(aes(label=iso3166_2), colour="white") +
      scale_fill_viridis(name=input$column,
                         option="rocket", direction=-1, 
                         labels = unit_format(unit = "M", scale = 1e-6)) +
      ggtitle(input$column,
              subtitle="Number killed in USA annually *") +
      theme(plot.background = element_rect(fill="white",color="white"),
            legend.position="bottom",
            legend.key.height = unit(1, 'cm'),
            legend.key.width = unit(2, 'cm'),
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
    
    
    # Render the plot
    print(p)
    
    # Convert ggplot to plotly and specify tooltip
    ggplotly(p, tooltip = "text")  %>%
      layout(xaxis=list(visible=FALSE),
             yaxis=list(visible=FALSE))
    
  })
}

###########################
### Run the application ###
###########################

shinyApp(ui = ui, server = server)

