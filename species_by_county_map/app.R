#####################
### Load packages ###
#####################

library(shiny) #For creating the shiny web app
library(ggplot2) #For graphing
library(scales) #To help graphing
library(viridis) #To help graphing
library(plotly) #For interactive graphing

#################
### Load data ###
#################

read_df_shiny_3 <- readRDS("data/df_shiny_3.Rda")

#################################
### Define UI for application ###
#################################

ui <- fluidPage(
  titlePanel("Animals by Species and County"),
  

    
    selectInput("column", "Select Animal:",
                choices = colnames(read_df_shiny_3)[c(5:11)], 
                selected = colnames(read_df_shiny_3)[c(5:11)]),
    
    mainPanel(
      plotlyOutput("map")
  )
)

#####################
### Define server ###
#####################

server <- function(input, output) {
  output$map <- renderPlotly({
        colid <- which(names(read_df_shiny_3)==input$column)
        
        #Get the units for producing the graph subtitle
        graphunits <- ifelse(input$column %in% c("Cattle","Chickens","Hogs"),
                             "Individuals sold by farms",
                             ifelse(
                               input$column %in% c("Crustaceans","Trout","Catfish"),
                               "US Dollar",
                               "Dozen produced by farms"
                             ))
        # Create ggplot object

      g <- ggplot(data=read_df_shiny_3) +
      geom_sf(aes(colour=get(names(read_df_shiny_3)[colid]),
                  fill=get(names(read_df_shiny_3)[colid]),
                  text=paste0(county,", ",full,":\n",formatC(as.numeric(get(names(read_df_shiny_3)[colid])),format="d", big.mark=","))
              )
        ) +
      theme_void() +
      scale_fill_viridis(option="rocket", 
                         name=input$column,
                         direction=-1, 
                         labels = unit_format(unit = "M", scale = 1e-6)) +
      scale_colour_viridis(option="rocket", 
                           name=input$column,
                           direction=-1, 
                           labels = unit_format(unit = "M", scale = 1e-6)) +
      ggtitle(paste0(input$column," produced by county"), 
              subtitle=paste0("Unit = ",graphunits,"; Year = 2022")) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.background = element_rect(fill="white",color="white"),
            legend.position="bottom",
            legend.key.height = unit(1, 'cm'),
            legend.key.width = unit(2, 'cm'),
            legend.title = element_blank())
      
    # Render the plot
    print(g)
    
    # Convert ggplot to plotly and specify tooltip
    ggplotly(g, tooltip = "text") %>%
      layout(xaxis=list(visible=FALSE),
             yaxis=list(visible=FALSE))
    
  })
}

###########################
### Run the application ###
###########################

shinyApp(ui = ui, server = server)

