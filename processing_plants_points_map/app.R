#####################
### Load packages ###
#####################

library(shiny) #For creating the shiny web app
library(sf) #For spatial data
library(ggplot2) #For graphing
library(scales) #To help graphing
library(terra) #for spatial data
library(tidyterra) #For graphing spatial data
library(plotly) #For interactive graphing

#################
### Load data ###
#################

read_df_shiny_4 <- readRDS("data/df_shiny_4.Rda")
states_for_graphing <- readRDS("data/df_shiny_4_states_for_graphing.Rda")

#################################
### Define UI for application ###
#################################

ui <- fluidPage(
  titlePanel("Processing Plants"),
  
  selectInput("column", "Select Animal/Product:",
              choices = names(read_df_shiny_4)[c(7:11)], 
              selected = names(read_df_shiny_4)[c(7:11)]),
  
  mainPanel(
    plotlyOutput("map")
  )
)

#####################
### Define server ###
#####################

server <- function(input, output) {
  output$map <- renderPlotly({
        colid <- which(names(read_df_shiny_4)==input$column)
        
        # Filter data based on the selected product
        vector7 <- read_df_shiny_4$Beef_Slaughter
        vector8 <- read_df_shiny_4$Pork_Slaughter
        vector9 <- read_df_shiny_4$Chicken_Slaughter
        vector10 <- read_df_shiny_4$Catfish_Products
        vector11 <- read_df_shiny_4$Egg_Products
        
        filtered_data <- subset(read_df_shiny_4,
                                get(paste0("vector",colid)))
        
      g <- ggplot() +
        geom_sf(data=states_for_graphing, fill="white") +
        geom_sf(aes(size=Size2, 
                    shape=Size2, 
                    fill=Size2,
                    text=paste0(Company.x,",\n",StateName)),
                data=filtered_data) +
        geom_sf_text(aes(label=abbr), size=2.5,colour="#40404080", data=states_for_graphing) + 
        theme_void() +
        ggtitle(paste0("Processing plants"),
                gsub("_"," ",names(read_df_shiny_4)[colid])) +
        theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
              plot.background = element_rect(fill="white",color="white"),
              legend.key.size = unit(1, 'cm'),
              legend.text = element_text(size=12)) +
        scale_size_manual(values = c("Large"=3,"Small"=1)) +
        scale_shape_manual(values = c("Large"=21,"Small"=16)) +
        scale_fill_manual(values = c("Large"="goldenrod3","Small"="black"))
    
      # Render the plot
    print(g)
    
    # Convert to plotly and specify tooltip
    ggplotly(g,tooltip="text")%>%
      layout(xaxis=list(visible=FALSE),
             yaxis=list(visible=FALSE),
             legend = list(title=list(text=""),
                           orientation="h",
                           x = 0.35, 
                           y = -0.1))
    
  })
}

###########################
### Run the application ###
###########################

shinyApp(ui = ui, server = server)

