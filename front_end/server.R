library(shiny)
library(dplyr)
library(ggmap)
library(readr)

scwa <- read_csv("scwa.csv")


shinyServer(function(input, output) {
  
  filtered <- reactive({
    
    scwa %>%
      filter(Consumo_anual >= input$Consumo_anualInput[1],
             Consumo_anual <= input$Consumo_anualInput[2],
             Tipo_consumidor == input$Tipo_consumidorInput,
             Distrito == input$DistritoInput
      )
  })
  
  output$coolplot1 <- renderPlot({
    ggplot(filtered())  + geom_count(aes(Tipo_consumidor, Distrito), alpha=0.5, colour= "blue", show.legend = TRUE, stat = "sum") +
      scale_size_area(max_size = 20)+ theme_bw()
    
    #ggplot(filtered(), aes(lat, lon))+geom_point(aes(colour=Consumo_anual)) + coord_map() + stat_density_2d(alpha= 0.5, colour= "blue")
    
    
    
  })
  
  output$coolplot2 <- renderPlot({
    # ggplot(filtered())  + geom_count(aes(Tipo_consumidor, Distrito), alpha=0.5, colour= "blue", show.legend = TRUE, stat = "sum") +
    #scale_size_area(max_size = 20)+ theme_bw()
    
    ggplot(filtered(), aes(lat, lon))+geom_point(aes(colour=Consumo_anual)) + coord_map() + stat_density_2d(alpha= 0.5, colour= "blue") + theme_bw()
    
    
    
  })
  
  output$coolplot3 <- renderPlot({
    
    
    
    qmplot(lon, lat, data = filtered(), geom="blank", zoom=13, maptype = "toner-lite") + 
      geom_point(aes(colour=Consumo_anual), size=2) + coord_map() + 
      stat_density_2d(alpha= 0.5, colour= "blue")+
      scale_fill_gradient2()+ theme_bw()
    
    
  })
  
  output$results <- renderTable({
    filtered()
  })
}
)