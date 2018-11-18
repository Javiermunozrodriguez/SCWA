library(shiny)
library(DT)
library(data.table)
library(Rcpp)
library(ggplot2)
library(dplyr)
library(magrittr)
library(ggubr)
library(ggmap)

shinyUI(fluidPage(
  
  titlePanel("Perfil de consumo de Agua en Madrid"),
  
  sidebarLayout(sidebarPanel(width=3, 
                             
                             sliderInput("Consumo_anualInput","Consumo anual:", c(25, 10000), min = 0, max = 52000, post = " m3", animate = TRUE),
                             
                             checkboxGroupInput("Tipo_consumidorInput", "Tipo de consumidor",
                                                choices = c("comercial", "domestico baja", "domestico medio", "domestico residencial", "especial", "industrial","social"),
                                                selected = "comercial"),
                             
                             
                             selectInput("DistritoInput", "Distrito",
                                         choices = c("Centro", "Arganzuela", "Retiro", "Salamanca", "Chamartin", "Tetuan", "Chamberi", "Fuencarral",    
                                                     "Moncloa", "Latina", "Carabanchel", "Usera", "Puente_Vallecas", "Moratalaz", "Ciudad_Lineal", "Hortaleza",     
                                                     "Villaverde", "Vallecas", "Vicalvaro", "San_Blas", "barajas"),
                                         selected = "Centro"),
                             
                             uiOutput("DistritoOutput")
  ),
  
  mainPanel("Distribución del distrito",fluidRow(
    splitLayout(cellWidths = c("600px","500px","500px"), plotOutput("coolplot1"), plotOutput("coolplot2"), plotOutput("coolplot3"))),
    tableOutput("results")
  )
  )
  
)
)
