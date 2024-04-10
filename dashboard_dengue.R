## Options ##
options("scipen"=20)

## Pacotes ##
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(tmap)
library(dplyr)
library(leaflet)
library(ggplot2)

## Loadind data ##
load(file = 'Mapas.Rdata')

dengerj <- read.csv2("RJcasos.csv")

dengesp <- read.csv2("SPcasos.csv")

casos <- rbind(dengerj,dengesp)


## User Interface ##
## Cabeçalho
header <- dashboardHeader(title = "DASHBOARD DENGUE",
                              titleWidth = 600)

## Barra lateral
sidebar <- dashboardSidebar(
    width = 150,
    sidebarMenu(
        menuItem("Evolução Temporal", tabName = "distPlot"),
        menuItem("Mapa - Estados", tabName = "mapa")
        )
)

## Caixa 2 para o mapa 2
Boxm <-
    box(
        title = tags$b("Mapa coroplético dos municípios de provável contaminação ", style = 'font-family: "Georgia"'),
        closable = FALSE, 
        width = 12,
        height = 14,
        status = "primary", 
        solidHeader = TRUE, 
        collapsible = TRUE,
        enable_dropdown = TRUE,
        leafletOutput("mapa"), # What should i do here?
        footer = NULL
    )

## Body do dashboard
body <- dashboardBody(
    tabItems(
        # Maps
        tabItem(tabName = "distPlot",
                fluidRow(
                    box(selectInput(inputId="Muni1",label="Escolha o Estado",
                                    choices = c("Rio de Janeiro","São Paulo"),multiple = F,
                                    selected = "Rio de Janeiro"),
                        
                        conditionalPanel("input.Muni1 == 'São Paulo'",
                                         selectInput(inputId="Muni1_1",label="Escolha o Município de São Paulo",
                                                     choices = c(unique(dengesp$Municipio)),multiple = F,
                                                     selected = "Estado São Paulo"),),
                        
                        conditionalPanel("input.Muni1 == 'Rio de Janeiro'",
                                         selectInput(inputId="Muni1_2",label="Escolha o Município do Rio de Janeiro",
                                                     choices = c(unique(dengerj$Municipio)),multiple = F,
                                                     selected = "Estado Rio de Janeiro"),),
                        
                        selectInput(inputId="Muni2",label="Escolha o Estado",
                                    choices = c("Rio de Janeiro","São Paulo"),multiple = F,
                                    selected = "São Paulo"),
                        
                        conditionalPanel("input.Muni2 == 'São Paulo'",
                                         selectInput(inputId="Muni2_1",label="Escolha o Município de São Paulo",
                                                     choices = c(unique(dengesp$Municipio)),
                                                     selected = "Estado São Paulo",multiple = F),),
                        
                        conditionalPanel("input.Muni2 == 'Rio de Janeiro'",
                                         selectInput(inputId="Muni2_2",label="Escolha o Município do Rio de Janeiro",
                                                     choices = c(unique(dengerj$Municipio)),
                                                     selected = "Estado Rio de Janeiro",multiple = F)),
                        width = 3,
                        height = 0),
                    
                    box(title = tags$b("Comparativo da evolução de casos", style = 'font-family: "Georgia"'),
                        closable = TRUE, 
                         width = 9,
                         height = 5,
                         status = "primary", 
                         solidHeader = TRUE, 
                         collapsible = TRUE,
                         enable_dropdown = TRUE,
                         plotOutput(outputId = "distPlot"))
                    
                )
                ),
        tabItem(tabName = "mapa",
                fluidRow(
                    box(selectInput(inputId="Muni",label="Escolha o Estado",
                                    choices = c("Rio de Janeiro","São Paulo"),multiple = F,
                                    selected = "Rio de Janeiro")),
                    Boxm # Maps
                )
        )
        
    )
)

ui <- dashboardPage(header, sidebar, body)

## Server ##
server <- function(input, output) {
    
    output$mapa <- renderLeaflet({
        tmap_mode("view")
        if(input$Muni == "Rio de Janeiro"){
            tmap_leaflet(mapa_RJ)
        }else{
            tmap_leaflet(mapa_sp)
        }
        
    })
    
    output$distPlot <- renderPlot({
        
        
        if(input$Muni1 == "Rio de Janeiro" & input$Muni2 == "São Paulo" ){
            p1 <- ggplot(casos[casos$Municipio == input$Muni1_2 | casos$Municipio == input$Muni2_1,],
                         aes(x = Ano,y = Casos,group = Municipio,color = Municipio)) + geom_line() + geom_point(size = 3) + theme_light()
        }
        
        if(input$Muni1 == "São Paulo" & input$Muni2 == "Rio de Janeiro" ){
            p1 <- ggplot(casos[casos$Municipio == input$Muni1_2 | casos$Municipio == input$Muni2_1,],
                         aes(x = Ano,y = Casos,group = Municipio,color = Municipio)) + geom_line() + geom_point(size = 3) + theme_light()
        }
        
        if(input$Muni1 == "Rio de Janeiro" & input$Muni2 == "Rio de Janeiro" ){
            p1 <- ggplot(casos[casos$Municipio == input$Muni1_2 | casos$Municipio == input$Muni2_2,],
                         aes(x = Ano,y = Casos,group = Municipio,color = Municipio)) + geom_line() + geom_point(size = 3) + theme_light()
        }
        
        if(input$Muni1 == "São Paulo" & input$Muni2 == "São Paulo" ){
            p1 <- ggplot(casos[casos$Municipio == input$Muni1_1 | casos$Municipio == input$Muni2_1,],
                         aes(x = Ano,y = Casos,group = Municipio,color = Municipio)) + geom_line() + geom_point(size = 3) + theme_light()
        }
        
        
        p1
    })
}

## App ##
shinyApp(ui, server)
