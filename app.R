#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(latex2exp)
library(rmarkdown)
library(Pareto)
library(evd)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "green",
    # Application title
    dashboardHeader(title = "Probabilidad"),
    dashboardSidebar(
      sidebarMenu(
        
        menuItem("Presentacion", tabName = "presentacion", icon = icon("th")),
        menuItem("Section 1", tabName = "p1", icon=icon("book"),
                 menuItem("Distribuciones Discretas", tabName = "u1",
                      menuSubItem("Binomial", tabName = "dbinom"),
                      menuSubItem("Geometrica", tabName = "dgeom"),
                      menuSubItem("Binomial Negativa", tabName = "dbinomN")
                 ),
                 
                 
                 menuItem("Distribuciones Continuas", tabName = "u2",
                      menuSubItem("Distribucion de Pareto", tabName = "dparet"),
                      menuSubItem("Distribucion de Frechet", tabName = "dfrech"),
                      menuSubItem("Distribución Gamma", tabName = "dgama"))),
        
        menuItem("Section 2", tabName = "p2", icon=icon("book"),
                 menuItem("Esperanza y Varianza", tabName = "esperanza", 
                          menuSubItem("Variables Continuas", tabName = "vADiscEsp"), 
                          menuSubItem("Variables Discretas", tabName = "vAContEsp")),
                 menuItem("Funcion generadora", tabName = "fgenerad",
                          menuSubItem("Binomial"),
                          menuSubItem("Geometrica"))
                 )
      
    )),
    body <- dashboardBody(
      tabItems(
        
        tabItem(tabName = "presentacion",
                #------------------------PRESENTACIÃN-----------------------------------
                HTML('<br><br><center><img  style = width:150px src="LogoITT.png"></center><br>'),
                h4(strong("Instituto Tecnológico de Tlaxcala"),align = "center",tags$br(),tags$br()),
                h1(strong("Probabilidad"),align = "center", style = "font-size:40px"),
                h4(strong("Edgar Montiel"),align = "center",tags$br(),tags$br(),tags$br())
                #-----------------------------------------------------------------------
        ),
        
        tabItem(tabName = "dbinom",
                h2("Distribución Binomial"),
                
                p("La distribución binomial es una distribución de probabilidad discreta que cuenta el
                  número de éxitos en una secuenca de n ensayos independientes entre sí con una probabilidad
                  fija p de ocurrencia de éxito entre los ensayos."),
                
                fluidRow(
                  box(title = "Entradas", status = "warning", solidHeader = TRUE,
                      sliderInput("p10",
                                  "Probabilidad fija p:",
                                  min = 0,
                                  max = 1,
                                  value = 0.5),
                      sliderInput("n10",
                                  "Numero de intentos n:",
                                  min = 1,
                                  max = 40,
                                  step = 1,
                                  value = 10)
                  ),
                  box(
                    title = "Salidas",status = "primary", solidHeader = TRUE,
                    plotOutput("distPlot005")
                  )
                )
        ),
        
        tabItem(tabName = "dgeom",
          h2("Distribución Geométrica"),
          
          p("Tambien conocida como distribución de Pascal, la distribución geométrica es un modelo adecuacuado
            para los procesos en los que se repiten pruebas hasta la consecución del éxito a resultado deseado. 
            El proceso consta de un número no definido de pruebas o experimentos separados y dicho proceso
            fianlizará cuando se obtenga por primera vez el resultado deseado", align = "justify"),
                
            fluidRow(
              box(title = "Entradas", status = "warning", solidHeader = TRUE,
                sliderInput("p",
                  "Valor de la probabilidad p:",
                  min = 0,
                  max = 1,
                  value = 0.5),
                  sliderInput("n",
                  "Número de intentos n:",
                  min = 1,
                  max = 40,
                  step = 1,
                  value = 10)
                  ),
              box(
                  title = "Salidas",status = "primary", solidHeader = TRUE,
                    plotOutput("distPlot001")
                  )
                )
                
              ),
        
        #Distribucion binomial negativa
        tabItem(tabName = "dbinomN", solidHeader = TRUE,
                
                
                h2("Distribución Binomial Negativa"),
                
                p("La Distribución Binomial Negativa es una distribución de probabilidad discreta 
                  que incluye a la distribución de Pascal. Es una ampliación de las distribuciones 
                  geométricas, utilizada en procesos en los cuales se ve necesaria la repetición de
                  ensayos hasta conseguir un número de casos favorables (primer éxito).",align = "justify"),
                
                
                fluidRow(
                  box(title = "Entradas", status = "warning", solidHeader = TRUE,
                      sliderInput("p1",
                                  "Valor de la probabilidad p:",
                                  min = 0,
                                  max = 1,
                                  value = 0.5),
                      sliderInput("n1",
                                  "Número de intentos n:",
                                  min = 1,
                                  max = 40,
                                  step = 1,
                                  value = 10)
                  ),
                  box(
                    title = "Salidas",status = "primary", solidHeader = TRUE,
                    plotOutput("distPlot002")
                  )
                )
                ),
        
        tabItem(tabName = "dgama",
                
                h2("Distribucion Gamma"),
                p("la distribución gamma es una distribución con dos parámetros que pertenece a 
                  las distribuciones de probabilidad continuas."),
                
                
        
        fluidRow(
          box(title = "Entradas", status = "warning", solidHeader = TRUE,
              sliderInput("p11",
                          "Valor del parámetro K:",
                          min = 0,
                          max = 20,
                          value = 10),
              sliderInput("n11",
                          "Valor del parámetro teta:",
                          min = 0,
                          max = 10,
                          step = .5,
                          value = 1)
          ),
          box(
            title = "Salidas",status = "primary", solidHeader = TRUE,
            plotOutput("distPlot006")
          )
        )
        ),
        
        
        #Distribucion de pareto
        tabItem(tabName = "dparet", solidHeader = FALSE,
                
                h2("Distribución de Pareto"),
                
                p("Esta distribución surgió a finales del siglo XIX (Pareto, 1897) ante la preocupación
                    de los economistas matemáticos de proporcionar modelos probabilísticos que ajusten
                      correctamente la distribución de frecuencias de la renta personal.",align = "justify"),
                
                fluidRow(
                  box(title = "Entradas", status = "warning", solidHeader = TRUE,
                      sliderInput("p3",
                                  "X:",
                                  min = 0,
                                  max = 50,
                                  value = 15),
                      sliderInput("n3",
                                  "T:",
                                  min = 1,
                                  max = 100,
                                  step = .5,
                                  value = 3.5),
                      sliderInput("a",
                                  "alpha:",
                                  min = 1,
                                  max = 100,
                                  step = 1,
                                  value = 10)
                  ),
                      
                      
                  
                  box(
                    title = "Salidas",status = "primary", solidHeader = TRUE,
                    plotOutput("distPlot003")
                  )
                ),
        ),
          
        
        #Distribucion de Frechet
        
        tabItem(tabName = "dfrech", status = "warning", solidHeader = TRUE,
                
                h2("Distribución de Fréchet"),
                p("La distribución de Fréchet es un caso especial de la distribución de valores extremos generalizada.
                  En la hidrología, se utiliza la distribución de Fréchet para analizar variables aleatorias como valores máximos de la precipitación y la descarga de ríos,
                  y además para describir épocas de sequía."), 
                fluidRow(
                  box(title = "Entradas", status = "warning", solidHeader = TRUE,
                      sliderInput("p4",
                                  "Valor de x mínimo:",
                                  min = 0,
                                  max = 40,
                                  value = 1),
                      sliderInput("pmax",
                                  "Valor de x máximo:",
                                  min = 0,
                                  max = 40,
                                  value = 1
                      ),
                      sliderInput("n4",
                                  "Parámetro de localización m:",
                                  min = 1,
                                  max = 40,
                                  step = 1,
                                  value = 1),
                      sliderInput("p5",
                                  "Escala s>0:",
                                  min = 0,
                                  max = 1,
                                  step = .1,
                                  value = .1),
                      sliderInput("n5",
                                  "Valor de forma alpha>0:",
                                  min = 0,
                                  max = 1,
                                  step = .1,
                                  value = .1)
                  ),
                  box(
                    title = "Salidas",status = "primary", solidHeader = TRUE,
                    plotOutput("distPlot004")
                  )
                )
                ),
        
        tabItem(tabName = "vADiscEsp",
                
                h2("Esperanza de variables continuas"),
                fluidRow(
                  box(title = "Entradas", status = "warning", solidHeader = TRUE,
                      textInput("funcn", "Ingresa tu funcion de probabilidad ", "x"),
                      textInput("inferior", "Ingresa el indice inferior", "0"),
                      textInput("superior", "Ingresa el indice superior", "1")
                  ),
                  box(
                    title = "Salidas",status = "primary", solidHeader = TRUE,
                    HTML("<strong>Esperanza:</strong>"),
                    tags$div(style= "text-aling: center", textOutput("esperanza2"))
                  )
                ),
                
                h2("Varianza de variables continuas"),
                fluidRow(
                  box(title = "Entradas", status = "warning", solidHeader = TRUE,
                      textInput("funcn", "Ingresa tu funcion de probabilidad ", "x"),
                      textInput("inferior", "Ingresa el indice inferior", "0"),
                      textInput("superior", "Ingresa el indice superior", "1")
                  ),
                  box(
                    title = "Salidas",status = "primary", solidHeader = TRUE,
                    HTML("<strong>Esperanza:</strong>"),
                    tags$div(style= "text-aling: center", textOutput("esperanza2"))
                  )
                ),
                
        ), 
        
        tabItem(tabName = "vAContEsp",
                h2("Esperanza de variables aleatorias discretas"),
                fluidRow(
                  box(
                    title = "Entradas" , status = "warning", solidHeader = TRUE,
                    
                    textInput("valoresX", "Valores en x: ", "-1, 0, 1, 2"),
                    textInput("valoresfx", "Valores en f(x): ", ".125, .5, .125, .250")
                              
                    
                      
                  ),
                  box(
                    title = "Salidas",status = "primary", solidHeader = TRUE,
                    
                    HTML("<strong>Esperanza:</strong>"),
                    tags$div(style= "text-aling: center", textOutput("esperanz"))
                  ),
        
                ),
                h2("Varianza de variables aleatorias discretas"),
                fluidRow(
                  box(
                    title = "Entradas" , status = "warning", solidHeader = TRUE,
                    
                    textInput("valores", "Valores en x: ", "-1, 0, 1, 2"),
                    textInput("valoresdx", "Valores en f(x): ", ".125, .5, .125, .250")
                    
                    
                    
                  ),
                  box(
                    title = "Salidas",status = "primary", solidHeader = TRUE,
                    
                    HTML("<strong>Varianza:</strong>"),
                    tags$div(style= "text-aling: center", textOutput("varianza"))
                  ),
                  
                ))
           )
    )
) #Fin DashBoard


# Define server logic required to draw a histogram
service <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$n + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$distPlot001 <- renderPlot({
      x    <- 0:input$n
      probs <- dgeom(x, prob = input$p)
      qplot(x, probs, geom = c("point", "line"), 
            main = paste("Densidad de distribución Geometrica, n = ", as.character(input$n)),
            ylab = "p(x)", ylim = c(0, 1))
    })
    
    output$distPlot002 <- renderPlot({
      x <- 0:input$n1
      probs <- dnbinom(x, size = input$n1, prob = input$p1)
      qplot(x, probs, geom = c("point", "line"), 
            main = paste("Densidad de distribución Binomial Negativa, n = ", as.character(input$n)),
            ylab = "p(x)", ylim = c(0, 1))
    })
    
    
    output$distPlot003 <- renderPlot({
      x <- 0:input$p3
      probs <- dPareto(x, input$n3, input$a)
      qplot(x, probs, geom = c("point", "line"),
            main = paste("Densidad de distribución de Pareto, n = ", as.character(input$n)),
            ylab = "p(x)", ylim = c(1,0))
    })
    
    
    output$distPlot004 <- renderPlot({
      x <- input$p4:input$pmax
      probs <- dfrechet(x, input$n4, input$p5, input$n5)
      qplot(x, probs, geom = c("point", "line"),
            main = paste("Densidad de distribución de Fréchet"),
            ylab = "p(x)", ylim = c(1,0))
      
      
    })
    
    output$distPlot005 <- renderPlot({
      x    <- 0:input$n10
      probs <- dbinom(x, size = input$n10, prob = input$p10)
      qplot(x, probs, geom = c("point", "line"), 
            main = paste("Densidad de distribució binomial, n = ", as.character(input$n10)),
            ylab = "p(x)", ylim = c(0, 1)) + theme_bw()
    })
    
    output$distPlot006 <- renderPlot({
      x <- 0:20
      alph <- 1 / input$n11
      probs <- dgamma(x, input$p11, alph)
      qplot(x, probs, geom = c("point", "line"), 
            main = paste("Densidad de distribución gamma, n = ", as.character(input$n11)),
            ylab = "p(x)", ylim = c(0, 1)) + theme_bw()
      
    })
    
    output$esperanz <- renderText({
      x <- as.numeric(unlist(strsplit(input$valoresX,",")))
      fx <- as.numeric(unlist(strsplit(input$valoresfx,",")))
      suma <- 0.00 #
      for (i in 1: length (x)){ 
        suma <- suma + x[i]* fx[i]
      }
      paste(suma)
    
    })
    
    output$varianza <- renderText({
      suma <- 2+2
      paste(suma)
    })
    
    output$esperanza2 <- renderPrint({
      
      f <- function(x) input$funcn
      
      integrate(f, lower = 0, upper = 1)
    })
    }

# Run the application 
shinyApp(ui = ui, server = service)