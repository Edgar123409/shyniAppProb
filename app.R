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
                      menuSubItem("DistribuciÃ³n Gamma", tabName = "dgama"))),
        
        menuItem("Section 2", tabName = "p2", icon=icon("book"),
                 menuItem("Esperanza y Varianza", tabName = "esperanza", 
                          menuSubItem("Variables Continuas", tabName = "vADiscEsp"), 
                          menuSubItem("Variables Discretas", tabName = "vAContEsp")),
                 menuItem("Funcion generadora", tabName = "fgenerad",
                          menuSubItem("Binomial", tabName = "binomFge"),
                          menuSubItem("Geometrica", tabName = "geomFge"), 
                          menuSubItem("Pareto", tabName = "paretFg"))
                 )
      
    )),
    body <- dashboardBody(
      tabItems(
        
        tabItem(tabName = "presentacion",
                #------------------------PRESENTACIÃÂN-----------------------------------
                HTML('<br><br><center><img  style = width:150px src="LogoITT.png"></center><br>'),
                h4(strong("Instituto TecnolÃ³gico de Tlaxcala"),align = "center",tags$br(),tags$br()),
                h1(strong("Probabilidad"),align = "center", style = "font-size:40px"),
                h4(strong("Edgar Montiel"),align = "center",tags$br(),tags$br(),tags$br())
                #-----------------------------------------------------------------------
        ),
        
        tabItem(tabName = "dbinom",
                h2("DistribuciÃ³n Binomial"),
                
                p("La distribuciÃ³n binomial es una distribuciÃ³n de probabilidad discreta que cuenta el
                  nÃºmero de Ã©xitos en una secuenca de n ensayos independientes entre sÃ­ con una probabilidad
                  fija p de ocurrencia de Ã©xito entre los ensayos."),
                
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
          h2("DistribuciÃ³n GeomÃ©trica"),
          
          p("Tambien conocida como distribuciÃ³n de Pascal, la distribuciÃ³n geomÃ©trica es un modelo adecuacuado
            para los procesos en los que se repiten pruebas hasta la consecuciÃ³n del Ã©xito a resultado deseado. 
            El proceso consta de un nÃºmero no definido de pruebas o experimentos separados y dicho proceso
            fianlizarÃ¡ cuando se obtenga por primera vez el resultado deseado", align = "justify"),
                
            fluidRow(
              box(title = "Entradas", status = "warning", solidHeader = TRUE,
                sliderInput("p",
                  "Valor de la probabilidad p:",
                  min = 0,
                  max = 1,
                  value = 0.5),
                  sliderInput("n",
                  "NÃºmero de intentos n:",
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
                
                
                h2("DistribuciÃ³n Binomial Negativa"),
                
                p("La DistribuciÃ³n Binomial Negativa es una distribuciÃ³n de probabilidad discreta 
                  que incluye a la distribuciÃ³n de Pascal. Es una ampliaciÃ³n de las distribuciones 
                  geomÃ©tricas, utilizada en procesos en los cuales se ve necesaria la repeticiÃ³n de
                  ensayos hasta conseguir un nÃºmero de casos favorables (primer Ã©xito).",align = "justify"),
                
                
                fluidRow(
                  box(title = "Entradas", status = "warning", solidHeader = TRUE,
                      sliderInput("p1",
                                  "Valor de la probabilidad p:",
                                  min = 0,
                                  max = 1,
                                  value = 0.5),
                      sliderInput("n1",
                                  "NÃºmero de intentos n:",
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
                p("la distribuciÃ³n gamma es una distribuciÃ³n con dos parÃ¡metros que pertenece a 
                  las distribuciones de probabilidad continuas."),
                
                
        
        fluidRow(
          box(title = "Entradas", status = "warning", solidHeader = TRUE,
              sliderInput("p11",
                          "Valor del parÃ¡metro K:",
                          min = 0,
                          max = 20,
                          value = 10),
              sliderInput("n11",
                          "Valor del parÃ¡metro teta:",
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
                
                h2("DistribuciÃ³n de Pareto"),
                
                p("Esta distribuciÃ³n surgiÃ³ a finales del siglo XIX (Pareto, 1897) ante la preocupaciÃ³n
                    de los economistas matemÃ¡ticos de proporcionar modelos probabilÃ­sticos que ajusten
                      correctamente la distribuciÃ³n de frecuencias de la renta personal.",align = "justify"),
                
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
                
                h2("DistribuciÃ³n de FrÃ©chet"),
                p("La distribuciÃ³n de FrÃ©chet es un caso especial de la distribuciÃ³n de valores extremos generalizada.
                  En la hidrologÃ­a, se utiliza la distribuciÃ³n de FrÃ©chet para analizar variables aleatorias como valores mÃ¡ximos de la precipitaciÃ³n y la descarga de rÃ­os,
                  y ademÃ¡s para describir Ã©pocas de sequÃ­a."), 
                fluidRow(
                  box(title = "Entradas", status = "warning", solidHeader = TRUE,
                      sliderInput("p4",
                                  "Valor de x mÃ­nimo:",
                                  min = 0,
                                  max = 40,
                                  value = 1),
                      sliderInput("pmax",
                                  "Valor de x mÃ¡ximo:",
                                  min = 0,
                                  max = 40,
                                  value = 1
                      ),
                      sliderInput("n4",
                                  "ParÃ¡metro de localizaciÃ³n m:",
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
                
                h2("Esperanza y Varianza de variables aleatorias continuas"),
                fluidRow(
                  box(title = "Entradas", status = "warning", solidHeader = TRUE,
                      textInput("funcn", "Ingresa tu funcion de probabilidad ", "function(x) x*(3/2*(1-x^2))"),
                      textInput("inferior", "Ingresa el indice inferior", "0"),
                      textInput("superior", "Ingresa el indice superior", "1")
                  ),
                  box(
                    title = "Esperanza",status = "primary", solidHeader = TRUE,
                    HTML("<strong>Resultado:</strong>"),
                    tags$div(style= "text-aling: center", textOutput("esperanza2"))
                  ),
                  box(
                    title = "Varianza",status = "primary", solidHeader = TRUE,
                    HTML("<strong>Resultado:</strong>"),
                    tags$div(style= "text-aling: center", textOutput("varianza2"))
                  )
                ),
              
        ), 
        
        tabItem(tabName = "vAContEsp",
                h2("Esperanza y Varianza de variables aleatorias discretas"),
                fluidRow(
                  box(
                    title = "Entradas" , status = "warning", solidHeader = TRUE,
                    
                    textInput("valoresX", "Valores en x: ", "-1, 0, 1, 2"),
                    textInput("valoresfx", "Valores en f(x): ", ".125, .5, .125, .250")
                              
                    
                      
                  ),
                  box(
                    title = "Esperanza",status = "primary", solidHeader = TRUE,
                    
                    HTML("<strong>Resultado:</strong>"),
                    tags$div(style= "text-aling: center", textOutput("esperanz"))
                  ),
                  
                  box(
                    title = "Varianza",status = "primary", solidHeader = TRUE,
                    HTML("<strong>Resultado:</strong>"),
                    tags$div(style= "text-aling: center", textOutput("varianza"))
                  ),
        
                ),
            ), #fin tabItem vaContEsp
        
            tabItem(tabName = "binomFge",
                    h2("Funcion generadora"),
                    fluidRow(
                      box(
                        title = "Entrada", status = "warning", solidHeader = TRUE,
                        sliderInput("fg",
                                    "Tamaño entre cada valor:",
                                    min = 0,
                                    max = 500,
                                    step = 50,
                                    value = 200)
                        
                      ), box(
                        title = "Salidas",status = "primary", solidHeader = TRUE,
                        plotOutput("fgeneradora")
                      )
                    )),
          tabItem(tabName = "geomFge",
                  h2("Funcion generadora"),
                  fluidRow(
                    box(
                      title = "Entrada", status = "warning", solidHeader = TRUE,
                      sliderInput("fg2",
                                  "Tamaño entre cada valor:",
                                  min = 0,
                                  max = 500,
                                  step = 50,
                                  value = 50)
                      
                    ), box(
                      title = "Salidas",status = "primary", solidHeader = TRUE,
                      plotOutput("fgeneradora2")
                    )
                  )),
        tabItem(tabName = "paretFg",
                h2("Funcion generadora"),
                fluidRow(
                  box(
                    title = "Entrada", status = "warning", solidHeader = TRUE,
                    sliderInput("fg3",
                                "Tamaño entre cada valor:",
                                min = 0,
                                max = 500,
                                step = 50,
                                value = 50)
                    
                  ), box(
                    title = "Salidas",status = "primary", solidHeader = TRUE,
                    plotOutput("fgeneradora3")
                  )
                ))
        ) #fin tabItems
    )#Fin DashboardBody
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
            main = paste("Densidad de distribuciÃ³n Geometrica, n = ", as.character(input$n)),
            ylab = "p(x)", ylim = c(0, 1))
    })
    
    output$distPlot002 <- renderPlot({
      x <- 0:input$n1
      probs <- dnbinom(x, size = input$n1, prob = input$p1)
      qplot(x, probs, geom = c("point", "line"), 
            main = paste("Densidad de distribuciÃ³n Binomial Negativa, n = ", as.character(input$n)),
            ylab = "p(x)", ylim = c(0, 1))
    })
    
    
    output$distPlot003 <- renderPlot({
      x <- 0:input$p3
      probs <- dPareto(x, input$n3, input$a)
      qplot(x, probs, geom = c("point", "line"),
            main = paste("Densidad de distribuciÃ³n de Pareto, n = ", as.character(input$n)),
            ylab = "p(x)", ylim = c(1,0))
    })
    
    
    output$distPlot004 <- renderPlot({
      x <- input$p4:input$pmax
      probs <- dfrechet(x, input$n4, input$p5, input$n5)
      qplot(x, probs, geom = c("point", "line"),
            main = paste("Densidad de distribuciÃ³n de FrÃ©chet"),
            ylab = "p(x)", ylim = c(1,0))
      
      
    })
    
    output$distPlot005 <- renderPlot({
      x    <- 0:input$n10
      probs <- dbinom(x, size = input$n10, prob = input$p10)
      qplot(x, probs, geom = c("point", "line"), 
            main = paste("Densidad de distribuciÃ³ binomial, n = ", as.character(input$n10)),
            ylab = "p(x)", ylim = c(0, 1)) + theme_bw()
    })
    
    output$distPlot006 <- renderPlot({
      x <- 0:20
      alph <- 1 / input$n11
      probs <- dgamma(x, input$p11, alph)
      qplot(x, probs, geom = c("point", "line"), 
            main = paste("Densidad de distribuciÃ³n gamma, n = ", as.character(input$n11)),
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
      
      x <- as.numeric(unlist(strsplit(input$valoresX,",")))
      fx <- as.numeric(unlist(strsplit(input$valoresfx,",")))
      
      med <- 0.00
      for (i in 1: length(x)){  #Rcorremos el tamaño el la lista
        med <- med + x[i]* fx[i] # en cada iteracion se realiza la operación correspondiente al indice y se suma al contador
      }
      
      var <- 0.00 #Iniciamos una variable que almacenará la varianza
      for (j in 1: length(x)){  #iteramos sobre el tamaño de la lista nuevamente
        var <- var + (x[i]*med)^2 * fx[i] #sumamos el valor de la operacion de cada iteracion, se usa la media
        }
      
      paste(var) #mandamos el valor de var 
    })
    
    output$esperanza2 <- renderPrint({
      
      f <- function(x) x*(3/2*(1-x^2))
      
      integrate(f, lower = 0, upper = 1)
    })
    
    output$varianza2 <- renderPrint({   
      
      f <- function(x) (x-2/3)^2 *(2*x)
      
      integrate(f, lower = 0, upper = 1)
      
    })
    
    output$fgeneradora <- renderPlot({
      Lista <- c()
      
      for (i in seq(50, 50000, by=input$fg)) {
        x <- i
        datos <- c(rbinom(x, 50000, 0.1))
        media = median(datos)
        Lista <- c(Lista, media)
      }
      
      plot(Lista, type = "l", main = "FunciÃ³n generadora de momentos")
    })
    
    output$fgeneradora2 <- renderPlot({
      Lista <- c()
      
      for (i in seq(50, 10000, by=input$fg2)) {
        x <- i
        datos <- c(rgeom(x, 0.1))
        media = median(datos)
        Lista <- c(Lista, media)
      }
      
      plot(Lista, type = "l", main = "FunciÃ³n generadora de momentos")
    })
    
    output$fgeneradora3 <- renderPlot({
      Lista <- c()
      
      for (i in seq(50, 10000, by=input$fg3)) {
        x <- i
        datos <- c(rPareto(x,10000, 0.1))
        media = median(datos)
        Lista <- c(Lista, media)
      }
      
      plot(Lista, type = "l", main = "FunciÃ³n generadora de momentos")
    })
    }

# Run the application 
shinyApp(ui = ui, server = service)