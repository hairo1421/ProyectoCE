#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Written by: Raul Eulogio

library(shiny)
library(shinydashboard)
library(DT)
library(here)
 # S
getwd()
#setwd("C:/Users/h_air/Documents")
source('helper_functions.R')
here()

getwd()
setwd("C:/Users/h_air/Documents/Git Proyecto CE/ProyectoCE/Insumos/Modelos")


#data_master <- read.csv( "data_master_1.csv")

# Create initial time series object
#sp500 <- ts(data_master$sp_500, start = c(1995, 1), frequency = 12)

# Create test set
#sp500_test <- window(sp500, 2015, c(2015, 12))


dif_reserva <- readRDS("dif_reserva.rds")
#saveRDS(dif_reserva, file = "dif_reserva.rds")
difSt_precios <- readRDS("difSt_precios.rds")
#saveRDS(difSt_precios, file = "difSt_precios.rds")

reserva.train <- window(dif_reserva, end =c(2016,12), frequency = 12)
reserva.test <- window(dif_reserva, 2017, c(2017, 12))

precios.train <- window(difSt_precios, end =c(2016,12), frequency = 12)
precios.test <- window(difSt_precios, 2017, c(2017, 12))

# Reservas
####################################################################
modelo_reserva <- auto.arima(reserva.train) # 0,1,5



modelo_reserva2 <- auto.arima(reserva.train %>% diff) # 0,0,5

# Precios
##########################################
modelo_precios <- auto.arima(precios.train) # 1,0,0


# MIOS
##########################################
modelo_reserva_1 <- arima(reserva.train, c(4,0,2)) 
modelo_reserva_2 <- arima(reserva.train %>% diff, c(0,0,4)) 


#saveRDS(modelo_reserva, file = "autoarima_reserva.rds")
#saveRDS(modelo_reserva2, file = "autoarima_2diff_reserva.rds")
#saveRDS(modelo_reserva_1, file = "model_1_reserva.rds")
#saveRDS(modelo_reserva_2, file = "model_2_reserva.rds")
#saveRDS(modelo_precios, file = "model_precios.rds")


reserva1 <-  readRDS("autoarima_reserva.rds")
reserva1 <- forecast::forecast(reserva1,12)

reserva2 <-  readRDS("autoarima_2diff_reserva.rds")
reserva2 <- forecast::forecast(reserva2,12)

reserva3 <-  readRDS("model_1_reserva.rds")
reserva3 <- forecast::forecast(reserva3,12)

reserva4 <-  readRDS("model_2_reserva.rds")
reserva4 <- forecast::forecast(reserva4,12)

precios1 <- readRDS("model_precios.rds")
precios1 <- forecast::forecast(precios1,12)





## app.R ##
# #00868B
ui <- dashboardPage(
  dashboardHeader(title = "Proyecto Cómputo Estadístico",
                  titleWidth = 450),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML('
    /* logo */
        .skin-blue .main-header .logo {
                              background-color: #318ce7;
                              font-family: Courier;
                              }
    /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #318ce7;
                              }'))),
    fluidRow(
      column(width = 8,
             box(plotlyOutput("forecast_plots"),
                 width = NULL),
             box(plotOutput("diag_plots"),
                 width = NULL)),
      column(width = 4,
             box(selectInput("forecast", "Choose Forecast Method:",
                             c("Reservas ARIMA(0,1,5)" = "reserva1",
                               "Reservas 2da Diff ARIMA(0,0,5)" = "reserva2",
                               "Reservas 1ra Diff ARIMA(4,0,2)" = "reserva3",
                               "Reservas 2da Diff ARIMA(0,0,4)" = "reserva4",
                               "Precios AR(1)" = "precios1")),
                 width=NULL),
             box(DT::dataTableOutput("accuracy_table"),
                 width=NULL),
             box(verbatimTextOutput('test_print'),
                 width=NULL))
    )
  )
)

server <- function(input, output) {
  output$forecast_plots <- renderPlotly({
    if (input$forecast == "reserva1") {
      autoplot(reserva1,
               holdout = reserva.test,
               forc_name = '',
               ts_object_name = 'Reserva de Petr?leo Internacional')
    } else if (input$forecast == "reserva2") {
      autoplot(reserva2,
               holdout = reserva.test,
               forc_name = '',
               ts_object_name = 'Reserva de Petr?leo Internacional')
    } else if (input$forecast == "reserva3") {
      autoplot(reserva3,
               holdout=reserva.test,
               forc_name = "",
               ts_object_name = 'Reserva de Petr?leo Internacional')
    } else if (input$forecast == "reserva4") {
      autoplot(reserva4,
               holdout = reserva.test,
               forc_name = "",
               ts_object_name = 'Reserva de Petr?leo Internacional')
    } else if (input$forecast == "precios1") {
      autoplot(precios1,
               holdout = precios.test,
               forc_name =  "",
               ts_object_name = 'Precios del Petr?leo de Venezuela')
    } 
  })
  output$diag_plots <- renderPlot({
    if (input$forecast == "reserva1") {
      ggtsdiag_custom(reserva1$model, ts_object_name = '') +
        theme(panel.background = element_rect(fill = "gray98"),
              panel.grid.minor = element_blank(),
              axis.line.y = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray"))
    } else if (input$forecast == "reserva2") {
      ggtsdiag_custom(reserva2$model, ts_object_name =  '') +
        theme(panel.background = element_rect(fill = "gray98"),
              panel.grid.minor = element_blank(),
              axis.line.y = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray"))
    } else if (input$forecast == "reserva3") {
      ggtsdiag_custom(reserva3$model, ts_object_name = "") +
        theme(panel.background = element_rect(fill = "gray98"),
              panel.grid.minor = element_blank(),
              axis.line.y = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray"))
    } else if (input$forecast == "reserva4") {
      ggtsdiag_custom(reserva4$model, ts_object_name =  "") +
        theme(panel.background = element_rect(fill = "gray98"),
              panel.grid.minor = element_blank(),
              axis.line.y = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray"))
    } else if (input$forecast == "precios1") {
      ggtsdiag_custom(precios1$model, ts_object_name = "") +
        theme(panel.background = element_rect(fill = "gray98"),
              panel.grid.minor = element_blank(),
              axis.line.y = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray"))
    } 
  })
  output$accuracy_table = DT::renderDataTable({
    if (input$forecast == "reserva1") {
      acc_arima <- round(accuracy(reserva1, reserva.test), 4)
      t(acc_arima)
    } else if (input$forecast == "reserva2") {
      acc_box <- round(accuracy(reserva2, reserva.test), 4)
      t(acc_box)
    } else if (input$forecast == "reserva3") {
      acc_ets <- round(accuracy(reserva3, reserva.test), 4)
      t(acc_ets)
    } else if (input$forecast == "reserva4") {
      acc_meanf <- round(accuracy(reserva4, reserva.test), 4)
      t(acc_meanf)
    } else if (input$forecast == "precios1") {
      acc_naive <- round(accuracy(precios1, precios.test), 4)
      t(acc_naive)
    } 
  })
  output$test_print = renderPrint({
    if (input$forecast == "reserva1") {
      reserva1$model
    } else if (input$forecast == "reserva2") {
      reserva2$model
    } else if (input$forecast == "reserva3") {
      reserva3$model
    } else if (input$forecast == "reserva4") {
      reserva4$model
    } else if (input$forecast == "precios1") {
      precios1$model
    } 
  })
}

shinyApp(ui, server)


# Referencia
# https://www.inertia7.com/projects/8
# https://github.com/inertia7/timeSeries_sp500_R
# https://www.showmeshiny.com/time-series-dashboard/