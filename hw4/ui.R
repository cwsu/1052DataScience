library(shiny)
shinyUI(fluidPage(
  titlePanel("105753005 Su Ching Wen HW4"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("rbTarget", label = h3("Target"), choices = list("Male" = "male", "Female" = "female")),
      checkboxGroupInput("cbgMethod", label = h3("Method"), choices = list("Method 1" = "method1.csv", "Method 2" = "method2.csv", "Method 3" = "method3.csv", "Method 4" = "method4.csv", "Method 5" = "method5.csv", "Method 6" = "method6.csv", "Method 7" = "method7.csv", "Method 8" = "method8.csv", "Method 9" = "method9.csv", "Method 10" = "method10.csv"))
    ),
    mainPanel(plotOutput("distPlot"))
  )
))