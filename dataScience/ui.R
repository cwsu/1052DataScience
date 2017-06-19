library(shiny)
shinyUI(fluidPage(
  titlePanel("105753005 Su Ching Wen final project"),
  sidebarLayout(
    sidebarPanel(
      h4("Predict what kind of person will attempt to suicide"),
      selectizeInput('siFeature', label = h3("Feature"), choices = list("time"="V1","gender"="V2","sexuallity"="V3","age"="V4","income"="V5","race"="V6","bodyweight"="V7","virgin"="V8","prostitution_legal"="V9","pay_for_sex"="V10","friends"="V11","social_fear"="V12","depressed"="V13","what_help_from_others"="V14","employment"="V16","job_title"="V17","edu_level"="V18","improve_yourself_how"="V19"), multiple = TRUE)
    ),
    mainPanel(plotOutput("distDTPlot"),plotOutput("distRFPlot"),textOutput("showResult"))
  )
))