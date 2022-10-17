library(tidyverse)
library(ggplot2)
library(shiny)

bodyfat_model<-function(weight,waist,wrist){
  round(-23.859-0.187*weight+0.874*waist-1.257*wrist,1)
}
#Define UI
ui<-fluidPage(
  #Application title
  titlePanel("Body Fat Calculator"),
  #Sidebar with numeric input
  sidebarPanel(
    shinyjs::useShinyjs(),
    id="side-panel",
    numericInput("weight","Weight(kg)",0,min=0),
    numericInput("waist","Abdomen Circumference(cm)",0,min = 0),
    numericInput("wrist","Wrist Circumference(cm)",0,min = 0),
    actionButton("submit","Calculate"),
    actionButton("reset","Clear")
  ),
  #Show the calculation result
  mainPanel(
    h3(textOutput("bodyfat"))
  )
)
server<-function(input,output){
  output$bodyfat<-renderText({
    validate(
      need(input$weight >=0,"Weight can not be negative!"),
      need(input$waist >=0 ,"Abdomen circumference can not be negative!"),
      need(input$wrist>=0,"Wrist circumference can not be negative !"))
    #bodyfat_model(input$weight,input$height,input$age,input$waist_circumference)
    if(input$submit == 0) ""
    else 
      paste0("Your body fat percentage is: ",bodyfat_model(input$weight,input$waist,input$wrist),"%")
  })
  observeEvent(
    input$reset,
    {shinyjs::reset("side-panel")
    })
}
shinyApp(ui,server)
