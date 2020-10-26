library(shiny)
library(ggplot2)

data <- read.csv(file = "BodyFat_clean.csv", header = T)
model <- lm(BODYFAT~WEIGHT+ABDOMEN+WRIST, data=data)
data_density <- data.frame(density(data$BODYFAT)[c("x", "y")])


ui <- fluidPage(
  titlePanel(h1(id = "title","Body Fat Calculator (For Adult Men)",align = "center")),
  tags$style(HTML("#title{font-size: 50px;font-family: Georgia;}")),
  hr(),
  fluidRow(column(4, h2("Input")), column(4, h2("Output"))),
  sidebarLayout(
    sidebarPanel(
      p("Please provide the following information and then click Calculate Button:"),
      
      h4("Weight"),
      
      helpText("Normal range:", br(), "80 ~ 400 lbs / 36.29 ~ 181.42 kg"),
      
      fluidRow(
        column(7,style=list("padding-right: 5px;"),
               numericInput("weight_value", label="Value", value=154, step=0.01)
        ),
        column(5,style=list("padding-left: 5px;"),
               selectInput("weight_unit",label="Unit",
                           choices=c("lbs","kg"), selected="lbs", multiple=F)
        )
      ),
      
      h4("Abdomen Circumference"),
      
      helpText("Normal range:", br(), "40 ~ 200 cm / 15.75 ~ 78.74 inches"),
      
      # the abdomen 2 circumference is measured "laterally, at the level of the iliac crests, and anteriorly, at the umbilicus." 
      fluidRow(
        column(7,style=list("padding-right: 5px;"),
               numericInput("abdomen_value",label="Value", value=85, step=0.01)
        ),
        column(5,style=list("padding-left: 5px;"),
               selectInput("abdomen_unit",label="Unit",
                           choices=c("cm","inches"), selected="cm", multiple=F)
        )
      ),
      
      h4("Wrist Circumference"),
      
      helpText("Normal range:", br(), "8 ~ 40 cm / 3.15 ~ 15.74 inches"),
      
      fluidRow(
        column(7,style=list("padding-right: 5px;"),
               numericInput("wrist_value",label="Value", value=17, step=0.01)
        ),
        column(5,style=list("padding-left: 5px;"),
               selectInput("wrist_unit",label="Unit",
                           choices=c("cm","inches"),selected="cm",multiple=F)
        )
      ),
      
      actionButton("calculate_botton", "Calculate", style = "color: white; background-color: #4040ff" ),
      width=4
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Prediction",
          helpText("Note: If you see any orange message, it means the result is unreliable. Please check your input!"),
          h3("Body Fat Percentage: "),
          tags$style("#bodyfatError1 {font-size:18px; font-weight: bold; color:#ff7f50; }"),
          textOutput(outputId = "bodyfatError1"),
          tags$style("#bodyfatPercentage {font-size:25px; font-weight: bold; color:#0000cd; }"),
          fluidRow(align = "center", textOutput(outputId = "bodyfatPercentage")),
          tags$style("#bodyfatWarning1 {font-size:18px; font-weight: bold; color:#ff7f50; }"),
          htmlOutput(outputId = "bodyfatWarning1"),
          br(),

          #h3("Body Fat Analysis: "),
          h3(textOutput(outputId = "bodyfatText1")),
          tags$style("#bodyfatPercentile {font-size:18px; }"),
          textOutput(outputId = "bodyfatPercentile"),
          tags$style("#bodyfatInterval {font-size:18px; }"),
          textOutput(outputId = "bodyfatInterval"),
          tags$style("#bodyfatText2 {font-size:18px; }"),
          textOutput(outputId = "bodyfatText2"),
          br(),
          tags$style("#bodyfatCategory {font-size:25px; font-weight: bold; color:#9932cc; }"),
          fluidRow(align = "center", textOutput(outputId = "bodyfatCategory")),
          br(),
          h3(textOutput(outputId = "bodyfatText3")),
          tags$style("#bodyfatText4 {font-size:16px; }"),
          textOutput(outputId = "bodyfatText4"),
          br(),
          br(),
          tags$style("#categoriesUrl1 {color:#808080; }"),
          uiOutput("categoriesUrl1"),
          tags$style("#dataUrl1 {color:#808080; }"),
          uiOutput("dataUrl1")
        ),
        tabPanel(
          "Plot",
          h3("Plot: "),
          tags$style("#bodyfatError2 {font-size:18px; font-weight: bold; color:#ff7f50; }"),
          textOutput(outputId = "bodyfatError2"),
          tags$style("#bodyfatWarning2 {font-size:18px; font-weight: bold; color:#ff7f50; }"),
          htmlOutput(outputId = "bodyfatWarning2"),
          plotOutput(outputId = "densityPlot"),
          br(),
          tags$style("#categoriesUrl2 {color:#808080; }"),
          uiOutput("categoriesUrl2"),
          tags$style("#dataUrl2 {color:#808080; }"),
          uiOutput("dataUrl2")
        ),
        
        tabPanel(
          "Contact us",
          p(br(),
            "Thanks for using this Body Fat Calculator!",
            style="font-size:20px; color:#4169e1; "
          ),
          p(br(),
            "Contact information (Email):",
            br(),
            "xwang2439@wisc.edu", 
            br(),
            "zfeng66@wisc.edu",
            style="font-size:16px; color:#808080; ")
        )
      )
    )
  )
)
