library(shiny)
library(ggplot2)

data <- read.csv(file = "BodyFat_clean.csv", header = T)
model <- lm(BODYFAT~WEIGHT+ABDOMEN+WRIST, data=data)
data_density <- data.frame(density(data$BODYFAT)[c("x", "y")])

server <- function(input, output, session) {
  
  ## Error message
  bodyfatErrorTemp <- eventReactive(input$calculate_botton, {
    if (! (is.numeric(input$weight_value) & input$weight_value>0)){
      return("Input format of Weight is incorrect. Must be a positive number.")
    }else if (! (is.numeric(input$abdomen_value) & input$abdomen_value>0)){
      return("Input format of Abdomen is incorrect. Must be a positive number.")
    }else if (! (is.numeric(input$wrist_value) & input$wrist_value>0)){
      return("Input format of Wrist is incorrect. Must be a positive number.")
    }else{
      return(NULL)
    }
  })
  output$bodyfatError1 <- renderText({bodyfatErrorTemp()})
  output$bodyfatError2 <- renderText({bodyfatErrorTemp()})
  
  ## input judge: positive number
  input_posnum <- eventReactive(input$calculate_botton, {
    return(is.numeric(input$weight_value) & input$weight_value>0 &
             is.numeric(input$abdomen_value) & input$abdomen_value>0 &
             is.numeric(input$wrist_value) & input$wrist_value>0)
  })
  
  ## change unit
  weight_lbs <- eventReactive(input$calculate_botton, {
    if (input_posnum()){
      weight_adjusted<-input$weight_value * ifelse(input$weight_unit=="lbs", 1, 2.20462262)
      return(weight_adjusted)
    }else{return(0)}
  })
  
  abdomen_cm <- eventReactive(input$calculate_botton, {
    if (input_posnum()){
      abdomen_adjusted<-input$abdomen_value * ifelse(input$abdomen_unit=="cm", 1, 2.54)
      return(abdomen_adjusted)
    }else{return(0)}
  })
  
  wrist_cm <- eventReactive(input$calculate_botton, {
    if (input_posnum()){
      wrist_adjusted<-input$wrist_value * ifelse(input$wrist_unit=="cm", 1, 2.54)
      return(wrist_adjusted)
    }else{return(0)}
  })
  
  ## predict body fat
  bodyfat <- eventReactive(input$calculate_botton, {
      return(predict(model, data.frame(WEIGHT=weight_lbs(), ABDOMEN=abdomen_cm(), WRIST=wrist_cm())))
  })
  
  
  ## output judge: in range
  output_inrange <- eventReactive(input$calculate_botton, {
    return(input_posnum() & bodyfat() >= 0 & bodyfat() <= 100)
  })
  
  ## output judge: in correct range
  output_correct <- eventReactive(input$calculate_botton, {
    return(input_posnum() & bodyfat() >= 2 & bodyfat() <= 50)
  })
  
  ## body fat percentage
  output$bodyfatPercentage <- renderText({
    if (input_posnum()){
      return(paste0(format(round(bodyfat(), digits = 2), nsmall = 2), "%"))
    }else{return(NULL)}
  })
  
  ## Warning message
  bodyfatWarningTemp <- eventReactive(input$calculate_botton, {
    if (input_posnum()){
      if (! (weight_lbs()>=80  & weight_lbs()<=400)){
        return("<br/>Input Weight is not in normal range (80~400 lbs / 36.29~181.42 kg).<br/>Please check it!")
      }else if (! (abdomen_cm()>=40  & abdomen_cm()<=200)){
        return("<br/>Input Abdomen Circumference is not in normal range (40~200 cm / 15.75~78.74 inches).<br/>Please check it!")
      }else if (! (wrist_cm()>=8  & wrist_cm()<=40)){
        return("<br/>Input Wrist Circumference is not in normal range (8~40 cm / 3.15~15.74 inches).<br/>Please check it!")
      }else if (! output_inrange()){
        return("The prediction is out of range (0%~100%). Please check your input!")
      }else if (! output_correct()){
        return("The prediction is irrational. Normal range is 2%~50%. Please check your input!")
      }else{
        return(NULL)
      }
    }else{return(NULL)}
  })
  output$bodyfatWarning1 <- renderText({bodyfatWarningTemp()})
  output$bodyfatWarning2 <- renderText({bodyfatWarningTemp()})
  
  ## input judge: in correct range
  input_correct <- eventReactive(input$calculate_botton, {
      return(input_posnum() &
               is.numeric(weight_lbs()) & weight_lbs()>=80  & weight_lbs()<=400 &
               is.numeric(abdomen_cm()) & abdomen_cm()>=40 & abdomen_cm()<=200 &
               is.numeric(wrist_cm()) & wrist_cm()>=8  & wrist_cm()<=40)
  })
  
  ## text: Body Fat Analysis: 
  output$bodyfatText1 <- renderText({
    if (input_correct() & output_correct()){
      return("Body Fat Analysis: ")
    }else{return(NULL)}
  })
  
  ## body fat percentile
  output$bodyfatPercentile <- renderText({
    if (input_correct() & output_correct()){
      bodyfatPercentileResult<-paste0(round(100*sum(bodyfat()>data$BODYFAT)/nrow(data)), "%")
      return(paste0("Your body fat is higher than  ", bodyfatPercentileResult, " of adult men in our data set."))
    }else{return(NULL)}
  })
  
  ## body fat prediction interval
  output$bodyfatInterval <- renderText({
    if (input_correct() & output_correct()){
      bodyfatPredict<-predict(model, data.frame(WEIGHT=weight_lbs(), ABDOMEN=abdomen_cm(), WRIST=wrist_cm()), interval="prediction", level=0.95)
      bodyfatIntervalResult<-paste0("[ ", format(round(bodyfatPredict[2], digits = 2), nsmall = 2), "% , ", format(round(bodyfatPredict[3], digits = 2), nsmall = 2), "% ]")
      return(paste0("Your 95% body fat prediction interval is: ", bodyfatIntervalResult))
    }else{return(NULL)}
  })
  
  ## text: According to Body Fat Norms given by ACE, Your are classified as: 
  output$bodyfatText2 <- renderText({
    if (input_correct() & output_correct()){
      return("According to Body Fat Norms given by ACE, Your are classified as: ")
    }else{return(NULL)}
  })
  
  ## body fat category
  output$bodyfatCategory <- renderText({
    if (input_correct() & output_correct()){
      if (bodyfat()>= 2 & bodyfat() < 6){
        return("Essential fat")
      }else if(bodyfat()>= 6 & bodyfat() < 14){
        return("Athletes")
      }else if(bodyfat()>= 14 & bodyfat() < 18){
        return("Fitness")
      }else if(bodyfat()>= 18 & bodyfat() < 25){
        return("Average")
      }else if(bodyfat()>= 25 & bodyfat() <= 50){
        return("Obese")
      }else{
        return("Unavailable")
      }
    }else{return(NULL)}
  })
  
  ## text: See plot in next tab!
  output$bodyfatText3 <- renderText({
    if (input_correct() & output_correct()){
      return("See plot in next tab!")
    }else{return(NULL)}
  })
  
  ## text: (You may need to wait 1 second for plot loading...)
  output$bodyfatText4 <- renderText({
    if (input_correct() & output_correct()){
      return("(You may need to wait 1 second for plot loading...)")
    }else{return(NULL)}
  })
  
  ## Density plot
  output$densityPlot <- renderPlot({
    if (input_correct() & output_correct()){
      g <- ggplot(data,aes(x = BODYFAT))+
        scale_x_continuous(limits = c(0, 50))+
        geom_histogram(aes(y=..density..), color="#88ada6", alpha=.25, fill="#fffbf0", binwidth = 2, center=1)+
        geom_density()+
        geom_area(data = subset(data_density, x >= 2 & x < 6), aes(x, y, fill = "Essential fat"), alpha=.4)+
        geom_area(data = subset(data_density, x >= 6 & x < 14), aes(x, y, fill = "Athletes"), show.legend = FALSE)+
        geom_area(data = subset(data_density, x >= 14 & x < 18), aes(x, y, fill = "Fitness"), show.legend = FALSE)+
        geom_area(data = subset(data_density, x >= 18 & x < 25), aes(x, y, fill = "Average"), show.legend = FALSE)+
        geom_area(data = subset(data_density, x >= 25 & x <= 50), aes(x, y, fill = "Obese"), show.legend = FALSE)+
        scale_fill_manual("Categories", 
                          breaks = c("Essential fat", "Athletes", "Fitness", "Average", "Obese"), 
                          values = c("Essential fat"="#4b5cc466", "Athletes"="#16a95166", "Fitness"="#ffb61e66", "Average"="#ff750066", "Obese"="#c3272b66"))+
        xlab("Body Fat %")+ 
        labs(title="Body Fat Density Plot and Your Position",
             subtitle="The blue vertical dash line shows your position",
             caption = "Data source: See below")+
        geom_vline(xintercept = bodyfat(),linetype = "twodash",color="#0000cd",size = 1)+
        annotate("rect", xmin = bodyfat()+0.5, xmax = bodyfat()+7.5, ymin = 0.057, ymax = 0.063, alpha = .5, fill="white")+
        annotate("text", fontface = "bold", color="#0000cd",
                 x = bodyfat()+4, y=0.06,
                 label = as.character(paste0(format(round(bodyfat(), digits = 2), nsmall = 2),"%")), size=6)+
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5),
              plot.caption = element_text(size = 12, face = "italic"),
              axis.text = element_text(size=12),
              axis.title = element_text(size=14, face="bold"))
      return(g)
    }else{return(NULL)}
  })
  

  ## URL: ACE
  categories_url <- a("body fat categories", href="https://www.acefitness.org/education-and-resources/lifestyle/tools-calculators/percent-body-fat-calculator/")
  output$categoriesUrl1 <- renderUI({tagList("The ", categories_url, " are given by American Council on Exercise (ACE).")})
  output$categoriesUrl2 <- renderUI({tagList("The ", categories_url, " are given by American Council on Exercise (ACE).")})
  
  ## URL: Data
  data_url <- a("here", href="https://github.com/zfeng66/STAT628Module2/blob/master/data/BodyFat_clean.csv")
  output$dataUrl1 <- renderUI({tagList("The cleaned data set to train our model is avalible ", data_url, ".")})
  output$dataUrl2 <- renderUI({tagList("The cleaned data set to train our model is avalible ", data_url, ".")})
}
