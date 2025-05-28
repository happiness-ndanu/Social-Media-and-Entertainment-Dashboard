library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(janitor)
library(readr)
library(plotly)
library(shinyjs)
library(DT)


#here::i_am("app.R")
social_media_entertainment_data <- read_csv("C:/Users/user/Documents/socials/social_media_entertainment_data.csv")
print(social_media_entertainment_data)

#standardize column names
social_media_entertainment_data <- social_media_entertainment_data %>% 
  clean_names()

#missing values?
colSums(is.na(social_media_entertainment_data))
#no missing value present

#convert to right data types
str(social_media_entertainment_data)

social_media_entertainment_data <- social_media_entertainment_data %>% 
  mutate(
    # gender=as.factor(gender),
    occupation=as.factor(occupation),
    marital_status=as.factor(marital_status),
    device_type=as.factor(device_type),
    preferred_content_type=as.factor(preferred_content_type),
    primary_social_media_goal=as.factor(primary_social_media_goal),
    preferred_entertainment_platform=as.factor(preferred_entertainment_platform),
    parental_status=as.factor(parental_status),
    preferred_device_for_entertainment=as.factor(preferred_device_for_entertainment),
    data_plan_used=as.factor(data_plan_used),
    digital_well_being_awareness=as.factor(digital_well_being_awareness))

#duplicates?
social_media_entertainment_data <- social_media_entertainment_data %>% distinct()

#inconsistencies???
# Check unique values in categorical columns
unique(social_media_entertainment_data$gender)
unique(social_media_entertainment_data$marital_status)
unique(social_media_entertainment_data$occupation)
unique(social_media_entertainment_data$device_type)
unique(social_media_entertainment_data$preferred_content_type)
unique(social_media_entertainment_data$primary_social_media_goal)
unique(social_media_entertainment_data$preferred_entertainment_platform)
unique(social_media_entertainment_data$parental_status)
unique(social_media_entertainment_data$preferred_device_for_entertainment)
unique(social_media_entertainment_data$data_plan_used)
unique(social_media_entertainment_data$digital_well_being_awareness)

data <- social_media_entertainment_data

social_media_pred<- read_csv("D:/projects/socials/social_media_entertainment_data.csv")
print(social_media_pred)

#standardize column names
social_media_pred <- social_media_pred%>% 
  clean_names()

#missing values?
colSums(is.na(social_media_pred))
#no missing value present

#convert to right data types
str(social_media_pred)

social_media_pred <- social_media_pred %>% 
  mutate(
    gender=as.factor(gender),
    occupation=as.factor(occupation),
    marital_status=as.factor(marital_status),
    device_type=as.factor(device_type),
    preferred_content_type=as.factor(preferred_content_type),
    primary_social_media_goal=as.factor(primary_social_media_goal),
    preferred_entertainment_platform=as.factor(preferred_entertainment_platform),
    parental_status=as.factor(parental_status),
    preferred_device_for_entertainment=as.factor(preferred_device_for_entertainment),
    data_plan_used=as.factor(data_plan_used),
    digital_well_being_awareness=as.factor(digital_well_being_awareness))

#duplicates?
social_media_pred<- social_media_pred %>% distinct()

#inconsistencies???
# Check unique values in categorical columns
unique(social_media_pred$gender)
unique(social_media_pred$marital_status)
unique(social_media_pred$occupation)
unique(social_media_pred$device_type)
unique(social_media_pred$preferred_content_type)
unique(social_media_pred$primary_social_media_goal)
unique(social_media_pred$preferred_entertainment_platform)
unique(social_media_pred$parental_status)
unique(social_media_pred$preferred_device_for_entertainment)
unique(social_media_pred$data_plan_used)
unique(social_media_pred$digital_well_being_awareness)

summary(social_media_pred)
#-------------------------Does Social Media Usage Impact Fatigue level?
lm_fatigue <- lm(social_media_fatigue_level_scale_1_10 ~ daily_social_media_time_hrs, data = social_media_pred)
summary(lm_fatigue)
#p-value is 0.508 This value is greater than 0.05, meaning that social media time does not significantly predict fatigue.
#Coefficient for daily_social_media_time_hrs (-0.0014) → Each extra hour spent on social media slightly decreases fatigue by 0.0014 units (which is almost negligible).
#CONCLUSION:Daily social media usage does not significantly impact fatigue.


#-------------------------Does Social Media Usage Impact Isolation level?
lm_isolation <- lm(social_isolation_feeling_scale_1_10 ~ daily_social_media_time_hrs, data = social_media_pred)
summary(lm_isolation)
#p-value is 0.834 -Not significant (greater than 0.05), so social media time does not significantly predict isolation.
#Coefficient for daily_social_media_time_hrs (0.00045) → A tiny positive effect, meaning that more social media usage very slightly increases isolation, but this effect is nearly zero.
#CONCLUSION:Daily social media usage does not significantly impact social isolation.


#Does Occupation Affect Social Media Usage
anova_test <- aov(daily_social_media_time_hrs ~ occupation, data = social_media_pred)
summary(anova_test)
TukeyHSD(anova_test)
# p-value (0.253) → The p-value is greater than 0.05, meaning there is no significant difference in social media usage across different occupations.
#None of the differences are statistically significant (all p-values > 0.05).
#Occupation does not significantly impact how much time people spend on social media.


#What Predicts Digital Well-Being Awareness
log_model <- glm(digital_well_being_awareness ~ daily_social_media_time_hrs + gender + occupation + age, 
                 data = social_media_pred, family = "binomial")
summary(log_model)

#Daily Social Media Time (p value = 0.567) → No significant effect.

#Gender (p-values > 0.05) → Gender does not significantly impact awareness.

#Occupation (p-values > 0.05) → No significant effect.

#Age (p = 0.966) → Age does not impact awareness.

#CONCLUSION: None of the predictors (social media time, gender, occupation, age) significantly affect digital well-being awareness

#Final Summary
#✅ No evidence that daily social media usage significantly affects fatigue or isolation.
#✅ No significant difference in social media usage across different occupations.
#✅ No strong predictors of digital well-being awareness (age, gender, occupation, and social media time do not play a major role).

#It seems like social media usage is not a major factor in these psychological variables—or at least, the dataset does not show any strong relationships.


#----------------------------------------

model_age <- lm(social_media_fatigue_level_scale_1_10 ~ daily_social_media_time_hrs * age, data = social_media_pred)
summary(model_age)
#p-value = 0.324 (not significant)
#Age does not significantly moderate the relationship between social media time and fatigue. In other words, the effect of social media on fatigue is not different across age groups.
model_gender <- lm(social_media_fatigue_level_scale_1_10 ~ daily_social_media_time_hrs * gender, data = social_media_pred)
summary(model_gender)
#Gender (Male): Estimate = -0.0021, p-value = 0.694
#Gender (Other): Estimate = -0.00105, p-value = 0.843
#Gender does not significantly moderate the relationship between social media time and fatigue. The effect of social media on fatigue is similar across different genders.

#------------------------------------------------------THE APP-----------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = div("Social Media & Entertainment Dashboard", style = "width:100%;text-align:center;"),
    titleWidth = "100%"
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    tags$style(HTML("
  /* General body font style */
  body {
    font-family: 'Arial', sans-serif;
    font-size: 17px;
    color: #333;
    background-color: #f4f4f9;
  }
  
  /* Header Styling */
  .main-header .logo {
    font-size: 24px;
    font-weight: bold;
    color: #2c3e50;
  }

  .main-header .navbar {
    background-color: #3498db;
  }

  .main-header .navbar .navbar-nav > li > a {
    color: white !important;
    font-size: 16px;
  }

  /* Title Text in Header */
  #loadingMessage, #completeMessage {
    font-size: 18px;
    font-weight: bold;
    padding-top: 10px;
  }
  
  #loadingMessage {
    color: #e74c3c;
  }

  #completeMessage {
    color: #27ae60;
  }

  /* Styling for value boxes */
  .box {
    font-size: 18px;
    font-weight: bold;
    text-align: center;
  }

  .value-box {
    background-color: #ecf0f1;
    color: #2980b9;
    font-size: 22px;
  }
  
  /* Styling for the tabs and content */
  .tabBox .tabPanel {
    font-family: 'Arial', sans-serif;
    font-size: 14px;
  }
  
  .row {
    margin-bottom: 20px;
  }
  
  .container-fluid {
    padding: 20px;
  }
  
  /* Interactive elements (input, slider, selectInput) */
  input[type='text'], input[type='number'], select, sliderInput {
    font-size: 16px;
    padding: 8px;
    border-radius: 5px;
    border: 1px solid #ccc;
  }

  /* Button Styling */
  .btn {
    font-size: 16px;
    font-weight: bold;
    padding: 12px 20px;
    background-color: #3498db;
    color: white;
    border-radius: 5px;
    border: none;
  }
  .btn:hover {
    background-color: #2980b9;
    cursor: pointer;
  }

  /* Customizing plot titles and axes labels */
  .plotly .title {
    font-size: 18px;
    font-weight: bold;
    color: #34495e;
  }
  
  .plotly .xaxis, .plotly .yaxis {
    tickfont = list(size = 14, color = '#34495e');
  }
  
  /* Customizing About Section */
  #summaryText p, #summaryText ul {
    font-size: 16px;
    font-family: 'Arial', sans-serif;
    color: #2c3e50;
  }

  #summaryText ul {
    margin-left: 20px;
  }
")),
    
    
    tabBox(
      title = "", width = 12,
      tabPanel("Overview",icon = icon("list-alt"),
               
               fluidRow(
                 valueBoxOutput("avgUsage", width = 4),
                 valueBoxOutput("avgFatigue", width = 4),
                 valueBoxOutput("avgIsolation", width = 4)
               ),
               fluidRow(
                 column(6, selectInput("country", "Select Country:", choices = c("All", unique(social_media_entertainment_data$country)), selected = "All", multiple = TRUE)),
                 column(6, selectInput("gender", "Select Gender:", choices = c("All", unique(social_media_entertainment_data$gender)), selected = "All"))
               ),
               fluidRow(
                 column(12, sliderInput("usage_filter", "Filter by Social Media Hours:", min = 0, max = 24, value = c(1, 10)))
               ),
               fluidRow(
                 column(6, plotlyOutput("ageDist")),
                 column(6, plotlyOutput("usageDist"))
               ),
               fluidRow(
                 column(6, plotlyOutput("fatigueLevel")),
                 column(6, plotlyOutput("isolationFeeling"))
               ),
               fluidRow(
                 column(6, plotlyOutput("platformTrend")),
                 column(6, plotlyOutput("contentType"))
               ),
               fluidRow(
                 column(6, plotlyOutput("socialGoal")),
                 column(6, plotlyOutput("entertainmentPref"))
               ),
               fluidRow(
                 column(12, DTOutput("summaryTable"))
               )
      ),
      
      tabPanel("Prediction Model", icon = icon("line-chart"),
               fluidRow(
                 column(6,
                        selectInput("pred_gender", "Select Gender for Prediction:", choices = c("Male", "Female", "Other")),
                        numericInput("pred_age", "Enter Age:", value = 30, min = 10, max = 100),
                        sliderInput("pred_usage", "Enter Social Media Time (hrs):", min = 0, max = 24, value = 5),
                        actionButton("updatePred", "Update Prediction")                 
                 ),
                 column(6,
                        textOutput("predictionResult"),
                        plotOutput("predictionPlot"),
                        div(id = "loadingMessage"),
                        div(id = "completeMessage")
                 )
               ),
               # Add summary of what's happening on the page
               fluidRow(
                 column(12,
                        div(id = "summaryText",
                            p("This page allows you to predict the level of social media fatigue based on three inputs:"),
                            tags$ul(
                              tags$li("Select the gender of the individual."),
                              tags$li("Enter the age of the individual."),
                              tags$li("Specify the amount of time (in hours) spent on social media daily.")
                            ),
                            p("Once you've entered your inputs, click 'Update Prediction'. The model will process the data and provide a predicted social media fatigue level, along with a plot showing how fatigue level changes with social media time."),
                            p("A loading message will appear while the prediction is being processed. After completion, the result will be displayed, and the plot will update.")
                        )
                 )
               )
      ),
      
      
      tabPanel("About", icon = icon("info-circle"),
               tags$head(
                 tags$style(HTML("
    #about-section {
      position: relative;
      background-image: url('tree.png');
      background-size: cover;
      background-position: center;
      background-repeat: no-repeat;
      min-height: 100vh;
      padding: 40px;
      font-family: 'Segoe UI', sans-serif;
      font-size: 17px;
      line-height: 1.6;
    }

    #about-section::before {
      content: '';
      position: absolute;
      top: 0; left: 0; right: 0; bottom: 0;
      background-color: rgba(255, 255, 255, 0.90); /* Higher opacity = lighter overlay */
      z-index: 1;
    }

    #about-section > * {
      position: relative;
      z-index: 2;
    }

    #about-section h4,
    #about-section p,
    #about-section li {
      color: #111111;
    }

    #about-section h4 {
      margin-top: 25px;
      font-size: 20px;
      font-weight: bold;
    }

    #about-section ul {
      padding-left: 20px;
    }

    #about-section li {
      margin-bottom: 8px;
    }
  "))
               ),
               
               
               
               # Wrap everything inside this div
               div(id = "about-section",
                   p("Welcome to the Social Media & Entertainment Dashboard. This project began as a vision to create an intuitive platform that offers deep insights into user behavior across various social media and entertainment platforms."),
                   br(),
                   h4("Key Features"),
                   tags$ul(
                     tags$li("User Behavior Insights: Analyze patterns and trends in social media usage."),
                     tags$li("Predictive Modeling: Estimate potential outcomes based on user-defined parameters."),
                     tags$li("Interactive Visualizations: Engage with data through dynamic charts and graphs.")
                   ),
                   br(),
                   h4("Technologies Used"),
                   p("This dashboard leverages the power of R Shiny, incorporating packages such as shinydashboard for the interface, plotly for interactive plots, and DT for dynamic tables."),
                   br(),
                   h4("Disclaimer"),
                   p("The insights provided by this dashboard are for informational purposes only and should be interpreted within the appropriate context.")
               )
      )
      
    )     
  )
  
)




# Server Section
server <- function(input, output) {
  
  
  
  
  filteredData <- reactiveVal()
  
  observe({
    filteredData(social_media_entertainment_data %>%
                   filter(
                     ("All" %in% input$country | country %in% input$country),
                     (input$gender == "All" | gender == input$gender),
                     daily_social_media_time_hrs >= input$usage_filter[1] & daily_social_media_time_hrs <= input$usage_filter[2]
                   ))
  })
  
  
  # Render filtered data table
  output$summaryTable <- renderDT({
    datatable(filteredData() %>% select(user_id, age, gender, primary_platform, daily_social_media_time_hrs),
              options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE, searchable = TRUE, order = list(list(1, 'asc'))))
  })
  
  # Render value boxes
  output$avgUsage <- renderValueBox({
    valueBox(round(mean(filteredData()$daily_social_media_time_hrs, na.rm = TRUE), 1), "Avg Social Media Hours", icon = icon("clock"), color = "maroon")
  })
  
  output$avgFatigue <- renderValueBox({
    valueBox(round(mean(filteredData()$social_media_fatigue_level_scale_1_10, na.rm = TRUE), 1), "Avg Fatigue Level", icon = icon("tired"), color = "yellow")
  })
  
  output$avgIsolation <- renderValueBox({
    valueBox(round(mean(filteredData()$social_isolation_feeling_scale_1_10, na.rm = TRUE), 1), "Avg Isolation Level", icon = icon("user"), color = "purple")
  })
  
  # Render plots for user behavior insights
  output$ageDist <- renderPlotly({
    gender_colors <- c("Female" = "#E91E63", "Male" = "#2196F3", "Other" = "#9E9E9E")
    p <- ggplot(filteredData(), aes(x = age, fill = gender)) +
      geom_histogram(binwidth = 5, position = "dodge") +
      labs(title = "Age Distribution by Gender", x = "Age", y = "Count") +
      scale_fill_manual(values = gender_colors)
    ggplotly(p)
  })
  
  output$usageDist <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = daily_social_media_time_hrs)) +
      geom_histogram(binwidth = 1, fill = "#4CAF50", alpha = 0.7) +
      labs(title = "Daily Social Media Usage", x = "Hours", y = "Count")
    ggplotly(p)
  })
  
  output$fatigueLevel <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = social_media_fatigue_level_scale_1_10)) +
      geom_bar(fill = "#FF5722") +
      labs(title = "Social Media Fatigue Levels", x = "Fatigue Level (1-10)", y = "Count")
    ggplotly(p)
  })
  
  output$isolationFeeling <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = social_isolation_feeling_scale_1_10)) +
      geom_bar(fill = "#9C27B0") +
      labs(title = "Social Isolation Feeling Levels", x = "Isolation Level (1-10)", y = "Count")
    ggplotly(p)
  })
  
  output$platformTrend <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = primary_platform)) +
      geom_bar(fill = "#8BC34A") +
      labs(title = "Most Used Social Media Platforms", x = "Platform", y = "Count")
    ggplotly(p)
  })
  
  output$contentType <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = preferred_content_type)) +
      geom_bar(fill = "#FF9800") +
      labs(title = "Preferred Content Types", x = "Content Type", y = "Count")
    ggplotly(p)
  })
  
  output$socialGoal <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = primary_social_media_goal)) +
      geom_bar(fill = "#00BCD4") +
      labs(title = "Primary Social Media Goals", x = "Goal", y = "Count")
    ggplotly(p)
  })
  
  output$entertainmentPref <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = preferred_entertainment_platform)) +
      geom_bar(fill = "#FFC107") +
      labs(title = "Preferred Entertainment Platforms", x = "Platform", y = "Count")
    ggplotly(p)
  })
  
  #Prediction Model logic with dynamic interaction
  prediction_model <- eventReactive(input$updatePred, {
    
    # Show "Model loading..." message
    shinyjs::html("loadingMessage", "Model loading...")
    shinyjs::hide("completeMessage")  # Hide the complete message initially
    
    age <- input$pred_age
    gender <- input$pred_gender
    time <- input$pred_usage
    
    new_data <- data.frame(age = age, daily_social_media_time_hrs = time, gender = gender)
    # # Simulate model prediction with a delay
    # Sys.sleep(1)  # Simulating delay for model prediction
    
    # Add progress bar
    withProgress(message = "Running prediction", value = 0, {
      n <- 5  # Number of steps
      for (i in 1:n) {
        Sys.sleep(0.2)  # Simulate processing delay
        incProgress(1/n, detail = paste("Step", i, "of", n))
      }
    })
    # Simulate model prediction
    Sys.sleep(1)  # Simulating delay for model prediction
    
    # Assuming 'model_gender' is the pre-trained prediction model
    pred_value <- predict(model_gender, newdata = new_data)
    
    shinyjs::html("loadingMessage", "")  # Remove the loading message
    shinyjs::html("completeMessage", "Complete!")  # Show the complete message
    
    return(pred_value)
  })
  
  
  output$predictionPlot <- renderPlot({
    # Extract the prediction values
    pred_values <- prediction_model()
    
    # Create a plot with enhancements
    plot(input$pred_usage, pred_values, type = "b", col = "blue", pch = 19, lwd = 2, 
         xlab = "Social Media Time (hrs)", ylab = "Predicted Fatigue Level",
         main = "Predicted Social Media Fatigue vs Time", cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.1)
    
    # Add gridlines for better readability
    grid(col = "gray", lty = "dotted")
    
    # Add a custom legend
    legend("topright", legend = "Fatigue Prediction", col = "blue", pch = 19, lwd = 2, bty = "n")
  })
  
  # Display prediction result
  output$predictionResult <- renderText({
    paste("The predicted social media fatigue level is:", round(prediction_model(), 2))
  })
  
  
  
}
# Run the Shiny app
shinyApp(ui = ui, server = server)

