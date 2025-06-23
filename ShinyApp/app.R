library(shiny)
library(ggplot2)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(dplyr)
library(viridis)
library(ggthemes)
library(DT)

# Load datasets
load("Countries.Rdata") 
load("GDP.Rdata")  
load("Top80medaltally.Rdata")
load("Gender-wise Para 2024.Rdata")

# Define UI for the application
ui <- fluidPage(
  
  # Application title
  titlePanel("PARALYMPICS 2024"),
  
  # Sidebar layout with dynamic UI content
  sidebarLayout(
    sidebarPanel(
      uiOutput("sidebarUI"),
      conditionalPanel(
        condition = "input.tabs == 'World Map and GDP Analysis'",
        tableOutput("performanceRatioTable")  
      )
    ),
    
    # Main Panel with tabset for multiple views
    mainPanel(
      tabsetPanel(
        id = "tabs", 
        tabPanel("Participation by Gender", 
                 plotOutput("Plot1"),
                 plotOutput("Plot2")
        ),
        tabPanel("World Map and GDP Analysis",
                 uiOutput("conditionalPlot")# Only show the GDP and Medal map
        ),
        tabPanel("Countries Comparison",
                 plotOutput("CountriesPlot",height = "700px"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Standardize country names for consistency
  standardize_country_name <- function(name) {
    name <- gsub(" ", "_", name) 
    if (name == "France_(host)") return("France")
    return(name)
  }
  
  GDP_data$Country <- sapply(GDP_data$Country, standardize_country_name)
  medal_table$Country <- sapply(medal_table$Country, standardize_country_name)
  
  # Calculate gender performance data (medal-to-participant ratio)
  gender_performance <- lapply(1:16, function(i) {
    country_data <- countries_dataset[[i]][[2]]
    country <- standardize_country_name(names(countries_dataset)[i])
    
    total_male_participants <- as.numeric(country_data$Men[nrow(country_data)])
    total_female_participants <- as.numeric(country_data$Women[nrow(country_data)])
    
    total_male_medals <- as.numeric(medal_table$T_Male[medal_table$Country == country])
    total_female_medals <- as.numeric(medal_table$T_Female[medal_table$Country == country])
    
    male_ratio <- ifelse(!is.na(total_male_participants) && total_male_participants > 0, 
                         total_male_medals / total_male_participants, NA)
    female_ratio <- ifelse(!is.na(total_female_participants) && total_female_participants > 0, 
                           total_female_medals / total_female_participants, NA)
    
    if (!is.na(male_ratio) && !is.na(female_ratio)) {
      performance <- ifelse(male_ratio > female_ratio, "Male", "Female")
      ratio <- max(male_ratio, female_ratio, na.rm = TRUE)
    } else if (!is.na(male_ratio)) {
      performance <- "Male"
      ratio <- male_ratio
    } else if (!is.na(female_ratio)) {
      performance <- "Female"
      ratio <- female_ratio
    } else {
      performance <- NA
      ratio <- NA
    }
    
    data.frame(Country = names(countries_dataset)[i], Performance = performance, Ratio = ratio)
  }) %>% bind_rows()
  
  # Reactive expression for selected country data
  selected_country_data <- reactive({
    countries_dataset[[input$country]][[2]]  
  })
  selected_country_medal_data <- reactive({
    countries_dataset[[input$country]][[1]] 
  })
  
  # Dynamically render sidebar UI based on active tab
  output$sidebarUI <- renderUI({
    if (input$tabs == "Participation by Gender") {
      tagList(
      selectInput(inputId = "country",
                  label = "Select Country",
                  choices = names(countries_dataset)
      ),
      DTOutput("dataTable")
      )
      
    } else if (input$tabs == "World Map and GDP Analysis") {
      checkboxInput(inputId = "showComparison",
                    label = "Show Comparison Plot",
                    value = FALSE)
      
    } else if (input$tabs == "Countries Comparison"){
      tagList(
        selectInput(inputId = "countrycount",
                    label="No. of Countries for comparison",
                    choices=c("Top 10","Top 20","Top 30","Top 40","Top 50",
                              "Top 60","Top 70"," Top 80"),
                    selected="Top 80"
        ),
        selectInput(inputId = "Medal_Type",
                    label="Type of medal",
                    choices=c("Gold","Silver","Bronze","Total"),
                    selected="Total"
        ),
        verbatimTextOutput("dataSummary"))}
  })
  
  # Plot 1: Participation by Gender
  output$Plot1 <- renderPlot({
    dat <- selected_country_data()
    dat_long <- dat %>%
      pivot_longer(cols = c(Men, Women), names_to = "gender", values_to = "participation") 
    dat_long <- dat_long %>%
      slice(1:(nrow(dat_long) - 2))
    
    ggplot(dat_long, aes(x = Sport, y = participation, fill = gender)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.6) +
      labs(x = "Sport", y = "Total Participation", title = "Participation in Sports by Gender") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.6))+
      theme(legend.position = "right", 
           plot.title = element_text(hjust = 0.5,face = "bold", size =20),
           legend.title = element_text(size = 15), 
           legend.text = element_text(size = 12))
  })
  # Plot 2: 
  output$Plot2 <- renderPlot({
    data<-selected_country_medal_data()
    data <- data%>%slice(1:(nrow(data) - 1))
    medal_heatmap <- data %>%
      select(Sport, Gold, Silver, Bronze) %>%
      pivot_longer(cols = c(Gold, Silver, Bronze), names_to = "Medal", values_to = "Count")
    
    # Heatmap plot
    ggplot(medal_heatmap, aes(x = Medal, y = Sport, fill = Count)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightyellow", high = "orange") +
      labs(
        title = "Medal Count by Sport and Medal Type",
        x = "Medal Type",
        y = "Sport",
        fill = "Medal Count"
      ) +theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size =20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
      )+theme_tufte()
  })
  output$dataTable <- renderDT({
    dat <- selected_country_data()
    datatable(dat, options = list(pageLength=5))
  })
  output$gdpAnalysisPlot <- renderPlot({
    world <- ne_countries(scale = "medium", returnclass = "sf")  # Load world map data
    
    world$name[world$name == "United States of America"] <- "United States"
    world$name <- sapply(world$name, standardize_country_name)

    GDP <- GDP_data %>% select(Country, GDP_per_billion)
    
    # Merge GDP data with world map
    world <- world %>%
      left_join(GDP, by = c("name" = "Country"))
    
    # Convert GDP column to numeric and handle NA values
    world$GDP_per_billion <- as.numeric(world$GDP_per_billion)
    mean_gdp <- mean(world$GDP_per_billion, na.rm = TRUE)
    world$GDP_per_billion[is.na(world$GDP_per_billion)] <- mean_gdp
    
    # Calculate centroids for countries to position medal data points
    world_centroids <- st_centroid(world)
    world_centroids <- cbind(world_centroids, st_coordinates(world_centroids))
    
    # Select relevant columns from medal data
    medal_data <- medal_table %>% select(Country, Total) %>% rename(name = Country)
    
    # Merge medal data with centroids
    world_centroids <- world_centroids %>%
      left_join(medal_data, by = "name")
    
    # Plot the map with GDP as fill and medals as points
    ggplot(data = world) +
      geom_sf(aes(fill = GDP_per_billion), color = "black") +
      scale_fill_viridis_c(option = "C", na.value = "gray90") +
      labs(title = "World Map Colored by GDP with Olympic Medals",
           fill = "GDP (in billions)") +
      theme_minimal() +
      geom_point(data = world_centroids, aes(x = X, y = Y, size = Total), 
                 color = "red", alpha = 0.7) +
      scale_size_continuous(range = c(1, 10), name = "Olympic Medals") +
      theme(legend.position = "right", 
            plot.title = element_text(hjust = 0.5,size=25),
            legend.title = element_text(size = 15), 
            legend.text = element_text(size = 12))  # Refined legend
  })
  
  # Second Plot for Comparison
  output$secondPlot <- renderPlot({
    world <- ne_countries(scale = "medium", returnclass = "sf")
    world$name[world$name == "United States of America"] <- "United States"
    world$name <- sapply(world$name, standardize_country_name)
    
    # Merge GDP and gender performance data with the world map
    world <- world %>%
      left_join(GDP_data, by = c("name" = "Country")) %>%
      left_join(gender_performance, by = c("name" = "Country"))
    
    # Handle missing GDP data by replacing NA values with the mean GDP
    mean_gdp <- mean(world$GDP_per_billion, na.rm = TRUE)
    world$GDP_per_billion[is.na(world$GDP_per_billion)] <- mean_gdp
    
    # Calculate centroids for country locations
    world_centroids <- st_centroid(world)
    world_centroids <- cbind(world_centroids, st_coordinates(world_centroids))
 
    # Plot the map with GDP as fill and medals as points based on performance ratio   
    ggplot(data = world) +
      geom_sf(aes(fill = GDP_per_billion), color = "black") +
      scale_fill_viridis_c(option = "C", na.value = "gray90") +
      labs(title = "World Map by GDP with Gender-Based Performance in Paralympics",
           fill = "GDP (in billions)") +
      theme_minimal() +
      geom_point(data = world_centroids, aes(x = X, y = Y, color = Performance, size = Ratio), 
                 alpha = 0.9) +
      scale_color_manual(values = c("Male" = "green", "Female" = "pink")) +
      scale_size_continuous(name = "Performance Ratio", range = c(1, 10)) +
      theme(legend.position = "right", 
            plot.title = element_text(hjust = 0.5,size=22),
            legend.title = element_text(size = 15), 
            legend.text = element_text(size = 12))
  })
  
  # Conditional rendering of either gdpAnalysisPlot or secondPlot
  output$conditionalPlot <- renderUI({
    if (input$showComparison) {
      plotOutput("secondPlot",height="500px")  
    } else {
      plotOutput("gdpAnalysisPlot",height="500px") 
    }
  })
  
  # Display gender performance ratio table
  output$performanceRatioTable <- renderTable({
    gender_performance 
  })
  # Render Countries Comparison Plot
  output$CountriesPlot <- renderPlot({
    n_countries <- as.numeric(gsub("Top ", "", input$countrycount))
    
    # Determine the column based on medal type
    medal_column <- switch(input$Medal_Type,
                           "Gold" = "T_Gold",
                           "Silver" = "T_Silver",
                           "Bronze" = "T_Bronze",
                           "Total" = "Total")
    plot_data <- Top80medaltally %>%
      select(Country, all_of(medal_column)) %>%
      arrange(desc(!!sym(medal_column))) %>%
      head(n_countries)
    
    # Create the lollipop chart
    ggplot(plot_data, aes(x = reorder(Country, !!sym(medal_column)), y = !!sym(medal_column))) +
      geom_segment(aes(x = Country, xend = Country, y = 0, yend = !!sym(medal_column)), color = "black") +
      geom_point(color = "gold", size = 4) +
      coord_flip()+
      theme( 
            plot.title = element_text(hjust = 0.5,size=22),
            axis.title = element_text(size = 15), 
            axis.text = element_text(size = 12))+
      labs(
        title = paste("Top", n_countries, "Countries by", input$Medal_Type, "Medals"),
        x = "Country",
        y = "Medal Count"
      ) 
  })
  output$dataSummary <- renderPrint({
    n_countries <- as.numeric(gsub("Top ", "", input$countrycount))
    
    # Determine the column based on medal type
    if (input$Medal_Type=="Gold"){
      summary_data<- Top80medaltally %>%
        select(G_Male,G_Female,G_Mixed,T_Gold)%>%
        head(n_countries)
    }
    else if (input$Medal_Type=="Silver"){
      summary_data<- Top80medaltally %>%
        select(S_Male,S_Female,S_Mixed,T_Silver)%>%
        head(n_countries)
    }
    else if (input$Medal_Type=="Bronze"){
      summary_data<- Top80medaltally %>%
        select(B_Male,B_Female,B_Mixed,T_Bronze)%>%
        head(n_countries)
    }
    else if (input$Medal_Type=="Total"){
      summary_data<- Top80medaltally %>%
        select(T_Male,T_Female,T_Mixed,Total)%>%
        head(n_countries)
    }
    # Filter and arrange data
    summary(summary_data)
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)


