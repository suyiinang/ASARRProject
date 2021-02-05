#########################################################
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(plotly)
library(stringr)

############################## loading data for visualization ###########################
merged_happiness <- read.csv("./data/merged_happiness.csv")
years <- as.numeric((merged_happiness$Year))
regions <- as.vector(unique(merged_happiness$Region))
factors <- read.csv("./data/factors.csv")
factors_var <- data.frame(name = c("Hospital Beds", "Marriage Rate", "Divorce Rate", "Work Hours", "Gini Coefficient", "Internet Access", "Obesity Rate", "Press Freedom"),
                          colname = c("Hospital_Beds", "Marriage_Rate", "Divorce_Rate", "Work_Hours", "Gini_Coefficient", "Internet_Access", "Obesity_Rate", "Press_Freedom"),
                          stringsAsFactors = F)
r <- read.csv("./data/r.csv")
palette <- colorRampPalette(c("darkred", "darkorange", "khaki", "olivedrab2", "aquamarine3", "cornflowerblue", "darkblue"))
model_coefs <- read.csv("coefs.csv")

############################## end of data loading ######################################

############################## start of SHINY #######################

############################## start of SHINY UI #######################

ui <- dashboardPage(
                    dashboardHeader(title = strong('The Math of Happiness'), titleWidth = 400),
                    dashboardSidebar(width = 400,
                                     sidebarMenu(id = 'sbm',
                                                 menuItem('Distribution', tabName = 'Distribution', icon = icon('globe')),
                                                 menuItem('Evolution', tabName = 'Evolution', icon = icon('chart-line')),
                                                 menuItem('Factors', tabName = 'Factors', icon = icon('project-diagram')),
                                                 menuItem('Prediction', tabName = 'Prediction', icon = icon('laptop-code')),
                                                 menuItem('About', tabName = 'About', icon = icon('address-card'))
                                     )
                    ),
                    dashboardBody(
                      shinyDashboardThemes(theme = "blue_gradient"), #purple_gradient
                        tabItems(
                            
                            ### start of Distribution
                            
                            tabItem(tabName = 'Distribution',
                                    fluidPage(
                                        titlePanel(strong("Introduction & The Distribution of Happiness")),
                                        
                                        h3("Happiness Score is measured through surveys asking respondents to rate their happiness on a ladder scale of 1 to 10."),
                                        h3("The shade of countries below corresponds to their respective Happiness Score."),
                                        
                                        # Sidebar with slider input for year
                                        sidebarLayout(
                                            sidebarPanel(
                                                sliderInput(inputId = "year", "Select a year:",
                                                            min = min(years),
                                                            max = max(years),
                                                            value = min(years),
                                                            sep = ''
                                                            ),
                                                h3("Median World's Happiness Score for the year:"),
                                                verbatimTextOutput("worldMedian")
                                            ),
                                        
                                         # main panel with output of the map
                                            mainPanel(
                                                plotlyOutput("mymap", width = "100%", height = "100%")
                                                )
                                            )
                                        )),
                                    
                                
                            ### end of Distribution, start of Evolution
                        
                            tabItem(tabName = 'Evolution',
                                    fluidPage(
                                      titlePanel(strong("The Evolution of Happiness")),
                                      h3("The graph shows the change in happiness index of a country over the years."),
                                      #slidepanel
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput("trends_region", "Select a region:", regions)
                                        ),
                                        
                                        #mainpanel 
                                        mainPanel(plotlyOutput("trends_graph", width = "100%", height = "100%")
                                        )))),
                                        
                        
                            ### end of Evolution, start of Factors
                        
                            tabItem(tabName = 'Factors',
                                    fluidPage(
                                      titlePanel(strong("Factors Affecting Happiness")),
                                      h3("The following graphs shows the influence of individual factors on happiness score globally."),
                                      #slidepanel
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput("factors", "Select a factor:", as.list(factors_var$name)),
                                          h3("Correlation coefficient r:"),
                                          verbatimTextOutput("inputastext")
                                        ),
                                        
                                        #mainpanel 
                                        mainPanel(plotlyOutput("factors_graph", width = "100%", height = "100%"))
                                        )
                                      )
                                    ),
                            
                            ## end of Factors, start of Prediction
                            
                            tabItem(tabName = 'Prediction',
                                    fluidPage(
                                        titlePanel(strong("Engineering Your Country's Happiness")),
                                        h3("The following variables will significantly impact your happiness, sorted in decreasing order of importance."),
                                        h3("How does tuning them influence happiness? Try to maximize happiness by adjusting the sliders!"),
                                        sidebarLayout(
                                          sidebarPanel(
                                            sliderInput('workhours', 'Enter # work hours annually', min = 1370, max = 1830, value = 1, step = 50),
                                            sliderInput('obesityrate', 'Enter obesity rate', min = 19, max = 40, value = 1, step = 0.5),
                                            sliderInput('pressfreedom', 'Enter press freedom index', min = 7, max = 31, value = 1, step = 0.5),
                                            sliderInput('hospitalbeds', 'Enter # hospital beds per 1000 inhabitants', min = 1.5, max = 8.5, value = 1, step = 0.1),
                                            sliderInput('divorcerate', 'Enter divorce rate per 1000 inhabitants', min = 0.5, max = 3.5, value = 0.1, step = 0.1),
                                            
                                          ),
                                          
                                          #mainpanel 
                                          mainPanel(
                                            h3('Your country\'s happiness score is '),
                                            verbatimTextOutput("happinessscore")
                                            
                                          )
                                          )
                                        )
                                    ),
                            
                            tabItem(tabName = 'About',
                                    fluidPage(
                                      titlePanel(strong("About")),
                                      h4('The Team: Ang Su Yiin, Anne Nguyen Nhi Thai An, E-Lynn Toh, Foo Kai Loon, Sean Samuel Prajs, Tay Kai Lin'),
                                      h4("This app was created as part of the requirements for the Applied Statistical Analysis with R module for Master of IT in Business program at Singapore Management University (SMU)."),
                                      h3(strong("Limitations")), 
                                      h4('The following limitations in the investigation of happiness and model construction are acknowledged:'),
                                      h4('- Data for influencing factors was drawn from multiple sources, with issues of data completeness to address as some nations may not record certain performance matrices yearly or at all. In addition, there is an inherent bias of data completeness towards developed nations over developing nations as developed nations have the greater capability to survey, model and score these additional statistical results. All conclusions drawn are thus only generalizable to the set of developed countries used.'), 
                                      h4('- The analysis assumes that the relationship between happiness and other influencing factors is linear.'), 
                                      h4('- Correlation does not imply causation, and thus the model is valid to the extent that it is predictive without directly suggesting causality. There could be a third or more underlying factors that link a variable with happiness.') 
                                      
                                      )
                                    )
                        )
                    )
)

############################## start of SHINY SERVER #######################

server <- function(input, output) {
    
    ############## start of Distribution
    
    output$worldMedian <- renderText(round(median(merged_happiness$HappinessScore[merged_happiness$Year == input$year]), digits = 2))
      
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.2)
    
    # specify map projection/options
    g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'Mercator')
    )
    
    ## create df by years 
    year2015 <- merged_happiness %>%
        filter(Year == 2015)
    
    year2016 <- merged_happiness %>%
        filter(Year == 2016)
    
    year2017 <- merged_happiness %>%
        filter(Year == 2017)
    
    year2018 <- merged_happiness %>%
        filter(Year == 2018)
    
    year2019 <- merged_happiness %>%
        filter(Year == 2019)
    
    year2020 <- merged_happiness %>%
        filter(Year == 2020)
    
    
    ## output
    output$mymap <- renderPlotly({
        if(input$year == 2015) {
            plot_geo(year2015) %>%
                add_trace(
                    z = ~round(HappinessScore,2), color = ~HappinessScore,
                    colors = palette(50),
                    text = ~Country, locations = ~Code, marker = list(line = l)) %>%
                colorbar(title = 'Happiness Score', limits=c(2,8), thickness=10, x = 1.1, y = 0.8) %>%
                layout(
                    title = list(text = 'World Happiness Score 2015', y = 0.9),
                    geo = g
                )
        }
        
        else if(input$year == 2016){
            plot_geo(year2016) %>%
                add_trace(
                    z = ~round(HappinessScore,2), color = ~HappinessScore,
                    colors = palette(50),
                    text = ~Country, locations = ~Code, marker = list(line = l)) %>%
                colorbar(title = 'Happiness Score', limits=c(2,8), thickness=10, x = 1.1, y = 0.8) %>%
                layout(
                    title = list(text = 'World Happiness Score 2016', y = 0.9),
                    geo = g
                )
        }
        
        else if(input$year == 2017){
            plot_geo(year2017) %>%
                add_trace(
                    z = ~round(HappinessScore,2), color = ~HappinessScore,
                    colors = palette(50),
                    text = ~Country, locations = ~Code, marker = list(line = l)) %>%
                colorbar(title = 'Happiness Score', limits=c(2,8), thickness=10, x = 1.1, y = 0.8) %>%
                layout(
                    title = list(text = 'World Happiness Score 2017', y = 0.9),
                    geo = g
                )
        }
        
        else if (input$year == 2018){
            plot_geo(year2018) %>%
                add_trace(
                    z = ~round(HappinessScore,2), color = ~HappinessScore, 
                    colors = palette(50),
                    text = ~Country, locations = ~Code, marker = list(line = l)) %>%
                colorbar(title = 'Happiness Score', limits=c(2,8), thickness=10, x = 1.1, y = 0.8) %>%
                layout(
                    title = list(text = 'World Happiness Score 2018', y = 0.9),
                    geo = g
                )
        }
        
        else if(input$year == 2019){
            plot_geo(year2019) %>%
                add_trace(
                    z = ~round(HappinessScore,2), color = ~HappinessScore, 
                    colors = palette(50),
                    text = ~Country, locations = ~Code, marker = list(line = l)) %>%
                colorbar(title = 'Happiness Score', limits=c(2,8), thickness=10, x = 1.1, y = 0.8) %>%
                layout(
                    title = list(text = 'World Happiness Score 2019', y = 0.9),
                    geo = g
                )
        }
        
        else if (input$year == 2020){
            plot_geo(year2020) %>%
                add_trace(
                    z = ~round(HappinessScore,2), color = ~HappinessScore, 
                    colors = palette(50),
                    text = ~Country, locations = ~Code, marker = list(line = l)) %>%
                colorbar(title = 'Happiness Score', limits=c(2,8), thickness=10, x = 1.1, y = 0.8) %>%
                layout(
                    title = list(text = 'World Happiness Score 2020', y = 0.9),
                    geo = g
                )
        }
        
        
    })
    
 
    ############## start of Evolution
    
    output$trends_graph <- renderPlotly({
      
      selected_region_df <- merged_happiness %>%
        filter(Region == input$trends_region)
      
      p <- ggplot(selected_region_df, aes(x = Year, y = HappinessScore)) +
          geom_point(aes(color = Country)) +
          geom_line(aes(color = Country)) +
        ggtitle("Country's Happiness Score over Time") + 
        theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
      
      p
      
    })
    
    ############## start of Factors
    
    #subsetting dataframes for individual factors
    factors_hb <- factors %>% select(Country, Year, Happiness_Score, Hospital_Beds, Region)
    factors_mr <- factors %>% select(Country, Year, Happiness_Score, Marriage_Rate, Region)
    factors_dr <- factors %>% select(Country, Year, Happiness_Score, Divorce_Rate, Region)
    factors_wh <- factors %>% select(Country, Year, Happiness_Score, Work_Hours, Region)
    factors_gc <- factors %>% select(Country, Year, Happiness_Score, Gini_Coefficient, Region)
    factors_ia <- factors %>% select(Country, Year, Happiness_Score, Internet_Access, Region)
    factors_or <- factors %>% select(Country, Year, Happiness_Score, Obesity_Rate, Region)
    factors_pf <- factors %>% select(Country, Year, Happiness_Score, Press_Freedom, Region)
    
    #read off correlation coefficient
    output$inputastext <- renderText(unlist(r %>% select(factors_var$colname[factors_var$name == input$factors])))
    
    #create graph with subsetted dfs depending on input$factors
    output$factors_graph <- renderPlotly({
      if (input$factors == "Hospital Beds") {
      p <- factors_hb %>% ggplot(aes(Hospital_Beds, Happiness_Score)) +
        xlab(input$factors) +
        ylab("Happiness Score") +
        geom_point(aes(color = Region, text = paste("Country: ", Country))) +
        geom_smooth(method = "lm", se = F, color = "black", lwd = 0.75) +
        ggtitle(paste0("Correlation between Happiness Score and ", input$factors, ": r = ", unlist(r %>% select(factors_var$colname[factors_var$name == input$factors])))) +
        theme(legend.title = element_blank())
      }
      else if (input$factors == "Marriage Rate") {
        p <- factors_mr %>% ggplot(aes(Marriage_Rate, Happiness_Score)) +
          xlab(input$factors) +
          ylab("Happiness Score") +
          geom_point(aes(color = Region, text = paste("Country: ", Country))) +
          geom_smooth(method = "lm", se = F, color = "black", lwd = 0.75) +
          ggtitle(paste0("Correlation between Happiness Score and ", input$factors, ": r = ", unlist(r %>% select(factors_var$colname[factors_var$name == input$factors])))) +
          theme(legend.title = element_blank())
      }
      else if (input$factors == "Divorce Rate") {
        p <- factors_dr %>% ggplot(aes(Divorce_Rate, Happiness_Score)) +
          xlab(input$factors) +
          ylab("Happiness Score") +
          geom_point(aes(color = Region, text = paste("Country: ", Country))) +
          geom_smooth(method = "lm", se = F, color = "black", lwd = 0.75) +
          ggtitle(paste0("Correlation between Happiness Score and ", input$factors, ": r = ", unlist(r %>% select(factors_var$colname[factors_var$name == input$factors])))) +
          theme(legend.title = element_blank())
      }
      else if (input$factors == "Work Hours") {
        p <- factors_wh %>% ggplot(aes(Work_Hours, Happiness_Score)) +
          xlab(input$factors) +
          ylab("Happiness Score") +
          geom_point(aes(color = Region, text = paste("Country: ", Country))) +
          geom_smooth(method = "lm", se = F, color = "black", lwd = 0.75) +
          ggtitle(paste0("Correlation between Happiness Score and ", input$factors, ": r = ", unlist(r %>% select(factors_var$colname[factors_var$name == input$factors])))) +
          theme(legend.title = element_blank())
      }
      else if (input$factors == "Gini Coefficient") {
        p <- factors_gc %>% ggplot(aes(Gini_Coefficient, Happiness_Score)) +
          xlab(input$factors) +
          ylab("Happiness Score") +
          geom_point(aes(color = Region, text = paste("Country: ", Country))) +
          geom_smooth(method = "lm", se = F, color = "black", lwd = 0.75) +
          ggtitle(paste0("Correlation between Happiness Score and ", input$factors, ": r = ", unlist(r %>% select(factors_var$colname[factors_var$name == input$factors])))) +
          theme(legend.title = element_blank())
      }
      else if (input$factors == "Internet Access") {
        p <- factors_ia %>% ggplot(aes(Internet_Access, Happiness_Score)) +
          xlab(input$factors) +
          ylab("Happiness Score") +
          geom_point(aes(color = Region, text = paste("Country: ", Country))) +
          geom_smooth(method = "lm", se = F, color = "black", lwd = 0.75) +
          ggtitle(paste0("Correlation between Happiness Score and ", input$factors, ": r = ", unlist(r %>% select(factors_var$colname[factors_var$name == input$factors])))) +
          theme(legend.title = element_blank())
      }
      else if (input$factors == "Obesity Rate") {
        p <- factors_or %>% ggplot(aes(Obesity_Rate, Happiness_Score)) +
          xlab(input$factors) +
          ylab("Happiness Score") +
          geom_point(aes(color = Region, text = paste("Country: ", Country))) +
          geom_smooth(method = "lm", se = F, color = "black", lwd = 0.75) +
          ggtitle(paste0("Correlation between Happiness Score and ", input$factors, ": r = ", unlist(r %>% select(factors_var$colname[factors_var$name == input$factors])))) +
          theme(legend.title = element_blank())
      }
      else if (input$factors == "Press Freedom") {
        p <- factors_pf %>% ggplot(aes(Press_Freedom, Happiness_Score)) +
          xlab(input$factors) +
          ylab("Happiness Score") +
          geom_point(aes(color = Region, text = paste("Country: ", Country))) +
          geom_smooth(method = "lm", se = F, color = "black", lwd = 0.75) +
          ggtitle(paste0("Correlation between Happiness Score and ", input$factors, ": r = ", unlist(r %>% select(factors_var$colname[factors_var$name == input$factors])))) +
          theme(legend.title = element_blank())
      }
      
      p
      
    })
    
    ############## start of Prediction
    
    happiness_prediction <- function(wh, or, pf, hb, dr) {
      result <- round(model_coefs$Coefs[1] + 
               model_coefs$Coefs[6]*wh + 
               model_coefs$Coefs[4]*or + 
               model_coefs$Coefs[5]*pf + 
               model_coefs$Coefs[2]*hb + 
               model_coefs$Coefs[3]*dr, digits = 2)
      return(result)
    }
    
    output$outworkhours <- renderPrint(input$workhours)
    output$outobesityrate <- renderPrint(input$obesityrate)
    output$outpressfreedom <- renderPrint(input$pressfreedom)
    output$outhospitalbeds <- renderPrint(input$hospitalbeds)
    output$outdivorcerate <- renderPrint(input$divorcerate)
    output$happinessscore <- renderPrint(happiness_prediction(input$workhours, input$obesityrate, input$pressfreedom, input$hospitalbeds, input$divorcerate))
}

############################## end of SHINY SERVER #######################

shinyApp(ui, server)