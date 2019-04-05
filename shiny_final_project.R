# Download libraries
library(shiny)
library(tidyverse)
library(rvest) # For data scraping
library(readxl) # For reading excel file
library(randomForest) # For running random forest 
library(caret) # For cross-validation
library(class) # For running knn
library(stringr) # For string manipulation
library(ggthemes) # For colorblind theme
library(plotly) # For interactive graphics
library(viridis) # For Viridis color scales
library(shinythemes) # For Shiny theme 
library(maps) # For map display
library(e1071) # For caret package
library(MLmetrics) # For evaluation metrics

# Read in tables
life_exp <- read_csv("data/life_exp.csv")
education <- read_csv("data/education.csv")
traffic <- read_csv("data/traffic.csv")
cigarette <- read_csv("data/cigarette.csv")
wealth_per_adult <- read_csv("data/wealth_per_adult.csv")
gas_emission <- read_csv("data/gas_emission.csv")
gdp_per_capita <- read_excel(
  "data/API_NY.GDP.PCAP.CD_DS2_en_excel_v2_10473786.xls")

# Function to classify life expectancy into three levels
change_to_level <- function(col_name){
  one_third <- quantile(col_name, 1/3)
  two_third <- quantile(col_name, 2/3)
  
  both_life_level <- character(length(col_name))
  
  for (i in 1:length(col_name)) {
    if (col_name[i] < one_third) {
      both_life_level[i] <- "low"
    } else if (col_name[i] < two_third) {
      both_life_level[i] <- "medium"
    } else {
      both_life_level[i] <- "high"
    }
  }
  both_life_level
}

# Tidy life expectancy table
life_exp_tidy <- life_exp %>%
  select(c(1,3,5,7)) %>%
  rename(Country = `Country and regions`,
         Both_life_exp_15 = `Both sexes lifeexpectancy`,
         Female_life_exp_15 = `Female life expectancy`,
         Male_life_exp_15 = `Male life expectancy`)

# Tidy education table
education_tidy <- education %>%
  select(c(2:5)) %>%
  rename(Edu_index_15 = EducationIndex,
         Exp_school_15 = `Expected yearsof schooling`,
         Mean_school_15 = `Mean years of schooling`)

# Tidy traffic table
traffic_tidy <- traffic %>%
  select(c(1:3, 5)) %>%
  rename(
    Road_fatal_per_100k_people_13 = 
      `Road fatalitiesper 100,000inhabitantsper year[4]`,
    Road_fatal_per_100k_vehicles_13 = 
      `Road fatalitiesper 100,000motor vehicles[4]`,
    Total_fatal_13 = 
      `Total fatalitieslatest year(adjusted/estimatedfigures by WHO report)[4]`)

# Tidy cigarette table
cigarette_tidy <- cigarette %>%
  select(c(2:3)) %>%
  rename(Cig_consumption_16 = `Cigarette consumption per year per person[9]`)

# Tidy wealth per adult variable
wealth_per_adult <- wealth_per_adult %>% 
  slice(-(1:181))
countries_wealth <- wealth_per_adult %>%
  slice(seq(1, 870, by = 5))
medians_wealth <- wealth_per_adult %>% 
  slice(seq(1, 870, by = 5) + 1)
# Join two tables by column
wealth_per_adult <- cbind(countries_wealth, medians_wealth) 
colnames(wealth_per_adult) <- c("Country", "Wealth_18")

# Tidy GDP per capita table
gdp_per_capita_tidy <- gdp_per_capita %>%
  select(c(1,60)) %>%
  rename(Country = `Data Source`,
         Gdp_per_capita_15 = X__58)

# Tidy gas emission table
gas_emission_tidy <- gas_emission %>%
  select(c(1:2)) %>%
  rename(Gas_emission_14 = `GHG emissions(MtCO2e)`)

# Full join all variables and convert them to numeric type
final_table_initial <- life_exp_tidy %>%
  full_join(education_tidy) %>%
  full_join(traffic_tidy) %>%
  full_join(cigarette_tidy) %>%
  full_join(wealth_per_adult) %>%
  full_join(gdp_per_capita_tidy) %>%
  full_join(gas_emission_tidy) %>%
  mutate(Total_fatal_13 = parse_number(Total_fatal_13),
         Wealth_18 = parse_number(Wealth_18),
         Cig_consumption_16 = as.numeric(Cig_consumption_16),
         Road_fatal_per_100k_people_13 = as.numeric(
           Road_fatal_per_100k_people_13),
         Road_fatal_per_100k_vehicles_13 = as.numeric(
           Road_fatal_per_100k_vehicles_13),
         Gdp_per_capita_15 = as.numeric(Gdp_per_capita_15)) %>%
  na.omit()

# Convert life expectancy into levels using the function we wrote above
final_table_organized <- final_table_initial %>% 
  mutate(
    Both_lifeExp_level = change_to_level(final_table_initial$Both_life_exp_15),
    Female_lifeExp_level = change_to_level(
      final_table_initial$Female_life_exp_15),
    Male_lifeExp_level = change_to_level(final_table_initial$Male_life_exp_15))

# Rename all variables to be more user-friendly
final_table <- final_table_organized %>% 
  rename(
    "Education_index" = Edu_index_15,
    "Expected_years_of_schooling" = Exp_school_15,
    "Mean_years_of_schooling" = Mean_school_15,
    "Road_fatalities_per_100000_inhabitants_per_year" = 
      Road_fatal_per_100k_people_13,
    "Road_fatalities_per_100000_motor_vehicles" = 
      Road_fatal_per_100k_vehicles_13,
    "Total_fatalities" = Total_fatal_13,
    "Cigarette_consumption_per_year_per_person" = Cig_consumption_16,
    "Wealth_per_adult" = Wealth_18,
    "GDP_per_capita" = Gdp_per_capita_15,
    "Gas_emission" = Gas_emission_14,
    "Life_expectancy" = Both_lifeExp_level,
    "Life_expectancy_of_female" = Female_lifeExp_level,
    "Life_expectancy_of_male" = Male_lifeExp_level
  ) %>%
  select(-c(2:5, 10, 16:17))

final_table_worldmap <- final_table %>%
  mutate(Country = if_else(Country == "United States", 'USA', 
                           if_else(Country == "United Kingdom", 'UK', Country)))

map.world <- map_data('world')

final_map <- left_join(map.world, final_table_worldmap, 
                       by = c('region' = 'Country'))

# Random forest classifier
final_table_randomforest <- final_table %>% 
  mutate(Life_expectancy = as.factor(Life_expectancy))
rand_forest <- randomForest(Life_expectancy ~ . - Country, 
                            data = final_table_randomforest, mtry = sqrt(8))


# KNN classifier
set.seed(03122022)
# Cross validation
k_grid <- data.frame(k = c(1, 4, 5, 7, 8, 10, 11, 13, 14))

more_metrics  <- function(data, lev = NULL, model = NULL){
  def <- defaultSummary(data, lev, model)
  met2 <- multiClassSummary(data, lev, model)
  c(def, met2)
}

train_control <- trainControl(
  method = "cv",                  # we're performing cross validation
  number = 10,                    # with 10 folds
  summaryFunction = more_metrics, # pass in our function
  savePredictions = TRUE,         # save all predictions
  classProbs = TRUE               # compute class probabilities
)

knn_cv <- train(
  Life_expectancy ~ . - Country, # response ~ explanatory variables
  data = final_table,              # data set used for k-fold cv
  method = "knn",                  # specifying knn model
  preProc = c("center", "scale"),  # within each fold, standardize
  tuneGrid = k_grid,               # grid for parameters being tuned
  trControl = train_control        # pass in training details
)

k_values <- knn_cv$results %>% pull(k)
accuracy <- knn_cv$results %>% pull(Accuracy)
cv_table <- tibble(
  K = as.integer(k_values),
  accuracy = accuracy
)

# k = 7
standardize <- function(x, na.rm = FALSE) {
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
}
std_final_table <- final_table %>%
  mutate_if(is.numeric, standardize, na.rm = TRUE)

std_life <- std_final_table %>% pull(Life_expectancy) 
std_preds <- std_final_table %>% select(-c(Life_expectancy, Country))

nn7 <- knn(
  train = std_preds, 
  test = std_preds,  
  cl = std_life,    
  k = 7                    
)

testing_result <- std_final_table %>%
  mutate(pred.life_expectancy = nn7) %>%
  summarize(
    accuracy = mean(Life_expectancy == pred.life_expectancy))

intro_text <- "According to the United Nations, the average life expectancy at \
birth was 71.5 years over the period 2010-2015. Life expectancy is an important\
measure in understanding the overall health of a community. Understanding life \
expectancy at the world level can be problematic; people who live in developed \
countries tend to have a much higher average life expectancy than those who \
live in developing countries.  In addition, life expectancy can be affected by \
many socioeconomic factors such as: income and education level. Therefore, we \
are interested in understanding the effect of socioeconomic factors on the \
average life expectancy at the country level. We encourage users to explore the\
potential aspects that a country should focus on, in order to strive for a \
higher range of life expectancy. The classification tool proposed in this study\
allows users to incorporate changes into particular factors for a new \
prediction on a country’s average life expectancy.
"
objectives_text1 <- "In this project, we are looking into:"
objectives_text2 <- "What are the underlying factors that affect the average \
life expectancy in each country?"
objectives_text3 <- "How are the variables associated with one another?"
objectives_text4 <- "What classification method provides the best accuracy of \
the average life expectancy?"

map_text <- "The world map above displays the information of each predictor \
variable in our model. Users can choose a predictor variable (with the default \
of expected years of schooling) to explore its level across countries. For \
example, from the map of wealth per adult, we can tell that the United States \
has a higher median wealth per adult than countries in South America and \
Africa. Note: The gray color indicates missing data from those countries. 
"

scatter_text <- "In our exploratory data analysis, we looked into how different\
variables are associated with one another. The interactive graph above first \
asks users to pick a variable, then the scatterplot with the selected variable\
on the y-axis will be displayed. The default plot shows the life expectancy of \
each country by expected years of schooling. In all plots, the color represents\
the life expectancy level, which allows users to visualize the relationships \
between each variable and life expectancy. For example, countries with high \
average life expectancy seem to be associated with longer expected years of \
schooling. Countries with low average life expectancy seem to be associated \
with greater number of road fatalities. 
"

rand_text <- "We also ran a random forest algorithm on the dataset to classify \
the average life expectancy in each country. We first sampled with replacement \
from the dataset to get a number of subsets. For each subset, we considered a \
random sample of 3 out of 8 predictors to select the best attribute and further\
 partitioned the data by each variable. 
The random forest algorithm provides the final prediction of each country’s \
average life expectancy using majority voting scheme. The random forest \
algorithm has a 28.7% error rate in predicting a country’s average life \
expectancy. 
This suggests that the model would falsely classify approximately 3 out of 10 \
countries’ life expectancy. 
The variable importance plot indicates that the mean year of schooling \
contributes the most to our random forest classification, measured by the \
average decrease in total Gini coefficient."

knn_text_p1 <- "After investigating the relationships between the predictor \
variables, we used a k-nearest neighbors algorithm on the dataset to classify \
the average life expectancy based on user’s inputs. In order to determine the \
number of neighbors that our algorithm should consider, we performed a 10-fold\
cross validation. In this process, we first randomly divided the dataset into \
10 subsets of roughly equal sizes. Then we calculated a performance metric for \
each k by averaging the metrics from each validation set. We chose to evaluate\
k values of 1, 4, 5, 7, 8, 10, 11, 13, and 14. We did not include k of 2 or \
values that are multiples of 3 because our response variable life expectancy \
has 3 levels, which would potentially lead to failure in classifying our new \
observation without a majority vote of neighbors in the model. The table below\
displays the results after we performed the cross validation."

knn_text_p2 <- "Based on the accuracy results, the cross validation shows that\
the 7-nearest neighbors, with a 75.8% accuracy rate, is the optimal classifier.\
We then standardized the predictor variables and the test variables (user input\
) using the means and the standard deviations from the predictor variables. \
Finally, we ran a 7-NN classifier and informed the predicted life expectancy \
level of the sample input to the user. 
"
 
datatable_text_p1 <- "We downloaded a dataset that contains information of GDP \
per capita from the World Bank website and scraped all other data tables from \
individual wikipedia pages. We then joined all the tables and filtered out \
countries with missing data. Our final data consists of 136 observations \
(countries) and 9 variables: the "
datatable_text_p2 <- "In order to compare the level of life expectancy across \
countries, we first created a function that classifies life expectancy into \
three levels: high, medium, and low. The cut-off point of low life expectancy \
is the lowest ⅓ quartile of our dataset. The middle ⅓ quartile and highest \
⅓ quartile are for medium and high life expectancy, respectively. After we \
established the cut-points, 
we created a new column that stores the result of each country’s level of life \
expectancy. Below is the summary statistics of the number of countries in each \
life expectancy category."      

explore_text_p1 <- "Using a knn classifier, we developed an interactive tool \
that allows users to explore the level of average life expectancy in each \
country. Users would first select a country of their own interest. Then the \
tool would automatically show the current values of the country’s life \
expectancy and each explanatory variable."
explore_text_p2 <- "After reading the current information, users are encouraged\
to use the sliders to adjust value of each variable. When the user clicks the \
prediction button, the tool would produce a new prediction based on the user \
input. This allows users to examine the influence of each variable on different\
countries. For example, China has a high life expectancy based on the current \
information from the dataset. If the user lowers the total greenhouse gas \
emissions from 12454.7 to 6033.8 (MtCO2e), our model would predict the China's \
life expectancy to be medium. This suggests a potential positive association \
between the greenhouse gas emissions and the average life expectancy in China."

wiki_url <- 'https://en.wikipedia.org/wiki/'

life_expectancy_url <- 
  str_c(wiki_url, 'List_of_countries_by_life_expectancy')
expected_years_schooling_url <- 
  str_c(wiki_url, 'Education_Index')
mean_years_schooling_url <- 
  str_c(wiki_url, 'Education_Index')
road_fatalities_inhabitants_url <- 
  str_c(wiki_url, 'List_of_countries_by_traffic-related_death_rate')
road_fatalities_vehicles_url <- 
  str_c(wiki_url, 'List_of_countries_by_traffic-related_death_rate')
cigarette_consumption_url <- 
  str_c(wiki_url, 'List_of_countries_by_cigarette_consumption_per_capita')
median_wealth_url <- 
  str_c(wiki_url, 'List_of_countries_by_wealth_per_adult')
gdp_url <- 'https://data.worldbank.org/indicator/ny.gdp.pcap.cd'
greenhouse_gas_url <- 
  str_c(wiki_url, 'List_of_countries_by_greenhouse_gas_emissions')

library(shiny)

# Define the ui
# Not show prediction before first click
ui <- navbarPage("Life expectancy",
                 tabPanel("Introduction",
                          fluidPage(
                            column(12, 
                                   offset = 4, 
                                   imageOutput("introImage", 
                                               width = "60%", 
                                               height = "60%"))
                          ),
                          br(),
                          p("Motivation", 
                            style = "font-family: 'Arial'; 
                                     font-weight: bold; 
                                     font-size: 20pt;"),
                          p(intro_text, style = "font-family: 'Arial';"),
                          br(),
                          p("Objectives", 
                            style = "font-family: 'Arial'; 
                                     font-weight: bold; 
                                     font-size: 20pt;"),
                          p(objectives_text1, style = "font-family: 'Arial';"),
                          p(objectives_text2, style = "font-family: 'Arial';"),
                          p(objectives_text3, style = "font-family: 'Arial';"),
                          p(objectives_text4, style = "font-family: 'Arial';")),
                 tabPanel("The Data",
                          DT::dataTableOutput("table"),
                          br(),
                          br(),
                          p(HTML(paste0(datatable_text_p1, 
                                        a(href = life_expectancy_url, 
                                          'average life expectancy'),
                                        ' at birth of each country and 8 other \
                                        predictors. Our predictors are ',
                                        a(href = expected_years_schooling_url, 
                                          'expected years of schooling'),
                                        ', ',
                                        a(href = mean_years_schooling_url, 
                                          'mean years of schooling'),
                                        ', ',
                                        a(href = road_fatalities_inhabitants_url, 
                                          'road fatalities per 100,000 \
                                          inhabitants per years'),
                                        ', ',
                                        a(href = road_fatalities_vehicles_url, 
                                          'road fatalities per 100,000 motor \
                                          vehicles'),
                                        ', ',
                                        a(href = cigarette_consumption_url, 
                                          'cigarette consumption per year per \
                                          person'),
                                        ', ',
                                        a(href = median_wealth_url, 
                                          'median wealth per adult'),
                                        ', ',
                                        a(href = gdp_url, 
                                          'GDP per capita'),
                                        ', and ',
                                        a(href = greenhouse_gas_url, 
                                          'total greenhouse gas emissions'),
                                        '.'
                          )), 
                          style = "font-family: 'Arial'; font-si16pt"),
                          br(),
                          p(datatable_text_p2, style = 
                              "font-family: 'Arial'; font-si16pt"),
                          plotOutput("bar")),
                 tabPanel("Visualizations",
                          fluidPage(tabsetPanel(
                            tabPanel("World map",
                                  br(),
                                  selectInput(
                                    "worldvar", 
                                    label = "Select a Variable",
                                    choices = colnames(final_table)[2:9], 
                                    selected = "Expected_years_of_schooling"),
                                   plotOutput("worldmap"),
                                  br(),
                                  p(map_text, 
                                    style = "font-family: 'Arial'; font-si16pt"
                                    )),
                            
                            tabPanel("Scatterplot",
                                   br(),
                                   selectInput("yvar", 
                                               label = "Y-axis variable",
                                               choices = 
                                                 colnames(final_table)[2:9], 
                                               selected = 
                                                 "Expected_years_of_schooling"),
                                   plotlyOutput("scatter"),
                                   br(),
                                   p(scatter_text, 
                                     style = "font-family: 'Arial'; font-si16pt"
                                     ))))
                 ),
                 tabPanel("Statistical Results",
                          fluidPage(tabsetPanel(
                            tabPanel("KNN",
                                     br(),
                                     p(knn_text_p1, 
                                       style = 
                                         "font-family: 'Arial'; font-si16pt"
                                       ),
                                     br(),
                                     tableOutput("cvtable"),
                                     br(),
                                     p(knn_text_p2, 
                                       style = 
                                         "font-family: 'Arial'; font-si16pt"
                                       )),
                            tabPanel("Random forest",
                                     br(),
                                     p(rand_text, 
                                       style = 
                                         "font-family: 'Arial'; font-si16pt"
                                       ),
                                     plotOutput("randomforest"))
                          ))),
                 tabPanel("Explore",
                          fluidPage(tabsetPanel(
                            tabPanel("How do we predict?",
                                     br(),
                                     p(explore_text_p1, 
                                       style = 
                                         "font-family: 'Arial'; font-si16pt"
                                       ),
                                     br(),
                                     p(explore_text_p2, 
                                       style = 
                                         "font-family: 'Arial'; font-si16pt"
                                       ),
                                     br(),
                                     p("Have fun predicting!", 
                                       style = 
                                         "font-family: 'Arial'; font-si16pt"
                                       )),
                            tabPanel("Your turn!",
                                     br(),
                                     fluidRow(
                                       column(6, 
                                              selectInput("country", 
                                                          label = "Choose a \
                                                          country to explore: ",
                                                          choices = 
                                                            final_table$Country, 
                                                          selected = "China"
                                                          )),
                                       column(6, tableOutput("lifeExp_current"))
                                     ),
                                     fluidRow(
                                       textOutput("choose_var")
                                     ),
                                     fluidRow(
                                       tableOutput("current_info")
                                     ),
                                     
                                     fluidRow(
                                       column(4, 
                                              uiOutput(
                                                "expected_years_of_schooling"
                                                )),
                                       column(4, 
                                              uiOutput(
                                                "mean_years_of_schooling"
                                                )),
                                       column(4, 
                                              uiOutput(
                                                "road_fatalities_inhabitants"
                                                )),
                                       column(4, 
                                              uiOutput(
                                                "road_fatalities_vehicles"
                                                )),
                                       column(4, 
                                              uiOutput(
                                                "cigarette_consumption"
                                                )),
                                       column(4, 
                                              uiOutput(
                                                "wealth_per_adult"
                                                ))
                                     ),
                                     fluidRow(
                                       column(width = 4, 
                                              offset = 2, 
                                              uiOutput("gdp_per_capita")),
                                       column(4, 
                                              uiOutput("gas_emission"))
                                     ),
                                     fluidRow(
                                       column(4, 
                                              actionButton("prediction_button", 
                                                           "Prediction")
                                              )),
                                     fluidRow(
                                       column(4, 
                                              textOutput("knn_result")),
                                       tags$head(tags$style("
                                                 #knn_result{
                                                    font-size: 20px;
                                                    }
                                                 #choose_var{
                                                    margin-left: 1em; 
                                                    color: #2b70a8; 
                                                    font-family: Helvetica; 
                                                    font-weight: bold;
                                                    }
                                                 #preImage{
                                                    width: 15px; 
                                                    height: 15px;
                                                    }"
                                       ))))
                            
                          
                 ))),
                 tabPanel('Meet Us :)',
                          fluidRow(
                            column(12,
                                   offset = 3, 
                                   imageOutput("preImage", 
                                               width = "100%", 
                                               height = "100%"))
                          ),
                          fluidRow(
                            column(6, offset = 3,
                                   br(),
                                   p("We believe there is a positive \
                                     correlation between ice cream and life \
                                     expectancy :)", 
                                     style = "font-family: 'Arial'; 
                                              font-weight: bold; 
                                              color: #2b70a8;"),
                                   p("Contact us if you have any question or \
                                     want to learn more about the project!", 
                                     style = "font-family: 'Arial'; 
                                              font-weight: bold; 
                                              color: #2b70a8;"),
                                   p("Chunjin Ruan :  cjinruan@gmail.com", 
                                     style = "font-family: 'Arial'; 
                                              font-si16pt"),
                                   p("Teerat Wongrattanapiboon :  \
                                     teeratwong@gmail.com", 
                                     style = "font-family: 'Arial'; 
                                     font-si16pt"),
                                   p("Megan Zhao :   zhaoxiazhao.megan@gmail.\
                                     com", 
                                     style = "font-family: 'Arial'; 
                                     font-si16pt")
                                   )
                          )),
                 theme = shinytheme("united")
)

server <- function(input, output) {
  output$preImage <- renderImage({
    list(src = "group_photo.png", width = "50%", height = "50%")
  }, deleteFile = FALSE)
  
  output$introImage <- renderImage({
    list(src = "earth.png", width = "50%", height = "50%")
  }, deleteFile = FALSE)
  
  # Bar chart of total number of countries in each life expectancy category
  output$bar <- renderPlot({
    final_table %>%
      count(Life_expectancy) %>% 
      mutate(dummy_variable = "0") %>% 
      ggplot(aes(x = dummy_variable)) +
      geom_bar(aes(fill = Life_expectancy), width = 0.15) +
      geom_text(aes(y = 0.5, family = "Arial"), 
                label = "Medium: 45", 
                vjust = 0.6, 
                size = 6, 
                color = "white") +
      geom_text(aes(y = 1.5), 
                label = "Low: 44", 
                vjust = 0.5, 
                size = 6, 
                color = "white") + 
      geom_text(aes(y = 2.5), 
                label = "High: 47", 
                vjust = 0.5, 
                size = 6, 
                color = "white") + 
      geom_text(aes(y = 1.5), 
                label = 
                  "Total number of countries in each life expectancy category", 
                vjust = 5, 
                size = 6, 
                family = "Courier", 
                fontface="italic") + 
      theme(panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "none") +
      scale_fill_colorblind("Life Expectancy") +
      coord_flip()
  })
  
  # Reactive object for plots
  reactive_plot <- reactive ({
    plot <- ggplot(data = final_table, aes(x = Country)) +
      geom_point(aes_string(y = input$yvar, 
                            color = fct_relevel(final_table$Life_expectancy, 
                                                c("high", "medium", "low")))) +
      labs(
        title = str_c("Life Expectancy by \n", input$yvar, " in Each Country"),
        y = input$yvar
      )  +
      theme(
        axis.text.x = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold")
      ) +
      scale_y_log10() +
      scale_color_colorblind("Life\nExpectancy") 
    plot
  })
  
  # Output object for a scatterplot
  output$scatter <- renderPlotly({
    ggplotly(reactive_plot(),
             tooltip = c("y", "x"),
             width = 700, height = 400)
  })
  
  # Reactive object for world map
  reactive_worldmap <- reactive({
    worldmap <- ggplot(data = final_map, 
                       aes(x = long, y = lat, group = group)) +
      geom_polygon(aes_string(fill = input$worldvar)) + 
      scale_fill_viridis(option = 'plasma') +
      labs(title = NULL,
           x = "Latitude",
           y = "Longtitude") + 
      theme_bw()
    
    worldmap
    
  })
  
  # World map 
  output$worldmap <- renderPlot({
    reactive_worldmap()
  }, height = "auto", width = 900)
  
  # Output object for data table
  output$table <- DT::renderDataTable(DT::datatable({
    final_table %>% 
      select(-c(10)) %>%
      mutate(Life_expectancy = final_table_initial$Both_life_exp_15)
  }))
  
  output$choose_var <- renderText({
    "Current information of your country"
  })
  
  # Output object for current variables of a country
  output$current_info <- renderTable({
    final_table %>%
      filter(Country == input$country) %>%
      select(c(2:9))
  })
  
  # Output object for the current life expectancty level 
  output$lifeExp_current <- renderTable({
    final_table %>% 
      filter(Country == input$country) %>%
      select(Life_expectancy) %>%
      rename(`Current Life Expectancy` = Life_expectancy)
  })
  
  # Create reactive object for knn-classifier
  reactive_knn <- reactive({
    test_preds <- data.frame(
      Expected_years_of_schooling = c(input$expected_years_of_schooling),
      Mean_years_of_schooling = c(input$mean_years_of_schooling),
      Road_fatalities_per_100000_inhabitants_per_year = 
        c(input$road_fatalities_inhabitants),
      Road_fatalities_per_100000_motor_vehicles = 
        c(input$road_fatalities_vehicles),
      Cigarette_consumption_per_year_per_person = 
        c(input$cigarette_consumption),
      Wealth_per_adult = c(input$wealth_per_adult),
      GDP_per_capita = c(input$gdp_per_capita),
      Gas_emission = c(input$gas_emission)
    )  
    
    means_final_table <- final_table %>% 
      summarize_if(is.numeric, mean, na.rm = TRUE)
    
    sds_final_table <- final_table %>%
      summarize_if(is.numeric, sd, na.rm = TRUE)
    
    for (i in colnames(means_final_table)) {
      test_preds[[i]] <- 
        (test_preds[[i]] - means_final_table[[i]]) / sds_final_table[[i]]
    }
    
    nn7_pred <- knn(
      train = std_preds, 
      test = test_preds,  
      cl = std_life,    
      k = 7                    
    )
    nn7_pred
    
  })
  
  # Slider bar for expected_years_of_schooling
  output$expected_years_of_schooling <- renderUI({
    info <- final_table %>%
      filter(Country == input$country)
    sliderInput("expected_years_of_schooling", "Expected years of schooling",
                0, 25, info$Expected_years_of_schooling, step = 0.1)
  })
  
  # Slider bar for mean_years_of_schooling 
  output$mean_years_of_schooling <- renderUI({
    info <- final_table %>%
      filter(Country == input$country)
    sliderInput("mean_years_of_schooling", "Mean years of schooling",
                0, 15, info$Mean_years_of_schooling, step = 0.1)
  })
  
  # Slider bar for road_fatalities_inhabitants 
  output$road_fatalities_inhabitants <- renderUI({
    info <- final_table %>%
      filter(Country == input$country)
    sliderInput("road_fatalities_inhabitants", 
                "Road fatalities/100000 inhabitants/year",
                0, 80, info$Road_fatalities_per_100000_inhabitants_per_year, 
                step = 0.1)
  })  
  
  # Slider bar for road_fatalities_vehicles 
  output$road_fatalities_vehicles <- renderUI({
    info <- final_table %>%
      filter(Country == input$country)
    sliderInput("road_fatalities_vehicles", 
                "Road fatalities/100000 motor vehicles/year",
                0, 10000, info$Road_fatalities_per_100000_motor_vehicles, 
                step = 0.1)
  }) 
  
  # Slider bar for cigarette_consumption 
  output$cigarette_consumption <- renderUI({
    info <- final_table %>%
      filter(Country == input$country)
    sliderInput("cigarette_consumption", 
                "Cigarette consumption/year/person",
                0, 7000, info$Cigarette_consumption_per_year_per_person, 
                step = 0.1)
  }) 
  
  # Slider bar for wealth_per_adult 
  output$wealth_per_adult <- renderUI({
    info <- final_table %>%
      filter(Country == input$country)
    sliderInput("wealth_per_adult", "Wealth per adult",
                0, 210000, info$Wealth_per_adult, step = 1)
  }) 
  
  # Slider bar for gdp_per_capita 
  output$gdp_per_capita <- renderUI({
    info <- final_table %>%
      filter(Country == input$country)
    sliderInput("gdp_per_capita", "GDP per capita",
                300, 100000, info$GDP_per_capita, step = 1)
  }) 
  
  # Slider bar for gas_emission 
  output$gas_emission <- renderUI({
    info <- final_table %>%
      filter(Country == input$country)
    sliderInput("gas_emission", "Gas emission",
                0, 15000, info$Gas_emission, step = 0.1)
  }) 
  
  # Action button for country plot
  prediction <- eventReactive(input$prediction_button,{
    result <- as.character(reactive_knn())
    if (result == "high"){
      "High"
    } else if (result == "medium"){
      "Medium"
    } else {
      "Low"
    }
  })
  
  # Output object for the prediction result
  output$knn_result <- renderText({
    prediction()
  })
  
  # Output object for variable importance plot
  output$randomforest <- renderPlot({
    varImpPlot(rand_forest, main = "Variable Importance Plot", color = "brown4")
  }, height = "auto", width = 900)
  
  # Output object for cross-validation table
  output$cvtable <- renderTable({
    cv_table
  })
}

shinyApp(ui = ui, server = server)