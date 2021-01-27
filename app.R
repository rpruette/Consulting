library(dplyr)
library(stringr)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(intrval)
library(ggmap)


ui <- fluidPage(
    theme = shinytheme("journal"),
    
    # Application title
    titlePanel(title = div(tags$img(src = "dallas.jpg", height = 50, width = 50), "Dallas Animal Services -- Impounds Data")),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            style = "overflow-y:scroll; max-height: 600px; position:relative;",
            radioGroupButtons(inputId = "repeats",
                              label = "Select button(s):",
                              choices = c("All Impounds", "Repeats Only"),
                              selected = "All Impounds"),
            checkboxGroupInput(inputId = "year",
                               label = "Select the year:",
                               choices = c(2016, 2017, 2018, 2019, 2020),
                               selected = c(2016, 2017, 2018, 2019, 2020),
                               inline = FALSE),
            selectInput(inputId = "month",
                        label = "Select the month(s):",
                        choices = c("January", "February", "March", "April", "May", "June", "July", 
                                    "August", "September", "October", "November", "December"),
                        selected = c("January", "February", "March", "April", "May", "June", "July", 
                                     "August", "September", "October", "November", "December"),
                        multiple = TRUE),
            selectInput(inputId = "type",
                        label = "Impound type:",
                        choices = c("Confiscate", "Disposal Req", "Keepsafe", "Owner Sur", "Stray", "Transfer", "All types"),
                        selected = "All types"),
            checkboxGroupInput(inputId = "animaltype",
                               label = "Select animal type:",
                               choices = c("Cat", "Dog"),
                               selected = c("Cat", "Dog"),
                               inline = TRUE),
            radioButtons(inputId = "sex",
                        label = "Neutered/Spayed?:",
                        choices = c("Neutered/Spayed", "Not Neutered/Spayed", "Display All"),
                        selected = c("Display All"),
                        inline = FALSE),
            sliderInput(inputId = "age",
                        label = "Select range of animal age:",
                        min = 0,
                        max = 30,
                        step = 0.5,
                        value = c(0, 30)),
            selectInput(inputId = "zipcode",
                        label = "Select a zip code:",
                        choices = c("All zip codes", 75001, 75006, 75007, 75019, 75039, 75040, 75041, 75043, 
                                    75051, 75052, 75056, 75057, 75058, 75060, 75061, 75062, 75063, 75067, 75080, 
                                    75081, 75088, 75089, 75093, 75104, 75114, 75115, 75116, 75134, 75137, 75146, 
                                    75149, 75150, 75154, 75159, 75180, 75181, 75201, 75202, 75203, 75204, 75205, 
                                    75206, 75207, 75208, 75209, 75210, 75211, 75212, 75214, 75215, 75216, 75217, 
                                    75218, 75219, 75220, 75223, 75224, 75225, 75226, 75227, 75228, 75229, 75230, 
                                    75231, 75232, 75233, 75234, 75235, 75236, 75237, 75238, 75240, 75241, 75242, 
                                    75243, 75244, 75246, 75247, 75248, 75249, 75251, 75252, 75253, 75254, 75270, 
                                    75287, 75390, 76012, 76013, 76014, 76017, 76051, 76063, 76107, 76120),
                        selected = "All zip codes"),
            sliderInput(inputId = "zoom",
                        label = "Zoom In/Out",
                        min = -1,
                        max = 3,
                        step = 1,
                        value = 0),
            sliderInput(inputId = "bins",
                        label = "Select bins for Density plot:",
                        min = 10,
                        max = 40,
                        step = 5,
                        value = 30),
            sliderInput(inputId = "alpha",
                        label = "Adjust visibility of raw data:",
                        min = 0,
                        max = 0.5,
                        step = 0.01,
                        value = 0.1),
            submitButton(text = "Apply Changes")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Raw Data", 
                         plotOutput("graphPlot")),
                tabPanel("Density Plot",
                         plotOutput("densityPlot"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    library(dplyr)
    output$graphPlot <- renderPlot({
        month.graph <- input$month %>% 
            str_replace("January", "1") %>% 
            str_replace("February", "2") %>% 
            str_replace("March", "3") %>%
            str_replace("April", "4") %>%
            str_replace("May", "5") %>%
            str_replace("June", "6") %>%
            str_replace("July", "7") %>%
            str_replace("August", "8") %>%
            str_replace("September", "9") %>%
            str_replace("October", "10") %>%
            str_replace("November", "11") %>%
            str_replace("December", "12") %>%
            as.numeric()
        
        if(input$repeats == "All Impounds"){
            repeat.graph <- c(0,1)
        }else{repeat.graph <- 1}
        
        if(input$type == "All types"){
            type.graph <- c("Confiscate", "Disposal Req", "Keepsafe", "Owner Surrender", "Stray", "Transfer")
        }else{type.graph <- input$type}
        
        if(input$sex == "Neutered/Spayed"){
            sex.graph <- c("N", "S")
        }else if(input$sex == "Not Neutered/Spayed"){
            sex.graph <- c("F", "M")
        }else{sex.graph <- c("F", "M", "N", "S", "U")}
        
        impounds_data <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/Impounds/final_impounds_v3.csv")
        impounds_data <- impounds_data %>% 
            filter(year %in% input$year) %>% 
            filter(month %in% month.graph) %>%
            filter(Type %in% toupper(type.graph))%>%
            filter(animal_type %in% toupper(input$animaltype)) %>%
            filter(Repeat %in% repeat.graph) %>%
            filter(sex %in% sex.graph) %>%
            filter(age_num_years %[]% input$age)
        ggmap::register_google(key = "AIzaSyD4cZ61AGLNQ-BELA--YHFX261Sq_hzuUg") 
        
        if(input$zipcode == "All zip codes"){
            center <- "dallas tx"
        }else{
            center <- paste("dallas tx,", input$zipcode)
        }
        
        big_d <- ggmap::get_googlemap(center, zoom = input$zoom + 11, maptype = "terrain")
        ggmap(big_d, extent = "device", padding = 0)+geom_point(aes(x = lon, y = lat), color = "midnightblue", data = impounds_data, alpha = input$alpha, size = 0.5) +
            theme(legend.position = 0)
    })
    
    output$densityPlot <- renderPlot({
        month.graph <- input$month %>% 
            str_replace("January", "1") %>% 
            str_replace("February", "2") %>% 
            str_replace("March", "3") %>%
            str_replace("April", "4") %>%
            str_replace("May", "5") %>%
            str_replace("June", "6") %>%
            str_replace("July", "7") %>%
            str_replace("August", "8") %>%
            str_replace("September", "9") %>%
            str_replace("October", "10") %>%
            str_replace("November", "11") %>%
            str_replace("December", "12") %>%
            as.numeric()
        
        if(input$repeats == "All Impounds"){
            repeat.graph <- c(0,1)
        }else{repeat.graph <- 1}
        
        if(input$type == "All types"){
            type.graph <- c("Confiscate", "Disposal Req", "Keepsafe", "Owner Surrender", "Stray", "Transfer")
        }else{type.graph <- input$type}
        
        if(input$sex == "Neutered/Spayed"){
            sex.graph <- c("N", "S")
        }else if(input$sex == "Not Neutered/Spayed"){
            sex.graph <- c("F", "M")
        }else{sex.graph <- c("F", "M", "N", "S", "U")}
        
        impounds_data <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/Impounds/final_impounds_v3.csv")
        impounds_data <- impounds_data %>% 
            filter(year %in% input$year) %>% 
            filter(month %in% month.graph) %>%
            filter(Type %in% toupper(type.graph)) %>%
            filter(animal_type %in% toupper(input$animaltype))%>%
            filter(Repeat %in% repeat.graph)%>%
            filter(sex %in% sex.graph) %>%
            filter(age_num_years %[]% input$age)
        
        if(input$zipcode == "All zip codes"){
            center <- "dallas tx"
        }else{
            center <- paste("dallas tx,", input$zipcode)
        }
        
        big_d <- ggmap::get_googlemap(center, zoom = input$zoom + 11, maptype = "terrain")
        ggmap(big_d, extent = "device", padding = 0) +
            stat_density2d(aes(x = lon, y = lat, fill = ..level..), size = 0.5, bins = input$bins, 
                           data = impounds_data, geom = "polygon", alpha = 0.1) +
            scale_fill_continuous(name = "Count")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
