library(shiny)
library(shinyauthr)
library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(shinyalert)
library(ggridges)
library(scales)
library(RColorBrewer)
library(tidyr)

#dataframe in care sunt stocate date despre user: username, password
user_base <- tibble::tibble(
  user = c("admin", "user"),
  password = sapply(c("admin", "user"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

movies_data <- read_csv("data/movies.csv")  #utilizată la choise
movies_data_jh <- read_csv("data/movies.csv") #utilizată la liste cu valori unice
genre_choices <- c('Toate genurile', sort(unique(movies_data$genre)))
rating_choices <- c('Ratings', sort(unique(movies_data$rating)))
unique_years <- c(sort(unique(movies_data_jh$year)))
measures <- c('score', 'gross')
col_vals <- c(1,2,3,4,6,13)
genre_options <- c(sort(unique(movies_data_jh$genre)))

ui <- fluidPage(
  # logout button
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  
  # login section
  shinyauthr::loginUI(id = "login"),
  
  # Sidebar care se va afisa dupa login
  uiOutput("sidebarpanel"),
  
  #Style (elemente css)
  tags$head(
    tags$title("Dashboard"),
    tags$style(HTML("
     .tabbable .nav-tabs, .nav-tabs>li{
         border-bottom: solid 1px #dddddd;
     }
    .col-sm-12 .row{
        display: flex;
        flex-direction: row;
        padding: 30px 52px;
        align-content: flex-start;
        align-items: center;
     }
     .col-sm-12 .row .col-sm-6:nth-child(1){
     width: fit-content;
     }
     
     .row .col-sm-12 .col-sm-8{
     padding-left: 52px;
     }
     .tabbable{
         max-width: 97%;
     }
     .col-sm-6 h2:nth-child(2){
     margin: 0px;
     }
     
       @media all and (max-device-width: 600px){
                      .tabbable .nav-tabs, .nav-tabs{
                      display: flex;
                      flex-direction: column;
                          max-width: 60%;
                      }
       }
                    .nav-tabs, .tabbable .nav-tabs{
                        border-bottom: none;
                    }
                    
                    .pull-right{
                        position: absolute;
                        top: 0;
                        right: 0;
                        z-index: 3;
                    }
                   .tab-content .tab-pane > h1 > p{
                    font-size: 30px;
                   }
                    .shiny-split-layout{
                        display: flex;
                        width: 100%;
                        white-space: nowrap;
                        flex-direction: column;
                    }
                    
                    .shiny-split-layout>div{
                    width:100% !important;
                    }
                    
                    .shiny-split-layout .shiny-plot-output img{
                    width:100%;
                    }
                    
                    .col-sm-12 .row{
                      display: flex;
                      flex-direction: column;
                      padding: 0px 22px;
                      align-content: flex-start;
                      align-items: flex-start;
                      justify-content: center;
                    }
                    
                    .row .col-sm-12 .col-sm-8{
                        padding-left: 22px;
                    }
                    
                   .tab-pane:nth-child(4) .row:nth-child(3){
                       background-color: #E7EAEF;
                        padding: 32px 0;
                   }
     "
                  
                    )),
    #Icon: https://fontawesome.com/  -- se comporta ca bootstrap
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css")
    
  ),
  theme = shinytheme("united")
  
)

server <- function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  #Crearea paginilor (tab-urilor folosind: fluidRow, tabsetPanel, tabPanel)
  output$sidebarpanel <- renderUI({
    
    # Se va afisa doar dupa autentificare
    req(credentials()$user_auth)
    fluidRow(
      ##Se crează o listă de componente in linie, reprezentand navbarul 
      #Alternativa la tabsetPanel: navlistPanel
      tabsetPanel(
        
        id = "tab_being_displayed",
        
        #Prima pagina
        tabPanel((strong("Scor")),
                 
                 useShinyalert(),
                 tags$head(tags$style(HTML('.nav .active a{ background-color:#79c5f0 !important; color:#fff !important}',
                                           '.nav a {background-color: #fff !important; color:#79c5f0 !important; font-weight: 700 !important;     font-size: 18px !important;}'
                 ))),
                 
                 titlePanel(h1(p(
                   strong("Comparația filmelor pe genuri")), align = "center")),
                 
                 sidebarLayout(
                   
                   sidebarPanel(
                     
                     h3(p(strong("Sortarea datelor"))),
                     
                     h5("Utilizați funcționalitatea de sortare de mai jos pentru a opține informații detaliate pentru două genuri de filme. 😊"),
                     br(),
                     
                     h3("Input pentru primul grafic"),
                     
                     selectInput(inputId = "genre_input_1", label = "Genul filmului",
                                 choices = genre_choices,
                                 selected = "Toate genurile"),
                     br(),
                     
                     h3("Input pentru al doilea grafic"),
                     
                     selectInput(inputId = "genre_input_2", label = "Genul filmului",
                                 choices = genre_choices,
                                 selected = "Toate genurile"),
                     hr(),
                     
                     h3(p(strong("Importanța vizualizării graficelor"))),
                     
                     h5("Scopul principal îl constituie compararea și alegerea genului care are cel mai bun rating."),
                     
                     br(),
                     
                     img(src = "https://static.vecteezy.com/system/resources/previews/011/720/979/original/cinema-tickets-with-pop-corn-drink-amd-glasses-free-png.png",
                         
                         width="45%", height="45%", alt = "cinema Logo",
                         style="display: block; margin-left: auto; margin-right: auto;")
                   ),
                   
                   mainPanel(
                     fluidRow(
                       splitLayout(cellWidths = c("50%", "50%"),
                                   plotOutput(outputId = "density_plot_1"),
                                   plotOutput(outputId = "density_plot_2")),
                       fluidRow(plotOutput(outputId = "ridge_plot_1")))
                   )
                 )),
        
        tabPanel("Venituri și cheltuieli",
                 
                 titlePanel(h1(p(strong("Venituri și cheltuieli de-a lungul anilor")), align = "center")),
                 
                 sidebarLayout(
                   
                   sidebarPanel(
                     
                     h3(p(strong("Selectează genul și anul 😊"))),
                     
                     h5("Analizați veniturile și investițiile filmelor."),
                     
                     h5("Utilizați filtrele de mai jos pentru a afla mai multe informatii."),
                     
                     br(),
                     
                     h3("Genul filmului"),
                     
                     selectInput(inputId = "var", label = "Alege un gen",
                                 choices = genre_choices,
                                 selected = "Toate genurile"),
                     br(),
                     
                     h3("Tipul filmului"),
                     
                     selectInput(inputId = "var2", label = "Alege valoarea rating-ului",
                                 choices = rating_choices,
                                 selected = "Toate scorurile"),
                     br(),
                     
                     sliderInput(inputId = "alpha_level", label = "Transparența graficului",
                                 min = 0, max = 1, value = 0.8, step = 0.2),
                     br(),
                     
                     img(src = "https://static.vecteezy.com/system/resources/previews/011/720/979/original/cinema-tickets-with-pop-corn-drink-amd-glasses-free-png.png",
                         width="45%", height="45%", alt = "IMDb Logo",
                         style="display: block; margin-left: auto; margin-right: auto;")
                     
                   ),
                   
                   mainPanel(
                     plotOutput(outputId = "bar"),
                     
                     br(),
                     
                     h6(p(strong("Barele sunt imbricate permițând")), align = "center"),
                     
                     h6(p(strong("o vizualizare și o comparație mult mai bună între buget și venituri")), align = "center")
                     
                   )
                 )),
        
        tabPanel("Topul filmelor",
                 
                 titlePanel(h1(p(strong("Topul filmelor în funcție de venituri sau scor")), align = "center")),
                 
                 sidebarLayout(
                   
                   sidebarPanel(
                     
                     h3(p(strong("Selectează genul sau anul"))),
                     
                     h5("Observați veniturile obținute pentru fiecare categorie de film"),
                     
                     br(),
                     
                     h3("Genul"),
                     
                     selectInput(inputId = "genre", label = "Selectează genul",
                                 choices = genre_options,
                                 selected = "Action"),
                     br(),
                     
                     h3("Anul"),
                     
                     selectInput(inputId = "year", label = "Alege un an",
                                 choices = unique_years,
                                 selected = "2009"),
                     br(),
                     
                     h3("Criterii de analiză"),
                     
                     selectInput(inputId = "measure", label = "Selectează un criteriu",
                                 choices = c('score', 'gross'),
                                 selected = 'gross'),
                     br(),
                     
                     img(src = "https://static.vecteezy.com/system/resources/previews/011/720/979/original/cinema-tickets-with-pop-corn-drink-amd-glasses-free-png.png",
                         width="45%", height="45%", alt = "Movie Logo",
                         style="display: block; margin-left: auto; margin-right: auto;")
                     
                   ),
                   
                   mainPanel(
                     plotOutput(outputId = "movies_bar"),
                     
                     checkboxInput(inputId = "show_table", label = "Vezi datele",
                                   value = FALSE),
                     
                     DT::dataTableOutput(outputId = "movies_table")
                   )
                 )),
        
        tabPanel("Despre",
                 
                 verticalLayout(
                   
                   titlePanel(h1(p(strong("Despre lucrarea de cercetare și autor")), align = "center")),
                   br(),
                   
                   grid <- tagList(
                     fluidRow(
                       column(
                         width = 6,
                         tags$img(src = "https://avatars.githubusercontent.com/u/113201109?v=4", style="max-width: 200px;
    border-radius: 18px;
    border: 2px solid; box-shadow: 0 12px 12px 0 rgba(0, 0, 0, 0.5), 0 10px 24px 0 rgba(0, 0, 0, 0.39);")                   ),
                       column(
                         width = 6,
                         h2("Nume:" , span("Lupancu")),
                         h2("Prenume:" , span("Denisa-Petronela")),
                         h4("Data nașterii:" , span("11/06/2001")),
                         h4("Contact: ", span("denisalupancu@gmail.com")),
                         h4("GitHub:", a(tags$i(class = "fab fa-github"), href = "https://github.com/denisa110"))
                         
                       )
                     )
                   ),
                   
                   
                   mainPanel(h4("  Contribuția mea, pentru această cercetare constă în transpunerea cunoștințelor tehnologice adaptate contextului economic și cinematografic. "),
                             h4(" Prin intermediul aplicației, utilizatorii pot compara performanța filmelor între ele 
                                sau cu industria în ansamblu. Pot fi create grafice și rapoarte comparative pentru a evidenția 
                                diferențele și a identifica filmele de succes sau tendințele emergente. "),
                             h4("Pentru mai multe metode de analizare a datelor utilizate în cadrul lucrării de cercetare accesați ", a("aici", href = "https://github.com/denisa110"), "."),
                             br(),)
                 )
        ))
    )
  })
  
  observeEvent(req(input$genre_input_1 == c('History', 'Music', 'Sport')), {
    shinyalert("Date insuficiente pentru generarea unui grafic concludent.", type = "error")
  })
  
  observeEvent(req(input$genre_input_2 == c('History', 'Music', 'Sport')), {
    shinyalert("Date insuficiente pentru generarea unui grafic concludent.", type = "error")
  })
  
  # Plot
  output$density_plot_1 <- renderPlot({
    #functie shiny care ajuta la afisarea 
    movies_data %>%
      filter(if(input$genre_input_1 != 'Toate genurile') (genre == input$genre_input_1) else TRUE) %>%
      ggplot(aes(score)) +
      geom_density(stat = "density",
                   color = "#4169E1",
                   lwd = 1, 
                   fill = "#79c5f0",
                   alpha = 0.8,
                   kernel = "gaussian") +
      labs(x = "Scorul filmelor (de la 1 la 10) ",
           y = "Densitatea",
           title = paste0("Densitatea scorurilor pentru filmele de ",input$genre_input_1)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(face = "bold")) +
      theme(axis.title = element_text(face = "bold"))
  })
  
  output$density_plot_2 <- renderPlot({
    
    movies_data %>%
      filter(if(input$genre_input_2 != 'Toate genurile') (genre == input$genre_input_2) else TRUE) %>%
      ggplot(aes(score)) +
      geom_density(stat = "density",
                   color = "darkorange4",
                   lwd = 1,
                   fill = "#D2042D",
                   alpha = 0.8,
                   kernel = "gaussian") +
      labs(x = "Scorul filmelor (de la 1 la 10)",
           y = "Densitatea",
           title = paste0("Densitatea scorurilor pentru filmele de ",input$genre_input_2)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(face = "bold")) +
      theme(axis.title = element_text(face = "bold"))
  })
  
  output$ridge_plot_1 <- renderPlot({
    
    movies_data %>%
      ggplot(aes(score, genre, fill = genre)) +
      geom_density_ridges(scale = 4, size = 1, alpha = 0.7) +
      scale_y_discrete(limits = rev) +
      scale_fill_cyclical(
        name = "Color scheme",
        values = c( "#D2042D","#79c5f0")
      ) +
      labs(x = "Scorul filmelor (de la 1 la 10)",
           y = "Genul filmelor",
           title = "Densitatea scorurilor în comparație cu toate genurile") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(face = "bold")) +
      theme(axis.title = element_text(face = "bold"))
  })
  output$ridge_plot_1 <- renderPlot({
    
    movies_data %>%
      ggplot(aes(score, genre, fill = genre)) +
      geom_density_ridges(scale = 4, size = 1, alpha = 0.7) +
      scale_y_discrete(limits = rev) +
      scale_fill_cyclical(
        name = "Color scheme",
        values = c( "#D2042D","#79c5f0")
      ) +
      labs(x = "Scorul filmelor (de la 1 la 10)",
           y = "Genul filmelor",
           title = "Densitatea scorurilor în comparație cu toate genurile") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(face = "bold")) +
      theme(axis.title = element_text(face = "bold"))
  })
  
  #---------------------------------- Grafic cu bare imbricate (NESTED BARS) ----------------------------------# 
  
  output$bar <- renderPlot({
    movies_data %>%
      filter(if(input$var != 'Toate genurile' ) (genre == input$var) else TRUE) %>%
      filter(if(input$var2 != 'Ratings' ) (rating == input$var2) else TRUE) %>%
      ggplot(aes(x=year)) + 
      geom_bar(aes(y = gross, fill="Budget"), alpha = 0.9, size = 1.1, stat='identity') +
      geom_bar(aes(y = budget, fill = "Gross"), alpha = input$alpha_level, size = 1.1, stat='identity') +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = c("#79c5f0","#D2042D"),
                        labels = c("Gross", "Budget")) +
      labs(x = "Anul realizării filmului",
           y = "Bugetul filmului și venitul brut în dolari",
           title=paste0("Performanța pentru ",input$var," care are rating-ul ",input$var2),
           fill = "Legenda") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(face = "bold")) +
      theme(axis.title = element_text(face = "bold")) +
      theme(legend.position = 'right')
  })
  
  #---------------------------------- Top 10 si 100 filme pentru grafic de tip bars ----------------------------------# 
  
  top_10 <- reactive({as.data.frame(
    movies_data_jh %>%
      replace_na(list(gross=0, score=0)) %>%
      filter(year == input$year) %>%
      filter(genre == input$genre) %>%
      arrange(input$measure) %>%
      top_n(10))
    
  })
  
  top_100 <- reactive({as.data.frame(
    movies_data_jh %>%
      replace_na(list(gross=0, score=0)) %>%
      filter(year == input$year) %>%
      filter(genre == input$genre) %>%
      arrange(input$measure) %>%
      top_n(100))
    
  })
  
  output$movies_bar <- renderPlot({
    temp <- as.data.frame(top_10())
    m_bar <-temp %>% ggplot(
      aes(x = reorder(name,get(input$measure)), y = get(input$measure))) +
      geom_bar(stat = 'identity', fill = "dodgerblue1", alpha = 0.8) + 
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(x = element_blank(),
           y = str_to_title(input$measure),
           title=paste0("Top 10 filme în funcție de ",str_to_title(input$measure)," și de genul ",input$genre," pentru anul ",input$year)) + 
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(face = "bold")) +
      theme(axis.title = element_text(face = "bold"))
    
    m_bar
  })
  
  output$movies_table <- DT::renderDataTable({
    if(input$show_table){
      DT::datatable(top_100()[, col_vals], rownames = FALSE)
    }})
}

shinyApp(ui = ui, server = server)

