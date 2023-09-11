library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(tidyverse)
library(readxl)
library(DT)
library(plyr)
library(data.table)

# read nutrition data table
nutrition <-
  read.table('data/sbux_nutrition.txt', sep = ',', header = TRUE)
nutrition[, c(-1:-3,-7)] <-
  sapply(nutrition[, c(-1:-3,-7)], as.integer)

# subset nutrition data table based on drink type
all_drinks <- sort(unique(nutrition$name))
hot_coffees <-
  sort(unique(nutrition$name[nutrition$type == 'Hot Coffees']))
hot_teas <-
  sort(unique(nutrition$name[nutrition$type == 'Hot Teas']))
hot_drinks <-
  sort(unique(nutrition$name[nutrition$type == 'Hot Drinks']))
all_hot_drinks <- c(hot_coffees, hot_teas, hot_drinks)
frappuccinos <-
  sort(unique(nutrition$name[nutrition$type == 'Frappuccino® Blended Beverages']))
cold_coffees <-
  sort(unique(nutrition$name[nutrition$type == 'Cold Coffees']))
iced_teas <-
  sort(unique(nutrition$name[nutrition$type == 'Iced Teas']))
cold_drinks <-
  sort(unique(nutrition$name[nutrition$type == 'Cold Drinks']))
all_iced_drinks <-
  c(frappuccinos, cold_coffees, iced_teas, cold_drinks)

drink_types <- append(unique(nutrition$type), 'All', after = 0)

# read sweeteners data table
sweeteners <- read_excel('data/starbucks_sweeteners.xlsx')

# read sauces data table and convert
sauces <- read_excel('data/starbucks_sauces.xlsx')
sauces[, c(-1,-2)] <- sapply(sauces[, c(-1,-2)], as.integer)


sauce_names <- sort(unique(sauces$name))

syrups <- read_excel('data/starbucks_syrups.xlsx')
syrups[, c(-1,-2)] <- sapply(syrups[, c(-1,-2)], as.integer)
syrup_names <- sort(unique(syrups$name))

vectorBulletList <- function(vector) {
  if (length(vector) > 0) {
    paste0("<ul><li>",
           paste0(vector, collapse = '</li><li>'),
           '</li></ul>')
  }
}

# Define UI for application
ui <- function(request) {
  header <- dashboardHeader(title = 'Starbucks')
  
  sidebar <- dashboardSidebar(disable = TRUE)
  
  body <- dashboardBody(tags$style(
    HTML(
    '.content-wrapper {
    background-color: #f2f0ea;
    }
    
    .well {
    background-color: #f2f0ea;
    }
    
    .fluid-page-color {
    background-color: #f2f0ea;
    }
    
    .row-fluid {
    background-color: #f2f0ea;
    }
    
    .skin-blue .main-header .logo {
    font-weight: 400;
    background-color: #00754b;
    margin-left: -5.5vh;
    }

    .skin-blue .main-header .navbar{
    background-color: #00754b;
    }
    
    .skin-blue .main-header {
    background-color: #00754b;
    }'
  )),
  
  fluidPage(
    class='fluid-page-color', 
    
    # Application title
    titlePanel(div("Nutrition Calculator", 
               style = 'font-size: 3.5vh;')),
    
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            12,
            radioButtons(
              'type',
              label = 'Drinks',
              choices = drink_types,
              selected = 'All'
            )
          ),
          column(
            12,
            selectizeInput(
              'drink',
              label = NULL,
              choices = all_drinks,
              selected = NULL,
              options = list(
                placeholder = 'Select a drink',
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          ),
          column(
            12,
            selectizeInput(
              'size',
              label = 'Size',
              choices = c('Short', 'Tall', 'Grande', 'Venti'),
              selected = NULL,
              options = list(
                placeholder = 'Select a size',
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          )
        ),
        
        fluidRow(
          column(
            10,
            selectizeInput(
              'liquid_sweeteners',
              label = 'Sweeteners',
              choices = c('Classic Syrup', 'Honey Blend', 'Liquid Cane Sugar'),
              options = list(
                placeholder = 'Add Liquid Sweetener',
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          ),
          column(
            2,
            actionButton("add_liquid_sweeteners",
                         "Add"),
            style = "margin-top: 25px;",
            style = "margin-left: -15px;"
          )
        ),
        fluidRow(
          column(
            10,
            selectizeInput(
              'sweetener_packets',
              label = NULL,
              choices = c(
                'Honey',
                'Splenda',
                'Stevia in the Raw',
                'Sugar',
                'Sugar in the Raw'
              ),
              options = list(
                placeholder = 'Add Sweetener Packet',
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          ),
          column(2,
                 actionButton("add_sweetener_packets",
                              "Add"),
                 style = "margin-left: -15px;")
        ),
        helpText(tags$b('Flavors')),
        fluidRow(
          column(3,
                 numericInput(
                   'sauce_pumps',
                   label = 'Pumps',
                   value = 0
                 )),
          column(
            7,
            selectizeInput(
              'sauce',
              label = 'Sauces',
              choices = sauce_names,
              options = list(
                placeholder = 'Add Sauce',
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            style = "margin-left: -5px;"
          ),
          column(
            2,
            actionButton("add_sauce",
                         "Add"),
            style = "margin-top: 25px;",
            style = "margin-left: -10px;"
          )
        ),
        fluidRow(
          column(3,
                 numericInput(
                   'syrup_pumps',
                   label = 'Pumps',
                   value = 0
                 )),
          column(
            7,
            selectizeInput(
              'syrup',
              label = 'Syrups',
              choices = syrup_names,
              options = list(
                placeholder = 'Add Syrup',
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            style = "margin-left: -5px;"
          ),
          column(
            2,
            actionButton("add_syrup",
                         "Add"),
            style = "margin-top: 25px;",
            style = "margin-left: -10px;"
          )
        )
      ),
      mainPanel(
        fluidRow(
          textOutput('selected_drink'),
          htmlOutput('selected_sweeteners'),
          htmlOutput('selected_sauces'),
          htmlOutput('selected_syrups')
        ),
        br(),
        fluidRow(
          align = 'center',
          tags$h4('Nutrition Facts'),
          tableOutput('nutrition_table')
        )
      )
    )
  ))
  dashboardPage(header, sidebar, body)
}

server <- function(input, output, session) {
  observe({
    if (input$type == 'Hot Coffees') {
      updateSelectInput(session,
                        'drink',
                        choices = hot_coffees)
    } else if (input$type == 'Hot Teas') {
      updateSelectInput(session,
                        'drink',
                        choices = hot_teas)
    } else if (input$type == 'Hot Drinks') {
      updateSelectInput(session,
                        'drink',
                        choices = hot_drinks)
    } else if (input$type == 'Frappuccino® Blended Beverages') {
      updateSelectInput(session,
                        'drink',
                        choices = frappuccinos)
    } else if (input$type == 'Cold Coffees') {
      updateSelectInput(session,
                        'drink',
                        choices = cold_coffees)
    } else if (input$type == 'Iced Teas') {
      updateSelectInput(session,
                        'drink',
                        choices = iced_teas)
    } else if (input$type == 'Cold Drinks') {
      updateSelectInput(session,
                        'drink',
                        choices = cold_drinks)
    }
  })
  
  data <- reactiveVal()
  text <- reactiveVal()
  sweeteners_list <- reactiveValues()
  sauce_list <- reactiveValues()
  syrup_list <- reactiveValues()
  
  onRestore(function(state) {
    data(state$values$data)
    text(state$values$text)
    
    updateSelectizeInput(
      session,
      'drink',
      label = NULL,
      choices = all_drinks,
      selected = state$values$drink,
      options = list(
        placeholder = 'Select a drink',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
    
    updateSelectizeInput(
      session,
      'size',
      label = 'Size',
      choices = c('Short', 'Tall', 'Grande', 'Venti'),
      selected = state$values$size,
      options = list(
        placeholder = 'Select a size',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
    
    sweeteners_list$dList <- state$values$sweeteners
    sauce_list$dList <- state$values$sauce
    syrup_list$dList <- state$values$syrup
  })
  
  observeEvent(c(input$drink, input$size),
               {
                 temp <-
                   nutrition[nutrition$name == input$drink &
                               nutrition$size == input$size, ] %>% select(-1:-3)
                 data(temp)
                 text(paste('You ordered a ',
                            input$size,
                            input$drink))
                 sweeteners_list$dList <- NULL
                 sauce_list$dList <- NULL
                 syrup_list$dList <- NULL
               })
  
  observeEvent(input$add_liquid_sweeteners,
               {
                 temp <-
                   rbindlist(list(data(), sweeteners[sweeteners$name == input$liquid_sweeteners, ][, c(-1)]),
                             fill = TRUE)[, lapply(.SD, sum, na.rm = TRUE)]
                 data(temp)
                 
                 if (input$liquid_sweeteners == '') {
                   sweeteners_list$dList
                 }
                 else {
                   sweeteners_list$dList <-
                     c(isolate(sweeteners_list$dList),
                       isolate(input$liquid_sweeteners))
                   
                   updateSelectizeInput(
                     session,
                     'liquid_sweeteners',
                     label = 'Sweeteners',
                     choices = c('Classic Syrup', 'Honey Blend', 'Liquid Cane Sugar'),
                     options = list(
                       placeholder = 'Add Liquid Sweetener',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
                   )
                 }
               })
  
  observeEvent(input$add_sweetener_packets,
               {
                 temp <-
                   rbindlist(list(data(), sweeteners[sweeteners$name == input$sweetener_packets, ][, c(-1)]),
                             fill = TRUE)[, lapply(.SD, sum, na.rm = TRUE)]
                 data(temp)
                 
                 if (input$sweetener_packets == '') {
                   sweeteners_list$dList
                 }
                 else {
                   sweeteners_list$dList <-
                     c(isolate(sweeteners_list$dList),
                       isolate(input$sweetener_packets))
                   updateSelectizeInput(
                     session,
                     'sweetener_packets',
                     label = NULL,
                     choices = c(
                       'Honey',
                       'Splenda',
                       'Stevia in the; Raw',
                       'Sugar',
                       'Sugar in the Raw'
                     ),
                     options = list(
                       placeholder = 'Add Sweetener Packet',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
                   )
                 }
               })
  
  observeEvent(input$add_sauce,
               {
                 if (input$drink %in% all_hot_drinks) {
                   sauces <- sauces[sauces$temp == 'hot', ]
                 }
                 else {
                   sauces <- sauces[sauces$temp == 'cold', ]
                 }
                 
                 temp <-
                   rbindlist(list(data(), input$sauce_pumps * sauces[sauces$name == input$sauce, ][, c(-1, -2)]),
                             fill = TRUE)[, lapply(.SD, sum, na.rm = TRUE)]
                 data(temp)
                 
                 if (input$sauce_pumps == 0 || input$sauce == '') {
                   sauce_list$dList
                 }
                 else if (input$sauce_pumps == 1) {
                   sauce_list$dList <-
                     c(isolate(sauce_list$dList),
                       paste(input$sauce_pumps,
                             'pump of',
                             input$sauce))
                 }
                 else if (input$sauce_pumps >= 1) {
                   sauce_list$dList <-
                     c(isolate(sauce_list$dList),
                       paste(input$sauce_pumps,
                             'pumps of',
                             input$sauce))
                 }
                 
                 updateSelectizeInput(
                   session,
                   'sauce',
                   label = 'Sauces',
                   choices = sauce_names,
                   options = list(
                     placeholder = 'Add Sauce',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
                 )
                 
                 updateNumericInput(session,
                                    'sauce_pumps',
                                    label = 'Pumps',
                                    value = 0)
               })
  
  observeEvent(input$add_syrup,
               {
                 if (input$drink %in% all_hot_drinks) {
                   sauces <- sauces[sauces$temp == 'hot', ]
                 }
                 else {
                   sauces <- sauces[sauces$temp == 'cold', ]
                 }
                 
                 temp <-
                   rbindlist(list(data(), input$syrup_pumps * syrups[syrups$name == input$syrup, ][, c(-1, -2)]),
                             fill = TRUE)[, lapply(.SD, sum, na.rm = TRUE)]
                 data(temp)
                 
                 if (input$syrup_pumps == 0 || input$syrup == '') {
                   syrup_list$dList
                 }
                 else if (input$syrup_pumps == 1) {
                   syrup_list$dList <-
                     c(isolate(syrup_list$dList),
                       paste(input$syrup_pumps,
                             'pump of',
                             input$syrup))
                 }
                 else if (input$syrup_pumps >= 1) {
                   syrup_list$dList <-
                     c(isolate(syrup_list$dList),
                       paste(input$syrup_pumps,
                             'pumps of',
                             input$syrup))
                 }
                 
                 updateSelectizeInput(
                   session,
                   'syrup',
                   label = 'Syrups',
                   choices = syrup_names,
                   options = list(
                     placeholder = 'Add Syrup',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
                 )
                 
                 updateNumericInput(session,
                                    'syrup_pumps',
                                    label = 'Pumps',
                                    value = 0)
               })
  
  output$selected_drink <- renderText({
    text()
  })
  
  output$selected_sweeteners <- renderText({
    vectorBulletList(sweeteners_list$dList)
  })
  
  output$selected_sauces <- renderText({
    vectorBulletList(sauce_list$dList)
  })
  
  output$selected_syrups <- renderText({
    vectorBulletList(syrup_list$dList)
  })
  
  output$nutrition_table <- renderTable({
    validate(
      need(input$drink, "Please select a drink"),
      need(input$size, "Please select a size")
    )
    validate(
      need(
        data() > 0,
        "No nutritional information for this size! Please select another size."
      )
    )
    c(
      Calories = data()$calories,
      Fat = paste(data()$fat, ' ', 'g'),
      Cholesterol = paste(data()$cholesterol, ' ', 'mg'),
      Sodium = paste(data()$sodium, ' ', 'mg'),
      Carb = paste(data()$carb, ' ', 'g'),
      Sugar = paste(data()$sugar, ' ', 'g'),
      Protein = paste(data()$protein, ' ', 'g'),
      Caffeine = paste(data()$caffeine, ' ', 'mg')
    ) %>% enframe()
  },
  digits = 0,
  height = '1000px',
  colnames = FALSE,
  rownames = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)
