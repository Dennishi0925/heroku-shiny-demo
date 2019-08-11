# library(shiny)
# library(dplyr)
# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
#    # Application title
#    titlePanel("Old Faithful Geyser Data"),
# 
#    helpText(paste("Process Id", Sys.getpid())),
# 
#    # Sidebar with a slider input for number of bins
#    sidebarLayout(
#       sidebarPanel(
#          sliderInput("bins",
#                      "Number of 你好:",
#                      min = 1,
#                      max = 50,
#                      value = 30)
#       ),
# 
#       # Show a plot of the generated distribution
#       mainPanel(
#          plotOutput("distPlot")
#       )
#    )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output, session) {
#    session$allowReconnect("force")
#    output$distPlot <- renderPlot({
#       # generate bins based on input$bins from ui.R
#       x    <- faithful[, 2]
#       bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#       # draw the histogram with the specified number of bins
#       hist(x, breaks = bins, col = 'darkgray', border = 'white')
#    })
# }
# 
# # Run the application
# shinyApp(ui = ui, server = server)








pkgs <- c('showtext', "shiny", 'shinythemes', 'shinyjs', 'shinydashboard',
          'shinyWidgets', 'DT', 'plotly',
          'magrittr', 'dplyr', 'readr', 'stringr')

lapply(pkgs, require, character.only = TRUE)
showtext_auto()
options(shiny.usecairo = FALSE)

table_index_print <- read_rds("table_index_print_eng.rds")
table_detail_print <- read_rds("table_detail_print_eng.rds")

getdeps <- function() {
   htmltools::attachDependencies(
      htmltools::tagList(),
      c(
         htmlwidgets:::getDependency("datatables","DT")
      )
   )
}

transpose_df <- function(df) {
   t_df <- data.table::transpose(df)
   colnames(t_df) <- rownames(df)
   rownames(t_df) <- colnames(df)
   t_df <- t_df %>%
      tibble::rownames_to_column(.data = .) %>%
      tibble::as_data_frame(x = .)
   return(t_df)
}

ui <- fluidPage(theme = shinytheme("yeti"),
                #tags$head(includeScript("google_analytics.js")),
                titlePanel("Open NTU Course Selection"),
                tabsetPanel(type = "tabs",
                            #tabPanel("Map", align="center", plotlyOutput("statebins", width = "900px", height = "600px")),
                            tabPanel("Searchable Table",
                                     fluidPage(
                                        checkboxGroupButtons(inputId = "mandotary", 
                                                             label = "Label",
                                                             choices = c("必" = "必",
                                                                         "選" = "選"),
                                                             selected = c("必", "選"),
                                                             checkIcon = list(
                                                                yes = icon("ok",
                                                                           lib = "glyphicon")
                                                             )
                                        ),
                                        checkboxGroupButtons(inputId = "Monday", 
                                                             label = NULL,
                                                             choices  = c('一1'='一1', '一2'='一2', '一3'='一3', '一4'='一4', '一5'='一5', '一6'='一6', '一7'='一7', '一8'='一8', '一9'='一9', '一10'='一10', '一A'='一A', '一B'='一B', '一C'='一C'),
                                                             selected = c('一1'='一1', '一2'='一2', '一3'='一3', '一4'='一4', '一5'='一5', '一6'='一6', '一7'='一7', '一8'='一8', '一9'='一9', '一10'='一10', '一A'='一A', '一B'='一B', '一C'='一C'),
                                                             size =  'xs',
                                                             checkIcon = list(yes = icon("ok",lib = "glyphicon")
                                                             )
                                        ),
                                        checkboxGroupButtons(inputId = "Tuesday", 
                                                             label = NULL,
                                                             choices  = c('二1'='二1', '二2'='二2', '二3'='二3', '二4'='二4', '二5'='二5', '二6'='二6', '二7'='二7', '二8'='二8', '二9'='二9', '二10'='二10', '二A'='二A', '二B'='二B', '二C'='二C'),
                                                             selected = c('二1'='二1', '二2'='二2', '二3'='二3', '二4'='二4', '二5'='二5', '二6'='二6', '二7'='二7', '二8'='二8', '二9'='二9', '二10'='二10', '二A'='二A', '二B'='二B', '二C'='二C'),
                                                             size =  'xs',
                                                             checkIcon = list(yes = icon("ok",lib = "glyphicon")
                                                             )
                                        ),
                                        checkboxGroupButtons(inputId = "Wednesday", 
                                                             label = NULL,
                                                             choices  = c('三1'='三1', '三2'='三2', '三3'='三3', '三4'='三4', '三5'='三5', '三6'='三6', '三7'='三7', '三8'='三8', '三9'='三9', '三10'='三10', '三A'='三A', '三B'='三B', '三C'='三C'),
                                                             selected = c('三1'='三1', '三2'='三2', '三3'='三3', '三4'='三4', '三5'='三5', '三6'='三6', '三7'='三7', '三8'='三8', '三9'='三9', '三10'='三10', '三A'='三A', '三B'='三B', '三C'='三C'),
                                                             size =  'xs',
                                                             checkIcon = list(yes = icon("ok",lib = "glyphicon")
                                                             )
                                        ),
                                        checkboxGroupButtons(inputId = "Thursday", 
                                                             label = NULL,
                                                             choices  = c('四1'='四1', '四2'='四2', '四3'='四3', '四4'='四4', '四5'='四5', '四6'='四6', '四7'='四7', '四8'='四8', '四9'='四9', '四10'='四10', '四A'='四A', '四B'='四B', '四C'='四C'),
                                                             selected = c('四1'='四1', '四2'='四2', '四3'='四3', '四4'='四4', '四5'='四5', '四6'='四6', '四7'='四7', '四8'='四8', '四9'='四9', '四10'='四10', '四A'='四A', '四B'='四B', '四C'='四C'),
                                                             size =  'xs',
                                                             checkIcon = list(yes = icon("ok",lib = "glyphicon")
                                                             )
                                        ),
                                        checkboxGroupButtons(inputId = "Friday", 
                                                             label = NULL,
                                                             choices  = c('五1'='五1', '五2'='五2', '五3'='五3', '五4'='五4', '五5'='五5', '五6'='五6', '五7'='五7', '五8'='五8', '五9'='五9', '五10'='五10', '五A'='五A', '五B'='五B', '五C'='五C'),
                                                             selected = c('五1'='五1', '五2'='五2', '五3'='五3', '五4'='五4', '五5'='五5', '五6'='五6', '五7'='五7', '五8'='五8', '五9'='五9', '五10'='五10', '五A'='五A', '五B'='五B', '五C'='五C'),
                                                             size =  'xs',
                                                             checkIcon = list(yes = icon("ok",lib = "glyphicon")
                                                             )
                                        ),
                                        checkboxGroupButtons(inputId = "Saturday", 
                                                             label = NULL,
                                                             choices  = c('六1'='六1', '六2'='六2', '六3'='六3', '六4'='六4', '六5'='六5', '六6'='六6', '六7'='六7', '六8'='六8', '六9'='六9', '六10'='六10', '六A'='六A', '六B'='六B', '六C'='六C'),
                                                             selected = c('六1'='六1', '六2'='六2', '六3'='六3', '六4'='六4', '六5'='六5', '六6'='六6', '六7'='六7', '六8'='六8', '六9'='六9', '六10'='六10', '六A'='六A', '六B'='六B', '六C'='六C'),
                                                             size =  'xs',
                                                             checkIcon = list(yes = icon("ok",lib = "glyphicon")
                                                             )
                                        ),
                                        column(4,
                                               selectizeInput("course_credit", "Choose course_credit",
                                                              c("All"= "All", levels(table_index_print$course_credit)),
                                                              multiple = TRUE,
                                                              selected = "3",
                                                              width = '200px'
                                               )),
                                        column(4,
                                               selectizeInput("course_TA", "Choose State(s)",
                                                              c("All"= "All", levels(table_index_print$course_TA)),
                                                              multiple = TRUE,
                                                              selected = "工管系",
                                                              width = '200px'
                                               )),
                                        column(12,
                                               DTOutput("data" )))
                            ),
                            # tabPanel("Course Info", align="center", numericInput("length","Length",0,0,10)
                            #          ),
                            # tabPanel("Course Info", inputPanel(
                            #   numericInput("course_ID2", label = "courseID:", 0, min(table_index_print$course_ID), max(table_index_print$course_ID))
                            # ),
                            tabPanel("Course Info", textOutput("page2"),
                                     #numericInput("course_ID2", label = "courseID:", 0, min(table_index_print$course_ID), max(table_index_print$course_ID)),
                                     DT::dataTableOutput("table2"),getdeps()
                            ),
                            
                            #textOutput("page2", DT::dataTableOutput("table2"),getdeps()),
                            tabPanel("About",
                                     fluidPage(
                                        column(10,
                                               h3("An App to View NTU Course Data More Easily"),
                                               p("I created this app to improve students' experiences when viewing course data",
                                                 a("a list",
                                                   href = "http://cawp.rutgers.edu/buzz-2018-potential-women-candidates-us-congress-and-statewide-elected-executive",
                                                   target = "_blank"),
                                                 "of women potentially running for US Congress and State offices. The data are current as of 6/20/18."),
                                               p(a("R", href = "https://www.r-project.org", target = "_blank"),
                                                 "code and details of data processing and visualization are available on",
                                                 a("GitHub.", icon("github"), href = "https://github.com/JListman/Scrape_WomenRunning_CAWP", target = "_blank"),
                                                 "Find me, Jenny Listman, on", a("Twitter", icon("twitter"),
                                                                                 href = "https://twitter.com/jblistman", target = "_blank"),
                                                 a("LinkedIn", icon("linkedin"),
                                                   href = "https://www.linkedin.com/in/jenniferlistman/", target = "_blank"),
                                                 "or read more about my data science and consulting work on my",
                                                 a("website.", href = "https://twitter.com/jblistman", target = "_blank")),
                                               HTML("<br><br><br>")
                                               #)
                                        )
                                     )
                            ),
                            tags$div(class="footer", checked=NA, tags$p("An interactive app to view course data powered by ",
                                                                        a("Dennis", href = "http://www.cawp.rutgers.edu", target = "_blank")),
                                     tags$p(a("View code",icon("github"), href = "https://github.com/JListman/Scrape_WomenRunning_CAWP", target = "_blank"))
                            )
                )
)

server <- function(input, output) {
   
   output$data <- renderDataTable({
      weekday = c(input$Monday,input$Tuesday,input$Tuesday,input$Thursday,input$Friday,input$Saturday)
      
      if ("All" %in% input$course_credit & "All" %in% input$course_TA)
         datatable(data.frame(table_index_print  %>%
                                 mutate(course_ID = paste0("<a href='#table2'", "alt='",course_ID,                                    
                                                           "'onclick=\"tabs = $('.tabbable .nav.nav-tabs li');tabs.each(function() {$(this).removeClass('active')});$(tabs[1]).addClass('active');tabsContents = $('.tabbable .tab-content .tab-pane');tabsContents.each(function() {$(this).removeClass('active')});$(tabsContents[1]).addClass('active');$('#table2').trigger('change').trigger('shown');Shiny.onInputChange('course_ID', getAttribute('alt'));\">",
                                                           course_ID,"</a>"))
         ),filter = list(position = 'top', clear = FALSE), escape = FALSE)
      else if ("All" %in% input$course_credit & input$course_TA != "All")
         datatable(data.frame(table_index_print %>% filter(course_TA %in% input$course_TA)  %>%
                                 mutate(course_ID = paste0("<a href='#table2'", "alt='",course_ID,                                    
                                                           "'onclick=\"tabs = $('.tabbable .nav.nav-tabs li');tabs.each(function() {$(this).removeClass('active')});$(tabs[1]).addClass('active');tabsContents = $('.tabbable .tab-content .tab-pane');tabsContents.each(function() {$(this).removeClass('active')});$(tabsContents[1]).addClass('active');$('#table2').trigger('change').trigger('shown');Shiny.onInputChange('course_ID', getAttribute('alt'));\">",
                                                           course_ID,"</a>"))
         ),filter = list(position = 'top', clear = FALSE), escape = FALSE)# %>% arrange(desc(college))
      else if (input$course_credit != "All" & "All" %in% input$course_TA)
         datatable(data.frame(table_index_print %>% filter(course_credit %in% input$course_credit) %>%
                                 mutate(course_ID = paste0("<a href='#table2'", "alt='",course_ID,                                    
                                                           "'onclick=\"tabs = $('.tabbable .nav.nav-tabs li');tabs.each(function() {$(this).removeClass('active')});$(tabs[1]).addClass('active');tabsContents = $('.tabbable .tab-content .tab-pane');tabsContents.each(function() {$(this).removeClass('active')});$(tabsContents[1]).addClass('active');$('#table2').trigger('change').trigger('shown');Shiny.onInputChange('course_ID', getAttribute('alt'));\">",
                                                           course_ID,"</a>"))
         ),filter = list(position = 'top', clear = FALSE), escape = FALSE)# %>% arrange(desc(college))
      else
         datatable(data.frame(table_index_print %>%
                                 filter(str_detect(course_mandatory,str_c(input$mandotary, collapse = "|"))) %>%#weekday
                                 filter(str_detect(course_timeplace,str_c(weekday, collapse = "|"))) %>%#weekday
                                 filter(course_credit %in% input$course_credit) %>% filter(course_TA %in% input$course_TA) %>%
                                 mutate(course_ID = paste0("<a href='#table2'", "alt='",course_ID,                                    
                                                           "'onclick=\"tabs = $('.tabbable .nav.nav-tabs li');tabs.each(function() {$(this).removeClass('active')});$(tabs[1]).addClass('active');tabsContents = $('.tabbable .tab-content .tab-pane');tabsContents.each(function() {$(this).removeClass('active')});$(tabsContents[1]).addClass('active');$('#table2').trigger('change').trigger('shown');Shiny.onInputChange('course_ID', getAttribute('alt'));\">",
                                                           course_ID,"</a>"))
         ),filter = list(position = 'top', clear = FALSE), escape = FALSE)
   }
   #,escape = FALSE
   
   
   # callback = JS(
   #   'table.on("click.dt", "tr", function() {
   #   tabs = $(".tabbable .nav.nav-tabs li a");
   #   $(tabs[1]).click();})'
   # )
   
   #   callback=JS(
   #     'table.on("click.dt", "tr", function() {
   #     
   #     tabs = $(".tabbable .nav.nav-tabs li a");
   #     var data=table.row(this).data();
   #     document.getElementById("course_ID").value=data[1];
   #     Shiny.onInputChange("course_ID",data[1]);
   #     $(tabs[1]).click();
   #     table.row(this).deselect();
   # })'
   #                                       )
   )
   #var data=table.row(this).data();
   #var data=table.cell(".selected", 0).data();
   #['title']
   output$table2 <- DT::renderDataTable({
      # selected <- input$data_rows_selected
      # if(is.null(selected)){
      #   datatable(table_detail_print)
      # } else {
      #   datatable(table_detail_print %>% filter(course_ID==input$course_ID) %>% select(-course_ID) %>% transpose_df())
      # }
      # 
      if(!is.null(input$course_ID)){
         datatable(table_detail_print %>% filter(course_ID==input$course_ID) %>% select(-course_ID) %>% transpose_df())
         # }else if(!is.null(input$course_ID2)){
         #   datatable(table_detail_print)
      }else {
         datatable(table_detail_print)
      }
   })
   
   output$page2 <- renderText({
      #print(input$course_ID)
      course_name_print = table_index_print %>% filter(course_ID == input$course_ID) %>% pull(course_name)
      paste0("Detailed course information for  ", course_name_print, " :")
      
   })
   
}
# table_detail_print %>% filter(course_ID == 8800)
shinyApp(ui = ui, server = server)









