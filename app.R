pkgs <- c("shiny", 'shinythemes', 
          'shinyjs', #'shinydashboard',
          'shinyWidgets', 'DT', 
          'data.table', 'showtext', #'plotly', 
          'magrittr', 'dplyr', #'readr', 
          'stringr')

lapply(pkgs, require, character.only = TRUE)
showtext_auto()
#font_add("jh", "msjh.ttc")
options(shiny.usecairo = FALSE)
`%notin%` <- Negate(`%in%`)
# table_index_print %>% distinct()
table_index_print <- readRDS("table_index_print_eng_1081.rds") %>% 
   rename(link_add = link_addcourse, course_teacher = course_theteacher) %>%
   mutate(course_addway = if_else(is.na(course_addway), "NA", as.character(course_addway)))
table_detail_print <- readRDS("table_detail_print_eng_1081.rds") %>% 
   rename(ID = course_ID)
# table_index_print %>% count(course_addway)
# table_index_print %>% filter(is.na(course_addway))
# filter(str_detect(mandatory,str_c(input$mandotary, collapse = "|"))) %>%
# table_index_print %>% count(ID)
colnames(table_index_print) <- str_replace(colnames(table_index_print),"course_|link_", "")
cool <- readRDS("dennis_good.rds")

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
                # tags$head(includeHTML(("google-analytics.html"))), 
                tags$head(includeScript("google_analytics.js")),
                titlePanel("Open NTU Course Selection"),
                tabsetPanel(type = "tabs",
                            #tabPanel("Map", align="center", plotlyOutput("statebins", width = "900px", height = "600px")),
                            tabPanel("Searchable Table",
                                     fluidPage(
                                        fluidRow(
                                           column(4,
                                                  checkboxGroupButtons(inputId = "mandotary", 
                                                                       label = "Required",
                                                                       choices = c("required" = "必",
                                                                                   "elective" = "選"),
                                                                       selected = c("必", "選"),
                                                                       size = "xs",
                                                                       checkIcon = list(yes = icon("ok",lib = "glyphicon")
                                                                       )
                                                  )),
                                           column(4,
                                                  checkboxGroupButtons(inputId = "addway", 
                                                                       label = "Add Way",
                                                                       choices = c("1" = "1",
                                                                                   "2" = "2",
                                                                                   "3" = "2",
                                                                                   "NA" = "NA"),
                                                                       selected = c("1", "2", "3", "NA"),
                                                                       size = "xs",
                                                                       checkIcon = list(yes = icon("ok",lib = "glyphicon")
                                                                       )
                                                  ))),
                                        fluidRow(
                                        column(4,
                                               selectizeInput("credit", "Choose 學分數",
                                                              c("All"= "All", levels(table_index_print$credit)),
                                                              multiple = TRUE,
                                                              selected = "3",
                                                              width = '200px'
                                               )),
                                        column(4,
                                               selectizeInput("detail_time", "Choose 上課時間",
                                                              c("All"= "All", cool),
                                                              multiple = TRUE,
                                                              selected = "All",
                                                              width = '200px'
                                               )),
                                        column(4,
                                               selectizeInput("TA", "Choose 授課對象",
                                                              c("All"= "All", levels(table_index_print$TA)),
                                                              multiple = TRUE,
                                                              selected = "工管系",
                                                              width = '200px'
                                               ))),
                                        # searchInput(
                                        #   inputId = "search", label = "Enter your text",
                                        #   placeholder = "A placeholder",
                                        #   btnSearch = icon("search"),
                                        #   btnReset = icon("remove"),
                                        #   width = "450px"
                                        # ),
                                        # column(4,
                                        #        selectizeInput("JD", "Search 課程描述",
                                        #                       c("All"= "All", levels(table_index_print$TA)),
                                        #                       multiple = TRUE,
                                        #                       selected = "工管系",
                                        #                       width = '200px'
                                        #        )),
                                        column(12,
                                               DTOutput("data" )))
                            ),
                            # tabPanel("Course Info", align="center", numericInput("length","Length",0,0,10)
                            #          ),
                            # tabPanel("Course Info", inputPanel(
                            #   numericInput("ID2", label = "courseID:", 0, min(table_index_print$ID), max(table_index_print$ID))
                            # ),
                            tabPanel("Course Info", textOutput("page2"),
                                     #numericInput("ID2", label = "courseID:", 0, min(table_index_print$ID), max(table_index_print$ID)),
                                     DT::dataTableOutput("table2"),getdeps()
                            ),
                            
                            #textOutput("page2", DT::dataTableOutput("table2"),getdeps()),
                            tabPanel("About",
                                     fluidPage(
                                        column(10,
                                               h3("An App to View NTU Course Data More Easily"),
                                               p("I created this app to improve students' experiences when viewing course data. "),
                                                 # "The official website is this: ",
                                                 # a("ntu online course website",
                                                 #   href = "https://nol.ntu.edu.tw/nol/guest/index.php",
                                                 #   target = "_blank")),
                                               p("Details of website creating process are available on",
                                                 a("medium", icon("medium"),
                                                   href = "https://medium.com/dennis-r-data-news", target = "_blank")),
                                               p("Code are available on",
                                                 a("GitHub.", icon("github"), 
                                                   href = "https://github.com/Dennishi0925/open_ntucourse", target = "_blank"),
                                                 "Find me, Dennis Tseng, on", 
                                                 a("LinkedIn", icon("linkedin"),
                                                   href = "https://linkedin.com/in/tzu-hsuan-tseng/", target = "_blank"),
                                                 ", or you can read my work on",
                                                 a("blog", icon("blog"),
                                                   href = "https://dennisrdatanews.netlify.com", target = "_blank")
                                               ),
                                               HTML("<br><br><br>")
                                               #)
                                        )
                                     )
                            ),
                            tags$div(class="footer", checked=NA, tags$p("An interactive app to view course data powered by ",
                                                                        a("Dennis Tseng", href = "http://bit.ly/2LSPmuu", target = "_blank"))
                            )
                )
)
# DT::datatable(head(iris), colnames = c('Here', 'Are', 'Some', 'New', 'Names')) heroku ok?
# DT::datatable(
#   iris, extensions = 'Buttons', options = list(
#     dom = 'Bfrtip',
#     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
#   )
# )
# DT::datatable(iris2, extensions = 'Responsive')

server <- function(input, output) {
   
   output$data <- renderDataTable({
      # weekday = c(input$Monday,input$Tuesday,input$Wednesday,input$Thursday,input$Friday,input$Saturday)
      
      if ("All" %in% input$credit & "All" %in% input$TA & "All" %in% input$detail_time)
         DT::datatable(data.frame(table_index_print %>%
                                     filter(str_detect(mandatory,str_c(input$mandotary, collapse = "|"))) %>%
                                     filter(str_detect(addway,str_c(input$addway, collapse = "|"))) %>%
                                     mutate(add = paste0("<a href='",add,"'>","add","</a>"),
                                            ptt = paste0("<a href='",ptt,"'>","ptt","</a>"),
                                            ID = paste0("<a href='#table2'", "alt='", ID,                                    
                                                               "'onclick=\"tabs = $('.tabbable .nav.nav-tabs li');tabs.each(function() {$(this).removeClass('active')});$(tabs[1]).addClass('active');tabsContents = $('.tabbable .tab-content .tab-pane');tabsContents.each(function() {$(this).removeClass('active')});$(tabsContents[1]).addClass('active');$('#table2').trigger('change').trigger('shown');Shiny.onInputChange('ID', getAttribute('alt'));\">",
                                                               ID,"</a>"))), 
                       options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                    columnDefs = list(list(visible=FALSE, targets=c(14,15)))
                       ),filter = list(position = 'top', clear = FALSE), escape = FALSE)
      
      else if ("All" %in% input$credit & "All" %in% input$TA & "All" %notin% input$detail_time)
         DT::datatable(data.frame(table_index_print %>% filter(str_detect(detail_time, str_c(input$detail_time, collapse = "|"))) %>%
                                     filter(str_detect(mandatory,str_c(input$mandotary, collapse = "|"))) %>%
                                     filter(str_detect(addway,str_c(input$addway, collapse = "|"))) %>%
                                     mutate(add = paste0("<a href='",add,"'>","add","</a>"),
                                            ptt = paste0("<a href='",ptt,"'>","ptt","</a>"),
                                            ID = paste0("<a href='#table2'", "alt='",ID,                                    
                                                        "'onclick=\"tabs = $('.tabbable .nav.nav-tabs li');tabs.each(function() {$(this).removeClass('active')});$(tabs[1]).addClass('active');tabsContents = $('.tabbable .tab-content .tab-pane');tabsContents.each(function() {$(this).removeClass('active')});$(tabsContents[1]).addClass('active');$('#table2').trigger('change').trigger('shown');Shiny.onInputChange('ID', getAttribute('alt'));\">",
                                                        ID,"</a>"))), 
                       options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                    columnDefs = list(list(visible=FALSE, targets=c(14,15)))
                       ),filter = list(position = 'top', clear = FALSE), escape = FALSE)
      
      else if ("All" %in% input$credit & "All" %notin% input$TA & "All" %in% input$detail_time)
         DT::datatable(data.frame(table_index_print %>% filter(TA %in% input$TA) %>%
                                     filter(str_detect(mandatory,str_c(input$mandotary, collapse = "|"))) %>%
                                     filter(str_detect(addway,str_c(input$addway, collapse = "|"))) %>%
                                     mutate(add = paste0("<a href='",add,"'>","add","</a>"),
                                            ptt = paste0("<a href='",ptt,"'>","ptt","</a>"),
                                            ID = paste0("<a href='#table2'", "alt='",ID,                                    
                                                        "'onclick=\"tabs = $('.tabbable .nav.nav-tabs li');tabs.each(function() {$(this).removeClass('active')});$(tabs[1]).addClass('active');tabsContents = $('.tabbable .tab-content .tab-pane');tabsContents.each(function() {$(this).removeClass('active')});$(tabsContents[1]).addClass('active');$('#table2').trigger('change').trigger('shown');Shiny.onInputChange('ID', getAttribute('alt'));\">",
                                                        ID,"</a>"))), 
                       options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                    columnDefs = list(list(visible=FALSE, targets=c(14,15)))
                       ),filter = list(position = 'top', clear = FALSE), escape = FALSE)
            
      else if ("All" %notin% input$credit & "All" %in% input$TA & "All" %in% input$detail_time)
         DT::datatable(data.frame(table_index_print %>% filter(credit %in% input$credit) %>%
                                     filter(str_detect(mandatory,str_c(input$mandotary, collapse = "|"))) %>%
                                     filter(str_detect(addway,str_c(input$addway, collapse = "|"))) %>%
                                     mutate(add = paste0("<a href='",add,"'>","add","</a>"),
                                            ptt = paste0("<a href='",ptt,"'>","ptt","</a>"),
                                            ID = paste0("<a href='#table2'", "alt='",ID,                                    
                                                        "'onclick=\"tabs = $('.tabbable .nav.nav-tabs li');tabs.each(function() {$(this).removeClass('active')});$(tabs[1]).addClass('active');tabsContents = $('.tabbable .tab-content .tab-pane');tabsContents.each(function() {$(this).removeClass('active')});$(tabsContents[1]).addClass('active');$('#table2').trigger('change').trigger('shown');Shiny.onInputChange('ID', getAttribute('alt'));\">",
                                                        ID,"</a>"))), 
                       options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                    columnDefs = list(list(visible=FALSE, targets=c(14,15)))
                       ),filter = list(position = 'top', clear = FALSE), escape = FALSE)
      
      else if ("All" %in% input$credit & "All" %notin% input$TA & "All" %notin% input$detail_time)
         DT::datatable(data.frame(table_index_print %>% filter(TA %in% input$TA)  %>% filter(str_detect(detail_time, str_c(input$detail_time, collapse = "|"))) %>%
                                     filter(str_detect(mandatory,str_c(input$mandotary, collapse = "|"))) %>%
                                     filter(str_detect(addway,str_c(input$addway, collapse = "|"))) %>%
                                     mutate(add = paste0("<a href='",add,"'>","add","</a>"),
                                            ptt = paste0("<a href='",ptt,"'>","ptt","</a>"),
                                            ID = paste0("<a href='#table2'", "alt='",ID,                                    
                                                               "'onclick=\"tabs = $('.tabbable .nav.nav-tabs li');tabs.each(function() {$(this).removeClass('active')});$(tabs[1]).addClass('active');tabsContents = $('.tabbable .tab-content .tab-pane');tabsContents.each(function() {$(this).removeClass('active')});$(tabsContents[1]).addClass('active');$('#table2').trigger('change').trigger('shown');Shiny.onInputChange('ID', getAttribute('alt'));\">",
                                                               ID,"</a>"))), 
                       options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                    columnDefs = list(list(visible=FALSE, targets=c(14,15)))
                       ),filter = list(position = 'top', clear = FALSE), escape = FALSE)
      
      else if ("All" %notin% input$credit & "All" %in% input$TA & "All" %notin% input$detail_time)
         DT::datatable(data.frame(table_index_print %>% filter(credit %in% input$credit) %>% filter(str_detect(detail_time, str_c(input$detail_time, collapse = "|"))) %>%
                                     filter(str_detect(mandatory,str_c(input$mandotary, collapse = "|"))) %>%
                                     filter(str_detect(addway,str_c(input$addway, collapse = "|"))) %>%
                                     mutate(add = paste0("<a href='",add,"'>","add","</a>"),
                                            ptt = paste0("<a href='",ptt,"'>","ptt","</a>"),
                                            ID = paste0("<a href='#table2'", "alt='",ID,                                    
                                                               "'onclick=\"tabs = $('.tabbable .nav.nav-tabs li');tabs.each(function() {$(this).removeClass('active')});$(tabs[1]).addClass('active');tabsContents = $('.tabbable .tab-content .tab-pane');tabsContents.each(function() {$(this).removeClass('active')});$(tabsContents[1]).addClass('active');$('#table2').trigger('change').trigger('shown');Shiny.onInputChange('ID', getAttribute('alt'));\">",
                                                               ID,"</a>"))), 
                       options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                    columnDefs = list(list(visible=FALSE, targets=c(14,15)))
                       ),filter = list(position = 'top', clear = FALSE), escape = FALSE)# %>% arrange(desc(college))
      
      
      else if ("All" %notin% input$credit & "All" %notin% input$TA & "All" %in% input$detail_time)
         DT::datatable(data.frame(table_index_print %>% filter(credit %in% input$credit) %>% filter(TA %in% input$TA) %>%
                                     filter(str_detect(mandatory,str_c(input$mandotary, collapse = "|"))) %>%
                                     filter(str_detect(addway,str_c(input$addway, collapse = "|"))) %>%
                                     mutate(add = paste0("<a href='",add,"'>","add","</a>"),
                                            ptt = paste0("<a href='",ptt,"'>","ptt","</a>"),
                                            ID = paste0("<a href='#table2'", "alt='",ID,                                    
                                                        "'onclick=\"tabs = $('.tabbable .nav.nav-tabs li');tabs.each(function() {$(this).removeClass('active')});$(tabs[1]).addClass('active');tabsContents = $('.tabbable .tab-content .tab-pane');tabsContents.each(function() {$(this).removeClass('active')});$(tabsContents[1]).addClass('active');$('#table2').trigger('change').trigger('shown');Shiny.onInputChange('ID', getAttribute('alt'));\">",
                                                        ID,"</a>"))), 
                       options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                    columnDefs = list(list(visible=FALSE, targets=c(14,15)))
                       ),filter = list(position = 'top', clear = FALSE), escape = FALSE)# %>% arrange(desc(college))
      
      else
         DT::datatable(data.frame(table_index_print %>%
                                     filter(credit %in% input$credit) %>% 
                                     filter(TA %in% input$TA) %>% 
                                     filter(str_detect(detail_time, str_c(input$detail_time, collapse = "|"))) %>%
                                     filter(str_detect(mandatory,str_c(input$mandotary, collapse = "|"))) %>%
                                     filter(str_detect(addway,str_c(input$addway, collapse = "|"))) %>%
                                     mutate(add = paste0("<a href='",add,"'>","add","</a>"),
                                            ptt = paste0("<a href='",ptt,"'>","ptt","</a>"),
                                            ID = paste0("<a href='#table2'", "alt='",ID,                                    
                                                               "'onclick=\"tabs = $('.tabbable .nav.nav-tabs li');tabs.each(function() {$(this).removeClass('active')});$(tabs[1]).addClass('active');tabsContents = $('.tabbable .tab-content .tab-pane');tabsContents.each(function() {$(this).removeClass('active')});$(tabsContents[1]).addClass('active');$('#table2').trigger('change').trigger('shown');Shiny.onInputChange('ID', getAttribute('alt'));\">",
                                                               ID,"</a>"))), 
                       # options = list(columnDefs = list(list(
                       #                             targets = 15,
                       #                             render = JS(
                       #                               "function(data, type, row, meta) {",
                       #                               "return type === 'display' && data.length > 6 ?",
                       #                               "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                       #                               "}")
                       #                           ))
                       options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                    columnDefs = list(list(visible=FALSE, targets=c(14,15)))
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
   #     document.getElementById("ID").value=data[1];
   #     Shiny.onInputChange("ID",data[1]);
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
      #   DT::datatable(table_detail_print)
      # } else {
      #   DT::datatable(table_detail_print %>% filter(ID==input$ID) %>% select(-ID) %>% transpose_df())
      # }
      # 
      if(!is.null(input$ID)){
         DT::datatable(table_detail_print %>% filter(ID==input$ID) %>% select(-ID) %>% transpose_df())
         # }else if(!is.null(input$ID2)){
         #   DT::datatable(table_detail_print)
      }else {
         DT::datatable(table_detail_print)
      }
   })
   
   # output$page2 <- renderText({
   #    #print(input$ID)
   #    name_print = table_index_print %>% filter(ID == input$ID) %>% pull(name)
   #    paste0("Detailed course information for  ", name_print, " :")
   #    
   # })
   
}
# table_detail_print %>% filter(ID == 8800)
shinyApp(ui = ui, server = server)
