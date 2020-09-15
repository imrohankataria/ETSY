#TrackEtsy.com
library(shiny)
library(jsonlite)
library(tidyverse)
library(shinydashboard)
library(DT)


#Postgresql connection
library(RPostgreSQL)
library(DBI)
library(odbc)

pw = pw
drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "dbname",
                host = "host", port = 5432,
                user = "user", password = pw)



ui <- dashboardPage(
    dashboardHeader(title = "ETSY Tracker"),
    dashboardSidebar(
        sidebarMenu(
            menuItem('Product', tabName='product',
                     menuSubItem('Select Data', tabName='product1'),
                     menuSubItem('Tracked Products', tabName='product2')
            ),
            menuItem('Store', tabName='store',
                     menuSubItem('Select Data', tabName='store1'),
                     menuSubItem('Tracked Products', tabName='store2')
            )
        )
    ),
    
    dashboardBody(
        
        tabItems(
            tabItem(tabName = 'product1',
                    textInput('link', 'Product Link:', 'Post Product Link Here...'),
                    actionButton("searchproduct", "Search", class = "btn btn-info"),
                    br(),
                    br(),
                    fluidRow(
                        column(6, DT::dataTableOutput('producttable')),
                        column(6, actionButton('trackproduct', label = 'Track Selected', class = "btn btn-info"))
                        )
                    ),
            tabItem(tabName = 'product2',

                    DT::dataTableOutput('producttable2')
                    
                    ),
            
            tabItem(tabName='store1',
                   textInput('usernameinput', 'Username:', 'PigMintStudio'),
                    actionButton("search", "Search", class = "btn btn-info"),
                    br(),
                   tags$div(tags$h3(tags$b(" Select the Products To Track",align="middle",style="color: rgb(57,156,8)"))),
                    br(),
                    fluidRow(
                        column(6, DT::dataTableOutput('table')),
                        column(6, DT::dataTableOutput('selected'))
                    ),
                   fluidRow(
                        actionButton('track', label = 'Track Selected', class = "btn btn-info")
                    )
            ),
            tabItem(tabName='store2'
            )
        )
    )
)

server <- function(input, output, session) {
    
    ##FOR PRODUCT 
    observeEvent(input$searchproduct, {
    #extracting listingproduct
    first <- gsub("\\https://www.etsy.com/listing/*","", input$link)
    listingproduct <- gsub("/.*","",first, perl = TRUE)
    
    #creating data for the listingID, this remains global
        urlproduct <- paste0("https://openapi.etsy.com/v2/listings/",listingproduct,"?api_key=")
        productjson <- fromJSON(urlproduct, flatten=T)
        dataproduct <- as.data.frame(productjson$results) %>%
        rename("ListingID" = listing_id, "Status" = state, "Title" = title, "Views" = views, "Likes" = num_favorers) %>%
        select("ListingID", "Status", "Title", "Views", "Likes")
        
        #Had to make data reactive to work with other reactivity elements
        mydataproduct <- reactive({dataproduct})
        
        #Creating DataTable for the given data
        output$producttable <- renderDataTable({
            DT::datatable(mydataproduct(),
                          
                          extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller'),
                          rownames=FALSE,
                          options=list(dom = 'Bfrtip',
                                       searching = T,
                                       pageLength = 25,
                                       searchHighlight = TRUE,
                                       colReorder = TRUE,
                                       fixedHeader = TRUE,
                                       filter = 'bottom',
                                       buttons = c('copy', 'csv','excel', 'print'),
                                       paging    = TRUE,
                                       deferRender = TRUE,
                                       scroller = TRUE,
                                       scrollX = TRUE,
                                       scrollY = 700
                          ))
        })
        
        #Track button to send selected data
        # Value = AS DATA FRAME solved error
        observeEvent(input$trackproduct, {
            # #data1 <- as.data.frame(t(productjson))
            # data1 <- as.character(t(productjson))
            
            dbWriteTable(con, "product", 
                         value = dataproduct, append = TRUE, row.names = FALSE)
            #mongo_db_product$insert(data1)
        })
    })
    
    #TABLE FOR TRACKED Products
    #Alrernate is dbGetQuery
    queryproduct <- dbSendQuery(con, "select * from product")
    
    #Alternate is as.data.frame(queryproduct)
    trackedproduct <- fetch(queryproduct)
    
    #let it stay renderDataTable it genereates output
    output$producttable2 <- DT::renderDataTable({    
        datatable(trackedproduct ,selection = "multiple",
                  
                  extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller'),
                  rownames=FALSE,
                  options=list(dom = 'Bfrtip',
                               searching = T,
                               pageLength = 25,
                               searchHighlight = TRUE,
                               colReorder = TRUE,
                               fixedHeader = TRUE,
                               filter = 'bottom',
                               buttons = c('copy', 'csv','excel', 'print'),
                               paging    = TRUE,
                               deferRender = TRUE,
                               scroller = TRUE,
                               scrollX = TRUE,
                               scrollY = 700
                  ))
    })
    
    
    ##FOR STORE    
#creating data for the username, this remains global
    observeEvent(input$search, {
    url <- paste0("https://openapi.etsy.com/v2/shops/", input$usernameinput, "/listings/active?api_key=")
    store <- fromJSON(url, flatten=T)
    data <- as.data.frame(store$results) %>%
        rename("ListingID" = listing_id, "Status" = state, "Title" = title, "Views" = views, "Likes" = num_favorers) %>%
        select("ListingID", "Status", "Title", "Views", "Likes")
    
#Had to make data reactive to work with other reactivity elements
    mydata <- reactive({data})

#Creating DataTable for the given data
    output$table <- renderDataTable({
        DT::datatable(mydata(),selection = "multiple",
                      
                      extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller'),
                      rownames=FALSE,
                      options=list(dom = 'Bfrtip',
                                   searching = T,
                                   pageLength = 25,
                                   searchHighlight = TRUE,
                                   colReorder = TRUE,
                                   fixedHeader = TRUE,
                                   filter = 'bottom',
                                   buttons = c('copy', 'csv','excel', 'print'),
                                   paging    = TRUE,
                                   deferRender = TRUE,
                                   scroller = TRUE,
                                   scrollX = TRUE,
                                   scrollY = 700
                                   ))
                                })
    
    #This gets Row numbers for selected rows
    selectedRow <- eventReactive(input$table_rows_selected,{
        row.names(data)[c(input$table_rows_selected)]
    })
    
    #Creates DataTable for selected rows
    output$selected <- DT::renderDataTable({    
        datatable(mydata()[selectedRow(), ],selection = "multiple",
                  
                  extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller'),
                  rownames=FALSE,
                  options=list(dom = 'Bfrtip',
                               searching = T,
                               pageLength = 25,
                               searchHighlight = TRUE,
                               colReorder = TRUE,
                               fixedHeader = TRUE,
                               filter = 'bottom',
                               buttons = c('copy', 'csv','excel', 'print'),
                               paging    = TRUE,
                               deferRender = TRUE,
                               scroller = TRUE,
                               scrollX = TRUE,
                               scrollY = 700
                  ))
         })
    
    #Track button to send selected data
    
    observeEvent(input$track, {
        data <- as.data.frame(t(mydata()[selectedRow(), ]))
        mongo_db_store$insert(store)
    })
    })
    


}

shinyApp(ui, server)
    
