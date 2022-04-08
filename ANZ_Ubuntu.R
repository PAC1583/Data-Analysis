# For a challenge
# Cleaning 
# Start by doing some basic checks - 
# Are there any data issues? Does the data need to be cleaned?

# what insights can you draw from the location information provided in the dataset?
# what is the average transaction amount? (C)
# How many transactions do customers make each month, on average? (C)

# Segment the dataset by transaction date and time

# Put together 2-3 slides summarising your most interesting findings to ANZ management

#-----------------------------------------------------------------------------------------------
#                                 Packages & Libraries & File
#-----------------------------------------------------------------------------------------------

install.packages('ggrepel')
install.packages('ggmap')
install.packages('ggspatial')
install.packages("devtools")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("shinydashboard")
install.packages("geojsonio")
install.packages("tigris")
install.packages("magrittr")
install.packages("ggiraph")
install.packages("maps")
install.packages("plotly")

library(lubridate)
library(dplyr)
library(ggplot2)
library(ggmap)
library(tidyr)
library(jcolors)
library(plotly)
library(shiny)
library(shinydashboard)
library(leaflet)
library(geojson)
library(geojsonio)
library(tigris)
library(magrittr)
library(ggiraph)
library(sf)

setwd('D:/3 PAULA/TSom/Course/PROJECT3/ANZ/Exploratory_Data/')


txn <- read.csv('ANZ_transaction.csv', stringsAsFactors = F, sep = ",")
View(txn)
str(txn)

#-----------------------------------------------------------------------------------------------
#                                 Basic Checks of the DataSet & Modifications 
#-----------------------------------------------------------------------------------------------


# Check Statistics and data structure, data type 
str(txn)        
dim(txn)



#change date, extraction type  (chr to date)
txn$date<-as.Date(txn$extraction)

# extractiion format changed from char to POSIXct format
txn$extraction <- ymd_hms(txn$extraction)
str(txn)

# Separate Merchandise longitude and latitude (POS-SALE / SALE)
table(txn$merchant_long_lat)
txn$M_longitude <-  gsub(" .*$","", txn$merchant_long_lat)
txn$M_longitude <- as.numeric(txn$M_longitude)
str(txn$M_longitude)

txn$M_latitude <-  gsub(".* ","", txn$merchant_long_lat)
txn$M_latitude <- as.numeric(txn$M_latitude)
table(txn$M_latitude)
str(txn$M_latitude)
hist(txn$M_latitude)

# Separate Customer longitude and latitude (Credit)
table(txn$long_lat)
txn$C_longitude <-  gsub(" .*$","", txn$long_lat)
txn$C_longitude <- as.numeric(txn$C_longitude)
str(txn$C_longitude)

txn$C_latitude <-  gsub(".* ","", txn$long_lat)
txn$C_latitude <- as.numeric(txn$C_latitude)
table(txn$C_latitude)
str(txn$C_latitude)



#Eliminate columns that are not needed
txn <- txn[, -c(3,9,22)]

# Check for NA?s
nas<- function(x){
  x<- sum(is.na(x))
}

txn_nas<- apply(txn, 2, nas)
View(txn_nas)

# Na?s --> card_present_flag = 4326 = Merchant_latitude/Longitude


#Check for blanks, replace for NA?s
txn$merchant_state <- ifelse(txn$merchant_state == "",NA,txn$merchant_state)
txn$merchant_suburb <- ifelse(txn$merchant_suburb == "",NA,txn$merchant_suburb)

# change colname
colnames(txn)[1] <- "status"

save.image("M1.RData")
load("M1.RData")

#write.csv(txn, file = "txn.csv")

#-----------------------------------------------------------------------------------------------
#                                 Number of Clients
#-----------------------------------------------------------------------------------------------

Cust <- txn %>%
  distinct(customer_id)%>%
  count(customer_id)

Cust_Total <-sum(Cust$n)


#-----------------------------------------------------------------------------------------------
#                                 Period of Data
#-----------------------------------------------------------------------------------------------

Min_day <- min(txn$extraction)
Max_day <- max(txn$extraction)

Lapsed_Days <- round(Max_day - Min_day, digits = 0)

#-----------------------------------------------------------------------------------------------
#                                 No. Observations
#-----------------------------------------------------------------------------------------------

Obs <- dim(txn)
Obs <- Obs[[1]]

#-----------------------------------------------------------------------------------------------
#                                Segmentation of dataSet by transaction date and time. 
#-----------------------------------------------------------------------------------------------

#add Day of the Week and week number
SegmentDate <- txn %>%
  select(date, customer_id, gender, age, movement, extraction, amount) %>%
  mutate(Month = month(date), Segm_day = day(date), Segm_wk = week(date), .after = date) %>%
  mutate(Volume = 1)

#segment Time of the day 
SegmentDate$Segm_time <- if_else(hour(SegmentDate$extraction)>=00 & hour(SegmentDate$extraction)<12, 'Morning', 
                                 if_else(hour(SegmentDate$extraction)>=12 & hour(SegmentDate$extraction)<18, 'Afternoon','Night'))


#-----------------------------------------------------------------------------------------------
#                               PREPARING LOCATION DATA OF CUSTOMERS 
#-----------------------------------------------------------------------------------------------

#Change data as json file -->  remove the javascript assignment
#code taken from website: https://learn.r-journalism.com/en/mapping/census_maps/census-maps/
#maps in suburbs taken from https://exploratory.io/map

url <- "https://raw.githubusercontent.com/rowanhogan/australian-states/master/states.geojson"

statesAU <- geojson_read(url, what = "sp")

#convert into a sf object
statesAU2 <- st_as_sf(statesAU)

class(statesAU2)
names(statesAU2)

#customer state from  lon and lat data
points <- txn %>%
  select(customer_id, gender, age, C_longitude, C_latitude) %>%
  distinct(customer_id, gender, age, C_longitude, C_latitude)

#locate points in map1 -> 
leaflet(data = points) %>% addTiles() %>%
  addMarkers(~C_longitude, ~C_latitude, popup = ~as.character(C_longitude, C_latitude ), label = ~as.character(customer_id))

#CUS-1617121891 is located outside Australia --> new location assigned 
#criteria of new location --> same merchant location with highest purchases
points[35, 4] = 151.21
points[35, 5] = -33.87

#convert into a spatial data.frame 
points <- st_as_sf(points, coords = c("C_longitude", "C_latitude"), crs = st_crs(statesAU2))

#perform a spatial join!
pointscx <- st_join(points, statesAU2)

# state from CUS-331942311 couldnt be estimated, from the map1 -- New South Wales (1) 
pointscx[94, 4] = 1
pointscx[94, 5] = "New South Wales"

rm(points, statesAU2)

#-----------------------------------------------------------------------------------------------
#                                txn volume + amount By type of txn & customer per month 
#-----------------------------------------------------------------------------------------------

#General table - Avg txn per customer per month
txns_debit <- txn %>%
  filter(movement == "debit") %>%
  mutate( VolumeDebit = 1) %>%
  group_by(Month = month(date), customer_id)%>%
  summarise(Amount = round(sum(amount, na.rm = TRUE),0), VolumeDebit = round(sum(VolumeDebit, na.rm = TRUE),0))

txns_debit_AVG <-  txns_debit %>%
  group_by( customer_id) %>%
  summarise(Debit = round(mean(Amount, na.rm = TRUE)), VolumeDebit = round(mean(VolumeDebit, na.rm = TRUE)))

txns_credit <- txn %>%
  filter(movement == "credit") %>%
  mutate( VolumeCredit = 1) %>%
  group_by(Month = month(date), customer_id) %>%
  summarise(Amount = round(sum(amount, na.rm = TRUE),0), VolumeCredit = round(sum(VolumeCredit, na.rm = TRUE),0))

txns_credit_AVG <- txns_credit %>%
  group_by(customer_id) %>%
  summarise(Credit = round(mean(Amount, na.rm = TRUE)), VolumeCredit = round(mean(VolumeCredit, na.rm = TRUE)))

txns_AVG <- merge(x = txns_debit_AVG, y = txns_credit_AVG, by.x = c('customer_id'), by.y =c('customer_id'), all.x = F, all.y = F)

txnsMthly_AVG <- merge(x = txns_AVG, y = pointscx, by.x = c('customer_id'), by.y =c('customer_id'), all.x = F, all.y = F)


rm(txns_credit, txns_credit_AVG, txns_debit_AVG, txns_debit, txns_AVG)


save.image("M2.RData")
load("M2.RData")

#-----------------------------------------------------------------------------------------------
#                               PREPARING LOCATION DATA OF MERCHANTS 
#-----------------------------------------------------------------------------------------------

merchant <- txn %>%
  select(merchant_state) %>%
  filter(merchant_state != '')

State_Merch <- merchant %>% 
  unique()


#modify table in order that matches variables of json file

State_Merch$STATE_CODE <- ifelse(State_Merch$merchant_state == "NSW",1, 
                                 ifelse(State_Merch$merchant_state == "VIC", 2, 
                                        ifelse(State_Merch$merchant_state == "QLD", 3,
                                               ifelse(State_Merch$merchant_state == "SA", 4, 
                                                      ifelse(State_Merch$merchant_state == "WA", 5, 
                                                             ifelse(State_Merch$merchant_state == "TAS", 6,
                                                                    ifelse(State_Merch$merchant_state == "NT", 7,
                                                                           ifelse(State_Merch$merchant_state == "ACT", 8,0))))))))

# Now we use the Tigris function geo_join to bring together the states of the shapefile 
# and the State from dataframe -- State_Cust1  
# states are the two columns they'll be joined by


save.image("M3.RData")
load("M3.RData")

#-----------------------------------------------------------------------------------------------
#                               OUTLIERS 
#-----------------------------------------------------------------------------------------------

fig <- plot_ly(type = "box")
fig <- fig %>% add_boxplot(y=txnsMthly_AVG$Credit,  type = "box", quartilemethod="inclusive",
                           name = "Credit")
fig <- fig %>% add_boxplot(y=txnsMthly_AVG$Debit,  type = "box", quartilemethod="inclusive",
                           name = "Debit")
fig

fig1 <- plot_ly(type = "box")
fig1 <- fig1 %>% add_boxplot(y=txnsMthly_AVG$VolumeCredit,  type = "box", quartilemethod="inclusive",
                             name = "VolumeCredit")
fig1<- fig1 %>% add_boxplot(y=txnsMthly_AVG$VolumeDebit,  type = "box", quartilemethod="inclusive",
                            name = "VolumeDebit")
fig1


out <- boxplot.stats(txnsMthly_AVG$Debit)$out
out_ind <- which(txnsMthly_AVG$Debit %in% c(out))
txnsMthly_AVG[out_ind, ]

out1 <- boxplot.stats(txnsMthly_AVG$Credit)$out
out_ind1 <- which(txnsMthly_AVG$Credit %in% c(out1))
txnsMthly_AVG[out_ind1, ]

out2 <- boxplot.stats(txnsMthly_AVG$VolumeDebit)$out
out_ind2 <- which(txnsMthly_AVG$VolumeDebit %in% c(out2))
txnsMthly_AVG[out_ind2, ]

No_Out <- txnsMthly_AVG[-c(out_ind2),] 
No_Out <- No_Out[-c(out_ind),]

cx_out <- c(txnsMthly_AVG[out_ind2, 1])


# Outlier using percentiles
upper_bound <- quantile(txnsMthly_AVG$VolumeDebit, 0.975)
upper_bound


#-----------------------------------------------------------------------------------------------
#                               DashBoard - FrontEnd
#-----------------------------------------------------------------------------------------------
ui <- dashboardPage(  
  
  dashboardHeader(
    title = " Exploratory Analysis "
  ),  
  
  dashboardSidebar(
    width = 120,
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Map", tabName = "Map", icon = icon("map marked alt"))
    )
  ),  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Dashboard",
              fluidRow(
                h2('Data Description')
              ),
              fluidRow(
                # Dynamic infoBoxes
                valueBoxOutput("Clients"),
                valueBoxOutput("Months"),
                valueBoxOutput("DataObs")
              ),
              
              fluidRow(
                tabBox(
                  title = "",
                  id = "tabset1", width = 5, height = '460px',
                  tabPanel("Avg Txns", 
                           plotlyOutput(
                             "Plot3",
                             height = "200px"),
                           plotlyOutput(
                             "Plot4",
                             height = "200px")
                  ),
                  tabPanel("Histogram of Txns",
                           plotlyOutput("Plot8")
                  ),
                  tabPanel("by Txn Description",
                           plotlyOutput("Plot5")
                  )
                ),
                tabBox(
                  title = "",
                  id = "tabset2", width = 5, height = '460px',
                  tabPanel("Segment by txn date and time",
                           plotlyOutput(
                             outputId = "Plot6",
                             height = "220px"
                             ),
                           h6("Place the cursor on the line above"),
                           plotlyOutput(
                             outputId = "Plot7",
                             height = "150px"
                             )
                           ),
                  tabPanel("Outliers",
                           plotlyOutput(
                             outputId = "Plot9",
                             height = "220px"
                           ), 
                  )
                ),
                box(width=2,
                    #select cx gender
                    checkboxGroupInput("gender",
                                       "Gender",
                                       choices = unique(txn$gender),
                                       selected = unique(txn$gender),
                                       inline = TRUE), 
                    #Select Age
                    sliderInput(
                      inputId = "age",
                      label = "Customer age:",
                      min = min(txn$age),
                      max = max(txn$age),
                      value = c(18, 78),
                      step = 1,
                    ),
                    
                    radioButtons(
                      "outliers", "Outliers", choices=c("Yes", "No"),
                      selected = "No",
                      inline = TRUE,
                    ),
                    
                    h6("Use only for Time Segment"),
                    selectInput("movement", "Movement", 
                                choices = unique(txn$movement), 
                                selected = "debit"),
                    checkboxGroupInput("month", "Month", 
                                       choices = unique(month(txn$date)),
                                       selected = unique(month(txn$date)),
                                       inline = TRUE
                                      ),
                    h6("Use only for txn by description "),
                    radioButtons("card_flag", "Card flag",
                                 choices = list("All" = 1, "Yes" = 2, "No" = 3), 
                                 selected = 1,
                                 inline = TRUE),
                    )
                ),
              
              fluidRow(
                h3('Most Important Insights')
              ),
              
              fluidRow(
                box(
                   h4('- In average per customer per month , debits represents one-third of the credits'),
                   h4('- The average volume of debit transactions are 33. Volume of Credit transaction are 10 times lower than debits'),
                   h4('- No significant difference in amount/volume of transactions between males and females of all ages'),
                   h4('- Highest average amount/volume transactions are among people with an age of 31 and 40 years old'),
                   h4('- Lowest average amount/volume transactions are among people older than 61 years'),
                   h4('- Removing outliers does not make a difference on the results')
                   ),
                box( 
                  h4('- Daily debit transactions over a month exhibits a periodic behavior similar to a sinusoidal function'),
                  h4('- Daily average amount of debit transaction pre customer are about 113 AUD and 1882 AUD of credit transactions'),
                  h4('- Females have slightly higher amount of debit transactions than men'),
                  h4('- In average per month, customers made no more than 2 daily debit transactions in the mornings in 80% of the days of a month'),
                  h4('- In the afternoons and nights, the volume of transactions were only one per customer in ~80% of the days of a month in average')
                  )
                )
              ),
      
      tabItem(tabName = "Map",
              fluidRow(h2(' Avg. Monthly Transactions per Customer & Location ')),
              fluidRow (
                box(width = 2,
                    radioButtons("txn_type", "Monthly Average Transaction per Customer by:", 
                                 choices = c("amount [AUD]" = "Amount", 
                                             "volume" = "Volume"),
                                 selected = "Amount"),
                    
                    #type of movement
                    radioButtons("movement1", "Movement", 
                                 choices = unique(txn$movement), 
                                 selected = "debit",
                                 inline = TRUE),
                    
                    #select cx gender
                    checkboxGroupInput("gender1","Gender",
                                       choices = unique(txn$gender),
                                       selected = unique(txn$gender),
                                       inline = TRUE
                                       ), 
                    #Select Age
                    sliderInput(
                      inputId = "age1",
                      label = "Customer age:",
                      min = min(txn$age),
                      max = max(txn$age),
                      value = c(18, 78),
                      step = 1,
                    ),
                ), 
                tabBox(
                  title = "",
                  id = "tabset2",
                  width = 8,
                  height = '460px',
                  
                  tabPanel("Customer Location",
                           leafletOutput("map1", height="450px")),
                  tabPanel("Merchant Location",
                           h5("Only Points of Sales(POS) Transactions"),
                           leafletOutput("map2", height="450px"))
                )
              ))
    )
  )
)


#-----------------------------------------------------------------------------------------------
#                               Dashboard - SERVER 
#-----------------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  output$Clients <- renderValueBox({
    valueBox(
      Cust_Total, "No. Customers", icon = icon("users", lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  output$Months <- renderValueBox({
    valueBox(
      paste0(Lapsed_Days, "days"), "Period of Data", icon = icon("hourglass-end",  lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$DataObs <- renderValueBox({
    valueBox(
      Obs, "Observations", icon = icon("database", "fa-1x",  lib = "font-awesome"),
      color = "purple"
    )
  })
  
  output$Plot3 <- renderPlotly({

    txns_p1 <- txnsMthly_AVG %>%
      filter(gender %in% input$gender) %>%
      filter(age >= input$age[1], age <= input$age[2])
    
    if (input$outliers == "No"){
      txns_p1 <- subset(txns_p1, !(customer_id %in% cx_out))
    }
    
    txns_p2 <- txns_p1 %>%
      summarize(Credit = round(mean(Credit, na.rm = TRUE)), VolumeCredit = round(mean(VolumeCredit, na.rm = TRUE)), 
                Debit = round(mean(Debit, na.rm = TRUE)), VolumeDebit = round(mean(VolumeDebit, na.rm = TRUE))) %>%
      gather('Credit', 'VolumeCredit','Debit', 'VolumeDebit', key = "txns", value = "Value")
      
    
    UseRow <- c("Credit", "Debit")
    
    txns_p2 <- subset(txns_p2, (txns %in% UseRow))
    
    plot3 <- plot_ly(txns_p2, x = ~txns, y = ~Value, type = 'bar',
                     marker = list(color = c('rgba(0,0,205,1)', 'rgba(0, 140, 255, 0.27)'))
    )
    plot3 <- plot3 %>% layout(title = 'Monthly Avg. Transactions per Customer',
                              xaxis = list(
                                title = ""),
                              yaxis = list(
                                title = "Amount[CAD]"))
    plot3
  
  })
  
  output$Plot4 <- renderPlotly({
    
    txns_Ap1 <- txnsMthly_AVG %>%
      filter(gender %in% input$gender) %>%
      filter(age >= input$age[1], age <= input$age[2])
    
    if (input$outliers == "No"){
      txns_Ap1 <- subset(txns_Ap1, !(customer_id %in% cx_out))
    }
    
    
    txns_Ap2 <- txns_Ap1 %>%
      summarize(Credit = round(mean(Credit, na.rm = TRUE)), VolumeCredit = round(mean(VolumeCredit, na.rm = TRUE)), 
                Debit = round(mean(Debit, na.rm = TRUE)), VolumeDebit = round(mean(VolumeDebit, na.rm = TRUE))) %>%
      gather('Credit', 'VolumeCredit','Debit', 'VolumeDebit', key = "txns", value = "Value")
    
    UseRow <- c("VolumeCredit", "VolumeDebit")
    
    txns_Ap2 <- subset(txns_Ap2, (txns %in% UseRow))
    
    plot4 <- plot_ly(txns_Ap2, x = ~txns, y = ~Value, type = 'bar',
                     name = 'Volume[n]',
                     marker = list(color = c('rgba(0,0,205,1)', 'rgba(0,140,255,0.27)'))
                     ) 
    plot4 <- plot4 %>% layout(xaxis = list(
                                title = ""),
                              yaxis = list(
                                title = "Volume [n]"))
    
    plot4
    
  })
  
  output$Plot5 <- renderPlotly({
    
    cxn_typtxn <-  txn %>%
      filter(gender %in% input$gender) %>%
      filter(age >= input$age[1], age <= input$age[2]) %>%
      group_by(Month,customer_id, movement, status, card_flag, typeTxn) %>%
      summarise(Amount = sum(Amount, na.rm = TRUE))
    
    cxn_typtxn$card_flag <- ifelse(is.na(cxn_typtxn$card_flag) == TRUE, 2,cxn_typtxn$card_flag)
    
    if (input$movement == "debit" & input$card_flag == 1) {
      typtxn_cx <-  cxn_typtxn %>%
      filter(movement == "debit") %>%
        group_by(movement, status, typeTxn) %>%
        summarise(Amount = round(mean(Amount, na.rm = TRUE)))
      
      
    } else if (input$movement == "debit" & input$card_flag == 2) {
      typtxn_cx <-  cxn_typtxn %>%
        filter(movement == "debit", card_flag >= 1) %>%
        group_by(movement, status, typeTxn) %>%
        summarise(Amount = round(mean(Amount, na.rm = TRUE)))
      print(typtxn_cx)
      
    } else if (input$movement == "debit" & input$card_flag == 3) {
      
      typtxn_cx <-  cxn_typtxn %>%
        filter(movement == "debit") %>%
        filter(card_flag != 1) %>%
        group_by(movement, status, typeTxn) %>%
        summarise(Amount = round(mean(Amount, na.rm = TRUE)))
      
      
      print(typtxn_cx)
      
    } else {
      typtxn_cx <-  cxn_typtxn %>%
        filter(movement == "credit") %>%
        group_by(movement, status, typeTxn) %>%
        summarise(Amount = round(mean(Amount, na.rm = TRUE)))
      print(typtxn_cx)
    }
    
    
    t1 <- theme(
      axis.line = element_line(size = 0.4),
      panel.background = element_blank(),
      legend.title = element_blank()
    ) 
    
    m <- list(
      l = 50,
      r = 50,
      b = 100,
      t = 80,
      pad = 4
    )
    
    p1 <- ggplot(typtxn_cx, aes(x = status, y = Amount, fill = typeTxn))+
      geom_bar(stat = "identity", width = 0.8) + xlab("") + ylab("Avg Amount [CAD]") +
      scale_fill_brewer(palette = "Blues") + t1 + coord_flip()
    
    
    fig <- ggplotly(p1)
    fig <- fig %>%
      layout(
        title = list(text = 'Average Amount per Type of Transactions', y = 0.95),
        autosize = T, margin = m,
        yaxis = list(
          showgrid = TRUE,
          showline = TRUE,
          showticklabels = TRUE
        ),
        xaxis = list(
          zeroline = FALSE,
          showline = TRUE,
          showticklabels = TRUE,
          showgrid = TRUE
        ),
        legend = list( x = 1, y =0.5)
      ) 
    
    fig
    
  })
  
  output$Plot6 <- renderPlotly({
    
    #sum of txn per segment per cx per day
    
    sTime_daily <-  SegmentDate %>%
      filter(age >= input$age[1], age <= input$age[2]) %>%
      filter(movement %in% input$movement) %>%
      filter(Month %in% input$month) %>%
      filter(gender %in% input$gender) %>%
      group_by(Segm_day, Month, customer_id, movement) %>%
      summarise(Amount = sum(amount, na.rm = TRUE), Volume = sum(Volume, na.rm = TRUE))
    
    if (input$outliers == "Yes"){
      sTime_daily <- subset(sTime_daily, !(customer_id %in% cx_out))
    }
    
    # avg txn per cx per segment per day
    avgDaily <-  sTime_daily %>%
      group_by(Segm_day) %>%
      summarise(Amount = round(mean(Amount, na.rm = TRUE)), Volume = round(mean(Volume, na.rm = TRUE)))
    
    
    xform <- list(title = "")
    
    my <- round(mean(avgDaily$Amount),1)
    
    p <- plot_ly(avgDaily, x=~Segm_day, y=~Amount, type="scatter", mode="points + lines", source = "A") %>% 
      layout(xaxis = xform, 
             title = "Avg Daily Txns per Cust",
             showlegend = FALSE)%>%
      add_trace(x=~Segm_day, y = my, type="scatter",type="line")
    
  })
  
  output$Plot7 <- renderPlotly({
    
    mouse_event <- event_data("plotly_hover", source="A")
    Day_event <- mouse_event
    
    SegmentDate_subset <- SegmentDate[SegmentDate$Segm_day==Day_event[3]$x,]
    
    if (nrow(SegmentDate_subset) > 0 ){
      
      print(dim(SegmentDate_subset))
      
      sDate_day <-  SegmentDate_subset %>%
        filter(movement %in% input$movement) %>%
        filter(age >= input$age[1], age <= input$age[2]) %>%
        filter(gender %in% input$gender) %>%
        group_by(Segm_day, Month, Segm_time, customer_id) %>%
        summarise(Amount = sum(amount, na.rm = TRUE), Volume = sum(Volume, na.rm = TRUE))
      
      sDate_avgDay <-  sDate_day %>%
        group_by(Segm_time) %>%
        summarise(Amount = round(mean(Amount, na.rm = TRUE)), Volume = round(mean(Volume, na.rm = TRUE)))
      
    xform <- list(categoryorder = "array",
                  categoryarray = c("Morning", 
                                    "Afternoon", 
                                    "Night"),
                  title = "Segment time")
    
    plot_ly(sDate_avgDay, x=~Segm_time, y=~Volume, type = "bar")%>% 
      layout(xaxis = xform)
    }
    
    })
  
  output$Plot8 <- renderPlotly({
    
    plot_ly(x = ~txnsMthly_AVG$VolumeDebit, type = "histogram")
    
  })
  
  output$map1 <- renderLeaflet({
    
    txn_cust <- txn %>%
      select(date, customer_id, movement, amount)  %>%
      mutate(Volume=1)
    
    #average of amount by customer 
    txns_cx <- txn_cust %>%
      filter(movement %in% input$movement1)%>%
      group_by(Month=month(date),customer_id) %>%
      summarise(Amount = sum(amount, na.rm = TRUE), Volume = sum(Volume, na.rm = TRUE))
    
    txns_cxAVG <- txns_cx %>%
      group_by(customer_id) %>%
      summarise(Amount = round(mean(Amount, na.rm = TRUE)), Volume = round(mean(Volume, na.rm = TRUE)))
    
    
    txns_cxAVG <-merge(x = txns_cxAVG, y = pointscx, by.x = c('customer_id'), by.y =c('customer_id'), all.x = F, all.y = F)
    
    State_Cust1 <- txns_cxAVG  %>%
      filter(gender %in% input$gender1)%>%
      filter(age >= input$age1[1], age <= input$age1[2])%>%
      group_by(STATE_CODE) %>%
      summarise(Amount = round(mean(Amount, na.rm = TRUE)), Volume = round(mean(Volume, na.rm = TRUE)))
    
    states_merged <- geo_join(statesAU, State_Cust1, "STATE_CODE", "STATE_CODE")
    
    print(states_merged)
    
    
    # Create variableplot
    states_merged$variableplot <- as.numeric(
      states_merged@data[, input$txn_type]
    )
    
    
    # Creating a color palette based on the number range in the amount/n column, palette "Greens" or "YlOrRd"
    pal <- colorNumeric("YlOrRd", domain=states_merged$variableplot)
    
    
    # Setting up the pop up text
    
    if (input$txn_type == 'Amount'){
      popup_sb <- paste0("<strong>", states_merged$STATE_NAME, 
                         "</strong><br/>Amount [AUD]: ", states_merged$variableplot
      )
    } else {
      popup_sb <- paste0("<strong>", states_merged$STATE_NAME, 
                         "</strong><br/> Volume: ", states_merged$variableplot
      )
    }
    
    head(popup_sb)
    
    
    #Creating map
    leaflet() %>%
      addProviderTiles("CartoDB.Positron")%>%
      setView( 140, -30, zoom=3.5) %>%
      addPolygons(data = states_merged,
                  fillColor = pal(states_merged$variableplot),
                  fillOpacity = 0.7,
                  weight = 0.2,
                  smoothFactor = 0.2,
                  popup = ~popup_sb)%>%
      addLegend(pal = pal,
                values = states_merged$variableplot,
                position = "bottomright")  
    
    # top_n(n=5, Amount)
    # The state that have higher amount in transactions is NSW followed by VIC where the 
    # major cities of Australia are.
    # avg No. of txns per customer across state  - Top 5 
    
  })
  
  output$map2 <- renderLeaflet({
    
    merch_txn <- txn %>%
      select(date,customer_id, merchant_state, amount) %>%
      filter(merchant_state != '') %>%
      mutate(Volume = 1)
    
    txns_merch <- merch_txn %>%
      #filter(txn_description %in% input$txn_description)%>%
      group_by(Month=month(date),customer_id, merchant_state) %>%
      summarise(Amount = sum(amount, na.rm = TRUE), Volume = sum(Volume, na.rm = TRUE))
    
    txns_merchAVG <- txns_merch %>%
      group_by(customer_id, merchant_state) %>%
      summarise(Amount = round(mean(Amount, na.rm = TRUE)), Volume = round(mean(Volume, na.rm = TRUE)))
    
    txns_merAVG <-merge(x = txns_merchAVG, y = State_Merch, by.x = c('merchant_state'), by.y =c('merchant_state'), all.x = F, all.y = F)
    
    State_Cust1 <- txns_merAVG  %>%
      group_by(STATE_CODE) %>%
      summarise(Amount = round(mean(Amount, na.rm = TRUE)), Volume = round(mean(Volume, na.rm = TRUE)))
    
    merchant_merged <- geo_join(statesAU, txns_merAVG, "STATE_CODE", "STATE_CODE")
    
    # Create variableplot
    merchant_merged$variableplot <- as.numeric(
      merchant_merged@data[, input$txn_type]
    )
    
    
    # Creating a color palette based on the number range in the amount/n column, palette "Greens" or "YlOrRd"
    pal <- colorNumeric("YlOrRd", domain=merchant_merged$variableplot)
    
    
    # Setting up the pop up text
    
    if (input$txn_type == 'Amount'){
      popup_sb <- paste0("<strong>", merchant_merged$STATE_NAME, 
                         "</strong><br/>Amount [AUD]: ", merchant_merged$variableplot
      )
    } else {
      popup_sb <- paste0("<strong>", merchant_merged$STATE_NAME, 
                         "</strong><br/> Volume: ", merchant_merged$variableplot
      )
    }
    
    head(popup_sb)
    
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron")%>%
      setView( 140, -30, zoom=3.5) %>%
      addPolygons(data = merchant_merged,
                  fillColor = pal(merchant_merged$variableplot),
                  fillOpacity = 0.7,
                  weight = 0.2,
                  smoothFactor = 0.2,
                  popup = ~popup_sb)%>%
      addLegend(pal = pal,
                values = merchant_merged$variableplot,
                position = "bottomright")  
    
    # top_n(n=5, Amount)
    # The state that have higher amount in transactions is NSW followed by VIC where the 
    # major cities of Australia are.
    # avg No. of txns per customer across state  - Top 5 
  })
  
}

shinyApp(ui, server)





