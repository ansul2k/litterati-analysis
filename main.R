
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(grid)
library(leaflet)
library(DT)
library(stringi)
library(dplyr)

df = read.csv("dataset.csv")

dim(df)

df$username = gsub("litterati","user",df$username)

df = df[df$lat>41 & df$lat<42,]
df = df[df$lon > -88 & df$lon < -87,]

dim(df)

df$litterTimestamp = ymd_hms(df$litterTimestamp)
df$litterTimestamp = with_tz(df$litterTimestamp, "America/Chicago")

df$tags = as.character(df$tags)
df$tags[df$tags == ""] = "untagged"
df$count = sapply(strsplit(df$tags, ","), length)

df$Date = format(as.POSIXct(df$litterTimestamp,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')
df$Day = wday(df$litterTimestamp, label = TRUE)

df$Hour = format(as.POSIXct(df$litterTimestamp,format='%m/%d/%Y %H:%M:%S'),format='%H')
df$Hour = as.numeric(df$Hour)

litterPerUser = aggregate(df$count, by = list(df$username), sum)
litterPerUser = litterPerUser[order(litterPerUser[,2],decreasing = TRUE),]
user = litterPerUser$Group.1
user = append(user,"All")
litterPerUser = head(litterPerUser,10)
colnames(litterPerUser) = c("User","Items Picked")

litterTags2 = ""
for(i in df$tags)
  litterTags2 = paste(litterTags2,i,sep=",")
litterTags2 = sub(',','',litterTags2)
litterTags2 = strsplit(litterTags2,",")
litterTags2 = as.data.frame(table(litterTags2))
litterTags2 = litterTags2[order(litterTags2[,2],decreasing = TRUE),]
litterTags2 = litterTags2$litterTags2
litterTags2 = as.character(litterTags2)
litterTags2 = append(litterTags2,"All")

timeofDay = c("Morning","Afternoon","Evening","Night","All")
Month = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December","All")
basemap = c("Stamen.Toner", "Default", "Esri.NatGeoWorldMap" ,"Esri.WorldTopoMap" )

# Create the shiny dashboard
ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 1"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
       sidebarMenu(
         
         
         menuItem("Home",tabName = "Home", selected = TRUE),
         selectInput("User", "Select the User #1", user, selected = "All"),
         selectInput("User2", "Select the User #2", user, selected = "All"),
         selectInput("User3", "Select the User #3", user, selected = "All"),
         
         selectInput("Tags", "Select the Tag #1", litterTags2, selected = "All"),
         selectInput("Tags2", "Select the Tag #2", litterTags2, selected = "All"),
         selectInput("Tags3", "Select the Tag #3", litterTags2, selected = "All"),
         
         selectInput("Time", "Select the Time of the Day #1", timeofDay, selected = "All"),
         selectInput("Time2", "Select the Time of the Day #2", timeofDay, selected = "All"),
         selectInput("Time3", "Select the Time of the Day #3", timeofDay, selected = "All"),
         
         selectInput("Month", "Select a Month #1", Month, selected = "All"),
         selectInput("Month2", "Select a Month #2", Month, selected = "All"),
         selectInput("Month3", "Select a Month #3", Month, selected = "All"),
         
         selectInput("User4", "Select a User for viewing Tagged Photo", user, selected = "All"),
         selectInput("Basemap", "Select a Basemap Style", basemap, selected = "Default"),

         menuItem("About",tabName = "About"))
                   
  ),
  dashboardBody(
    tabItems(
      tabItem( tabName = "Home",
        column(2,
          fluidRow(
            h4("Litters collected:",sum(df$count))
          ),
          fluidRow(
            box( title = "Top 10 Pickers", solidHeader = TRUE, status = "primary", width = 12,
              tableOutput("tab1")
            )
          ),
          fluidRow(
            box(title = "Image of Litter", solidHeader = TRUE, status = "primary", width = 12,
                leafletOutput("leaf2", height = 400)
            )
          )
        ),
        column(4,
               
               fluidRow(
                 box( title = "Top Tags", solidHeader = TRUE, status = "primary", width = 12,
                      plotOutput("bar4",height = 300)
                 )
               ),
               fluidRow(
                 box(title = "Top Tags", solidHeader = TRUE, status = "primary", width = 12,
                     dataTableOutput("tab5", height = 300)
                 )
               ),
               fluidRow(
                 box(title = "Location of Litter", solidHeader = TRUE, status = "primary", width = 12,
                     leafletOutput("leaf", height = 300)
                 )
               )
        ),
        column(2,
           
           fluidRow(
             box( title = "Litters Per Day", solidHeader = TRUE, status = "primary", width = 12,
                  plotOutput("bar1",height = 300)
             )
           ),
           fluidRow(
             box( title = "Litters Per Day of the Week", solidHeader = TRUE, status = "primary", width = 12,
                  plotOutput("bar2",height = 300)
             )
           ),
           fluidRow(
             box( title = "Litters Per Hour", solidHeader = TRUE, status = "primary", width = 12,
                  plotOutput("bar3",height = 300)
             )
           )
          ),
        column(4,
            
           fluidRow(
             box(title = "Litters Per Day", solidHeader = TRUE, status = "primary", width = 12,
                 dataTableOutput("tab2", height = 300)
             )
           ),
           fluidRow(
             box(title = "Litters Per Day of the Week", solidHeader = TRUE, status = "primary", width = 12,
                 dataTableOutput("tab3", height = 300)
             )
           ),
           fluidRow(
             box(title = "Litters Per Hour", solidHeader = TRUE, status = "primary", width = 12,
                 dataTableOutput("tab4", height = 300)
             )
          ),
        ),
      ),
    tabItem(tabName = "About",  h1("Author: Ansul Goenka"), h1("Libraries used: shiny, shinydashboard, ggplot2, lubridate, 
    grid, leaflet, DT, stringi, dplyr"), h1("Data by Litterati: https://www.litterati.org/"))
    )
  ))

server <- function(input, output) {
  # increase the default font size
  theme_set(theme_grey(base_size = 18))
  userReactive = reactive({
    req(input$User)
    if(stri_cmp(input$User, "All")!=0)
    {
      
      data = subset(df, df$username == input$User)
    }
    else{
      data = df
    }
  })
  
  baseMapReactive = reactive({
    map = input$Basemap
  })
  
  userReactive4 = reactive({
    if(stri_cmp(input$User4, "All")!=0)
    {
      
      data = subset(df, df$username == input$User4)
    }
    else{
      data = df
    }
  })
  
  tagsReactive = reactive({
    data = userReactive()
    if(stri_cmp(input$Tags, "All")!=0)
    {
      data = subset(data, grepl(input$Tags,data$tags,fixed = TRUE))
      if(dim(data)[1]>0)
        data$tags = input$Tags
      data = data
    }
    else{
      data = data
    }
  })
  
  timeOfDayReactive = reactive({
    data = tagsReactive()
    if(stri_cmp(input$Time, "All")!=0)
    {
      if(stri_cmp(input$Time, "Morning")==0){
        data = subset(data, with(data, Hour >=6 & Hour<=12))
      }
      else if(stri_cmp(input$Time, "AfterNoon")==0){
        data = subset(data, with(data, Hour >=12 & Hour<=16))
      }
      else if(stri_cmp(input$Time, "Evening")==0){
        data = subset(data, with(data, Hour >=16 & Hour<=20))
      }
      else if(stri_cmp(input$Time, "Night")==0){
        data = subset(data, with(data, (Hour >=20 & Hour<=24) | (Hour>=0 & Hour<=6 )))
      }
      data
    }
    else{
      data = data
    }
  })
  
  monthsReactive = reactive({
    data = timeOfDayReactive()
    if(stri_cmp(input$Month, "All")!=0)
    {
      print(paste0("You've chosen: ", input$Month))
      data = subset(data, months.Date(data$litterTimestamp) == input$Month)
    }
    else{
      data = data
    }
  })
  
  userReactive2 = reactive({
    req(input$User2)
    if(stri_cmp(input$User2, "All")!=0)
    {
      data = subset(df, df$username == input$User2)
    }
    else{
      data = df
    }
  })
  
  tagsReactive2 = reactive({
    data = userReactive2()
    if(stri_cmp(input$Tags2, "All")!=0)
    {
      data = subset(data, grepl(input$Tags2,data$tags,fixed = TRUE))
      if(dim(data)[1]>0)
        data$tags = input$Tags2
      data = data
    }
    else{
      data = data
    }
  })
  
  timeOfDayReactive2 = reactive({
    data = tagsReactive2()
    if(stri_cmp(input$Time2, "All")!=0)
    {
      if(stri_cmp(input$Time2, "Morning")==0){
        data = subset(data, with(data, Hour >=6 & Hour<=12))
      }
      else if(stri_cmp(input$Time2, "AfterNoon")==0){
        data = subset(data, with(data, Hour >=12 & Hour<=16))
      }
      else if(stri_cmp(input$Time2, "Evening")==0){
        data = subset(data, with(data, Hour >=16 & Hour<=20))
      }
      else if(stri_cmp(input$Time2, "Night")==0){
        data = subset(data, with(data, (Hour >=20 & Hour<=24) | (Hour>=0 & Hour<=6 )))
      }
      data
    }
    else{
      data = data
    }
  })
  
  monthsReactive2 = reactive({
    data = timeOfDayReactive2()
    if(stri_cmp(input$Month2, "All")!=0)
    {
      data = subset(data, months.Date(data$litterTimestamp) == input$Month2)
    }
    else{
      data = data
    }
  })
  
  userReactive3 = reactive({
    req(input$User3)
    if(stri_cmp(input$User3, "All")!=0)
    {
      data = subset(df, df$username == input$User3)
    }
    else{
      data = df
    }
  })
  
  tagsReactive3 = reactive({
    data = userReactive3()
    if(stri_cmp(input$Tags3, "All")!=0)
    {
      data = subset(data, grepl(input$Tags3,data$tags,fixed = TRUE))
      if(dim(data)[1]>0)
        data$tags = input$Tags3
      data = data
    }
    else{
      data = data
    }
  })
  
  timeOfDayReactive3 = reactive({
    data = tagsReactive3()
    if(stri_cmp(input$Time3, "All")!=0)
    {
      if(stri_cmp(input$Time3, "Morning")==0){
        data = subset(data, with(data, Hour >=6 & Hour<=12))
      }
      else if(stri_cmp(input$Time3, "AfterNoon")==0){
        data = subset(data, with(data, Hour >=12 & Hour<=16))
      }
      else if(stri_cmp(input$Time3, "Evening")==0){
        data = subset(data, with(data, Hour >=16 & Hour<=20))
      }
      else if(stri_cmp(input$Time3, "Night")==0){
        data = subset(data, with(data, (Hour >=20 & Hour<=24) | (Hour>=0 & Hour<=6 )))
      }
      data
    }
    else{
      data = data
    }
  })
  
  monthsReactive3 = reactive({
    data = timeOfDayReactive3()
    if(stri_cmp(input$Month3, "All")!=0)
    {
      data = subset(data, months.Date(data$litterTimestamp) == input$Month3)
    }
    else{
      data = data
    }
  })
  
  selectedReactive = reactive({
    if(stri_cmp(input$User, "All")!=0 || stri_cmp(input$Time, "All")!=0|| input$Month != "All" || stri_cmp(input$Tags, "All")!=0){
      val = 0
    }
    else {
      val = 1
    }
  })
  
  selectedReactive2 = reactive({
    if(stri_cmp(input$User2, "All")!=0 || stri_cmp(input$Time2, "All")!=0|| input$Month2 != "All" || stri_cmp(input$Tags2, "All")!=0){
      val = 0
    }
    else {
      val = 1
    }
  })
  
  selectedReactive3 = reactive({
    if(stri_cmp(input$User3, "All")!=0 || stri_cmp(input$Time3, "All")!=0|| input$Month3 != "All" || stri_cmp(input$Tags3, "All")!=0 ){
      val = 0
    }
    else {
      val = 1
    }
  })
  
  selectedReactiveP3 = reactive({
    if(stri_cmp(input$Tags3, "All")!=0 ){
      val = 0
    }
    else {
      val = 1
    }
  })
  
  litterPerDayTableReactive = reactive({
    data = monthsReactive()
    isSelected = selectedReactive()
    data2 = monthsReactive2()
    isSelected2 = selectedReactive2()
    data3 = monthsReactive3()
    isSelected3 = selectedReactive3()
  
    litterPerDayT <- aggregate(df$count, by = list(df$Date), sum)
    litterPerDayT$Group.1 = as.Date(litterPerDayT$Group.1, "%m/%d/%Y")
    
    if(isSelected == 0 ){
      if(dim(data)[1]>0) {
        litterPerDay <- aggregate(data$count, by = list(data$Date), sum)
        litterPerDay$Group.1 = as.Date(litterPerDay$Group.1, "%m/%d/%Y")
        litterPerDay = merge(litterPerDayT, litterPerDay,by = "Group.1",all = TRUE)
        colnames(litterPerDay)[dim(litterPerDay)[2]] <-  "#1"
      }
      else
        litterPerDay = litterPerDayT
      litterPerDay[is.na(litterPerDay)] <- 0
    }
    if(isSelected2 == 0 ){
      if(isSelected != 0)
        litterPerDay = litterPerDayT
      if(dim(data2)[1]>0) {
        litterPerDay2 <- aggregate(data2$count, by = list(data2$Date), sum)
        litterPerDay2$Group.1 = as.Date(litterPerDay2$Group.1, "%m/%d/%Y")
        litterPerDay = merge(litterPerDay, litterPerDay2,by = "Group.1",all = TRUE)
        colnames(litterPerDay)[dim(litterPerDay)[2]] <- "#2"
      }
      litterPerDay[is.na(litterPerDay)] <- 0
    }
    if(isSelected3 == 0 ){
      if(isSelected != 0 & isSelected2 != 0)
        litterPerDay = litterPerDayT
      if(dim(data3)[1]>0) {
        litterPerDay3 <- aggregate(data3$count, by = list(data3$Date), sum)
        litterPerDay3$Group.1 = as.Date(litterPerDay3$Group.1, "%m/%d/%Y")
        litterPerDay = merge(litterPerDay, litterPerDay3,by = "Group.1",all = TRUE)
        colnames(litterPerDay)[dim(litterPerDay)[2]] <- "#3"
      }
      litterPerDay[is.na(litterPerDay)] <- 0
    }
    if(isSelected!=0 & isSelected2!=0 & isSelected3 !=0)
      litterPerDay = litterPerDayT
    else
      litterPerDay
    colnames(litterPerDay)[1] <- "Date"
    colnames(litterPerDay)[2] <- "Total"
    litterPerDay
  })
  
  litterPerDayReactive = reactive({
    data = monthsReactive()
    isSelected = selectedReactive()
    data2 = monthsReactive2()
    isSelected2 = selectedReactive2()
    data3 = monthsReactive3()
    isSelected3 = selectedReactive3()
    
    litterPerDayT <- aggregate(df$count, by = list(df$Date), sum)
    litterPerDayT$Group.1 = as.Date(litterPerDayT$Group.1, "%m/%d/%Y")
    litterPerDayT$Type = "Total"
    
    if(isSelected == 0 ){
      if(dim(data)[1]>0) {
        litterPerDay <- aggregate(data$count, by = list(data$Date), sum)
        litterPerDay$Group.1 = as.Date(litterPerDay$Group.1, "%m/%d/%Y")
        litterPerDay$Type = "#1"
        litterPerDay = rbind(litterPerDay,litterPerDayT)
      }
      else {
        litterPerDay = litterPerDayT
      }
    }
    if(isSelected2 == 0 ){
      if(isSelected != 0)
        litterPerDay = litterPerDayT
      if(dim(data2)[1]>0) {
        litterPerDay2 <- aggregate(data2$count, by = list(data2$Date), sum)
        litterPerDay2$Group.1 = as.Date(litterPerDay2$Group.1, "%m/%d/%Y")
        litterPerDay2$Type = "#2"
        litterPerDay = rbind(litterPerDay,litterPerDay2)
      }
    }
    if(isSelected3 == 0 ){
      if(isSelected != 0 & isSelected2 != 0)
        litterPerDay = litterPerDayT
      if(dim(data3)[1]>0) {
        litterPerDay3 <- aggregate(data3$count, by = list(data3$Date), sum)
        litterPerDay3$Group.1 = as.Date(litterPerDay3$Group.1, "%m/%d/%Y")
        litterPerDay3$Type = "#3"
        litterPerDay = rbind(litterPerDay,litterPerDay3)
      }
    }
    if(isSelected!=0 & isSelected2!=0 & isSelected3 !=0)
      litterPerDay = litterPerDayT
    else
      litterPerDay
  })
  
  litterPerDayWeekTableReactive = reactive({
    data = monthsReactive()
    isSelected = selectedReactive()
    data2 = monthsReactive2()
    isSelected2 = selectedReactive2()
    data3 = monthsReactive3()
    isSelected3 = selectedReactive3()
    litterPerDayWeekT <- aggregate(df$count, by = list(df$Day), sum)
    if(isSelected == 0){
      if(dim(data)[1]>0) {
        litterPerDayWeek = aggregate(data$count, by = list(data$Day), sum)
        litterPerDayWeek = merge(litterPerDayWeekT, litterPerDayWeek,by = "Group.1",all = TRUE)
        colnames(litterPerDayWeek)[dim(litterPerDayWeek)[2]] <- "#1"
      }
      else
        litterPerDayWeek = litterPerDayWeekT
      litterPerDayWeek[is.na(litterPerDayWeek)] <- 0
    }
    if(isSelected2 == 0){
      if(isSelected !=0 )
        litterPerDayWeek = litterPerDayWeekT
      if(dim(data2)[1]>0) {
        litterPerDayWeek2 = aggregate(data2$count, by = list(data2$Day), sum)
        litterPerDayWeek = merge(litterPerDayWeek, litterPerDayWeek2,by = "Group.1",all = TRUE)
        colnames(litterPerDayWeek)[dim(litterPerDayWeek)[2]] <- "#2"
      }
      litterPerDayWeek[is.na(litterPerDayWeek)] <- 0
    }
    if(isSelected3 == 0){
      if(isSelected !=0 & isSelected2 !=0 )
        litterPerDayWeek = litterPerDayWeekT
      if(dim(data3)[1]>0) {
        litterPerDayWeek3 = aggregate(data3$count, by = list(data3$Day), sum)
        litterPerDayWeek = merge(litterPerDayWeek, litterPerDayWeek3,by = "Group.1",all = TRUE)
        colnames(litterPerDayWeek)[dim(litterPerDayWeek)[2]] <- "#3"
      }
      litterPerDayWeek[is.na(litterPerDayWeek)] <- 0
      colnames(litterPerDayWeek)[1] <- "Day"
      colnames(litterPerDayWeek)[2] <- "Total"
    }
    if(isSelected!=0 & isSelected2!=0 & isSelected3 !=0)
      litterPerDayWeek = litterPerDayWeekT
    else
      litterPerDayWeek
    colnames(litterPerDayWeek)[1] <- "Day"
    colnames(litterPerDayWeek)[2] <- "Total"
    litterPerDayWeek
  })
  
  litterPerDayWeekReactive = reactive({
    data = monthsReactive()
    isSelected = selectedReactive()
    data2 = monthsReactive2()
    isSelected2 = selectedReactive2()
    data3 = monthsReactive3()
    isSelected3 = selectedReactive3()
    if(dim(data)[1]>0) {
      litterPerDayWeek = aggregate(data$count, by = list(data$Day), sum)
    }
    if(dim(data2)[1]>0) {
      litterPerDayWeek2 = aggregate(data2$count, by = list(data2$Day), sum)
    }
    if(dim(data3)[1]>0) {
      litterPerDayWeek3 = aggregate(data3$count, by = list(data3$Day), sum)
    }
    litterPerDayWeekT <- aggregate(df$count, by = list(df$Day), sum)
    litterPerDayWeekT$Type = "Total"
    
    if(isSelected == 0 ){
      if(dim(data)[1]>0) {
        litterPerDayWeek$Type = "#1"
        litterPerDayWeek = rbind(litterPerDayWeek,litterPerDayWeekT)
      }
      else {
        litterPerDayWeek = litterPerDayWeekT
      }
    }
    if(isSelected2 == 0 ){
      if(isSelected2 != 0)
        litterPerDayWeek = litterPerDayWeekT
      if(dim(data2)[1]>0) {
        litterPerDayWeek2$Type = "#2"
        litterPerDayWeek = rbind(litterPerDayWeek,litterPerDayWeek2)
      }
      else
        litterPerDayWeek = litterPerDayWeekT
    }
    if(isSelected3 == 0 ){
      if(isSelected2 != 0 & isSelected != 0)
        litterPerDayWeek = litterPerDayWeekT
      if(dim(data3)[1]>0) {
        litterPerDayWeek3$Type = "#3"
        litterPerDayWeek = rbind(litterPerDayWeek,litterPerDayWeek3)
      }
    }
    if(isSelected!=0 & isSelected2!=0 & isSelected3 !=0)
      litterPerDayWeek = litterPerDayWeekT
    else
      litterPerDayWeek
    
  })
  
  litterPerHourTableReactive = reactive({
    data = monthsReactive()
    isSelected = selectedReactive()
    data2 = monthsReactive2()
    isSelected2 = selectedReactive2()
    data3 = monthsReactive3()
    isSelected3 = selectedReactive3()
    
    litterPerHourT <- aggregate(df$count, by = list(df$Hour), sum)
    
    if(isSelected == 0 ){
      if(dim(data)[1]>0) {
        litterPerHour = aggregate(data$count, by = list(data$Hour), sum)
        litterPerHour = merge(litterPerHourT, litterPerHour,by = "Group.1",all = TRUE)
        colnames(litterPerHour)[dim(litterPerHour)[2]] <- "#1"
      }
      else
        litterPerHour = litterPerHourT
      litterPerHour[is.na(litterPerHour)] <- 0
    }
    if(isSelected2 == 0 ){
      if(isSelected != 0) {
        litterPerHour = litterPerHourT
      }
      if(dim(data2)[1]>0) {
        litterPerHour2 = aggregate(data2$count, by = list(data2$Hour), sum)
        litterPerHour = merge(litterPerHour, litterPerHour2,by = "Group.1",all = TRUE)
        colnames(litterPerHour)[dim(litterPerHour)[2]] <- "#2"
      }
      litterPerHour[is.na(litterPerHour)] <- 0
    }
    if(isSelected3 == 0 ){
      if(isSelected !=0 & isSelected2 !=0) {
        litterPerHour = litterPerHourT
      }
      if(dim(data3)[1]>0) {
        litterPerHour3 = aggregate(data3$count, by = list(data3$Hour), sum)
        litterPerHour = merge(litterPerHour, litterPerHour3,by = "Group.1",all = TRUE)
        colnames(litterPerHour)[dim(litterPerHour)[2]] <- "#3"
      }
      litterPerHour[is.na(litterPerHour)] <- 0
    }
    if(isSelected!=0 & isSelected2!=0 & isSelected3 !=0)
      litterPerHour = litterPerHourT
    else
      litterPerHour
    colnames(litterPerHour)[1] <- "Hour"
    colnames(litterPerHour)[2] <- "Total"
    litterPerHour
  })
  
  litterPerHourReactive = reactive({
    data = monthsReactive()
    isSelected = selectedReactive()
    data2 = monthsReactive2()
    isSelected2 = selectedReactive2()
    data3 = monthsReactive3()
    isSelected3 = selectedReactive3()
    
    litterPerHourT <- aggregate(df$count, by = list(df$Hour), sum)
    litterPerHourT$Type = "Total"
    
    if(isSelected == 0 ){
      if(dim(data)[1]>0) {
        litterPerHour = aggregate(data$count, by = list(data$Hour), sum)
        litterPerHour$Type = "#1"
        litterPerHour = rbind(litterPerHour,litterPerHourT)
      }
      else {
        litterPerHour = litterPerHourT
      }
    }
    if(isSelected2 == 0 ){
      if(isSelected != 0) {
        litterPerHour = litterPerHourT
      }
      if(dim(data2)[1]>0) {
        litterPerHour2 = aggregate(data2$count, by = list(data2$Hour), sum)
        litterPerHour2$Type = "#2"
        litterPerHour = rbind(litterPerHour,litterPerHour2)
      }
    }
    if(isSelected3 == 0 ){
      if(isSelected !=0 & isSelected2 !=0) {
        litterPerHour = litterPerHourT
      }
      if(dim(data3)[1]>0) {
        litterPerHour3 = aggregate(data3$count, by = list(data3$Hour), sum)
        litterPerHour3$Type = "#3"
        litterPerHour = rbind(litterPerHour,litterPerHour3)
      }
    }
    if(isSelected!=0 & isSelected2!=0 & isSelected3 !=0)
      litterPerHour = litterPerHourT
    else
      litterPerHour
  })
  
  litterTagsTableReactive = reactive({
    data = monthsReactive()
    isSelected = selectedReactive()
    data2 = monthsReactive2()
    isSelected2 = selectedReactive2()
    data3 = monthsReactive3()
    isSelected3 = selectedReactive3()
    
    litterTagsT = ""
    for(i in df$tags)
      litterTagsT = paste(litterTagsT,i,sep=",")
    litterTagsT = sub(',','',litterTagsT)
    litterTagsT = strsplit(litterTagsT,",")
    litterTagsT = as.data.frame(table(litterTagsT))
    litterTagsT = head(litterTagsT[order(litterTagsT[,2],decreasing = TRUE),],10)
    colnames(litterTagsT)[1] = "litterTags"
    
    if(isSelected == 0 ){
      if(dim(data)[1]>0) {
        litterTags = ""
        for(i in data$tags)
          litterTags = paste(litterTags,i,sep=",")
        litterTags = sub(',','',litterTags)
        litterTags = strsplit(litterTags,",")
        litterTags = as.data.frame(table(litterTags))
        litterTags = head(litterTags[order(litterTags[,2],decreasing = TRUE),],10)
        litterTags = merge(litterTagsT, litterTags,by = "litterTags",all = TRUE)
        colnames(litterTags)[dim(litterTags)[2]] <- "#1"
      }
      else {
        litterTags = litterTagsT
      }
      litterTags[is.na(litterTags)] <- 0
    }
    if(isSelected2 == 0 ){
      if(isSelected != 0) {
        litterTags = litterTagsT
      }
      if(dim(data2)[1]>0) {
        litterTags2 = ""
        for(i in data2$tags)
          litterTags2 = paste(litterTags2,i,sep=",")
        litterTags2 = sub(',','',litterTags2)
        litterTags2 = strsplit(litterTags2,",")
        litterTags2 = as.data.frame(table(litterTags2))
        litterTags2 = head(litterTags2[order(litterTags2[,2],decreasing = TRUE),],10)
        colnames(litterTags2)[1] = "litterTags"
        litterTags = merge(litterTags, litterTags2,by = "litterTags",all = TRUE)
        colnames(litterTags)[dim(litterTags)[2]] <- "#2"
      }
      litterTags[is.na(litterTags)] <- 0
    }
    if(isSelected3 == 0 ){
      if(isSelected != 0 & isSelected2 != 0) {
        litterTags = litterTagsT
      }
      if(dim(data3)[1]>0) {
        litterTags3 = ""
        for(i in data3$tags)
          litterTags3 = paste(litterTags3,i,sep=",")
        litterTags3 = sub(',','',litterTags3)
        litterTags3 = strsplit(litterTags3,",")
        litterTags3 = as.data.frame(table(litterTags3))
        litterTags3 = head(litterTags3[order(litterTags3[,2],decreasing = TRUE),],10)
        colnames(litterTags3)[1] = "litterTags"
        litterTags = merge(litterTags, litterTags3,by = "litterTags",all = TRUE)
        colnames(litterTags)[dim(litterTags)[2]] <- "#3"
      }
      litterTags[is.na(litterTags)] <- 0
      colnames(litterTags)[1] <- "Tag"
      colnames(litterTags)[2] <- "Total"
    }
    if(isSelected!=0 & isSelected2!=0 & isSelected3 !=0)
      litterTags = litterTagsT
    else
      litterTags
    colnames(litterTags)[1] <- "Tag"
    colnames(litterTags)[2] <- "Total"
    litterTags
  })
  
  litterTagsReactive = reactive({
    data = monthsReactive()
    isSelected = selectedReactive()
    data2 = monthsReactive2()
    isSelected2 = selectedReactive2()
    data3 = monthsReactive3()
    isSelected3 = selectedReactive3()
    litterTagsT = ""
    for(i in df$tags)
        litterTagsT = paste(litterTagsT,i,sep=",")
      litterTagsT = sub(',','',litterTagsT)
      litterTagsT = strsplit(litterTagsT,",")
      litterTagsT = as.data.frame(table(litterTagsT))
      litterTagsT = head(litterTagsT[order(litterTagsT[,2],decreasing = TRUE),],10)
      litterTagsT$Type = "Total"
      colnames(litterTagsT)[1] = "litterTags"
    
    if(isSelected == 0 ){
      litterTags = ""
      if(dim(data)[1]>0) {
        for(i in data$tags)
          litterTags = paste(litterTags,i,sep=",")
        litterTags = sub(',','',litterTags)
        litterTags = strsplit(litterTags,",")
        litterTags = as.data.frame(table(litterTags))
        litterTags = head(litterTags[order(litterTags[,2],decreasing = TRUE),],10)
        litterTags$Type = "#1"
        litterTags = rbind(litterTags,litterTagsT)
      }
      else {
        litterTags = litterTagsT
      }
    }
    if(isSelected2 == 0 ){
      if(isSelected != 0) {
        litterTags = litterTagsT
      }
      litterTags2 = ""
      if(dim(data2)[1]>0) {
        for(i in data2$tags)
          litterTags2 = paste(litterTags2,i,sep=",")
        litterTags2 = sub(',','',litterTags2)
        litterTags2 = strsplit(litterTags2,",")
        litterTags2 = as.data.frame(table(litterTags2))
        litterTags2 = head(litterTags2[order(litterTags2[,2],decreasing = TRUE),],10)
        litterTags2$Type = "#2"
        colnames(litterTags2)[1] = "litterTags"
        litterTags = rbind(litterTags,litterTags2)
      }
    }
    if(isSelected3 == 0 ){
      if(isSelected != 0 & isSelected2 != 0) {
        litterTags = litterTagsT
      }
      litterTags3 = ""
      if(dim(data3)[1]>0) {
        for(i in data3$tags)
          litterTags3 = paste(litterTags3,i,sep=",")
        litterTags3 = sub(',','',litterTags3)
        litterTags3 = strsplit(litterTags3,",")
        litterTags3 = as.data.frame(table(litterTags3))
        litterTags3 = head(litterTags3[order(litterTags3[,2],decreasing = TRUE),],10)
        litterTags3$Type = "#3"
        colnames(litterTags3)[1] = "litterTags"
        litterTags = rbind(litterTags,litterTags3)
      }
    }
    if(isSelected!=0 & isSelected2!=0 & isSelected3 !=0)
      litterTags = litterTagsT
    else
      litterTags
  })
  
  output$tab1 <- renderTable(
    litterPerUser
  )
  output$bar1 <- renderPlot({
    litterPerDay = litterPerDayReactive()
    isSelected = selectedReactive()
    isSelected2 = selectedReactive2()
    isSelected3 = selectedReactive3()
    colnames(litterPerDay)[1] = "Date"
    if(isSelected == 0 | isSelected2 == 0 | isSelected3 == 0) {
      ggplot(litterPerDay) + geom_bar(aes(fill=Type,x=Date,y=x),stat="identity",position = position_dodge2()) + 
        labs(x="Date", y = "Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    else {
      ggplot(litterPerDay) + geom_bar(aes(x=Date,y=x),stat="identity",fill = "steelblue") + labs(x="Date", y = "Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
  })
  output$bar2 <- renderPlot({
    litterPerDayWeek = litterPerDayWeekReactive()
    isSelected = selectedReactive()
    colnames(litterPerDayWeek)[1] = "Day"
    if(isSelected == 0) {
      ggplot(litterPerDayWeek) + geom_bar(aes(x=Day,y=x, fill = Type) ,stat="identity",position = position_dodge2()) + labs(x="Day", y = "Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    else {
      ggplot(litterPerDayWeek) + geom_bar(aes(x=Day,y=x) ,stat="identity",fill = "steelblue") + labs(x="Day", y = "Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
  })
  output$bar3 <- renderPlot({
    litterPerHour = litterPerHourReactive()
    isSelected = selectedReactive()
    colnames(litterPerHour)[1] = "Hour"
    if(isSelected == 0) {
      ggplot(litterPerHour) + geom_bar(aes(x=Hour,y=x,fill=Type),stat="identity", position = position_dodge2()) + 
        labs(x="Hour", y = "Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    else {
      ggplot(litterPerHour, aes(x=Hour,y=x)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Hour", y = "Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
  })
  output$bar4 <- renderPlot({
    litterTags = litterTagsReactive()
    isSelected = selectedReactive()
    if(isSelected == 0) {
      ggplot(litterTags) + geom_bar(aes(x=litterTags,y=Freq,fill=Type),stat="identity",position = position_dodge2()) + 
        labs(x="Tags", y = "Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    else {
      ggplot(litterTags, aes(x=litterTags,y=Freq)) + geom_bar(stat="identity", fill="steelblue") + 
        labs(x="Tags", y = "Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
  })
  
  mapReactive = reactive({
    
  })
  
  output$leaf <- renderLeaflet({
    data = monthsReactive()
    isSelected = selectedReactive()
    data2 = monthsReactive2()
    isSelected2 = selectedReactive2()
    data3 = monthsReactive3()
    isSelected3 = selectedReactive3()
    basemap = baseMapReactive()
    if(isSelected2 == 0)
      data = rbind(data,data2)
    if(isSelected3 == 0)
      data = rbind(data,data3)
    map <- leaflet(data)
    
    if(basemap == "Default")
      map <- addTiles(map)
    else {
      map <- addProviderTiles(map, provider = basemap)
    }
    map <- addCircleMarkers(map,data = data, color = '#FA5',lng = data$lon, lat = data$lat, 
                            clusterOptions = markerClusterOptions(iconCreateFunction = JS("function (cluster) {    
      var childCount = cluster.getChildCount(); 
      var c = ' marker-cluster-';  
      if (childCount < 3) {  
        c += 'large';  
      } else if (childCount < 5) {  
        c += 'large';  
      } else { 
        c += 'large';  
      }    
      return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', 
      className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });

    }")))
    if(isSelected2 == 0) {
      map <- addCircleMarkers(map,data = data2, color = '#9D7',lng = data2$lon, lat = data2$lat, 
                              clusterOptions = markerClusterOptions(iconCreateFunction = JS("function (cluster) {    
        var childCount = cluster.getChildCount(); 
        var c = ' marker-cluster-';  
        if (childCount < 3) {  
          c += 'large';  
        } else if (childCount < 5) {  
          c += 'large';  
        } else { 
          c += 'large';  
        }    
        return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', 
        className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });
  
      }")))
      }
    if(isSelected3 == 0) {
      map <- addCircleMarkers(map,data = data3, color = '#00C',lng = data3$lon, lat = data3$lat, 
                              clusterOptions = markerClusterOptions(iconCreateFunction = JS("function (cluster) {    
        var childCount = cluster.getChildCount(); 
        var c = ' marker-cluster-';  
        if (childCount < 3) {  
          c += 'large';  
        } else if (childCount < 5) {  
          c += 'large';  
        } else { 
          c += 'large';  
        }    
        return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', 
        className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });
  
      }")))
    }
    
    map
  })
  
  output$leaf2 <- renderLeaflet({
    data = userReactive4()
    map <- leaflet(data)
    map <- addTiles(map)
    map <- addCircleMarkers(map,data = data, lng = data$lon, lat = data$lat, clusterOptions = markerClusterOptions(),popup = paste0("<img src = ", data$url, ">")) 
    map
  })
  
  output$tab2 <- DT::renderDataTable(
    DT::datatable({ 
      litterPerDay = as.data.frame(litterPerDayTableReactive())
      litterPerDay
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$tab3 <- DT::renderDataTable(
    DT::datatable({ 
      litterPerDayWeek = litterPerDayWeekTableReactive()
    }, 
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$tab4 <- DT::renderDataTable(
    DT::datatable({ 
      litterPerHour = litterPerHourTableReactive()
      litterPerHour$Hour = as.character(litterPerHour$Hour)
      litterPerHour
    }, 
    options = list(searching = FALSE, pageLength = 4, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$tab5 <- DT::renderDataTable(
    DT::datatable({ 
      litterTags = litterTagsTableReactive()
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
}

shinyApp(ui = ui, server = server)

