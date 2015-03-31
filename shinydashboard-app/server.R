#server.R

library(rCharts)
#two libraries for the bullet graph
library(htmlwidgets)
library(plyr)
library(grid)
source("helpers/BulletGraphSipemu.txt")


#function to convert to POSIXct date format, specifically for line chart
to_jsdate2 <- function(x){
  as.numeric(as.POSIXct(as.Date(x), origin="1970-01-01")) * 1000
}

#read in an altered sample of JIRA data 
merged <- read.csv("data/merged.csv")

#change the date columns format to Date
merged$created <- as.Date(merged$created, format = "%m/%d/%Y %H:%M")

#sort the data by date
merged <- merged[order(merged$created),]

#create a month variable for aggregation
merged$created_month <- as.Date(cut(merged$created, "month"))

#create a vector with cumulative sum of unique PIs
unique_PIs <- cummax(as.numeric(factor(merged$PI_name, levels = unique(merged$PI_name))))

#matching cumulative sum of unique PIs to unique months
PI_cumul_growth <- aggregate(unique_PIs, list(Month=merged$created_month), max)

#add variable with only new PIs added in each month
PI_cumul_growth$new_pi_num <- c(PI_cumul_growth$x[1], (tail(PI_cumul_growth$x, -1) - head(PI_cumul_growth$x, -1)))


shinyServer(
  function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$PI_growth <- renderChart({
      
      #convert the date format for JSON-friendly
      PI_cumul_growth$date <- to_jsdate2(as.Date(PI_cumul_growth$Month))
      #plot a line chart
      PI_growth_plot <- Highcharts$new()
      PI_growth_plot$series(name = "PIs", data = toJSONArray2(PI_cumul_growth[,c("date", "x")], json = F, names = F))
      PI_growth_plot$title(text = "Number of PIs")
      PI_growth_plot$xAxis(type='datetime', title = list(text = "Time"))
      PI_growth_plot$yAxis(title = list(text = "PIs"), 
                        labels = list(style=list(color= '#000066', fontWeight= 'bold')),
                        min = 0, gridLineColor = "#ffffff")
      PI_growth_plot$plotOptions(line = list(color = "#6699FF", marker = list(enabled = F)))
      PI_growth_plot$tooltip(dateTimeLabelFormats = list(month = "%B %Y"))
      PI_growth_plot$legend(enabled=F)
      PI_growth_plot$chart(zoomType="x", height = 300)
      PI_growth_plot$addParams(dom = 'PI_growth')
      return(PI_growth_plot)
    })
    
    output$new_PIs <- renderChart({
      PI_cumul_growth$strdate <- as.character(PI_cumul_growth$Month)
      prolific_months <- PI_cumul_growth[order(-PI_cumul_growth$new_pi_num),]
      newPIs_plot <- hPlot(new_pi_num ~ strdate, data = head(prolific_months, 7), type = "column", 
                           title = "Most Prolific Months")
      newPIs_plot$xAxis(type = "category", title = list(text = "Month Start"))
      newPIs_plot$yAxis(title = list(text = "New PIs Added"), gridLineColor = "#ffffff")
      newPIs_plot$tooltip( formatter = "#! function() { return this.point.y + ' New PIs'; } !#")
      newPIs_plot$chart(height = 300)
      newPIs_plot$addParams(dom = 'new_PIs')
      return(newPIs_plot)
    })
    
    output$bulletGraph <- renderPlot({
      #subset projects that are open (not "Completed", "Closed", "Resolved", "Done")
      currentProjects <- subset(merged, !status %in% c("Done", "Completed", "Closed", "Resolved"))
      currentJITs <- subset(currentProjects, FundingType == "JIT")
      All_projects <- data.frame(measure = c("JITs", "All Projects"),
        units = c("Tickets","Tickets"),
        low = c(5,20),
        mean = c(8,50),
        high = c(length(unique(as.character(currentJITs$id)))+5,length(unique(as.character(currentProjects$id)))+5),
        target = c(10,70), 
        value = c(length(unique(as.character(currentJITs$id))),length(unique(as.character(currentProjects$id)))))
         
        gridBulletGraphH(All_projects, nticks=10, 
                         format=c("s","s"), bcol=c("#a5a7a9", "#c5c6c8", "#e6e6e7"), font=11,
                         scfont=9, ptitle="Projects in Pipeline") 
      
    })
    
    output$mostPopularTool <- renderChart({
      valid_app_tickets <- subset(merged, Application != "")
      PI_by_product <- ddply(valid_app_tickets, c("Application"), summarise, uniquePIs = length(unique(PI_name)))
      PI_by_product <- PI_by_product[order(-PI_by_product$uniquePIs),]
      topProducts <- head(PI_by_product, (which(PI_by_product$Application=="Other")-1))
      topProducts <- rbind(topProducts, c("Other", sum(tail(PI_by_product$uniquePIs,-(which(PI_by_product$Application=="Other")-1))))) 
      topProducts$uniquePIs <- as.numeric(topProducts$uniquePIs)
      mostPopularProduct <- hPlot(uniquePIs ~ Application, type = 'column', data = topProducts, title = "Most Popular Product")
      mostPopularProduct$yAxis(gridLineColor = "#ffffff")
      mostPopularProduct$chart(height = 300, zoomType = "xy", marginRight= 50)
      mostPopularProduct$addParams(dom = 'mostPopularTool')
      return(mostPopularProduct)
    })

    output$PI_number <- renderValueBox({
      valueBox(
        paste0(tail(PI_cumul_growth$x, 1)), "Total PIs", icon = icon("user", lib = "glyphicon"),
        color = "yellow"
      )
    })
    
    output$JIT_number <- renderValueBox({
      #subset projects that are open (not "Completed", "Closed", "Resolved", "Done")
      currentProjects <- subset(merged, !status %in% c("Done", "Completed", "Closed", "Resolved"))
      currentJITs <- subset(currentProjects, FundingType == "JIT")
      valueBox(
        paste0(length(unique(as.character(currentJITs$id)))), "JITs in Pipeline", icon = icon("dollar"), 
        color = "green")
    })

  })