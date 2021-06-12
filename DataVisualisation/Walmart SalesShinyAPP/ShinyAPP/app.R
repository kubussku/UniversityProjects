#Packages
library(shiny)    #shinyapp
library(plotly)   #charts    
library(readr)    # load csv
library(plyr)     # merge df's
library(zoo)      # moving avg
library(DT)       # table display
library(reshape2) # melt corrmatrix
# LOAD AND CLEANSE____
df = read_csv("F:/WALMART SALES SHINYAPP/data/train.csv")
features = read_csv("F:/WALMART SALES SHINYAPP/data/features.csv")
data = merge(df,features,by=c('Store','Date')) 
data = subset(data, select = -Dept)
#subplot data
data2 = aggregate(Weekly_Sales ~ Store+Date+Temperature+Fuel_Price+CPI+Unemployment , data, sum) 
data2 = data2[order(as.Date(data2$Date, format="%Y-%m-%d"),decreasing = TRUE),]
data =  aggregate(Weekly_Sales ~ Store+Date , data, sum) 


# ________________PLOT  FUNCTIONS____________________________ #
FIG1 = function(storeid,somevalue,somevalue1){
    fig <- data[data$Store == storeid,]  %>% plot_ly(x = ~Date, type="scatter", mode = 'lines',y = ~Weekly_Sales)   
    fig <- fig %>% layout(showlegend = FALSE)
    fig <- fig %>% rangeslider()
    #ADDING REGRESSION
    if (somevalue == T){
        fit <- lm(Weekly_Sales ~ Date, data = tail(data[data$Store ==storeid,],12))                                    
        fig  = fig %>% add_lines(x = ~tail(data$Date[data$Store == storeid],12), y = fitted(fit))  }                
    #ADDING ROLLING AVG - 3
    if (somevalue1 == T){
        backcut = head(data$Date[data$Store == storeid],-1)                                                              
        Dates = tail(backcut,-1)
        fig  = fig %>% add_lines(x = ~Dates, y = rollmean(data$Weekly_Sales[data$Store == storeid], 3))}               
    fig 
}
FIG2 = function(storeid,somevalue2,somevalue3,somevalue4,somevalue5,somevalue6,somevalue7,somevalue8,somevalue9){
    fig2 = data2[data2$Store == storeid,] %>% plot_ly(x = ~Date) %>%                                                             
        add_lines(x = data2$Date[data2$Store == storeid], y = data2$Temperature[data2$Store == storeid], name = 'Temperatura(F)')
        if (somevalue2 == T){
            fit <- lm(Temperature ~ Date, data = head(data2[data2$Store ==storeid,],12))                                     
            fig2  = fig2 %>% add_lines(x = head(data2$Date[data2$Store == storeid],12), y = fitted(fit))  }
        if (somevalue3 == T){
            backcut = head(data2$Date[data2$Store == storeid],-1)                                                             
            Dates = tail(backcut,-1)
            fig2  = fig2 %>% add_lines(x = ~Dates, y = rollmean(data2$Temperature[data2$Store == storeid], 3))}     
    
    fig3 = data2[data2$Store == storeid,] %>% plot_ly(x = ~Date) %>%                                                          
        add_lines(x = data2$Date[data2$Store == storeid], y = data2$Fuel_Price[data2$Store == storeid], name = 'Cena Paliwa (USD/galon?)')
        if (somevalue4 == T){
            fit <- lm(Fuel_Price ~ Date, data = head(data2[data2$Store ==storeid,],12))                                   
            fig3  = fig3 %>% add_lines(x = head(data2$Date[data2$Store == storeid],12), y = fitted(fit))  }
        if (somevalue5 == T){
            backcut = head(data2$Date[data2$Store == storeid],-1)                                                          
            Dates = tail(backcut,-1)
            fig3  = fig3 %>% add_lines(x = ~Dates, y = rollmean(data2$Fuel_Price[data2$Store == storeid], 3))} 
    fig4 = data2[data2$Store == storeid,] %>% plot_ly(x = ~Date) %>%                                                          
        add_lines(x = data2$Date[data2$Store == storeid], y = data2$CPI[data2$Store == storeid], name = 'CPI')
        if (somevalue6 == T){
            fit <- lm(CPI ~ Date, data = head(data2[data2$Store ==storeid,],12))                                    
            fig4  = fig4 %>% add_lines(x = head(data2$Date[data2$Store == storeid],12), y = fitted(fit))  }
        if (somevalue7 == T){
            backcut = head(data2$Date[data2$Store == storeid],-1)                                                       
            Dates = tail(backcut,-1)
            fig4  = fig4 %>% add_lines(x = ~Dates, y = rollmean(data2$CPI[data2$Store == storeid], 3))} 
    fig5 = data2[data2$Store == storeid,] %>% plot_ly(x = ~Date) %>%                                                      
        add_lines(x = data2$Date[data2$Store == storeid], y = data2$Unemployment[data2$Store == storeid], name = 'Wskaznik bezrobocia')
        if (somevalue8 == T){
            fit <- lm(Unemployment ~ Date, data = head(data2[data2$Store ==storeid,],12))                                     
            fig5  = fig5 %>% add_lines(x = head(data2$Date[data2$Store == storeid],12), y = fitted(fit))  }
        if (somevalue9 == T){
            backcut = head(data2$Date[data2$Store == storeid],-1)                                                           
            Dates = tail(backcut,-1)
            fig5  = fig5 %>% add_lines(x = ~Dates, y = rollmean(data2$Unemployment[data2$Store == storeid], 3))} 
    subfig = subplot(fig2,fig3,fig4,fig5, shareX = T, nrows = 4,titleY = T,titleX =T) %>% rangeslider() %>% layout(showlegend = FALSE)
    subfig 
}
TAB1 = function(storeid){
    summstats = do.call(cbind, lapply(data2[data2$Store == storeid,], summary))
    summstats = round(summstats,2)
    return(data.frame(Statistic = row.names(summstats),summstats[,c(-1,-2)]))
}
CORPLOT = function(storeid){
    data1 <- data2[data2$Store == storeid,c(3:7)]
    corrdata <- cor(data1)
    
    corrdata[upper.tri(corrdata, diag = TRUE)] <- NA
    corrdata <- corrdata[-1, -ncol(corrdata)]
    
    #Store our variable names for later use
    x_labels <- colnames(corrdata)
    y_labels <- rownames(corrdata)
    
    #Change the variable names to numeric for the grid
    colnames(corrdata) <- 1:ncol(corrdata)
    rownames(corrdata) <- nrow(corrdata):1
    
    #Melt the data into the desired format
    plotdata <- melt(corrdata)
    
    #Adding the size variable & scaling it
    plotdata$size <- (abs(plotdata$value))
    scaling <- 500 / ncol(corrdata) / 2
    plotdata$size <- plotdata$size * scaling
    
    #Setting x and y ranges for the chart
    xrange <- c(0.5, length(x_labels)+0.5)
    yrange <- c(0.5, length(y_labels)+0.5)
    
    #Setting the gridlines
    x_grid <- seq(1.5, length(x_labels)-0.5, 1)
    y_grid <- seq(1.5, length(y_labels)-0.5, 1)
    
    #Axes definitions
    xAx1 <- list(showgrid = FALSE,
                 showline = FALSE,
                 zeroline =  FALSE,
                 tickvals = colnames(corrdata),
                 ticktext = x_labels,
                 title = "",
                 range = xrange,
                 rangemode = "tozero")
    
    xAx2 <- list(showgrid = TRUE,
                 showline = FALSE,
                 zeroline =  FALSE,
                 overlaying = "x",
                 showticklabels = FALSE,
                 range = xrange,
                 tickvals = x_grid)
    
    yAx1 <- list(autoaxis = FALSE,
                 showgrid = FALSE,
                 showline = FALSE,
                 zeroline =  FALSE,
                 tickvals = rownames(corrdata),
                 ticktext = y_labels,
                 title = FALSE,
                 rangemode = "tozero",
                 range = yrange)
    
    yAx2 <- list(showgrid = TRUE,
                 showline = FALSE,
                 zeroline =  FALSE,
                 overlaying = "y",
                 showticklabels = FALSE,
                 range = yrange,
                 tickvals = y_grid)
    
    
    corfig <- plot_ly(data = plotdata, width = 600, height = 400)
    corfig <- corfig %>% add_trace(x = ~Var2, y = ~Var1, type = "scatter", mode = "markers",
                                   color = ~value,
                                   marker = list(size = ~size, opacity = 1),
                                   symbol = I("square"),
                                   text = ~value,
                                   hovertemplate = "%{text:.2f} <extra></extra>",
                                   xaxis = "x1",
                                   yaxis = "y1")
    
    corfig <- corfig %>% add_trace(x = ~Var2, y = ~Var1, type = "scatter", mode = "markers",
                                   opacity = 0,
                                   showlegend = FALSE,
                                   xaxis = "x2",
                                   yaxis = "y2",
                                   hoverinfo = "none")
    
    corfig <- corfig %>% layout(xaxis = xAx1,
                                yaxis = yAx1, 
                                xaxis2 = xAx2,
                                yaxis2 = yAx2,
                                plot_bgcolor = "rgba(0,0,0,0)",
                                paper_bgcolor = "rgba(0, 0, 0, 0.03)")
    
    corfig <- corfig %>% colorbar(title = "", limits = c(-1,1), x = 1.1, y = 0.75)
    corfig
}




#AP UI
ui <- fluidPage(
    titlePanel(
        h1("Walmart weekly sales basic time series!", align = "center")),
    fluidRow(
        column(4,
               selectInput('storeid','Store ID', levels(factor(data$Store))),
               tableOutput('table')
        ),
        column(2,
               mainPanel(
                   h5("T-Series tools:"),
                   h5("Sales weekly"),width = 10),
               checkboxInput("somevalue", "Regression - last 12 periods", FALSE),
               checkboxInput("somevalue1", "Rolling avg - 5", FALSE),
               
        ),
        column(6,
               plotlyOutput('plot')
               
        )      
    ),
    fluidRow(
        column(4,
               plotlyOutput('plot3')
        ),
        column(2,
               mainPanel(
                   h5("T-Series tools:"),
                   h5("Temperature"),width = 10),
               checkboxInput("somevalue2", "Regression - last 12 periods", FALSE),
               checkboxInput("somevalue3", "Rolling avg - 5", FALSE),
               mainPanel(
                   h5("Fuel Price"),width = 10),
               checkboxInput("somevalue4", "Regression - last 12 periods", FALSE),
               checkboxInput("somevalue5", "Rolling avg - 5", FALSE),
               mainPanel(
                   h5("CPI"),width = 10),
               checkboxInput("somevalue6", "Regression - last 12 periods", FALSE),
               checkboxInput("somevalue7", "Rolling avg - 5", FALSE),
               mainPanel(
                   h5("Unemployment index"),width = 10),
               checkboxInput("somevalue8", "Regression - last 12 periods", FALSE),
               checkboxInput("somevalue9", "Rolling avg - 5", FALSE),
        ),
        column(6,
               plotlyOutput('plot1')
        )  
    )
)


#SERVER RESPONSE
server <- function(input, output) {
    store <- reactive({
        input$storeid
    })
    output$value <- renderText({ input$somevalue })
    output$value1 <- renderText({ input$somevalue1 })
    output$value2 <- renderText({ input$somevalue2 })
    output$value3 <- renderText({ input$somevalue3 })
    output$value4 <- renderText({ input$somevalue4 })
    output$value5 <- renderText({ input$somevalue5 })
    output$value6 <- renderText({ input$somevalue6 })
    output$value7 <- renderText({ input$somevalue7 })
    output$value8 <- renderText({ input$somevalue8 })
    output$value9 <- renderText({ input$somevalue9 })
    output$plot <- renderPlotly(
        plot1 <- FIG1(input$storeid, input$somevalue, input$somevalue1))
    output$table <- renderTable(
        TAB1(input$storeid))
    output$plot3 <- renderPlotly(
        plot3 <- CORPLOT(input$storeid))
    output$plot1 <- renderPlotly(
        plot2 <- FIG2(input$storeid,input$somevalue2,input$somevalue3,input$somevalue4,
                      input$somevalue5,input$somevalue6,input$somevalue7,input$somevalue8,
                      input$somevalue9))
}

#RUNAPP
shinyApp(ui = ui, server = server)



# TODO:
# Proper chart titles and axis naming, 
# Styling 

