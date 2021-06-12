# Packages
library(plotly)   # pretty charts
library(readr)    # load csv
library(plyr)     # merge df's
library(zoo)      # moving avg

# LOAD AND CLEANSE____
df = read_csv("data/train.csv")
features = read_csv("data/features.csv")
data = merge(df,features,by=c('Store','Date')) 
data = subset(data, select = -Dept)
data = merge(data,by = c('Store','Date'))
data2 = aggregate(Weekly_Sales ~ Store+Date+Temperature+Fuel_Price+CPI+Unemployment , data, sum) 
data = aggregate(Weekly_Sales ~ Store+Date , data, sum) 
# DATA CLEANSE END____


#CHART DRAW - SALES
fig <- data[data$Store == 1,]  %>% plot_ly(x = ~Date, type="scatter", mode = 'lines',y = ~Weekly_Sales)   #filter is set to 1 
fig <- fig %>% layout(showlegend = FALSE)
fig <- fig %>% rangeslider()
#ADDING REGRESSION
fit <- lm(Weekly_Sales ~ Date, data = tail(data[data$Store ==1,],12))                                     #regression is selected for 12 weeks, filter is set to 1 
fig  = fig %>% add_lines(x = ~tail(data$Date[data$Store == 1],12), y = fitted(fit))                       #regression is selected for 12 weeks, filter is set to 1 
#ADDING ROLLING AVG - 3
backcut = head(data$Date[data$Store == 1],-1)                                                              #filter is set to 1 
Dates = tail(backcut,-1)
fig  = fig %>% add_lines(x = ~Dates, y = rollmean(data$Weekly_Sales[data$Store == 1], 3))                 #filter is set to 1
fig 

#TODO: regresja dla wybranego przedzialu czasu. liczba tygodni.
#TODO: lista rozwijana dla ROLLING AVG , 3, 5, 7
#TODO: Upikszanie wykresu, colory tytuly itd.

  
fig2 = data2[data2$Store == 1,] %>% plot_ly(x = ~Date) %>%                                                              #filter is set to 1 
  add_lines(x = data2$Date[data2$Store == 1], y = data2$Temperature[data2$Store == 1], name = 'Temperatura(F)')       #filter is set to 1 
fig3 = data2[data2$Store == 1,] %>% plot_ly(x = ~Date) %>%                                                            #filter is set to 1 
  add_lines(x = data2$Date[data2$Store == 1], y = data2$Fuel_Price[data2$Store == 1], name = 'Cena Paliwa (USD/galon?)')       #filter is set to 1 
fig4 = data2[data2$Store == 1,] %>% plot_ly(x = ~Date) %>%                                                           #filter is set to 1  
  add_lines(x = data2$Date[data2$Store == 1], y = data2$CPI[data2$Store == 1], name = 'CPI')                          #filter is set to 1 
fig5 = data2[data2$Store == 1,] %>% plot_ly(x = ~Date) %>%                                                          #filter is set to 1 
  add_lines(x = data2$Date[data2$Store == 1], y = data2$Unemployment[data2$Store == 1], name = 'Wskaznik bezrobocia')   #filter is set to 1 
subplot(fig2,fig3,fig4,fig5, shareX = T, nrows = 4,titleY = T,titleX =T) %>% rangeslider() %>% layout(showlegend = FALSE)
                                                                                                       #filter is set to 1 

library(plotly)
library(data.table)

# data <- mtcars[,c(1,3:7)]
# corrdata <- cor(data)
data1
data1 = data2[,c(1,3,4,5,6,7)]

corrdata = cor(data1)

#do this before the transformation!
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


fig6 <- plot_ly(data = plotdata, width = 500, height = 500)
fig6 <- fig6 %>% add_trace(x = ~Var2, y = ~Var1, type = "scatter", mode = "markers",
                           color = ~value,
                           marker = list(size = ~size, opacity = 1),
                           symbol = I("square"),
                           text = ~value,
                           hovertemplate = "%{text:.2f} <extra></extra>",
                           xaxis = "x1",
                           yaxis = "y1")

fig6 <- fig6 %>% add_trace(x = ~Var2, y = ~Var1, type = "scatter", mode = "markers",
                           opacity = 0,
                           showlegend = FALSE,
                           xaxis = "x2",
                           yaxis = "y2",
                           hoverinfo = "none")

fig6 <- fig6 %>% layout(xaxis = xAx1,
                        yaxis = yAx1, 
                        xaxis2 = xAx2,
                        yaxis2 = yAx2,
                        plot_bgcolor = "rgba(0,0,0,0)",
                        paper_bgcolor = "rgba(0, 0, 0, 0.03)")

fig6 <- fig6 %>% colorbar(title = "", limits = c(-1,1), x = 1.1, y = 0.75)
fig6





