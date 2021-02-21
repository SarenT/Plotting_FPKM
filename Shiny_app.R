library(shiny)
library(data.table)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(plyr) 
library(ggplot2) 
library(reshape2) 
library(ggpubr)
library("stringr")
RPKM <-fread("PLoS_data.txt", sep="auto",head=T)
RPKM_60u <- RPKM %>% dplyr:: select(V1,starts_with("60"))
RPKM.df <- setNames(melt(RPKM_60u), c('rows', 'vars', 'values'))
#RPKM.df <-RPKM_25u %>% gather(vars,values,-X )
test <- plyr::arrange(RPKM.df, rows)
emb=c(substr(test$vars,5, 8))
slice=c(substr(test$vars,10, 13))
test<- test %>% cbind(emb,slice)
ui <- fluidPage(
  titlePanel("Expression in the Drosophila Embryo"),
  #textInput("inputId", "Input the name of a Drosophila Gene", value = ""),
  selectInput("Variable", "Gene:",
              c(RPKM$V1),multiple = FALSE),
    mainPanel(plotOutput("distPlot"))
  )

server <- function(input, output) {
  output$distPlot <- renderPlot({
    ggplot(subset(test,rows ==input$Variable), aes(x = slice, y = rows, fill = values)) + facet_grid(emb ~ ., scales='free_x', space="free_x") + geom_tile() + scale_fill_gradient(low = "#FFFFFF",high = "#012345")+ theme(plot.title = element_text(hjust = 0.5),axis.title.y=element_blank(),axis.ticks.y = element_blank(), axis.text.y = element_blank()) + ggtitle(paste(input$Variable))
  })
  
}

shinyApp(ui = ui, server = server)
