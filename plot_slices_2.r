library(data.table)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(plyr) 
library(ggplot2) 
library(reshape2) 
library(tidyverse) 
library(ggpubr)
library("stringr") 
setwd("~/git/Plotting_FPKM/") #setting working directory 

#loading and maniuplating the data
RPKM <-fread("PLoS_data.txt", sep="auto",head=T) 
RPKM.df <- setNames(melt(RPKM), c('rows', 'vars', 'values'))
test <- plyr::arrange(RPKM.df, rows)
test <- test %>%
  tidyr::separate(vars, c("NI","emb","slice","rest"),sep="_",remove = F,extra ="merge")
#plotting 
names_list=list("lab","pb","Dfd","Scr","Antp","Ubx","abd-A","Abd-B")
ggplot(subset(test,rows %in% names_list & NI=="60u"), aes(x = slice, y = emb, fill=values)) + facet_grid(emb ~ rows, scales="free",space="free_x") + geom_tile() +scale_fill_gradient(low = "#FFFFFF",high = "#012345")+ theme(plot.title = element_text(hjust = 0.5),axis.title.y=element_blank(),axis.ticks.y = element_blank(), axis.text.y = element_blank())
ggsave("hox_genes_60.pdf",width=40,height=5)
ggplot(subset(test,rows %in% names_list & NI=="25u"), aes(x = slice, y = emb, fill = values)) + facet_grid(emb ~ rows, scales="free",space="free_x") +geom_tile() + scale_fill_gradient(low = "#FFFFFF",high = "#012345")+ theme(plot.title = element_text(hjust = 0.5),axis.title.y=element_blank(),axis.ticks.y = element_blank(), axis.text.y = element_blank())
ggsave("hox_genes_25.pdf",width=40,height=5)
#bar-plot of average expression
testx <- subset(test,rows %in% names_list & NI=="60u") %>% 
  group_by(rows,emb) %>% 
  dplyr::summarise(averageexp=mean(values,na.rm=T))
ggplot(testx, aes(x=emb, y=averageexp)) + geom_bar(stat = "identity") + facet_grid(. ~ rows, scales="free",space="free_x") + geom_tile() + ylab("average expression across slices") +xlab("Embryo Number") + theme_classic()
ggsave("hox_genes_barplot.pdf",width=10,height=5)
