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
RPKM_60u <- RPKM %>% 
  dplyr::select(V1,starts_with("60"))
RPKM.df_60u <- setNames(melt(RPKM_60u), c('rows', 'vars', 'values'))
test1 <- plyr::arrange(RPKM.df, rows)
test1 <- test1 %>%
  tidyr::separate(vars, c("NI","emb","slice","rest"),sep="_",remove = F,extra ="merge")
RPKM_25u <- RPKM %>% 
  dplyr::select(V1,starts_with("25"))
RPKM.df_25u <- setNames(melt(RPKM_25u), c('rows', 'vars', 'values'))
test2 <- plyr::arrange(RPKM.df_25u, rows)
test2 <- test2 %>%
  tidyr::separate(vars, c("NI","emb","slice","rest"),sep="_",remove = F,extra ="merge")
#plotting 
names_list=list("lab","pb","Dfd","Scr","Antp","Ubx","abd-A","Abd-B")
ggplot(subset(test,rows %in% names_list), aes(x = slice, y = emb, fill = values)) + facet_grid(emb ~ rows, scales="free",space="free_x") +geom_tile() + scale_fill_gradient(low = "#FFFFFF",high = "#012345")+ theme(plot.title = element_text(hjust = 0.5),axis.title.y=element_blank(),axis.ticks.y = element_blank(), axis.text.y = element_blank())
ggsave("hox_genes_60.pdf",width=40,height=5)
ggplot(subset(test2,rows %in% names_list), aes(x = slice, y = emb, fill = values)) + facet_grid(emb ~ rows, scales="free",space="free_x") +geom_tile() + scale_fill_gradient(low = "#FFFFFF",high = "#012345")+ theme(plot.title = element_text(hjust = 0.5),axis.title.y=element_blank(),axis.ticks.y = element_blank(), axis.text.y = element_blank())
ggsave("hox_genes_25.pdf",width=40,height=5)