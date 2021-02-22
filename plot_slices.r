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

# Julian comment: You do not set the WD - hence the code below does not work for me. 
# If you share code you should adjust the code so that other people can run it too
# Julian wd
setwd(r'(C:\Data\Cache_Work\Coding\Plotting_FPKM)')

RPKM <-fread("PLoS_data.txt", sep="auto",head=T)
RPKM_60u <- RPKM %>% dplyr::select(V1,starts_with("60"))
RPKM.df <- setNames(melt(RPKM_60u), c('rows', 'vars', 'values'))

# Julian alternative for making it in dplyr way:
RPKM.df2 <- RPKM %>% 
  dplyr::select(V1,starts_with("60")) %>%
  tidyr::pivot_longer(c(starts_with("60")), "vars", values_to = "values") %>%
  dplyr::rename(rows = V1)

#RPKM.df <-RPKM_25u %>% gather(vars,values,-X )
test <- plyr::arrange(RPKM.df, rows)
emb=c(substr(test$vars,5, 8))
slice=c(substr(test$vars,10, 13))
test<- test %>% cbind(emb,slice)

# Julian comment: substr works here - but it s more a "quick and dirty" solution. e.g. if you want 60u and 120u - the positions would change.
# But as the underscore "_" seems to be always there you could use that as a separator:
test <- test %>%
  tidyr::separate(vars, c("NI","emb","slice","rest"),sep="_",remove = F,extra ="merge")


plotlist=list()
names_list=list("lab","pb","Dfd","Scr","Antp","Ubx","abd-A","Abd-B")
#names_list=list("bcd","Nos","gt","Kr","eve","wg")
for (i in 1:6) {
  p <- ggplot(subset(test,rows ==names_list[i]), aes(x = slice, y = rows, fill = values)) + facet_grid(emb ~ ., scales='free_x', space="free_x") + geom_tile() + scale_fill_gradient(low = "#FFFFFF",high = "#012345")+ theme(plot.title = element_text(hjust = 0.5),axis.title.y=element_blank(),axis.ticks.y = element_blank(), axis.text.y = element_blank()) + ggtitle(paste(names_list[i]))
  plotlist[[i]]=p
}
main<-grid.arrange(grobs=plotlist,ncol=6)
ggsave("bigplot.pdf",main,width=40,height=5)


