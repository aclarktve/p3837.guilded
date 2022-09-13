#load libraries
library(haven)
library(dplyr)
library(openxlsx)
#load data
setwd("C:/Work/p3837.guilded")
df <- read_sav("guilded_panel.sav")
coal <- function(arg1){
  return(coalesce(arg1,0))
}

coalmean <- function(arg1){
  #mean(coalesce(arg1,0))
  mean(arg1,na.rm=TRUE)
}
df <- df %>% mutate(nitro=ifelse(cqC5==3,1,0),discord=ifelse(cqC2_03==1,1,0),guilded=ifelse(cqC2_04==1,1,0),dg=ifelse(discord+guilded==2,1,0),nodg=ifelse(discord+guilded==0,1,0))
dfclust <- df %>% select(contains('cqS10'),contains('cqC13'))

#cqS12,cqB2,cqC3

#coalesce variables
dfclust <- dfclust %>% mutate(across(everything(),coal))
dfclust <- dfclust %>% select(-cqS10_08,-cqS10_09,-cqS10_10,-cqS10_11)
#run clustering
k<-kmeans(dfclust, centers=5, iter.max = 1000, nstart=5)
k

#add back to data frame
df$clust <- k$cluster
df %>% count(clust)

#summarize key variables - cqC4_,cqC2a1_,cqs8_,cqs9_,cqC5,cqC2b,cqB6_04,cqS7a,cqs12a
clust_stats<-df %>% select(clust,contains('cqS10'),contains('cqC13'),discord,guilded,nitro,dg,nodg,cqC14,cqS12,cqB2,contains('cqC3'),contains('cqC4'),contains('cqS9'),contains('cqS8'),contains('cqC4'),contains('cqS8'),contains('cqS9'),contains('cqC2b'),contains('cqC2a1'),cqS7a,contains('cqS12a')) %>% group_by(clust) %>% summarise(across(everything(),coalmean))
View(clust_stats)
#export data frame
write.xlsx(x=clust_stats,file='clust_stats.xlsx')