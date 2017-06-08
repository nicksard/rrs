#originally created on: January 12th, 2017
#by: Nick Sard

#ABOUT: This script was written to compare reproductive success estimates from the treatments to each other

#setting seed
set.seed(123)

#loading in libraries
library(dplyr)

#setting working directory and reading in data
setwd("C:/Users/sardnich/Documents/Research/R/Data analysis/2016/Hatchery Breeding/Cluster output/COLONY/rs comparisons/")
list.files("Input/")

#loading my own function
source("randomization.test.R")
source("unique.pairs.R")

#reading in reproductive success data for all treatments
df <- read.table("Input/all.rs.values.txt",header = T,sep = "\t",stringsAsFactors = F)
head(df)

#making a data frame to be filled with output from the randomization function
out <- rbind(unique.pairs(1:4),unique.pairs(5:6))
colnames(out) <- c("Trt1"  ,"Trt2")
out <- rbind(out,out,out,out)
out$sex <- rep(c("Female","Male"),each=7)
out$stat <- rep(c("mean","variance"),each=14)
out$test <- c(rep("Two_Sided",21),rep("Less_Than",7))
head(out)

k <- 1
k <- NULL
my.output <- NULL
for(k in 1:nrow(out)){
  
  #getting the variables I want
  my.picks <- c(out[k,1],out[k,2])
  my.sex <- out[k,3]
  my.stat <- out[k,4]
  my.group <- out[k,1]
  my.test <- out[k,5]
  
  #selecting the data I want for a randomization test
  tmp <- df %>% filter(trt %in% my.picks & sex == my.sex) %>% select(rs,trt)
  output1 <- randomization.test(tmp = tmp,test = my.test,paramater = my.stat,n = 10000, group = my.group) 
  my.output <- rbind(my.output,output1)
}

#combining input and output tables
out1 <- cbind(out,my.output)

#correcting for multiple tests
out1$Pval2 <- p.adjust(p = out1$Pval,method = "fdr")
head(out1)

#ok cleaning up a bit with some rounding and removing of redundant columns
table(paste0(out1$Trt1,out1$Trt2)==paste0(out1$Group1,out1$Group2))
out1$Group1 <- NULL
out1$Group2 <- NULL
out1$Par1 <- round(out1$Par1,2)
out1$Par2 <- round(out1$Par2,2)
out1$Diff <- round(out1$Diff,2)
head(out1)
out1

#writing to file
#write.table(x = out1,file = "Output/all.rrs.and.rvrs.tests.txt",append = F,quote = F,sep = "\t",row.names = F,col.names = T)

#fin!