#originally created on: January 6th, 2016
#by: Nick Sard

#ABOUT: This script was written to create a power graphic of randomization tests

#loading in libraries
library(ggplot2)

#loading my own functions
# - none -

#setting working directory and reading in data
setwd("C:/Users/sardnich/Documents/Research/R/Data analysis/2016/Hatchery Breeding/Cluster output/COLONY/rs comparisons/")
list.files("Input/")

#reading in data
df <- read.table(file = "Input/summaries.txt",header = F,sep = "\t",stringsAsFactors = F)
colnames(df) <- c("diff","power","size","type")
df$type <- ifelse(df$type == " means",yes = "Mean",no = "Variance")
df$size <- as.factor(df$size)
head(df)

#defining a geom_smooth function for logistic regression
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

#making the plot
ggplot(df, aes(x = diff,y = power,group =  size))+
  facet_wrap(~type, ncol = 1)+
  binomial_smooth(aes(color=size),se = F,size=1.25)+
  labs(x="|Parameter 1 - Parameter 2|",y="Power",color="Sample sizes")+
  theme_bw()+
  theme(axis.title = element_text(size=22),
        axis.text = element_text(size=18))
  
  
#fin