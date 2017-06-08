#originally created on: January 6th, 2017
#by: Nick Sard

#ABOUT: This script was written to estimate power for each RRS test

#loading in libraries
# - none -

#first loading the function to do the script
source("/mnt/home/sardnich/source.scripts/rs.power.function.R")

#defining sample sizes
ns <- 10

#next next randomly choosing the differnce between means
diff <- round(runif(n = 1,min = 1,max = 280))

#defining means
mean1 <- 20
mean2 <- 20

#incorporating that random choice into variances
var1 <- 40
var2 <-var1+diff

#now using that information in the power script
x <- rs.rand.power(n1 = ns, n2 = ns,
                   mean1 = mean1, mean2 = mean2,
                   var1 = var1, var2 = var2,
                   n = 1000, reps = 10000,
                   stat = "mean", type = "Two_sided", alpha = 0.05)
x1 <-as.data.frame(t(x[,2]))

#writing that information to standard out
write.table(x = x1,file = stdout(),append = F,quote = F,sep = "\t",row.names = F,col.names = F)

#fin