####################
### unique.pairs ###
####################

#Description
# This fuction was  written by Nick Sard in 2016. It is designed to take a vector of IDs
# and put all uniqe pairs into a two column data frame. It uses expand grid, but it removes
# all comparisons of one group to itself, as well as remove duplicate comparisons.

#first defining a function I will use later
unique.pairs <- function(ids){
  x <- expand.grid(ids,ids)
  x <- x[ifelse(x$Var1==x$Var2,T,F)==F,]
  x$both <- paste(pmin(as.character(x$Var1), as.character(x$Var2)),
                  pmax(as.character(x$Var1), as.character(x$Var2)), sep="_")
  x <- x[duplicated(x$both)==F,]
  x$both <- NULL
  row.names(x) <- NULL
  x <- x[,c(2,1)]
  colnames(x) <- c("Var1","Var2")
  return(x)
} # end of unique.pairs