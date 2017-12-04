#' Spencer Descriptives
#'
#' Computes summary descriptives and frequency tables. \cr
#' Uses pastecs stat.desc for numeric data. \cr
#' Prints tables for factor data. \cr
#' Able to take a vector or dataframe.
#' @name SpDesc
#' @param data dataframe to get descriptives/frequencies
#' @keywords descriptives
#' @export
#' @examples
#' SpDesc(XYData)
#' SpDesc(XYData$X)

library(pastecs)

SpDesc <- function(data)
{
  #If only 1 variable do separately
  if(dim(data.frame(data))[2]==1){
    if(class(data)=="numeric"){#if R can compute a mean then stat.desc
      print(t(stat.desc(data))[,-c(2,3,6,7,11,14)])
    }
    else{
      #cat(noquote(levels(data)))
      print(table(data))
    }
    return(noquote(""))
  }

  #separate categorical from numerical data
  data.num <- data.frame(Dummy=rep(NA,dim(data)[1])) #Make a dummy data.frame
  data.cat <- data.frame(Dummy=rep(NA,dim(data)[1]))

  for(i in 1:dim(data)[2]){
    if(!is.na(stat.desc(data[i])["mean",])){#if R can compute a mean then add to data.num
      data.num <- cbind(data.num, data[i])
    }
    else{
      data.cat <- cbind(data.cat, data[i])
    }
  }
  #Delete dummy variable
  data.num$Dummy <- NULL
  data.cat$Dummy <- NULL

  #Print Numerical results
  if(dim(data.num)[2]>0) {
    print(t(stat.desc(data.num))[,-c(2,3,6,7,11,14)])
    cat(noquote(""), sep="\n\n")
  }

  #Print categorical results
  if(dim(data.cat)[2]>0) {
    for(j in 1:dim(data.cat)[2]){
      cat(noquote(names(data.cat[j])))
      print(table(data.cat[j]))
    }
  }
}

#Usage examples
# SpDesc(XYData)
# SpDesc(XYData$X)
# x3=c("A","A","B")
# SpDesc(x3)
