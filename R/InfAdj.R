#' Adjusting for Inflation
#'
#' Function to adjust a monetary value for inflation\cr
#' Works for scalar and vectors
#' @name InfAdj
#' @param yr Year raw
#' @param adj.yr Year to adjust to (e.g. in 2016 dollars)
#' @param value dollar vallues
#' @return dataframe adding adjustment factors and inflation adjusted values
#' @keywords inflation
#' @export
#' @examples
#' InfAdj(yr=1990, adj.yr=2013, value=1)
#' Dollars <- data.frame(yr=seq(1990,2010), value=1)
#' InfAdj(yr=Dollars$yr, adj.yr=2013, value=Dollars$value)

InfAdj <- function(yr, adj.yr, value)
{
  #import yearly CPI data
  yearly_cpi <- read.csv("yearly_cpi.csv", header = TRUE)


  #if the adjusting for inflation is a scalar
  if(length(yr)==1){
    #calculate adjustment factor
    adj_factor <- yearly_cpi$cpi[yearly_cpi$Year==yr] / yearly_cpi$cpi[yearly_cpi$Year==adj.yr]

    #adjust for inflation
    value.Infadj <- value / adj_factor

    return(value.Infadj)
  } else { #If the adjustng for inflation is from a vector of yrs and values
    data=data.frame(yr=yr, value=value)

    for(i in 1:dim(data)[1]){
      data$adj_factor[i] <- yearly_cpi$cpi[yearly_cpi$Year==data$yr[i]] / yearly_cpi$cpi[yearly_cpi$Year==adj.yr]
    }

    data$value.Infadj <- data$value / data$adj_factor

    return(data)
  }
}
