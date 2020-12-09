#' @title Get the effective number of parties.
#' 
#' @name calculate_nep
#'
#' @description This function returns the effective number of parties.
#' 
#' @param seats \code{array}. The total of available seats.
#' @param magnitude \code{int}. The district magnitude.
#' 
#' @details If you want to calculate the actual number of parties in a mayoral election, 
#' the ratio must be calculated between the total number of votes received and the total 
#' number of valid votes rather than the ratio between available seats and the number 
#' of seats obtained.
#' 
#' @return A number.
#'
#' @author Mário Dias
#'
#' @examples
#' calculate_nep(c(2, 1, 1, 1, 1, 1, 2), 9)
#'
#' @export
calculate_nep <- function(seats = c(), magnitude) {
  
  #creates arrays to receive the proportion and proportion squared values.
  proportion <- c()
  proportion_squared <- c()
  
  #creates variables to receive the summation value and the index to while loop.
  summation <- 0
  i <- 0
  
  #repetition structure that will iterate through the array.
  while (i <= length(seats)) {
    
    #generates the proportion between the seats available seats and the obtained seats and sends the results to a vector.
    proportion[i] <- seats[i] / magnitude
    #generates the squared of all proportions and sends the results to a vector.
    proportion_squared[i] <- proportion[i] ** 2
    #makes the summation of all values squared.
    summation <- sum(proportion_squared)
    #updates the index value.
    i <- i + 1
    
  }
  
  #calculates the effective number of parties.
  nep <- 1 / summation
  
  #returns the effective number of parties.
  return(nep)
  
}
