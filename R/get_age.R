get.age <- function(born, visit, format.born="%m/%d/%Y", format.visit="%m/%d/%Y")
#' Evaluates age difference between dates
#' 
#' Given the date of birth and the date of visit, along with their format, it evaluates the 
#' subject's age at visit
#'
#' @author Alessia Visconti
#' @param born date of birth (character)
#' @param visit date of visit (character)
#' @param format.born date of birth format
#' @param format.visit date of visit format
#' @return age at visit
#' @examples
#' get.age("01/01/1980", "31/12/2019", "%d/%m/%Y", "%d/%m/%Y")
#' @export
{
	born <- as.Date(strptime(born, format.born))
	visit <- as.Date(strptime(visit, format.visit))
	tmp <- visit - born
	as.numeric(sapply(tmp, gsub, pattern=" days", replacement=""))/365
}