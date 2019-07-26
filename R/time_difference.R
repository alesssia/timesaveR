time.difference <- function(date1, date2, format1="%m/%d/%Y", format2="%m/%d/%Y")
#' Evaluates day, week, month, and age differences between dates
#' 
#' Given two dates, it returns the differences in days, weeks, months, and years.
#' Each month is estimated to be 30 day long.
#'
#' @author Alessia Visconti
#' @param date1 first date
#' @param date2 second date
#' @param format1 format of date1
#' @param format2 format of date2
#' @return difference between the dates
#' @examples
#' time.difference("01/01/1980", "31/12/2019", "%d/%m/%Y", "%d/%m/%Y")
{
	# days
	days <- difftime(strptime(date1, format = format1), strptime(date2, format = format2), units="days")
	days <- abs(as.numeric(gsub(days, pattern="Time difference of | days", replacement="")))

	# weeks
	weeks <- difftime(strptime(date1, format = format1), strptime(date2, format = format2), units="weeks")
	weeks <- abs(as.numeric(gsub(weeks, pattern="Time difference of | weeks", replacement="")))

	# months
	months <- days/30
	
	# years
	years <- days/365

	r <- c(days, weeks, months, years)
	names(r) <- c("days", "weeks", "months", "years")
	r
}