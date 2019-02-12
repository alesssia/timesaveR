get.age <- function(born, visit, format.born="%m/%d/%Y", format.visit="%m/%d/%Y")
# Given the date of birth and the date of visit, along with their format, it evaluates the 
# subject's age at visit
#
# Args:
#	born		 : date of birth
#	visit		 : date of visit
#	format.born	 : date of birth format
#	format.visit :date of visit format
# Output:
# 	age at visit
#
{
	born <- as.Date(strptime(born, format.born))
	visit <- as.Date(strptime(visit, format.visit))
	tmp <- visit - born
	age <- as.numeric(sapply(tmp, gsub, pattern=" days", replacement=""))/365
	age
}