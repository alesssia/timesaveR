get.age <- function(born, visit, format.born="%m/%d/%Y", format.visit="%m/%d/%Y")
{
	born <- as.Date(strptime(born, format.born))
	visit <- as.Date(strptime(visit, format.visit))
	tmp <- visit - born
	age <- as.numeric(sapply(tmp, gsub, pattern=" days", replacement=""))/365
	age
}