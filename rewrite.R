options(stringsAsFactors=FALSE)
library(dplyr)
library(reshape2)

# borrowed functions from tomkit
cleanNames <- function(x){
	names(x) <- names(x) %>%
		iconv("UTF-8", "UTF-8", sub="") %>%
		gsub(pattern="\\.", replacement=" ") %>%
		trimall() %>%
		gsub(pattern=" +", replacement="_") %>%
		tolower()
	return(x)
}

trimall <- function(tstring){
	return(gsub("(^ +)|( +$)", "", tstring))
}

# Data Processing Functions
getDifference <- function(x){
	c(x[1:(length(x)-1)] - x[2:length(x)], 0)
}

getStatus<- function(x){
	strsplit(x, "-")[[1]] %>%
		last() %>%
		gsub(pattern="\\.csv", replacement="")
}

readData <- function(paths){
	lapply(paths, function(path){
		read.csv(path) %>%
			cleanNames() %>%
			mutate(
				date = as.Date(date, "%m/%d/%Y"),
				status = getStatus(path)
			)
	}) %>%
		bind_rows()
}

collapseProvinceState <- function(dat){
	dat %>%
		group_by(country_region, status, date) %>%
		summarize(value = sum(value)) %>%
		ungroup()
}

appendGlobal <- function(dat){
	## calculates global and appends to data
	dat %>%
		group_by(status, date) %>%
		summarize(value = sum(value)) %>%
		ungroup() %>%
		mutate( country_region = "Global") %>%
		select(country_region, date, status, value) %>%
		rbind(dat)
}

createActive <- function(dat){
	dcast(country_region + date ~ status,
		data=dat, value.var="value") %>%
		mutate(Active = Confirmed - (Deaths + Recovered)) %>%
		cleanNames()
}

calcDiffs <- function(dat){
	dat %>%
		split(.$country_region) %>%
		lapply(function(x){
			mutate(
				confirmed_diff = getDifference(confirmed),
				deaths_diff = getDifference(deaths),
				recovcered_diff = getDifference(recovered),
				active_diff = getDifference(active)
			)
		}) %>%
		bind_rows()
}

## Run the code!
global <- list.files("raw", full.names = TRUE) %>%
	readData() %>%
	collapseProvinceState() %>%
	appendGlobal() %>%
	createActive() %>%
	calcDiffs()
