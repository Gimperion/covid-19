options(stringsAsFactors=FALSE)
library(dplyr)
library(tomkit)


getDifference <- function(x){
	c(x[1:(length(x)-1)] - x[2:length(x)], 0)
}

getStatus<- function(x){
	strsplit(x, "-")[[1]] %>%
		last() %>%
		gsub(pattern="\\.csv", replacement="")
}

raw <- list.files("raw", full.names = TRUE) %>%
	lapply(function(x){
		read.csv(x) %>%
			cleanNames() %>%
			mutate(
				date = as.Date(date, "%m/%d/%Y"),
				status = getStatus(x)
			)
	}) %>%
	bind_rows() %>%
	group_by(
		country_region, status, date
	) %>%
	summarize(
		value = sum(value)
	) %>%
	ungroup()

global_data <- raw %>%
	group_by(status, date) %>%
	summarize(
		value = sum(value)
	) %>%
	ungroup() %>%
	mutate(
		country_region = "Global"
	) %>%
	select(country_region, date, status, value) %>%
	list(raw) %>%
	bind_rows() %>%
	arrange(-as.numeric(date), status)

active <- global_data  %>%
	mutate(
		blah = sprintf("%s-%s", country_region, as.character(date))
	) %>%
	split(.$blah) %>%
	lapply(function(x){
		x <- x %>% arrange(status)
		data.frame(
			country_region = x$country_region[1],
			date = x$date[1],
			status = "Active",
			value = x$value[1] - (x$value[2] + x$value[3]),
			blah = x$blah[1]
		) %>%
		list(x) %>%
		bind_rows()
	}) %>%
	bind_rows() %>%
	select(-blah) %>%
	split(.$status) %>%
	lapply(function(x){
		x %>%
			split(.$country_region) %>%
			lapply(function(country_chunk){
				country_chunk %>%
					arrange(-as.numeric(date)) %>%
					mutate(
						diff = getDifference(value)
					)
			}) %>%
			bind_rows()
	}) %>%
	bind_rows()

#
# us <- subset(active, country_region == 'US') %>%
# 	unique() %>%
# 	tail(n=50)
