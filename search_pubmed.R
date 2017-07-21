#yeast*	177211
#fung*	285372
#viral*	592800
#virus*	758493
#phage*	50896
#bacteriophage*	53672
#archaea*	21871
#bacteri*	1285420
#	bacteria*	1206070
#	bacterio*	222268
#		bacterior*	4109
#		bacterios*	5501
#		bacteriot*	287
#		bacteriov*	448
#bacteriu*	73263
#microbi*	1381699
#microbe*	45691
#microorganism*	87684
#pathogen*	864424
#protist*	5886

#install.packages("devtools")
#devtools::install_github("ropensci/rentrez")
library("rentrez")
library(XML)

flatten_list_items <- function(x){
	year <- NA
	journal <- NA
	pmid <- NA

	if(names(x)[1] == "MedlineCitation"){
		year <- x$PubmedData$History$PubMedPubDate$Year
		journal <- x$MedlineCitation$Article$Journal$Title
		pmid <- x$MedlineCitation$PMID$text
	}
	c(pmid, year, journal)
}

search <- function(){
	terms <- c("yeast*",  "fung*",  "viral*",  "virus*",  "phage*",  "bacteriophage*",  "archaea*",
	 					"bacteri*",  "bacteria*",  "bacterio*",  "bacterior*",  "bacterios*",  "bacteriot*",
						"bacteriov*",  "bacteriu*",  "microbi*",  "microbe*",  "microorganism*",  "pathogen*",
						"protist*")

	search_string <- paste(paste0(terms, "[All Fields]"), collapse= " OR ")
	r_search <- entrez_search(db="pubmed", term=search_string, use_history=TRUE)

	webenv <- r_search$web_history$WebEnv
	querykey <- r_search$web_history$QueryKey
	hits <- r_search$count
	write(c(webenv, querykey, hits), "search_history.txt")
}

retrieve_records <- function(){
	if(file.exists("temp_pmid_year_journal.tsv")){
		composite <- read.table(file="temp_pmid_year_journal.tsv", sep='\t')
		query_start <- nrow(composite)+1
	} else {
		composite <- NULL
		query_start <- 1
		search()
	}
	cached_history <- scan(file="search_history.txt", what=character(), quiet=T)
	total_records <- as.numeric(cached_history[3])
	search_history <- list()
	search_history$web_history$WebEnv <- cached_history[1]
	search_history$web_history$QueryKey <- cached_history[2]

	chunk_size <- 100
	total_records <- 1000

	for(chunk_start in seq(query_start, total_records, chunk_size)){
		print(chunk_start)
		chunk_xml <- entrez_fetch(db="pubmed", web_history=search_history$web_history, rettype='xml',
															retmax=chunk_size, retstart=chunk_start)
		chunk_list <- xmlToList(chunk_xml)
		chunk_data <- lapply(chunk_list, flatten_list_items)
		composite <- rbind(composite, do.call(rbind, chunk_data))

		write.table(file="temp_pmid_year_journal.tsv", x=composite, quote=F, row.names=F, col.names=F,
								sep='\t')
	}
}

clean_up <- function(){
	final_data <- read.table(file="temp_pmid_year_journal.tsv", header=F, sep='\t')
	final_data <- final_data[!is.na(final_data[,1]),]
	colnames(final_data) <- c("pmid", "year", "journal")

	write.table(file="pmid_year_journal.tsv", final_data, row.names=F, quote=F, sep='\t')
	unlink("temp_pmid_year_journal.tsv")
}
