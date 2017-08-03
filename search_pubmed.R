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
	year <- x$PubDate
	journal <- x$FullJournalName
	id_table <- do.call(rbind.data.frame, x$ArticleIds)
	doi <- as.character(id_table[which(id_table[,'IdType']=='doi'),'Value'][1])

	if(is.null(doi) || is.na(doi)){ doi <- "NA" }
	if(is.null(year)){ year <- "NA" }
	if(is.null(journal)){ journal <- "NA" }

	c(doi=doi, year=year, journal=journal)
}

search <- function(){
	terms <- c("yeast*",  "fung*",  "viral*",  "virus*",  "phage*",  "bacteriophage*",  "archaea*",
	 					"bacteri*",  "bacteria*",  "bacterio*",  "bacterior*",  "bacterios*",  "bacteriot*",
						"bacteriov*",  "bacteriu*",  "microbi*",  "microbe*",  "microorganism*",  "pathogen*",
						"protist*")

	search_string <- paste(paste0(terms, "[All Fields]"), collapse= " OR ")
	r_search <- entrez_search(db="pubmed", term=search_string, use_history=TRUE)

	save(r_search, file="r_search.rdata")
}

retrieve_records <- function(){
	if(file.exists("temp_pmid_doi_year_journal.tsv")){
		composite <- read.table(file="temp_pmid_doi_year_journal.tsv", sep='\t', header=T, comment.char="", stringsAsFactors=F)
		query_start <- nrow(composite)+1
	} else {
		composite <- NULL
		query_start <- 1
	}

	if(!file.exists("r_search.rdata")){
		search()
		composite <- NULL
		query_start <- 1
	}
	load("r_search.rdata")

	total_records <- r_search$count
	chunk_size <- 10000

	for(chunk_start in seq(query_start, total_records, chunk_size)){
		print(chunk_start)
		chunk_summary <- entrez_summary(db="pubmed", web_history=r_search$web_history,
																		retmax=chunk_size, retstart=chunk_start, retmode="xml")

		chunk_list <- extract_from_esummary(chunk_summary, c("PubDate", "FullJournalName",
				"ArticleIds"), simplify=FALSE)

		chunk_df <- data.frame(t(sapply(chunk_list, flatten_list_items)))
		chunk_df$pmid <- rownames(chunk_df)

		composite <- rbind(composite, chunk_df)
		write.table(file="temp_pmid_doi_year_journal.tsv", x=composite, quote=T, row.names=F,
				col.names=T, sep='\t')
	}
}

clean_up <- function(){
	final_data <- read.table(file="temp_pmid_doi_year_journal.tsv", header=F, sep='\t')
	final_data <- final_data[!is.na(final_data[,1]),]
	colnames(final_data) <- c("pmid", "doi", "year", "journal")

	final_data$year <- gsub(".*(\\d{4}).*", "\\1", final_data$year)
	final_data$doi <- gsub(".*doi: (\\S*).*", "\\1", final_data$doi)

	write.table(file="pmid_doi_year_journal.tsv", final_data, row.names=F, quote=T, sep='\t')
	unlink("temp_pmid_year_journal.tsv")
	unlink("search_history.txt")
}
