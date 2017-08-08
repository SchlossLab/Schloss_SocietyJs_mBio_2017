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
library(parallel)



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



retrieve_record_chunk <- function(chunk_start, chunk_size=10000){

	load("r_search.rdata")

	Sys.sleep(runif(1) * 5) # slow down searches so they don't clobber server

	chunk_summary <- rentrez::entrez_summary(db="pubmed", web_history=r_search$web_history,
																	retmax=chunk_size, retstart=chunk_start, retmode="xml")

	chunk_list <- rentrez::extract_from_esummary(chunk_summary, c("PubDate", "FullJournalName",
																	"ArticleIds"), simplify=FALSE)

	chunk_df <- data.frame(t(sapply(chunk_list, flatten_list_items)))
	chunk_df$pmid <- rownames(chunk_df)

	write.table(file=paste0("temp_pmid_doi_year_journal_", chunk_start, ".tsv"), x=chunk_df,
													quote=T, row.names=F,col.names=T, sep='\t')
}



retrieve_records <- function(){
	search()
	load("r_search.rdata")

	indices <- seq(1, r_search$count, 10000)

	cl <- makeCluster(12)
	clusterExport(cl, "flatten_list_items")
	parLapply(cl, indices, retrieve_record_chunk)
	stopCluster(cl)

	temp_files <- paste0("temp_pmid_doi_year_journal_", indices, ".tsv")
	composite <- lapply(temp_files[1:10], function(x)read.table(file=x, stringsAsFactors=F, header=T))
	composite <- do.call(rbind.data.frame, composite)

	stopifnot(nrow(composite)==r_search$count)

	write.table(file="pmid_doi_year_journal.tsv", final_data, row.names=F, quote=T, sep='\t')

	unlink("temp_pmid_year_journal.tsv")
	unlink("r_search.rdata")
}
