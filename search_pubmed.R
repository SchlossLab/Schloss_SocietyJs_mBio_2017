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
	print(length(	c(pmid, year, journal)))
	c(pmid, year, journal)
}

terms <- c("yeast*",  "fung*",  "viral*",  "virus*",  "phage*",  "bacteriophage*",  "archaea*",
 					"bacteri*",  "bacteria*",  "bacterio*",  "bacterior*",  "bacterios*",  "bacteriot*",
					"bacteriov*",  "bacteriu*",  "microbi*",  "microbe*",  "microorganism*",  "pathogen*",
					"protist*")

search_string <- paste(paste0(terms, "[All Fields]"), collapse= " OR ")

r_search <- entrez_search(db="pubmed", term=search_string, use_history=TRUE)

#> r_search$count
#[1] 3488812

composite <- NULL
chunk_size <- 1000

for(chunk_start in seq(1, r_search$count, chunk_size)){
	print(chunk_start)
	chunk_xml <- entrez_fetch(db="pubmed", web_history=r_search$web_history, rettype='xml',
														retmax=chunk_size, retstart=chunk_start)
	chunk_list <- xmlToList(chunk_xml)
	chunk_data <- lapply(chunk_list, flatten_list_items)
	composite <- rbind(composite, do.call(rbind, chunk_data))
}

colnames(composite) <- c("pmid", "year", "journal")
rownames(composite) <- NULL
composite_journal_articles <- composite[!is.na(composite[,"pmid"]),]

write.table(file="pmid_year_journal.tsv", x=composite_journal_articles, quote=F, row.names=F,
						sep='\t')
