r_search.rdata : search_pubmed.R
	R -e "source('search_pubmed.R'); search()"

pmid_doi_year_journal.tsv : search_pubmed.R r_search.rdata
	R -e "source('search_pubmed.R'); retrieve_records()"
	R -e "source('search_pubmed.R'); clean_up()"

