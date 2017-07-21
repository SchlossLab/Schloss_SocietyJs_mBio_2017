pmid_year_journal.tsv : search_pubmed.R
	R -e "source('search_pubmed.R'); search()"
	R -e "source('search_pubmed.R'); retrieve_records()"
	R -e "source('search_pubmed.R'); clean_up()"
