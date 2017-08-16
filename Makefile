keyword_pmid_doi_year_journal.tsv : search_pubmed.R
	R -e "source('search_pubmed.R'); retrieve_keyword_records()"


issn_pmid_doi_year_journal.tsv : search_pubmed.R
	R -e "source('search_pubmed.R'); retrieve_issn_records()"

