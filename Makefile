keyword_pmid_doi_year_journal.tsv : search_pubmed.R
	R -e "source('search_pubmed.R'); retrieve_keyword_records()"


issn_pmid_doi_year_journal.tsv : search_pubmed.R
	R -e "source('search_pubmed.R'); retrieve_issn_records()"


pmid_doi_year_journal.tsv : search_pubmed.R keyword_pmid_doi_year_journal.tsv\
														issn_pmid_doi_year_journal.tsv
	R -e "source('search_pubmed.R'); merge_search_data()"
