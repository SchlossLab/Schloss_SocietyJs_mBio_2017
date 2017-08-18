data/keyword_pmid_doi_year_journal.tsv : code/search_pubmed.R
	R -e "source('code/search_pubmed.R'); retrieve_keyword_records()"


data/issn_pmid_doi_year_journal.tsv : code/search_pubmed.R
	R -e "source('code/search_pubmed.R'); retrieve_issn_records()"


data/pmid_doi_year_journal.tsv : code/search_pubmed.R\
														keyword_pmid_doi_year_journal.tsv\
														issn_pmid_doi_year_journal.tsv
	R -e "source('code/search_pubmed.R'); merge_search_data()"
