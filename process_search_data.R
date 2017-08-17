keyword_data <- read.table(file="keyword_pmid_doi_year_journal.tsv", header=T, stringsAsFactors=FALSE)
# 3501596 rows

issn_data <- read.table(file="issn_pmid_doi_year_journal.tsv", header=T, stringsAsFactors=FALSE)
#  496197 rows

pubmed_data <- unique(rbind(issn_data, keyword_data))
# 3530610 rows

pubmed_data$year <- gsub(".*(\\d\\d\\d\\d).*", "\\1", pubmed_data$year)
pubmed_data[!grepl("^10\\.\\d*\\/", pubmed_data$doi), "doi"] <- NA
recent_pubmed_data <- pubmed_data[pubmed_data$year >= 2000,]


# Need to find the proportion of articles each year that have a DOI
year_range <- 2000:2017

year_summary_df <- data.frame(matrix(0, nrow=length(year_range), ncol=4))
colnames(year_summary_df) <- c("year", "society", "specialty", "total")
year_summary_df$year <- year_range

year_summary_df$total <- table(recent_pubmed_data$year)[as.character(year_range)]
year_doi_df[, "total"] <-
