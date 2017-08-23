library(dplyr)
library(ggplot2)

all_data <- read.table(file="data/pmid_doi_year_journal_society_pub.tsv", header=T, stringsAsFactors=F)

all_data$year <- ifelse(is.na(all_data$epub_year) | all_data$pub_year < all_data$epub_year, all_data$pub_year, all_data$epub_year)

all_data$is_society <- !is.na(all_data$society)

#Figure 1A
all_data %>%
		group_by(year, is_society) %>%
		summarize(n_papers=n()) %>%
		ggplot(aes(year, n_papers)) +
			geom_line(aes(color=is_society)) +
			scale_x_continuous(limits = c(1980, 2015))


#Figure 1B
all_data %>% select(year, journal) %>%
		group_by(year) %>% summarize(n_journals=n_distinct(journal)) %>%
		ggplot(aes(year, n_journals)) +
			geom_line() +
			scale_x_continuous(limits = c(1980, 2015))



top_micro_societies <- all_data %>%
		filter(is_society) %>%
		select(year, society, journal) %>%
		group_by(society) %>%
		summarize(n_papers=n()) %>%
		top_n(5, n_papers)

#Figure 1C
all_data %>%
		filter(society %in% top_micro_societies$society) %>%
		group_by(year, society) %>% summarize(n_papers=n()) %>%
		ggplot(aes(year, n_papers)) +
			geom_line(aes(color=society)) +
			scale_x_continuous(limits = c(1980, 2015)) + ggsave('epub_date.png')

#Figure 1D
mega_journals <- c("Front Microbiol", "PLoS One", "Sci Rep", "PeerJ")
all_data %>%
		filter(journal %in% mega_journals) %>%
		group_by(year, journal) %>% summarize(n_papers=n()) %>%
		ggplot(aes(year, n_papers)) +
			geom_line(aes(color=journal)) +
			scale_x_continuous(limits = c(1980, 2015))
