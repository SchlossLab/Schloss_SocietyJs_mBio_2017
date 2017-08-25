#devtools::install_github("wilkelab/cowplot")
library(dplyr)
library(ggplot2)
library(cowplot)

all_data <- read.table(file="data/pmid_doi_year_journal_society_pub.tsv", header=T, stringsAsFactors=F)

all_data$year <- ifelse(is.na(all_data$epub_year) | all_data$pub_year < all_data$epub_year, all_data$pub_year, all_data$epub_year)

all_data$is_society <- !is.na(all_data$society)



top_micro_societies <- all_data %>%
		filter(is_society) %>%
		select(year, society, journal) %>%
		group_by(society) %>%
		summarize(n_papers=n()) %>%
		top_n(5, n_papers)

top_publishers_2015 <- all_data %>%
		filter(year==2015) %>%
		select(publisher) %>%
		group_by(publisher) %>%
		summarize(n_papers=n()) %>%
		arrange(desc(n_papers)) %>%
		top_n(6, n_papers)

journals_by_society <- all_data %>%
		filter(society %in% top_micro_societies$society, year==2015) %>%
		group_by(society) %>% summarize(n_journals=n_distinct(journal))

top_journals <- all_data %>%
		select(journal) %>%
		group_by(journal) %>%
		summarize(n_papers=n()) %>%
		arrange(desc(n_papers))


#	PLoS One
# Sci Rep
#	Front Microbiol
#	PeerJ
#	BMJ Open
#	SAGE Open (0?)
#	Open Biol
#	Springerplus (SpringerPlus)
#	Cell Rep
#	Nat Commun
#	F1000Res
#	BMC Res Notes
#	FEBS Open Bio

mega_journals <- c("PLoS One", "Sci Rep", "Front Microbiol", "BMC Res Notes", "Cell Rep")
mega_journal_data <- all_data %>%
		filter(journal %in% mega_journals) %>%
		group_by(year, journal) %>%
		summarize(n_papers=n())

all_mega_journals <- c("PLoS One", "Sci Rep", "Front Microbiol", "PeerJ", "BMJ Open", "Open Biol", "Springerplus", "Cell Rep", "F1000Res", "BMC Res Notes", "FEBS Open Bio")#, "Nat Commun")
all_data %>%
		filter(journal %in% all_mega_journals) %>%
		group_by(journal) %>%
		summarize(n_papers=n()) %>%
		arrange(desc(n_papers))


total_mega_data <- mega_journal_data %>% group_by(year) %>% summarize(total=sum(n_papers))
total_data <- all_data %>% group_by(year) %>% summarize(n_papers=n())

left_join(total_mega_data, total_data) %>% mutate(fraction=100*total/n_papers)


my_theme <- theme_classic() +
	theme(
		axis.text.y=element_text(size=7, margin=margin(r=2,0,0,0)),
		axis.title.y=element_text(size=7),
		axis.title.x=element_text(size=7),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.border = element_rect(color = "black", fill=NA, size=1),
		panel.background = element_rect(fill=NA),
		legend.position = "none"
	)

my_legend <- theme(
		legend.text = element_text(size=7),
		legend.key.size = unit(0.55, "line"),
 		legend.key = element_rect(fill = NA, linetype=0),
		legend.position=c(0.3, 0.7),
		legend.title=element_text(lineheight=-1),
		legend.background = element_rect(fill="white", color="black"),
		legend.margin = margin(t=0,4,4,4)
	)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#E69F00", "#56B4E9", "#009E73")

#Figure 1A
a <- all_data %>%
		group_by(year, is_society) %>%
		summarize(n_papers=n()) %>%
		ggplot(aes(year, n_papers)) +
			geom_line(aes(color=is_society), size=0.7) +
			scale_x_continuous(limits=c(1980,2015), breaks=seq(1980,2015,5)) +
			scale_y_continuous(limits=c(0,140000), breaks=seq(0,140000,2e4)) +
			scale_color_manual(breaks=c("FALSE", "TRUE"),
                            labels=c("Other Journal", "Microbiology Society Journal"),
														values=c("red", "blue"))+

			ylab("Number of Microbiology Papers Published") +
			xlab("Year") +
			my_theme +
			theme(
				legend.text = element_text(size=7),
				legend.key.size = unit(0.55, "line"),
				legend.key = element_rect(fill = NA, linetype=0),
				legend.position=c(0.4, 0.7),
				legend.title=element_blank(),#element_text(lineheight=-1),
				legend.background = element_rect(fill="white", color="black"),
				legend.margin = margin(t=0,4,4,4)
			)


#Figure 1B
b <- all_data %>%
		filter(society %in% top_micro_societies$society) %>%
		group_by(year, society) %>%
		summarize(n_papers=n()) %>%
		ggplot(aes(year, n_papers)) +
			geom_line(aes(color=society), size=0.6) +
			scale_x_continuous(limits=c(1980,2015), breaks=seq(1980,2015,5)) +
			scale_color_manual(breaks=c("American Society for Microbiology", "Microbiology Society", "Infectious Diseases Society of America", "Federation of European Microbiological Societies",   "Society for Applied Microbiology"),
														labels=c("ASM", "Microbiology Society", "IDSCA", "FEMS", "SfAM"),
														values=cbbPalette) +
			ylab("Number of Microbiology Papers Published") +
			xlab("Year") +
			my_theme +
			theme(
					legend.text = element_text(size=7),
					legend.key.size = unit(0.55, "line"),
					legend.key = element_rect(fill = NA, linetype=0),
					legend.position=c(0.75, 0.5),
					legend.title=element_blank(),#element_text(lineheight=-1),
					legend.background = element_rect(fill="white", color="black"),
					legend.margin = margin(t=0,4,4,4),
					axis.title.y = element_text(margin=margin(r=13,0,0,0))
				)

#Figure 1C
c <- all_data %>% select(year, journal) %>%
		group_by(year) %>% summarize(n_journals=n_distinct(journal)) %>%
		ggplot(aes(year, n_journals)) +
			geom_line(size=0.8) +
			scale_x_continuous(limits=c(1940,2015), breaks = seq(1940,2015,10)) +
			ylab("Number of Journals with Microbiology Papers") +
			xlab("Year") +
			my_theme


#Figure 1D
d <- mega_journal_data %>%
		ggplot(aes(year, n_papers)) +
			geom_line(aes(color=journal), size=0.8) +
			scale_x_continuous(limits=c(1980,2015), breaks=seq(1980,2015,5)) +
			scale_y_continuous(limits=c(0,7500), breaks=seq(0,7000,1000)) +
			scale_color_manual(breaks=c("PLoS One", "Sci Rep", "Front Microbiol", "BMC Res Notes", "Cell Rep"), labels=c("PLOS ONE", "Scientific Reports", "Frotiers in Microbiology", "BMC Research Notes", "Cell Reports"), values=rev(cbbPalette)) +
			ylab("Number of Microbiology Papers Published") +
			xlab("Year") +
			my_theme +
			theme(
				legend.text = element_text(size=7, face="italic"),
				legend.key.size = unit(0.55, "line"),
				legend.key = element_rect(fill = NA, linetype=0),
				legend.position=c(0.3, 0.7),
				legend.title=element_blank(),#element_text(lineheight=-1),
				legend.background = element_rect(fill="white", color="black"),
				legend.margin = margin(t=0,4,4,4)
			)


ggdraw() +
	draw_plot(a, x=0.0, y=0.5, 0.5, 0.5) +
	draw_plot(b, x=0.0, y=0.0, 0.5, 0.5) +
	draw_plot(c, x=0.5, y=0.5, 0.5, 0.5) +
	draw_plot(d, x=0.5, y=0.0, 0.5, 0.5) +
	draw_plot_label(c("A", "B", "C", "D"), x=c(0,0,0.5,0.5), y=c(1.00,0.5,1.00,0.5), size=12) +
	ggsave('figures/pub_analysis.png', width=6.5, height=6.5, unit='in') +
	ggsave('figures/pub_analysis.tiff', width=6.5, height=6.5, unit='in', dpi=600)
