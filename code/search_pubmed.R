#install.packages("devtools")
#devtools::install_github("ropensci/rentrez")
library(rentrez)
library(XML)
library(parallel)
library(dplyr)


flatten_list_items <- function(x){
	print(x)
	epub_year <- x$EPubDate
	pub_year <- x$PubDate
	journal <- x$Source
	issn <- x$ISSN
	essn <- x$ESSN
	id_table <- do.call(rbind.data.frame, x$ArticleIds)
	doi <- as.character(id_table[which(id_table[,'IdType']=='doi'),'Value'][1])

	if(is.null(doi) || is.na(doi)){ doi <- "NA" }
	if(is.null(epub_year)){ epub_year <- "NA" }
	if(is.null(pub_year)){ pub_year <- "NA" }
	if(is.null(journal)){ journal <- "NA" }
	if(is.null(issn)){ issn <- "NA" }
	if(is.null(essn)){ essn <- "NA" }
	c(doi=doi, epub_year=epub_year, pub_year=pub_year, journal=journal, issn=issn, essn=essn)
}



keyword_search <- function(){

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

	terms <- c("yeast*", "fung*", "viral*", "virus*", "phage*", "bacteriophage*", "archaea*",
	 					"bacteri*", "bacteria*", "bacterio*", "bacterior*", "bacterios*", "bacteriot*",
						"bacteriov*", "bacteriu*", "microbi*", "microbe*", "microorganism*", "pathogen",
						"pathogens", "pathogenic", "protist*")

	search_string <- paste(paste0(terms, "[All Fields]"), collapse= " OR ")
	r_search <- entrez_search(db="pubmed", term=search_string, use_history=TRUE)

	save(r_search, file="keyword_search.rdata")
}



retrieve_keyword_record_chunk <- function(chunk_start, chunk_size=10000){

	load("keyword_search.rdata")

	Sys.sleep(runif(1) * 60) # slow down searches so they don't clobber server

	#r_search <- entrez_search(db="pubmed", term="26446605[uid] OR 27795424[uid]", use_history=TRUE)
	#chunk_size <- 10000
	#chunk_start <- 1

	chunk_summary <- rentrez::entrez_summary(db="pubmed", web_history=r_search$web_history,
																	retmax=chunk_size, retstart=chunk_start, retmode="xml")

	chunk_list <- rentrez::extract_from_esummary(chunk_summary, c("EPubDate", "PubDate", "Source",
																	"ArticleIds", "ISSN", "ESSN"), simplify=FALSE)

	chunk_df <- data.frame(t(sapply(chunk_list, flatten_list_items)), stringsAsFactors=FALSE)
	chunk_df$pmid <- rownames(chunk_df)

	write.table(file=paste0("data/temp_keyword_pmid_doi_year_journal_", chunk_start, ".tsv"),
							x=chunk_df, quote=T, row.names=F,col.names=T, sep='\t')
}



retrieve_keyword_records <- function(){
	keyword_search()
	load("keyword_search.rdata")

	indices <- sprintf("%07d", seq(1, r_search$count, 10000))

	cl <- makeCluster(12)
	clusterExport(cl, "flatten_list_items")
	parLapply(cl, indices, retrieve_keyword_record_chunk)
	stopCluster(cl)

	temp_files <- paste0("data/temp_keyword_pmid_doi_year_journal_", indices, ".tsv")
	composite <- lapply(temp_files, function(x)read.table(file=x, stringsAsFactors=F, header=T))
	composite <- do.call(rbind.data.frame, composite)

	write.table(file="data/keyword_pmid_doi_year_journal.tsv", composite, row.names=F, quote=T,
							sep='\t')

	unlink(temp_files)
	unlink("keyword_search.rdata")
}




issn_search <- function(){

	issn <-	c("1217-8950",  # ACTA MICROBIOL IMM H
						"0065-1583",  # ACTA PROTOZOOL
						"0065-2164",  # ADV APPL MICROBIOL
						"0065-2911",  # ADV MICROB PHYSIOL
						"1075-9964",  # ANAEROBE
						"1476-0711",  # ANN CLIN MICROB ANTI
						"1590-4261",  # ANN MICROBIOL
						"0066-4227",  # ANNU REV MICROBIOL
						"0066-4804",  # ANTIMICROB AGENTS CH
						"2047-2994",  # ANTIMICROB RESIST IN
						"1572-9699",  # ANTON LEEUW INT J G
						"0108-0164", "0108-0180", "0108-0202", "0304-131X, 0304-1328", "0365-4184", "0365-5555", "0365-5571", "0903-4641",  # APMIS
						"1608-3024",  # APPL BIOCHEM MICRO+
						"0099-2240", "0003-6919",  # APPL ENVIRON MICROB
						"0948-3055",  # AQUAT MICROB ECOL
						"1432-072X",  # ARCH MICROBIOL
						"1472-3654",  # ARCHAEA
						"1876-2883",  # BENEF MICROBES
						"1471-2180",  # BMC MICROBIOL
						"1517-8382",  # BRAZ J MICROBIOL
						"1712-9532", "1180-2332",  # CAN J INFECT DIS MED
						"1480-3275",  # CAN J MICROBIOL
						"1931-3128",  # CELL HOST MICROBE
						"1462-5814",  # CELL MICROBIOL
						"1058-4838",  # CLIN INFECT DIS
						"1198-743X",  # CLIN MICROBIOL INFEC
						"0893-8512",  # CLIN MICROBIOL REV
						"1556-6811", "1071-412X",  # CLIN VACCINE IMMUNOL
						"0147-9571",  # COMP IMMUNOL MICROB
						"1040-841X, 0045-6454",  # CRIT REV MICROBIOL
						"0343-8651",  # CURR MICROBIOL
						"1369-5274",  # CURR OPIN MICROBIOL
						"0070-217X, 0367-1003",  # CURR TOP MICROBIOL
						"0732-8893",  # DIAGN MICR INFEC DIS
						"2222-1751",  # EMERG MICROBES INFEC
						"0213-005X",  # ENFERM INFEC MICR CL
						"1758-2229",  # ENV MICROBIOL REP
						"1462-2912",  # ENVIRON MICROBIOL
						"1210-7913", "0009-0522",  # EPIDEMIOL MIKROBI IM
						"1535-9778",  # EUKARYOT CELL
						"0934-9723", "0722-2211",  # EUR J CLIN MICROBIOL
						"0932-4739",  # EUR J PROTISTOL
						"1431-0651",  # EXTREMOPHILES
						"0168-6496",  # FEMS MICROBIOL ECOL
						"0378-1097",  # FEMS MICROBIOL LETT
						"0168-6445",  # FEMS MICROBIOL REV
						"1567-1356",  # FEMS YEAST RES
						"0015-5632",  # FOLIA MICROBIOL
						"1867-0342",  # FOOD ENVIRON VIROL
						"0740-0020",  # FOOD MICROBIOL
						"2235-2988",  # FRONT CELL INFECT MI
						"1664-302X",  # FRONT MICROBIOL
						"1746-0913",  # FUTURE MICROBIOL
						"2169-8287",	# GENOME ANNOUNC
						"1757-4749",  # GUT PATHOG
						"1083-4389",  # HELICOBACTER
						"0046-8991",  # INDIAN J MICROBIOL
						"0019-9567",	#	INFECT IMMUN
						"1753-4259", "0968-0519",  # INNATE IMMUN-LONDON
						"0924-8579",  # INT J ANTIMICROB AG
						"0168-1605",  # INT J FOOD MICROBIOL
						"1438-4221", "0934-8840",  # INT J MED MICROBIOL
						"1466-5026", "0020-7713",  # INT J SYST EVOL MICR
						"1618-1905",  # INT MICROBIOL
						"1751-7362",  # ISME J
						"0021-8820",  # J ANTIBIOT
						"0305-7453",  # J ANTIMICROB CHEMOTH
						"1364-5072", "0021-8847",  # J APPL MICROBIOL
						"0021-9193",  # J BACTERIOL
						"0233-111X",  # J BASIC MICROB
						"0095-1137",  # J CLIN MICROBIOL
						"1550-7408",  # J EUKARYOT MICROBIOL
						"0022-1260",  # J GEN APPL MICROBIOL
						"0022-1317",	# J GEN VIROL
					#	"0022-1767", "1047-7381", "1048-3233", # J IMMUNOL
						"0022-1899",  # J INFECT DIS
						"0022-2615",  # J MED MICROBIOL
						"1225-8873",  # J MICROBIOL
						"1017-7825",  # J MICROBIOL BIOTECHN
						"1684-1182",  # J MICROBIOL IMMUNOL
						"0167-7012",  # J MICROBIOL METH
						"1464-1801",  # J MOL MICROB BIOTECH
						"2000-2297",  # J ORAL MICROBIOL
						"0022-538X",	# J VIROL
						"1477-8920",  # J WATER HEALTH
						"2008-3645",  # JUNDISHAPUR J MICROB
						"0266-8254",  # LETT APPL MICROBIOL
						"2150-7511",  # MBIO
						"0300-8584", "0044-3077",  # MED MICROBIOL IMMUN
						"0580-9517",  # METHOD MICROBIOL
						"1751-7915",  # MICROB BIOTECHNOL
						"1076-6294",  # MICROB DRUG RESIST
						"1432-184X",  # MICROB ECOL
						"2057-5858",	# MICROB GENOM
						"0882-4010",  # MICROB PATHOGENESIS
						"1342-6311",  # MICROBES ENVIRON
						"1286-4579",  # MICROBES INFECT
						"0385-5600", "0021-5139",  # MICROBIOL IMMUNOL
						"1092-2172", "0146-0749", "0005-3678",  # MICROBIOL MOL BIOL R
						"0944-5013", "0232-4393",  # MICROBIOL RES
						"1350-0872", "0022-1287",  # MICROBIOL-SGM
						"0026-2617",  # MICROBIOLOGY+
						"2045-8827",  # MICROBIOLOGYOPEN
						"2049-2618",  # MICROBIOME
						"0374-9096",  # MIKROBIYOL BUL
						"0270-7306",	# MOL CELL BIOL
						"0891-4168",  # MOL GENET MICROBIOL+
						"0950-382X",  # MOL MICROBIOL
						"2041-1006", "1399-302X",  # MOL ORAL MICROBIOL
						"2379-5042",	#	MSYSTEMS
						"2379-5077",	# MSPHERE
						"1740-1526",  # NAT REV MICROBIOL
						"1121-7138,  0391-5352",  # NEW MICROBIOL
						"2049-632X, 0928-8244", "0920-8534",  # PATHOG DIS
						"0147-619X",  # PLASMID
						"1553-7366",  # PLOS PATHOG
						"1733-1331", "0137-1320", "0001-6195", "0567-7815", "0567-7823",  # POL J MICROBIOL
						"0079-4252",  # POSTEP MIKROBIOL
						"1867-1314",  # PROBIOTICS ANTIMICRO
						"1434-4610",  # PROTIST
						"0923-2508", "0769-2609",  # RES MICROBIOL
						"0325-7541", "0325-1713",  # REV ARGENT MICROBIOL
						"0214-3429",  # REV ESP QUIM
						"1944-3277",  # STAND GENOMIC SCI
						"0334-5114",  # SYMBIOSIS
						"0723-2020",  # SYST APPL MICROBIOL
						"1877-959X",  # TICKS TICK-BORNE DIS
						"0966-842X",  # TRENDS MICROBIOL
						"1472-9792", "0962-8479",  # TUBERCULOSIS
						"0378-1135",  # VET MICROBIOL
						"2150-5594",  # VIRULENCE
						"1097-0061")  # YEAST

	issn_or <- paste0(paste(issn, collapse= "[issn] OR "), "[issn]")

	r_search <- entrez_search(db="pubmed", term=issn_or, use_history=TRUE)

	save(r_search, file="issn_search.rdata")
}



retrieve_issn_record_chunk <- function(chunk_start, chunk_size=10000){

	load("issn_search.rdata")

	Sys.sleep(runif(1) * 60) # slow down searches so they don't clobber server

	chunk_summary <- rentrez::entrez_summary(db="pubmed", web_history=r_search$web_history,
																	retmax=chunk_size, retstart=chunk_start, retmode="xml")

	chunk_list <- rentrez::extract_from_esummary(chunk_summary, c("EPubDate", "PubDate", "Source",
																	"ArticleIds", "ISSN", "ESSN"), simplify=FALSE)

	chunk_df <- data.frame(t(sapply(chunk_list, flatten_list_items)), stringsAsFactors=FALSE)
	chunk_df$pmid <- rownames(chunk_df)

	write.table(file=paste0("data/temp_issn_pmid_doi_year_journal_", chunk_start, ".tsv"),
							x=chunk_df, quote=T, row.names=F,col.names=T, sep='\t')
}



retrieve_issn_records <- function(){
	issn_search()
	load("issn_search.rdata")

	indices <- sprintf("%07d", seq(1, r_search$count, 10000))

	cl <- makeCluster(12)
	clusterExport(cl, "flatten_list_items")
	parLapply(cl, indices, retrieve_issn_record_chunk)
	stopCluster(cl)

	temp_files <- paste0("data/temp_issn_pmid_doi_year_journal_", indices, ".tsv")
	composite <- lapply(temp_files, function(x)read.table(file=x, stringsAsFactors=F, header=T))
	composite <- do.call(rbind.data.frame, composite)

	write.table(file="data/issn_pmid_doi_year_journal.tsv", composite, row.names=F, quote=T, sep='\t')

	unlink(temp_files)
	unlink("issn_search.rdata")
}




merge_search_data <- function(){

	society_map <- data.frame(
		matrix(c("1217-8950", "Hungarian Academy of Sciences",
			"0065-1583", "Polish Society of Cell Biology",
			"1075-9964", "Anaerobe Society of the Americas and the Japanese Association for Anaerobic Infection Research",
			"0066-4804", "American Society for Microbiology",
			"0108-0164", "Scandinavian Societies for Medical Microbiology and Pathology",
			"0108-0180", "Scandinavian Societies for Medical Microbiology and Pathology",
			"0108-0202", "Scandinavian Societies for Medical Microbiology and Pathology",
			"0304-131X", "Scandinavian Societies for Medical Microbiology and Pathology",
			"0304-1328", "Scandinavian Societies for Medical Microbiology and Pathology",
			"0365-4184", "Scandinavian Societies for Medical Microbiology and Pathology",
			"0365-5555", "Scandinavian Societies for Medical Microbiology and Pathology",
			"0365-5571", "Scandinavian Societies for Medical Microbiology and Pathology",
			"0903-4641", "Scandinavian Societies for Medical Microbiology and Pathology",
			"1608-3024", "Russian Academy of Sciences",
			"0099-2240", "American Society for Microbiology",
			"0003-6919", "American Society for Microbiology",
			"1517-8382", "Brazilian Society for Microbiology",
			"1480-3275", "Canadian Society of Microbiologists",
			"1058-4838", "Infectious Diseases Society of America",
			"1198-743X", "European Society of Clinical Microbiology and Infectious Diseases",
			"0893-8512", "American Society for Microbiology",
			"1556-6811", "American Society for Microbiology",
			"1071-412X", "American Society for Microbiology",
			"0213-005X", "Spanish Society of Infectious Diseases and Clinical Microbiology",
			"1758-2229", "Society for Applied Microbiology",
			"1462-2912", "Society for Applied Microbiology",
			"1210-7913", "Czech Medical Association",
			"0009-0522", "Czech Medical Association",
			"1535-9778", "American Society for Microbiology",
			"0932-4739", "Federation of European Protistological Societies",
			"1431-0651", "International Society for Extremophiles",
			"0168-6496", "Federation of European Microbiological Societies",
			"0378-1097", "Federation of European Microbiological Societies",
			"0168-6445", "Federation of European Microbiological Societies",
			"1567-1356", "Federation of European Microbiological Societies",
			"0015-5632", "Institute of Microbiology, Academy of Sciences of the Czech Republic and Czechoslavak Society for Microbiology",
			"1867-0342", "International Society of Food and Environmental Virology",
			"1757-4749", "The International Society for Genomic and Evolutionary Microbiology",
			"0046-8991", "Association of Microbiologists of India",
			"1753-4259", "International Endotoxin Society",
			"0968-0519", "International Endotoxin Society",
			"0924-8579", "International Society of Chemotherapy",
			"1438-4221", "Deutsche Gesellschaft fur Hygiene und Mikrobiologie",
			"0934-8840", "Deutsche Gesellschaft fur Hygiene und Mikrobiologie",
			"1466-5026", "Microbiology Society",
			"0020-7713", "Microbiology Society",
			"1618-1905", "Spanish Society for Microbiology",
			"1751-7362", "International Society for Microbial Ecology",
			"0021-8820", "Japan Antibiotics Research Association and Society for Actinomycetes Japan",
			"0305-7453", "British Society for Antimicrobial Chemotherapy",
			"1364-5072", "Society for Applied Microbiology",
			"0021-8847", "Society for Applied Microbiology",
			"0021-9193", "American Society for Microbiology",
			"0095-1137", "American Society for Microbiology",
			"1550-7408", "International Society of Protistologists",
			"0022-1899", "Infectious Diseases Society of America",
			"0022-2615", "Microbiology Society",
			"1225-8873", "Microbiological Society of Korea",
			"1017-7825", "Korean Society for Microbiology and Biotechnology",
			"1684-1182", "Taiwan Society of Microbiology, the Chinese Society of Immunology, the Infectious Diseases Society of Taiwan and the Taiwan Society of Parasitology",
			"2008-3645", "Ahvaz Jundishapur University of Medical Sciences",
			"0266-8254", "Society for Applied Microbiology",
			"2150-7511", "American Society for Microbiology",
			"1751-7915", "Society for Applied Microbiology",
			"1342-6311", "Japanese Society of Microbial Ecology, Japanese Society of Soil Microbiology, Taiwan Society of Microbial Ecology, and Japanese Society of Plant Microbe Interactions",
			"0385-5600", "Japanese Society for Virology, Japanese Society for Host Defense Research, Japanese Society for Bacteriology",
			"0021-5139", "Japanese Society for Virology, Japanese Society for Host Defense Research, Japanese Society for Bacteriology",
			"1092-2172", "American Society for Microbiology",
			"0146-0749", "American Society for Microbiology",
			"0005-3678", "American Society for Microbiology",
			"1350-0872", "Microbiology Society",
			"0022-1287", "Microbiology Society",
			"0026-2617", "Russian Academy of Sciences",
			"0374-9096", "Ankara Microbiology Society",
			"1121-7138", "Italian Society for Medical Virology",
			"0391-5352", "Italian Society for Medical Virology",
			"2049-632X", "Federation of European Microbiological Societies",
			"0928-8244", "Federation of European Microbiological Societies",
			"0920-8534", "Federation of European Microbiological Societies",
			"1733-1331", "Polish Society of Microbiologists",
			"0137-1320", "Polish Society of Microbiologists",
			"0001-6195", "Polish Society of Microbiologists",
			"0567-7815", "Polish Society of Microbiologists",
			"0567-7823", "Polish Society of Microbiologists",
			"0079-4252", "Polskie Towarzystwo Mikrobiologow",
			"0923-2508", "Institut Pasteur",
			"0769-2609", "Institut Pasteur",
			"0325-7541", "Asociación Argentina de Microbiología",
			"0325-1713", "Asociación Argentina de Microbiología",
			"0214-3429", "Sociedad Española de Quimioterapia",
			"1944-3277", "Genomic Standards Consortium",
			"0334-5114", "International Symbiosis Society",
			"2379-5042", "American Society for Microbiology",
			"2379-5077", "American Society for Microbiology",
			"0270-7306", "American Society for Microbiology",
			"2057-5858", "Microbiology Society",
			"0022-1317", "Microbiology Society",
			"2169-8287", "American Society for Microbiology",
			"0019-9567", "American Society for Microbiology",
			"0022-538X", "American Society for Microbiology"#,
			#"0022-1767", "American Association of Immunologists",
			#"1047-7381", "American Association of Immunologists",
			#"1048-3233", "American Association of Immunologists"
		), ncol=2, byrow=T), stringsAsFactors=F
	)
	colnames(society_map) <- c("issn", "society")

	keyword_data <- read.table(file="data/keyword_pmid_doi_year_journal.tsv", header=T,
															stringsAsFactors=FALSE)
	# 3502545 rows

	issn_data <- read.table(file="data/issn_pmid_doi_year_journal.tsv", header=T,
													stringsAsFactors=FALSE)
	#  518830 rows

	pubmed_data <- unique(rbind(issn_data, keyword_data))
	# 3533643 rows

	pubmed_data$epub_year <- gsub(".*(\\d\\d\\d\\d).*", "\\1", pubmed_data$epub_year)
	pubmed_data$pub_year <- gsub(".*(\\d\\d\\d\\d).*", "\\1", pubmed_data$pub_year)
	pubmed_data[!grepl("^10\\.\\d*\\/", pubmed_data$doi), "doi"] <- NA
	pubmed_data$society <- NA

	x <- left_join(pubmed_data, society_map, by=c('essn'='issn'))
	y <- left_join(pubmed_data, society_map, by='issn')
	pubmed_data$society <- ifelse(is.na(x$society.y), y$society.y, x$society.y)

	write.table(pubmed_data[,c(7,1,2,3,4,5,6,8)], file="data/pmid_doi_year_journal_society.tsv",
							row.names=F)

}
