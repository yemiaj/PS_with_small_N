################
# Function to import and collate/organize articles identified during literature from databases 
# Very briefly, articles that meet several keywords were identified from Scopus, Web of Science, and PubMed 
# For specifics of the literature search, see ./Report/literature_search/new_search/databases searched.txt
################

library(readxl)

rd.dat <- function(name, type){
  if (type==".csv") {
    a <- read.csv(paste0('./Report/literature_search/new_search/', name, type))
  } else if (type==".xls") {
    a <- as.data.frame(read_excel(paste0('./Report/literature_search/new_search/', name, type)))
  }
  return(a)
}

################################################## Articles from Google Scholar ################################
sch1 <- rd.dat('GoogleScholar_results_1_05272026', '.csv')[, c(9, 3, 4, 11, 20:24)] #This selects the following columns [,c( "GSRank","Title", "Year", "Type", "CitesPerYear", "CitesPerAuthor", "AuthorCount", "Age", "Abstract")]

sch2a <- rd.dat('GoogleScholar_results_2a_05272026', '.csv')
sch2b <- rd.dat('GoogleScholar_results_2b_05282026', '.csv')
sch2c <- rd.dat('GoogleScholar_results_2c_05282026', '.csv')
sch2d <- rd.dat('GoogleScholar_results_2d_05282026', '.csv')
sch2e <- rd.dat('GoogleScholar_results_2e_05282026', '.csv')
sch2f <- rd.dat('GoogleScholar_results_2f_05282026', '.csv')
sch2 <- rbind(sch2a, sch2b, sch2c, sch2d, sch2e, sch2f)[, c(9, 3, 4, 11, 20:24)]
rm(list = c('sch2a', 'sch2b', 'sch2c', 'sch2d', 'sch2e', 'sch2f'))

sch3 <- rd.dat('GoogleScholar_results_3_05282026', '.csv')[, c(9, 3, 4, 11, 20:24)]

#First concatenate and flatten
goo.sch0 <- rbind(sch1, sch2, sch3) #566 obs
goo.sch <- goo.sch0[!duplicated(goo.sch0$Title), ] #533 obs
goo.sch$source.gschol <- "Google Scholar"
names(goo.sch)[names(goo.sch) == "Year"] <- "Year.gschol"
names(goo.sch)[names(goo.sch) == "Abstract"] <- "Abstract.gschol"


################################################## Articles from PubMed ################################
pmed <- rd.dat('pubmed_csv-propensity-set_05272026', '.csv')
pmed$seq <- 1:nrow(pmed)
pmed <- pmed[,c("seq", "Title", "PMID", "Publication.Year", "DOI")] #246 obs
pmed$source.pubmed <- "PubMed"
names(pmed)[names(pmed) == "Publication.Year"] <- "Year.pubmed"


################################################## Articles from Web of Science ################################
webos1 <- rd.dat('WebOfScience_results_1_05272026', '.xls')
webos2 <- rd.dat('WebOfScience_results_2_05272026', '.xls')
webos3 <- rd.dat('WebOfScience_results_3_05272026', '.xls')
webos4 <- rd.dat('WebOfScience_results_4_05272026', '.xls')

webos0 <- rbind(webos1, webos2, webos3, webos4)[, c(9, 13, 14, 22, 33, 34, 35, 47, 57, 62, 64, 66)] #828 obs
# selects c("Article Title", "Language", "Document Type", "Abstract", "Cited Reference Count", "Times Cited, WoS Core", "Times Cited, All Databases", "Publication Year", "DOI", "WoS Categories", "Research Areas", "Pubmed Id")
rm(list = c('webos1', 'webos2', 'webos3', 'webos4'))

webos <- webos0[!duplicated(webos0$`Article Title`), ] #573 obs
webos$source.webos <- "Web of Science"
names(webos)[names(webos) == "Publication Year"] <- "Year.webos"
names(webos)[names(webos) == "Article Title"] <- "Title"
names(webos)[names(webos) == "Abstract"] <- "Abstract.webos"


#Merge sources in 2 steps
lit.sources <- merge(goo.sch[,c("Title", "Year.gschol", "source.gschol", "Abstract.gschol")],
                     pmed[,c("Title", "Year.pubmed", "source.pubmed")],
                     by="Title", all=TRUE)
#goo.sch: 533 obs
#pmed: 246 obs
#webos: 573 obs

#PubMed + Google Scholar: 774 instead of 779 obs (5 obs in common between PubMed and Google Scholar)
#PubMed + Web of Science: 721 instead of 819 obs (98 obs in common between PubMed and Web of Science)
#Google Scholar + Web of Science: 1099 instead of 1106 obs (7 obs in common between Google Scholar and Web of Science)
#All sources have 1 article in common

lit.sources <- merge(lit.sources,
                     webos[,c("Title", "Year.webos", "source.webos", "Abstract.webos")],
                     by="Title", all=TRUE)


#Output pooled articles for manual review to identify those relevant to the project
write.csv(lit.sources, "./Report/literature_search/new_search/all.pooled.articles_05312026.csv")

