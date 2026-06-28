library(rentrez)
library(xml2)
library(tibble)
library(purrr)

search <- entrez_search(db = "pubmed", term = "Judo AND ACL", retmax = 100)

xml_txt <- entrez_fetch(db = "pubmed", id = search$ids, rettype = "xml", parsed = FALSE)
doc <- read_xml(xml_txt)

articles <- xml_find_all(doc, ".//PubmedArticle")

df <- map_dfr(articles, function(a) {
  data.frame(
    ID = xml_text(xml_find_first(a, ".//PMID")),
    Autor = xml_text(xml_find_first(a, ".//Author[1]/LastName")),
    Jahr = xml_text(xml_find_first(a, ".//PubDate/Year")),
    Titel = xml_text(xml_find_first(a, ".//ArticleTitle")),
    Zeitschrift = xml_text(xml_find_first(a, ".//Journal/Title")),
    Abstract = xml_text(xml_find_first(a, ".//Abstract/AbstractText")),
    stringsAsFactors = FALSE
  )
})
df

pmid <- "41802434"
paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/")




####### Nützliche Funktionen aus rentrez #######
entrez_dbs() # Vorhandene Datenbanken 

entrez_db_searchable("pubmed") # welche Infos von der Datenbank ausgege werden können 

search <- entrez_search(db="pubmed", term= "ACL AND judo", retmax= 100) # in datenbank suchen
search$ids # Ids ausgeben lassen 

search <- entrez_search(db="pubmed", 
term= "ACL AND judo [ORGN] AND 2020:2026[PDAT]", # wichtig das in Klammern[]!
retmax= 100)
search$ids # Ids ausgeben lassen 


search_year <- function(year, term){
    query <- paste(term, "AND (", year, "[PDAT])") #### zählt die Punblikationen im Jahr 
    entrez_search(db="pubmed", term=query, retmax=0)$count
}
year <- 2008:2014
papers <- sapply(year, search_year, term="Connectome", USE.NAMES=FALSE)
plot(year, papers, type='b', main="The Rise of the Connectome")


sum <- entrez_summary(db="pubmed", id=42045398)
sum 
