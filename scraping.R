
#library(checkpoint)
#checkpoint("2015-10-29")

##################################### Import required libraries

#library(RCurl)
#library(XML)
#library(tm)
#library(gtools)
#library(rvest)
library(readr)
library(dplyr)
library(magrittr)
library(scrapeML)

##  Load list of school websites

patterns <- c("curriculum", "department", "facult", "subject")

websites <- read_csv(
  "sites.txt"
) %>%
  rowwise %>%
  mutate(
    status = NA,
    file_out = file.path("school_websites", paste("urn_", URN, ".html", sep = "")),
    status = try(get_url(Website, file_out, 1)),
    curriculum_link = extract_link(file_out, patterns = patterns, root = Website)
  )



websites %>% View

i <- 1

out_dir <- file.path("curriculums")

#dir.create(out_dir)

links <- strsplit(websites$curriculum_link[i],";") %>% unlist

for(j in 1:length(links)) {
  
  link <- links[j]
  
  dest_file <- file.path(out_dir, paste(websites$URN[i],"_",j, ".html",sep = ""))
  
  get_url(
    site_url = link,
    dest = dest_file
  )
  
}


parse_dir(
  "curriculums/",
  "curriculums/parsed/"
)

word_filter <- which(!nchar(colnames(terms)) > 10)

corpus <- tdm_matrix(source_dir = "curriculums/parsed/")

library(wordcloud)

wordcloud(corpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

tdm.1g <- tm::TermDocumentMatrix(corpus)
dtm.1g <- tm::DocumentTermMatrix(corpus)

######################
##http://hack-r.com/n-gram-wordclouds-in-r/
# Useful Code Snippet:
findFreqTerms(tdm.1g, 40)
findFreqTerms(tdm.1g, 60)
findFreqTerms(tdm.1g, 80)
findFreqTerms(tdm.1g, 100)

findAssocs(dtm.1g, "level", .75)
findAssocs(dtm.1g, "curriculum", .5)
findAssocs(dtm.1g, "pupils", .75) 

tdm89.1g <- removeSparseTerms(tdm.1g, 0.89)
tdm9.1g  <- removeSparseTerms(tdm.1g, 0.9)
tdm91.1g <- removeSparseTerms(tdm.1g, 0.91)
tdm92.1g <- removeSparseTerms(tdm.1g, 0.92)

tdm2.1g <- tdm92.1g

# Creates a Boolean matrix (counts # docs w/terms, not raw # terms)
tdm3.1g <- inspect(tdm2.1g)
tdm3.1g[tdm3.1g>=1] <- 1 

# Transform into a term-term adjacency matrix
termMatrix.1gram <- tdm3.1g %*% t(tdm3.1g)

# inspect terms numbered 5 to 10
termMatrix.1gram[5:10,5:10]
termMatrix.1gram[1:10,1:10]

# Create a WordCloud to Visualize the Text Data ---------------------------
notsparse <- tdm2.1g
m = as.matrix(notsparse)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v),freq=v)

wordcloud(d)
