library(jsonlite)
library(rvest)
library(tm)
library(topicmodels)
library(SnowballC)
library(wordcloud)


page_numbers <- 2:10 
base_url <- "http://boards.4chan.org"

getHundredKeywords <- function(category){
  
  paging_urls <- paste0(base_url, category, page_numbers)
  
  head(paging_urls, 3)
  
  all_links <- NULL
  for (url_base in paging_urls) {
    html_document <- read_html(url_base)
    
    links <- html_document %>%
      html_nodes(".postMessage") %>%
      html_text()
    
    all_links <- c(all_links, links)
  }
  
  links
  
  all_links <- Corpus(VectorSource(all_links))
  
  all_links <- tm_map(all_links, removeNumbers)
  all_links <- tm_map(all_links, removePunctuation)
  all_links <- tm_map(all_links, stripWhitespace)
  all_links <- tm_map(all_links, content_transformer(tolower))
  all_links <- tm_map(all_links, removeWords, stopwords("en"))
  all_links <- tm_map(all_links, stemDocument)
  
  dtm <- DocumentTermMatrix(all_links)
  
  freq <- colSums(as.matrix(dtm))
  
  length(freq)

  ord <- order(freq, decreasing=TRUE)
  headwords <- head(freq[ord], 100)
  
 
  #df <- data.frame('category' = category, 'headwords' = headwords, 'dtm' = dtm, 'freq' = freq, 'ord' =  ord)
  
  return(headwords)
  
}

plotAndSaveWordMap <- function(vector, category){
  png(paste0("wordcloud", toString(category), ".png"), width=1280,height=800)
  
  wordcloud(words = names(vector), freq = vector, min.freq = 1,
            max.words=100, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  dev.off()
}

#Politically Incorrect section
polHead <- getHundredKeywords('/pol/')
plotAndSaveWordMap(polHead, 'pol')

#Fitness section
fitHead <- getHundredKeywords('/fit/')
plotAndSaveWordMap(fitHead, 'fit')

#Business and Finance section
bizHead <- getHundredKeywords('/biz/')
plotAndSaveWordMap(bizHead, 'biz')

