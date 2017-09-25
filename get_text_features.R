add_dtm_features <- function(subject){
  library(tm)
  subject <- clean_subject(subject)
  corpus <- SimpleCorpus(VectorSource(subject))
  dtm <- DocumentTermMatrix(corpus, control = list(removeNumbers = TRUE, weighting =function(x) weightTfIdf(x, normalize =FALSE), 
                                                   stopwords = stopwords(kind = "fr"),
                                                   language = "fr", wordLengths = c(3,Inf)))
  return(as.matrix(dtm))
}

add_bigram_features <- function(subject){
  library(tm)
  BigramTokenizer <- function(x) {
      unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)}
  
  corpus <- SimpleCorpus(VectorSource(subject))
  dtm <- DocumentTermMatrix(corpus, control = list(removeNumbers = TRUE, weighting =function(x) weightTfIdf(x, normalize =FALSE), 
                                                   stopwords = stopwords(kind = "fr"), tokenize = BigramTokenizer,
                                                   language = "fr", wordLengths = c(3,Inf)))
  return(as.matrix(dtm))
  
  
}

add_words_count <- function(subject){
  library(stringr)
  word_count <- str_count(subject, "\\S+")
  return(word_count)
}


add_short_words_count <- function(subject, nchar_word = 4){
  return(unlist(lapply(lapply(strsplit(subject," "), nchar), function(x) sum(sum(x < nchar_word)))))
}

detect_caps <- function(subject){
  return(str_detect(subject, "\\b[A-Z]+\\b"))  
}

detect_prices <- function(subject){
  prices <- str_detect(subject, "[[:digit:]]")
  promotion <- str_detect(subject, "[[%]]")
  
  return(cbind(prices,promotion))
}

detect_punctuation <- function(subject){
  excl <- str_detect(subject, "[[!]]")
  int <- str_detect(subject, "[[?]]")
  dot <- str_detect(subject, "[[...]]")
  return(cbind(excl, int, dot))
}

detect_brand_product <- function(subject, custom_brands){
  subject <- tolower(subject)
  brands <- c("guess","naf","naf naf","nike","asics","adidas","decathlon","décathlon","sandro","rayban","apple","iphone",
              "sgs","samsung","galaxy","s7","s6","s8","sgs7","sgs6","sgs8","ipad")
  if(!missing(custom_brands)){
    time_refs <- c(time_refs, custom_brands)
  }
  return(grepl(paste0(time_refs,collapse = "|"), subject))
}


detect_time_reference <- function(subject, custom_tags){
  subject <- tolower(subject)
  time_refs <- c("mois","jours","jour","semaine","été","hiver","printemps","automne","janvier","février","mars","avril",
                 "mai","juin","juillet","août","septembre","octobre","novembre","décembre")
  if(!missing(custom_tags)){
    time_refs <- c(time_refs, custom_tags)
  }
  return(grepl(paste0(time_refs,collapse = "|"), subject))
}