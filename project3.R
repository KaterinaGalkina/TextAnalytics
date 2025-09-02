library(tm)
library(rstudioapi) # To be able to select a directory 
library(wordcloud)
library(quanteda)
library(syuzhet)
library(topicmodels)
library(textmineR)

path <- selectDirectory()

text <- VCorpus(DirSource(path, ignore.case = TRUE, mode = "text"))
book <- as.character(text[[1]])

chapter_titles <- paste("CHAPTER", as.roman(1:15)) 

chapter_indices <- sapply(chapter_titles, function(title) {
  which(book == title)[1]
})

dir.create(file.path(path, "Chapters"))

for (i in 1:14) {
  chapter_lines <- book[(chapter_indices[i] + 1):(chapter_indices[i + 1] - 1)]
  file_name <- sprintf("chapter_%02d.txt", i)
  write.table(chapter_lines, file = file.path(path, "Chapters", file_name),
              sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
}

chapters_dir <- file.path(path, "Chapters")
corpus_chapters <- VCorpus(DirSource(chapters_dir, ignore.case = T, mode = "text"))
summary(corpus_chapters)

# Question 1. a

for (i in 1:14){
  chap <- get_text_as_string(file.path(chapters_dir, sprintf("chapter_%02d.txt", i)))
  sentences_chap <- get_sentences(chap)
  # we are splitting once we see a non alphabetic character : 
  words_chap <- unlist(strsplit(chap, "\\W+")) 
  words_chap <- words_chap[nchar(words_chap) > 0] # eliminating empty strings
  # We sort words and sentences by their number of characters 
  sorted_words <- words_chap[order(nchar(words_chap), decreasing = TRUE)] 
  sorted_sentences <- sentences_chap[order(nchar(sentences_chap), decreasing = TRUE)]
  
  cat("For chapter", as.roman(i), " -> \n")
  cat(" Longest word: ", sorted_words[1], "\n")
  cat(" Longest sentence: ", sorted_sentences[1], "\n\n")
}

# Question 1. b

str(corpus_chapters) # The data structure 
inspect(corpus_chapters) 

corpus_chapters
chap1 <- corpus_chapters[[1]] # extract the text itself from the corpus (first chapter)

chap1[1] # to see the content

chaptersDTM <- DocumentTermMatrix(corpus_chapters)
chaptersDTM

inspect(chaptersDTM)
str(chaptersDTM)

chaptersTDM <- TermDocumentMatrix(corpus_chapters)
chaptersTDM

# Function to remove all non alphabetic and non space characters 
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)

corpus_chapters_clean <- tm::tm_map(corpus_chapters, content_transformer(removeNumPunct))
corpus_chapters_clean
str(corpus_chapters_clean)
inspect(corpus_chapters_clean)

# Making everything in lower case: 
corpus_chapters_clean_low <- tm_map(corpus_chapters_clean, tm::content_transformer(tolower))
corpus_chapters_clean_low
str(corpus_chapters_clean_low)
inspect(corpus_chapters_clean_low)

chapters_clean_dtm <- DocumentTermMatrix(corpus_chapters_clean_low)
chapters_clean_dtm

inspect(chapters_clean_DTM)
str(chapters_clean_DTM)

words_freq <- as.matrix(chapters_clean_DTM)
words_freq[, 1: 10]

my_stop_words <- c(tm::stopwords("english"))
my_stop_words

corpus_chapters_clean_low_stop <- tm::tm_map(corpus_chapters_clean_low, 
                                             tm::removeWords, my_stop_words)
# We can see the cleaned chapter 1 for example 
tm::inspect(corpus_chapters_clean_low_stop[[1]])

for (i in 1:14) {
  tdm <- TermDocumentMatrix(corpus_chapters_clean_low_stop[[i]])
  cat("Chapter", i, "Top Frequent Terms:\n")
  freq_terms <- findFreqTerms(tdm, lowfreq = 4)
  print(freq_terms)
}

my_new_stop_words <- c("one", "two", "first", "second", 
        "upon", "now", "yet", "possibly", "also", 
        "even", "ever", "among", "much", "well", "just")

# Let's update to delete just found new stop words:
corpus_chapters_clean_low_stop <- tm::tm_map(corpus_chapters_clean_low_stop, 
                                             tm::removeWords, my_new_stop_words)

# We can check that everything was well deleted, and at the same time 
# trying new function to obtain terms' frequence

frequency_lists_per_chap <- list()
for (i in 1:14) {
  cat("Chapter", i, "Top 10 Frequent Terms:\n")
  freq_terms <- tm::termFreq(corpus_chapters_clean_low_stop[[i]])
  frequency_lists_per_chap[[i]] <- freq_terms
  top_terms <- head(sort(freq_terms, decreasing = TRUE), 10)
  print(top_terms)
  cat("\n")
}

# Dendrograms
# We are generating a dendrogram for each chapter's top 20 terms
for (i in 1:14) {
  cat("Dendrogram for Chapter", i, "\n")
  chapter_freq <- frequency_lists_per_chap[[i]]
  top_terms <- head(sort(chapter_freq, decreasing = TRUE), 20)
  freq_df <- data.frame(top_terms)
  dist_matrix <- dist(freq_df)
  hc <- hclust(dist_matrix, method = "ward.D2")
  str(hc)
  plot(hc, main = paste("Dendrogram of Top Terms in Chapter", i))
}

# Word Cloud
pal <- brewer.pal(9, "BuGn")
for (i in 1:14) {
  freq_terms <- frequency_lists_per_chap[[i]]
  top_terms <- head(sort(freq_terms, decreasing = TRUE), 35)
  wordcloud(words = names(top_terms), freq = top_terms, colors = pal[-(1:4)], 
            scale = c(2.5, 0.4))
  title(paste("Word Cloud for Chapter", i), line = -1)
}

chapters_char <- sapply(corpus_chapters_clean_low_stop, paste, collapse = " ")
corpus_chapters_tokens <- tokens(chapters_char)
str(corpus_chapters_tokens)

corpus_chapters_dfm <- quanteda::dfm(corpus_chapters_tokens)

corpus_chapters_docfreq <- quanteda::docfreq(corpus_chapters_dfm) # frequency of terms in dfm
str(corpus_chapters_docfreq)
corpus_chapters_docfreq[1:20]

corpus_chapters_weights <- quanteda::dfm_weight(corpus_chapters_dfm)
print(corpus_chapters_weights, max_ndoc = 14)

corpus_chapters_tfidf <- quanteda::dfm_tfidf(corpus_chapters_dfm, 
                         scheme_tf = "count", scheme_df = "inverse")
str(corpus_chapters_tfidf)
corpus_chapters_tfidf

# Sentences' sentiments
# First we can see the dictionary of different methods
sentiment_dictionary <- get_sentiment_dictionary()
sentiment_dictionary[1:15,,]

# Dictionary of another method (Bing)
sentiment_dictionary_bing <- get_sentiment_dictionary("bing")
sentiment_dictionary_bing[1:15,,]

for (i in 1:14) {
  chapter_path <- file.path(path, "Chapters", sprintf("chapter_%02d.txt", i))
  chapter_text <- get_text_as_string(chapter_path)
  chapter_sentences <- get_sentences(chapter_text)
  
  cat("Chapter", i, "information:\n")
  
  # syuzhet method
  chapter_sentences_sentiment <- get_sentiment(chapter_sentences, method = "syuzhet")
  cat("\nSentiment for each sentence obtained with syuzhet method:\n")
  print(chapter_sentences_sentiment)
  
  chapter_sentiment_sum <- sum(chapter_sentences_sentiment)
  cat("\nThe overall emotional valence in the text with syuzhet method:\n")
  print(chapter_sentiment_sum)
  
  chapter_sentiment_mean <- mean(chapter_sentences_sentiment)
  cat("\nThe mean of sentiment in the text with syuzhet method:\n")
  print(chapter_sentiment_mean)
  
  cat("\nThe distribution with syuzhet method:\n")
  print(summary(chapter_sentences_sentiment))
  
  plot(chapter_sentences_sentiment, main = "Book Plot Trajectory: syuzhet", 
       xlab = "Narrative", ylab = "Emotional Valence")
  
  sentiment_pct_syuzhet <- get_percentage_values(chapter_sentences_sentiment, bins = 10)
  cat("\nPercentage values with syuzhet method:\n")
  print(sentiment_pct_syuzhet)
  
  plot(sentiment_pct_syuzhet, main = "Book Plot PCTValue 10 Bins (syuzhet)",
       xlab = "Narrative", ylab = "Emotional Valence")
  
  readline(prompt = "\nPress [Enter] to see with Bing method")
  
  # bing method
  chapter_sentences_sentiment_bing <- get_sentiment(chapter_sentences, method = "bing")
  cat("\nSentiment for each sentence obtained with bing method:\n")
  print(chapter_sentences_sentiment_bing)
  
  chapter_sentiment_bing_sum <- sum(chapter_sentences_sentiment_bing)
  cat("\nThe overall emotional valence in the text with bing method:\n")
  print(chapter_sentiment_bing_sum)
  
  chapter_sentiment_bing_mean <- mean(chapter_sentences_sentiment_bing)
  cat("\nThe mean of sentiment in the text with bing method:\n")
  print(chapter_sentiment_bing_mean)
  
  cat("\nThe distribution with bing method:\n")
  print(summary(chapter_sentences_sentiment_bing))
  
  plot(chapter_sentences_sentiment_bing, main = "Book Plot Trajectory: bing", 
       xlab = "Narrative", ylab = "Emotional Valence")
  
  sentiment_pct_bing <- get_percentage_values(chapter_sentences_sentiment_bing, bins = 10)
  cat("\nPercentage values with bing method:\n")
  print(sentiment_pct_bing)
  
  plot(sentiment_pct_bing, main = "Book Plot PCTValue 10 Bins (bing)",
       xlab = "Narrative", ylab = "Emotional Valence")
  
  readline(prompt = "\nPress [Enter] to see with NRC method")
  
  # nrc method
  chapter_nrc_sentiment <- get_nrc_sentiment(chapter_sentences)
  cat("\nSentiment for each sentence obtained with nrc method:\n")
  print(chapter_nrc_sentiment)
  
  # We are calculating top 3 emotions for this chapter (without positive or negative)
  emotion_sums <- colSums(chapter_nrc_sentiment[, 1:8])
  sorted_emotions <- sort(emotion_sums, decreasing = TRUE)
  top_3_emotions <- head(sorted_emotions, 3)
  cat("Top 3 emotions for this chapter:\n")
  for (i in 1:length(top_3_emotions)) {
    emotion_name <- names(top_3_emotions)[i]
    emotion_score <- top_3_emotions[i]
    cat(emotion_name, ":", emotion_score, "; ")
  }
  
  if (i != 14){
    readline(prompt = "\nPress [Enter] to continue to the next chapter's sentimental analysis")
  } 
}

# Unsupervised Machine Learning
chapter_files <- list.files(file.path(path, "Chapters"), full.names = TRUE)
chapters <- lapply(chapter_files, function(f) paste(readLines(f), collapse = " "))

# Topic Models
set.seed(1)
cat("Here are topics given by the Topic Models library : ")

for (i in 1:14) {
  cat("Chapter", i, ":\n")
  # Split chapter into chunks of phrases (because it didn't worked with the whole text in one doc)
  sentences <- get_sentences(chapters[[i]])
  # 5 groups of approximate equal number of sentences
  chunks <- split(sentences, ceiling(seq_along(sentences) / 5)) 
  pseudo_docs <- sapply(chunks, function(group) paste(group, collapse = " "))
  
  # Converting our chapter's chunks into a quanteda corpus
  current_corpus <- corpus(pseudo_docs)
  # Cleaning everything useless
  current_tokens <- tokens(current_corpus, remove_punct = TRUE)
  current_tokens <- tokens_remove(current_tokens, stopwords("english"))
  
  # Removing specific to our context stop words
  my_stopwords <- c("three", "upon", "two", "now", "yet", "first", "one", "four", "ten", "except",
                        "near", "almost", "without", "whose", "seem", "five", "us", "will", "toward",
                        "must", "much", "may", "always", "even", "well", "kind", "might")
  current_tokens <- tokens_remove(current_tokens, my_stopwords)
  
  current_tokens <- tokens_wordstem(current_tokens)
  
  # Transforming into document-feature matrix
  current_dfm <- dfm(current_tokens)
  current_dfm <- dfm_trim(current_dfm, min_termfreq = 2)
  
  current_dfm <- convert(current_dfm, to = "topicmodels")
  current_lda <- topicmodels::LDA(current_dfm, k = 5, method = "Gibbs")
  
  current_terms <- terms(current_lda, 5)
  
  cat("Top 5 topics and corresponding terms for this chapter:\n")
  print(current_terms)
  
  # Measuring how well the model predicts new data
  cat("Measurement of how the prediction is good:")
  print(perplexity(current_lda, newdata = current_dfm))
  
  cat("\n")
}

# Text Mine R
for (i in 1:14) {
  cat("Chapter", i, ":\n")
  # We are splitting chapter into chunks of phrases (because it didn't worked with the whole text in one doc)
  sentences <- get_sentences(chapters[[i]])
  # 5 groups of approximate equal number of sentences
  chunks <- split(sentences, ceiling(seq_along(sentences) / 5)) 
  pseudo_docs <- sapply(chunks, function(group) paste(group, collapse = " "))
  names(pseudo_docs) = c("doc1", "doc2", "doc3", "doc4", "doc5")
  dtm <- CreateDtm(doc_vec = pseudo_docs, doc_names = names(pseudo_docs))
  
  cat("LDA model : \n")
  # LDA model 
  lda_model <- FitLdaModel(dtm = dtm, k = 5, iterations = 250)
  # Top terms per topic
  top_terms <- GetTopTerms(phi = lda_model$phi, M = 10)
  cat("Top terms per topic :\n")
  print(top_terms)
  cat("\n")
  # Topic proportions for each chapter
  topic_proportions <- lda_model$theta
  cat("Topic proportions for each chunk :\n")
  print(topic_proportions[1:5,])
  cat("\n")
  # Most likely topic per document
  cat("Most likely topic per chunk :\n")
  print(apply(lda_model$theta, 1, which.max)[1:5])
  cat("\n")
  
  readline(prompt = "\nPress [Enter] to continue to see the results of the LSA model.")
  
  cat("LSA model : \n")
  # LSA model 
  lsa_model <- FitLsaModel(dtm = dtm, k = 5)
  # Top terms per topic
  top_terms <- GetTopTerms(phi = lsa_model$phi, M = 10)
  cat("Top terms per topic :\n")
  print(top_terms)
  cat("\n")
  # Topic proportions for each chapter
  topic_proportions <- lsa_model$theta
  cat("Topic proportions for each chunk :\n")
  print(topic_proportions[1:5,])
  cat("\n")
  # Most likely topic per document
  cat("Most likely topic per chunk :\n")
  print(apply(lsa_model$theta, 1, which.max)[1:5])
  cat("\n")
  
  readline(prompt = "\nPress [Enter] to continue to see the results of the CTM model.")
  
  cat("CTM model : \n")
  # CTM model
  ctm_model <- FitCtmModel(dtm = dtm, k = 5)
  # Top terms per topic
  top_terms <- GetTopTerms(phi = ctm_model$phi, M = 10)
  cat("Top terms per topic :\n")
  print(top_terms)
  cat("\n")
  # Topic proportions for each chapter
  topic_proportions <- ctm_model$theta
  cat("Topic proportions for each chunk :\n")
  print(topic_proportions[1:5,])
  cat("\n")
  # Most likely topic per document
  cat("Most likely topic per chunk :\n")
  print(apply(ctm_model$theta, 1, which.max)[1:5])
  cat("\n")
  
  if (i != 14){
    readline(prompt = "\nPress [Enter] to continue to the next chapter's topic analysis.")
  } 
}
