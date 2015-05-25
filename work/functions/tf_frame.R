
tf_frame <- function(text_vector, term_sparse_factor, method = "") {
    
  library(tm)
  library(SnowballC)
  
  # Create corpus
  corpus = Corpus(VectorSource(text_vector))
  
  # Look at corpus
  #corpus
  #corpus[[1]]
  
  # Convert to lower-case
  corpus = tm_map(corpus, tolower)
  
  # IMPORTANT NOTE: If you are using the latest version of the tm package, 
  #you will need to run the following line before continuing (it converts corpus to a Plain Text Document). 
  #This is a recent change having to do with the tolower function that occurred after this video was recorded.
  
  corpus = tm_map(corpus, PlainTextDocument)
  
  # Remove punctuation
  corpus = tm_map(corpus, removePunctuation)
  
  # Look at stop words 
  #stopwords("english")[1:10]
  
  # Remove stopwords and apple
  #corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  # Stem document 
  corpus = tm_map(corpus, stemDocument)
  
  # Create matrix
  #frequencies = DocumentTermMatrix(corpus)
  if (method == "") {
    frequencies = DocumentTermMatrix(corpus) 
  } else if (method == "tfnormidf") {
    frequencies = DocumentTermMatrix(corpus, 
                                     control = list(weighting =
                                                    function(x)
                                                    weightTfIdf(x, normalize = TRUE)))
  } else if (method == "tfidf") {
    frequencies = DocumentTermMatrix(corpus, 
                                     control = list(weighting =
                                                    function(x)
                                                    weightSMART(x, spec = "ntn")))
  }
  
  # Look at matrix 
  #inspect(frequencies[1000:1005,505:515])
  
  # Check for sparsity
  #findFreqTerms(frequencies, lowfreq=20)
  
  # Remove sparse terms
  # Keep only those terms that exist in 0.5% of the tweets
  #sparse = removeSparseTerms(frequencies, 0.995)
  if (term_sparse_factor < 1) {
    sparse = removeSparseTerms(frequencies, term_sparse_factor)
  }
  else {
    sparse = frequencies
  }
  #sparse
  
  # Convert to a data frame
  sparse_frame = as.data.frame(as.matrix(sparse))
  
  # Make all variable names R-friendly
  colnames(sparse_frame) = make.names(colnames(sparse_frame))

  sparse_frame
}