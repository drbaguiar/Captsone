strip.text <- function(txt) {
  # remove apostrophes (so "don't" -> "dont", "Jane's" -> "Janes", etc.)
  txt <- gsub("'","",txt)
  # convert to lowercase
  txt <- tolower(txt)
  # change other non-alphanumeric characters to spaces
  txt <- gsub("[^a-z0-9]"," ",txt)
  # change digits to #
  txt <- gsub("[0-9]+"," ",txt)
  # split and make one vector
  txt <- unlist(strsplit(txt," "))
  # remove empty words
  txt <- txt[txt != ""]
  return(txt)
}

# Words within 1 transposition.
Transpositions <- function(word = FALSE) {
  N <- nchar(word)
  if (N > 2) {
    out <- rep(word, N - 1)
    word <- unlist(strsplit(word, NULL))
    # Permutations of the letters
    perms <- matrix(c(1:(N - 1), 2:N), ncol = 2)
    reversed <- perms[, 2:1]
    trans.words <- matrix(rep(word, N - 1), byrow = TRUE, nrow = N - 1)
    for(i in 1:(N - 1)) {
      trans.words[i, perms[i, ]] <- trans.words[i, reversed[i, ]]
      out[i] <- paste(trans.words[i, ], collapse = "")
    }
  }
  else if (N == 2) {
    out <- paste(word[2:1], collapse = "")
  }
  else {
    out <- paste(word, collapse = "")
  }
  return(out)
}

# Single letter deletions.
# Thanks to luiscarlosmr for partial correction in comments
Deletes <- function(word = FALSE) {
  N <- nchar(word)
  out<-mat.or.vec(1,N)
  word <- unlist(strsplit(word, NULL))
  for(i in 1:N) {
    out[i] <- paste(word[-i], collapse = "")
  }
  return(out)
}

# Single-letter insertions.
Insertions <- function(word = FALSE) {
  N <- nchar(word) 
  out <- list()
  for (letter in letters) {
    out[[letter]] <- rep(word, N + 1)
    for (i in 1:(N + 1)) {
      out[[letter]][i] <- paste(substr(word, i - N, i - 1), letter, 
                                substr(word, i, N), sep = "")
    }
  }
  out <- unlist(out)
  return(out)
}

# Single-letter replacements.
Replaces <- function(word = FALSE) {
  N <- nchar(word) 
  out <- list()
  for (letter in letters) {
    out[[letter]] <- rep(word, N)
    for (i in 1:N) {
      out[[letter]][i] <- paste(substr(word, i - N, i - 1), letter, 
                                substr(word, i + 1, N + 1), sep = "")
    }
  }
  out <- unlist(out)
  return(out)
}
# All Neighbors with distance "1"
Neighbors <- function(word) {
  neighbors <- c(word, Replaces(word), Deletes(word),
                 Insertions(word), Transpositions(word))
  return(neighbors)
}

# Probability as determined by our corpus.
Probability <- function(word, dtm) {
  # Number of words, total
  N <- length(dtm)
  word.number <- which(names(dtm) == word)
  count <- dtm[word.number]
  pval <- count/N
  return(pval)
}

# Correct a single word.
Correct <- function(word, dtm) {
  neighbors <- Neighbors(word)
  # If it is a word, just return it.
  if (word %in% names(dtm)) {
    out <- word
  }
  # Otherwise, check for neighbors.
  else {
    # Which of the neighbors are known words?
    known <- which(neighbors %in% names(dtm))
    N.known <- length(known)
    # If there are no known neighbors, including the word,
    # look farther away.
    if (N.known == 0) {
      print(paste("Having a hard time matching '", word, "'...", sep = ""))
      neighbors <- unlist(lapply(neighbors, Neighbors))
    }
    # Then out non-words.
    neighbors <- neighbors[which(neighbors %in% names(dtm))]
    N <- length(neighbors)
    # If we found some neighbors, find the one with the highest
    # p-value.
    if (N >= 1) {
      P <- 0*(1:N)
      for (i in 1:N) {
        P[i] <- Probability(neighbors[i], dtm)
      }
      out <- neighbors[which.max(P)]
    }
    # If no neighbors still, return the word.
    else {
      out <- word
    }
  }
  return(out)
}

# Correct an entire document.
CorrectDocument <- function(document, dtm) {
  by.word <- unlist(strsplit(document, " "))
  N <- length(by.word)
  for (i in 1:N) {
    by.word[i] <- Correct(by.word[i], dtm = dtm)
  }
  corrected <- paste(by.word, collapse = " ")
  return(corrected)
}

words <- scan("http://norvig.com/big.txt", what = character())
words <- strip.text(words)
counts <- table(words)
