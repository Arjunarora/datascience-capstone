library(stringi)
stopWords <- readLines("stop_words_en_US.txt")
stopWordPattern <- paste("\\b", paste(stopWords, collapse="\\b|\\b"), "\\b", sep="")

profaneWords <- readLines("profane_en.txt")
profaneWordPattern <- paste("\\b", paste(profaneWords, collapse="\\b|\\b"), "\\b", sep="")

removeStopWords <- function(x) {
    return (gsub(stopWordPattern, "", x))
}

replaceProfaneWords <- function(x) {
    return (gsub(profaneWordPattern, "****", x))
}

removeBadChars <- function(rawText) {
    return(gsub(
        "\u0091|\u0092|\u0093|\u0094|\u0096|\u0097",
        "'",
        x,
        perl=TRUE))
}

cleanText <- function(rawText) {
    cleanText <- tolower(rawText)
    # cleanText <- removeBadChars(cleanText)
    # cleanText <- removeStopWords(cleanText)
    cleanText <- replaceProfaneWords(cleanText)
    return(cleanText)
}

tokenize <- function(cleanText) {
    tokens <- c()
    sentences <- stri_split_regex(cleanText, "\\.|,|;|:|\\?|!")[[1]]
    for (i in 1:length(sentences)) {
        currentTokens <- stri_split_regex(
            gsub("[^A-Za-z']", " ", sentences[i]),
            "[ ]+",
            omit_empty=TRUE)[[1]]
        if (exists("dictenv", mode="environment")){
            currentTokens <- unlist(mget(currentTokens, envir=dictenv, ifnotfound=NA))
        }
        tokens <- c(
            tokens,
            currentTokens,
            rep(NA, 3))
    }
    return(tokens)
}
