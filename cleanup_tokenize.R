library(stringi)

library(data.table)
isLocal <- TRUE
if (isLocal) {
    nGrams <- readRDS("models/nGrams.rds")
    nGramSizes <- readRDS("models/nGramSizes.rds")
    dict <- readRDS("models/dictionary.rds")
    stopWords <- readLines("stop_words_en_US.txt")
    profaneWords <- readLines("profane_en.txt")
} else {
    con <- gzcon(url("http://s3-us-west-1.amazonaws.com/daigotanaka-data/keyboard-prediction/nGrams.rds"))
    nGrams <- readRDS(con)
    close(con)

    con <- gzcon(url("http://s3-us-west-1.amazonaws.com/daigotanaka-data/keyboard-prediction/nGramSizes.rds"))
    nGramSizes <- readRDS(con)
    close(con)

    con <- gzcon(url("http://s3-us-west-1.amazonaws.com/daigotanaka-data/keyboard-prediction/dictionary.rds"))
    dict <- readRDS(con)
    close(con)

    con <- file("http://s3-us-west-1.amazonaws.com/daigotanaka-data/keyboard-prediction/stop_words_en_US.txt", "r")
    stopWords <- readLines(con)
    close(con)

    con <- file("http://s3-us-west-1.amazonaws.com/daigotanaka-data/keyboard-prediction/profane_en.txt", "r")
    profaneWords <- readLines(con)
    close(con)
}

dictenv <- new.env(hash=TRUE, size=length(dict))
ret <- lapply(rep(1:length(dict)), FUN=function(i) {assign(dict[i], i, envir=dictenv)})
stopWordPattern <- paste("\\b", paste(stopWords, collapse="\\b|\\b"), "\\b", sep="")
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
            "'s|[ ]+",
            omit_empty=TRUE)[[1]]
        if (exists("dictenv", mode="environment")){
            currentTokens <- unlist(mget(currentTokens, envir=dictenv, ifnotfound=NA))
        } else {
            stop("dictenv not found!")
        }
        tokens <- c(
            tokens,
            currentTokens,
            rep(NA, 3))
    }
    return(tokens)
}
