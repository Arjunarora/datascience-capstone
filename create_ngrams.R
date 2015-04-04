library(data.table)

if (!exists("getFileName")) {
    source("getFileName.R")
}

create4GramChunks <- function(locale="en_US", source=c("twitter"), lines=-1L)
    {
    fourGramChunks <- NULL
    if (exists("dictenv", mode="environment")) message("Converting to index.")
    for (i in 1:length(source)) {
        s <- source[i]
        data = readLines(getFileName(locale, s), n=lines, encoding="UTF-8",
                         skipNul=TRUE)
        message(paste("Finished reading", s))

        # Tokenize
        charVec <- unlist(lapply(data, tokenize))

        rm(data)
        gc()

        # Create 4 gram chunks
        dt <- data.table(
            N1=charVec[1:(length(charVec) - 3)],
            N2=charVec[2:(length(charVec) - 2)],
            N3=charVec[3:(length(charVec) - 1)],
            N4=charVec[4:length(charVec)])
        rm(charVec)
        gc()

        # Remove n-grams starting with NA
        dt <- dt[!is.na(N1),]

        dt <- cbind(dt, N=rep(1, nrow(dt)))
        fourGramChunks <- rbindlist(list(fourGramChunks, dt))

        message(paste("Finished processing", s, " total rows=", nrow(fourGramChunks)))
    }

    return(fourGramChunks)
}

makeFourGrams <- function(fourGramChunks){

    # Make 1, 2, 3, 4-grams
    fourGrams <- list(
        fourGramChunks[, count(), .(N1)],
        fourGramChunks[!is.na(N2), count(), .(N1, N2)],
        fourGramChunks[!is.na(N2) & !is.na(N3), count(), .(N1, N2, N3)],
        fourGramChunks[!is.na(N2) & !is.na(N3) & !is.na(N4), count(), .(N1, N2, N3, N4)])

    # Pre-sort them by counts for convenience
    for (i in 1:4) {
        fourGrams[[i]] <- fourGrams[[i]][order(-fourGrams[[i]]$V1),]
    }

    return(fourGrams)
}

