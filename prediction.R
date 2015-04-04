getCondProb <- function(nGrams, input, n=4) {
    message(paste("Input: ", input, sep=" "))
    tokens <- tokenize(input)
    # In my implementation, the last 3 are NAs
    len <- length(tokens) - 3

    if (len < 1) {
        message("No input")
        return(NULL)
    }

    prob <- 0.0
    if (n >= 4 && len >= 4) {
        hit <- nGrams[[4]][
            N1 == tokens[len - 3] &
                N2 == tokens[len - 2] &
                N3 == tokens[len - 1] &
                N4 == tokens[len],]
        if (nrow(hit) > 0) count = hit$V1 else count = 0

        if (count > 0){
            base <- nGrams[[4]][
                N1 == tokens[len - 3] &
                    N2 == tokens[len - 2] &
                    N3 == tokens[len - 1], sum(V1)]
            message("4-gram")
            return (count / base)
        }
    }

    if (n >= 3 && len >= 3) {
        hit <- nGrams[[3]][
            N1 == tokens[len - 2] &
                N2 == tokens[len - 1] &
                N3 == tokens[len],]
        if (nrow(hit) > 0) count = hit$V1 else count = 0

        if (count > 0){
            base <- nGrams[[3]][
                N1 == tokens[len - 2] &
                    N2 == tokens[len - 1], sum(V1)]
            message("3-gram")
            return (0.4^(n - 3) * count / base)
        }
    }


    if (n >= 2 && len >= 2) {
         hit <- nGrams[[2]][N1 == tokens[len - 1] & N2 == tokens[len],]
         if (nrow(hit) > 0) count = hit$V1 else count = 0

         if (count > 0) {
             base <- nGrams[[2]][N1 == tokens[len - 1], sum(V1)]
             message("2-gram")
             return (0.4^(n - 2) * count / base)
         }
    }

    message("1-gram")
    hit <- nGrams[[1]][N1 == tokens[len],]
    if (nrow(hit) > 0) count = hit$V1 else count = 0
    return (0.4^(n - 1) * count / nGrams[[1]][, sum(V1)])
}

predictNext <- function(nGrams, input, n=4, k=0) {
    predictions <- NULL
    tokens <- tokenize(tolower(input))
    # In my implementation, the last 3 are NAs
    len <- length(tokens) - 3

    candidates = data.frame(word=rep("", 4), prob=rep(0, 4))

    if (len < 1) {
        message("No input")
        return(NULL)
    }

    if (n >= 4 && len >= 3) {
        predictions <- nGrams[[4]][
            N1 == tokens[len - 2] &
                N2 == tokens[len - 1] &
                N3 == tokens[len] &
                V1 >= k,]
        if (length(predictions$N4)) {
            candidates[4,]$word <- as.character(predictions$N4[1])
            candidates[4,]$prob <- getCondProb(
                nGrams,
                paste(
                    tokens[len - 2],
                    tokens[len - 1],
                    tokens[len],
                    predictions$N4[1],
                    sep=" "),
                n=n)
        }
    }

    if (n >= 3 && len >= 2) {
        predictions <- nGrams[[3]][
            N1 == tokens[len - 1] &
                N2 == tokens[len] &
                V1 >= k,]
        if (length(predictions$N3)) {
            candidates[3,]$word <- as.character(predictions$N3[1])
            candidates[3,]$prob <- getCondProb(
                nGrams,
                paste(
                    tokens[len - 1],
                    tokens[len],
                    predictions$N3[1],
                    sep=" "),
                n=n)
        }
    }

    if (n >= 2 && len >= 1) {
        predictions <- nGrams[[2]][N1 == tokens[len] & V1 >= k,]
        if (length(predictions$N2)) {
            candidates[2,]$word <- as.character(predictions$N2[1])
            candidates[2,]$prob <- getCondProb(
                nGrams, paste(tokens[len],  predictions$N2[1], sep=" "), n=n)
        }
    }

    return(candidates)
}

