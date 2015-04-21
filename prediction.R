library(data.table)

getCondProb <- function(nGrams, input, n=4) {
    message(paste("Input: ", paste(input, collapse=" "), sep=" "))
    if (is.character(input)) {
        message("Tokenizing the input")
        tokens <- tokenize(input)
    } else {
        tokens <- input
    }
    tokens <- tokens[!is.na(tokens)]
    message(paste("Tokens: ", paste(tokens, collapse=" "), sep=" "))

    # In my implementation, the last 3 are NAs
    len <- length(tokens)

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

predictNext <- function(nGrams, dict, input, n=4, k=0) {
    predictions <- NULL
    tokens <- tokenize(cleanText(input))
    tokens <- tokens[!is.na(tokens)]

    candidates = c()
    message(paste(tokens, collapse=" "))
    len <- length(tokens)
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
        num <- nrow(predictions)
        if (num > 0) {
            for (i in 1:min(5, num)) {
                predicted <- c(tokens[(len - 2):len], predictions$N4[i])
                candidates[dict[predictions$N4[i]]] <- getCondProb(nGrams, predicted, n=n)
            }
        }
    }

    if (n >= 3 && len >= 2) {
        predictions <- nGrams[[3]][
            N1 == tokens[len - 1] &
                N2 == tokens[len] &
                V1 >= k,]
        num <- nrow(predictions)
        if (num > 0) {
            for (i in 1:min(5, num)) {
                if (!predictions$N3[i] %in% candidates) {
                    predicted <- c(tokens[(len - 1):len], predictions$N3[i])
                    candidates[dict[predictions$N3[i]]] <- getCondProb(nGrams, predicted, n=n)
                }
            }
        }
    }

    if (n >= 2 && len >= 1) {
        predictions <- nGrams[[2]][N1 == tokens[len] & V1 >= k,]
        num <- nrow(predictions)
        if (num > 0) {
            for (i in 1:min(5, num)) {
                if (!predictions$N2[i] %in% candidates) {
                    predicted <- c(tokens[len], predictions$N2[i])
                    candidates[dict[predictions$N2[i]]] <- getCondProb(nGrams, predicted, n=n)
                }
            }
        }
    }

    return(candidates)
}

