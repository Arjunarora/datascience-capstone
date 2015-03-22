getCondProb <- function(nGrams, input) {
    tokens <- tokenize(input)
    # In my implementation, the last 3 are NAs
    len <- length(tokens) - 3

    if (len < 1) {
        message("No input")
        return(NULL)
    }

    prob <- 0.0
    if (len >= 4) {
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

    if (len >= 3) {
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
            return (0.4 * count / base)
        }
    }


    if (len >= 2) {
         hit <- nGrams[[2]][N1 == tokens[len - 1] & N2 == tokens[len],]
         if (nrow(hit) > 0) count = hit$V1 else count = 0

         if (count > 0) {
             base <- nGrams[[2]][N1 == tokens[len - 1], sum(V1)]
             message("2-gram")
             return (0.4 * 0.4 * count / base)
         }
    }

    message("1-gram")
    hit <- nGrams[[1]][N1 == tokens[len],]
    if (nrow(hit) > 0) count = hit$V1 else count = 0
    return (0.4 * 0.4 * 0.4 * count / nGrams[[1]][, sum(V1)])
}

predictNext <- function(nGrams, input, k=0) {
    predictions <- NULL
    tokens <- tokenize(tolower(input))
    # In my implementation, the last 3 are NAs
    len <- length(tokens) - 3

    if (len < 1) {
        message("No input")
        return(NULL)
    }

    if (len >= 3) {
        predictions <- nGrams[[4]][
            N1 == tokens[len - 2] &
                N2 == tokens[len - 1] &
                N3 == tokens[len] &
                V1 >= k,]
    }
    if (length(predictions$N4)) {
        message("4-gram")
        message("count: ", predictions$V1[1])
        return (predictions$N4)
    }

    if (len >= 2) {
        predictions <- nGrams[[3]][
            N1 == tokens[len - 1] &
                N2 == tokens[len] &
                V1 >= k,]
    }
    if (length(predictions$N3)) {
        message("3-gram")
        message("count: ", predictions$V1[1])
        return (predictions$N3)
    }

    if (len >= 1) {
        predictions <- nGrams[[3]][N1 == tokens[len] & V1 >= k,]
    }

    message("2-gram")
    message("count: ", predictions$V1[1])
    return (predictions$N2)
}

