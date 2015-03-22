library(RCurl)
library(utils)

dataUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/"
dataZip <- "Coursera-SwiftKey.zip"
dataDir <- "final"

# If it hasn't, download and unzip the data
if (!file.exists(dataDir)) {
    if (!file.exists(dataZip)) {
        # Download the data
        download.file(url=paste(dataUrl, dataZip, sep="/"),
                      destfile=dataZip,
                      method="wget")
    }
    unzip(dataZip, dataDir)
}

# Download the list of profane words in each language
profaneWordsUrl <- paste(
    "https://raw.githubusercontent.com/shutterstock/",
    "List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/",
    sep="")

downloadProfaneWordList <- function(lang, prefix="profane_", ext=".txt") {
    destFile <- paste(prefix, lang, ext, sep="")
    if (file.exists(destFile)) {
        message(paste("File", destFile, "exists", sep=" "))
        return()
    }
    url <- paste(profaneWordsUrl, lang, sep="")
    download.file(url, destfile=destFile, method="wget")
}

downloadProfaneWordList("en")
downloadProfaneWordList("de")
downloadProfaneWordList("ru")
downloadProfaneWordList("fi")

