predictWords <- function(s)
{
    gramSize <- 3
    #split given phrase and get vector(v) of strings
    sl <- strsplit(s," ")
    v <- unlist(sl)
    l <- length(v)
    if (l  <  gramSize) 
        gramSize <- l+1

    predicted <- vector("character", length = 0)
    nWords <- vector("character", length = 0)
    nPredicted <- 0
    toMatch <- NULL
    repeat {
        #v[l-(gramSize-1)+1] to v[l]
        # cat("gram size", gramSize)
        for (i in (l-(gramSize-1)+1):l) {
            toMatch <- paste(toMatch, paste(v[i], "_", sep = ""), sep="")
            # cat("toMatch = ", toMatch)
        }
        nWords <- predictNextWords(toMatch, gramSize)
    #     #add these words to predicted to the end
        if (length(nWords) > 0) {
            for (i in 1:length(nWords)){
                if (nPredicted < 3) {
                    predicted[nPredicted+1] <- nWords[i]
                    nPredicted <- nPredicted+1
                }
            }
        }
        nWords <- NULL
        # predicted <- c(predicted, nWords)
        if (length(predicted) >= 3){
            # cat("breaking as predicted length>3")
            break;
        }
        else {
            gramSize <- gramSize - 1
            toMatch <- NULL
            # predicted <- c("a", "b", "c")
        }
    }
    predicted
}
# predictWords("how are")

predictNextWords <- function(txtTogrep, gramSize) {
    stm <- paste(txtTogrep,collapse="|")
    stm <- paste0("^", stm)
    # print(paste0("searching trigrams for ", stm))

    if (gramSize == 3) {
        result <- head(trifreq[grep(stm,trifreq$feature),1],3)
    } else if (gramSize == 2) {
        result <- head(bifreq[grep(stm,bifreq$feature),1],3)
    } else
    {
        result <- head(unifreq[,1],3)
        cat("from unigrams")
    # if len(prdct) < 3, add the delta from unigram
    }

    pred <- vector(length=0)

    # print(paste0("class of result = ", result))
    if (length(result) > 0) {
        for (i in 1:length(result)) {
            sl <- strsplit(result[i],"_")
            v <- unlist(sl)
            pred[i] <- v[length(v)]
        }
        if (gramSize == 3) cat("from tri grams")
        if (gramSize == 2) cat("from bi grams")
    }
    # cat("\nin predicNextWords, predicted = ", pred)
    pred
}

# min (l, gramSize)
#if l  == 1 , gramSize <- l
#1

#2
#if (l  <  gramSize), gramSize <- l+1

#3 or more
test <- function() {
    cat("bifreq =", dim(bifreq))
}