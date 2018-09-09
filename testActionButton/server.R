library(shiny)
library(stringi)
library(data.table)

initializeSession <- function () {
    if (file.exists("unigrams.Rds")) {
        unifreq <<- readRDS("unigrams.Rds")
        setkey(unifreq, feature)
    } else {
        cat("File not found")
    }
    if (file.exists("bigrams.Rds")) {
        bifreq <<- readRDS("bigrams.Rds")
        setkey(bifreq, feature)
    } else {
        cat("File not found")
    }
    if (file.exists("trigrams.Rds")) {
        trifreq <<- readRDS("trigrams.Rds")
        setkey(trifreq, feature)
    } else {
        cat("File not found")
    }
    if (file.exists("quadgrams.Rds")) {
        quadfreq <<- readRDS("quadgrams.Rds")
        setkey(quadfreq, feature)
    } else {
        cat("File not found")
    }
    if (file.exists("pentagrams.Rds")) {
        pentafreq <<- readRDS("pentagrams.Rds")
        setkey(pentafreq, feature)
    } else {
        cat("File not found")
    }
}

predictWords <- function(s)
# predict.baseline <- function(s)
{
    gramSize <- 5
    s <- stri_replace_all_regex(s, "[^[:alnum:][:space:]\'\\\\?]", " ")
    #split given phrase and get vector(v) of strings
    sl <- strsplit(s," ")
    v <- unlist(sl)
    l <- length(v)
    predicted <- vector("character", length = 0)
    if (l > 0) {
        if (l  <  gramSize) 
            gramSize <- l+1
        
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
                    if (length(predicted) > 0 & nWords[i] %in% predicted)
                        next
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
    }
    else predicted <- c("\" \"")
    predicted
}

predictNextWords <- function(txtTogrep, gramSize) 
{
    stm <- paste(txtTogrep,collapse="|")
    stm <- paste0("^", stm)
    # print(paste0("searching trigrams for ", stm))
    
    if (gramSize == 5) {
        result <- head(pentafreq[grep(stm,pentafreq$feature),][order(-frequency)],3)
        result <- result[[1]]
    } 
    else if (gramSize == 4) {
        result <- head(quadfreq[grep(stm,quadfreq$feature),][order(-frequency)],3)
        result <- result[[1]]
    } 
    else if (gramSize == 3) {
        result <- head(trifreq[grep(stm,trifreq$feature),][order(-frequency)],3)
        result <- result[[1]]
    } 
    else if (gramSize == 2) {
        result <- head(bifreq[grep(stm,bifreq$feature),][order(-frequency)],3)
        result <- result[[1]]
    } 
    else
    {
        result <- head(unifreq[,][order(-frequency)],3)
        result <- result[[1]]
        # cat("from unigrams")
        # if len(prdct) < 3, add the delta from unigram
    }
    
    pred <- vector(length=0)
    # cat("class of result = ", class(result))
    # print(paste0("class of result = ", result))
    if (length(result) > 0) {
        # if (length(result) > 0) {
        for (i in 1:length(result)) {
            sl <- strsplit(result[i],"_")
            v <- unlist(sl)
            pred[i] <- v[length(v)]
        }
        # if (gramSize == 3) cat("from tri grams")
        # if (gramSize == 2) cat("from bi grams")
    }
    # cat("\nin predicNextWords, predicted = ", pred)
    pred
}

server <- function(input, output) {
    
    initializeSession()
    
    x <- eventReactive(input$go, {
        tolower(input$caption)
    })
    
    output$nextword <- renderPrint({
        # ptm <- proc.time()
        cat(predictWords(x())[1], sep = "\n")
        # print(proc.time()-ptm)
        # paste0(x(), dim(trifreq))
    })
}