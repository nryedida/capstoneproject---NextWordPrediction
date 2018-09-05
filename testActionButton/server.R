library(shiny)

initializeSession <- function () {
    if (file.exists("bigram.Rds")) {
        bifreq <<- readRDS("bigram.Rds")
    } else {
        cat("File not found")
    }
    if (file.exists("trigram.Rds")) {
        trifreq <<- readRDS("trigram.Rds")
    } else {
        cat("File not found")
    }
}

predictNextWord <- function(s) {
    #assume we only have trigrams
    #get the number of words entered
    #if empty string stop
    #if length is 1, concatenate with _ and search in bigrams
    #if length is 2,  concatenate with _ and search in trigrams
    #if length > 2 take the last two and search in trigrams 
    
    # print(paste("entered string = ", s))
    sl <- strsplit(s," ")
    v <- unlist(sl)
    len <- length(v)
    if(len < 1) stop("Please enter at least a word")
    # if (len == 1) print(paste("word entered = ", v[1]))
    # if (len == 2) {
    #         print("wordss entered are = ")
    #         print(v)
    # }
    # for (i in len:1) {
    #     if (i>1) print(paste(v[i-1], v[i], sep="_"))
    # }
    toMatch <- paste(v[len-1], v[len], sep="_")
    # print(paste0("toMatch is ", toMatch))
    stm <- paste(toMatch,collapse="|")
    stm <- paste0("^", paste0(stm,"_"))
    # print(paste0("searching trigrams for ", stm))
    result <<- head(trifreq[grep(stm,trifreq$feature),1],3)
    # print(paste0("class of result = ", result))
    s <- result$feature
    pred <- vector(length=2)
    for (i in 1:length(s)) {
        sl <- strsplit(s[i],"_")
        v <- unlist(sl)
        pred[i] <- v[length(v)]
    }
    pred <- cat(pred, sep = "\n")
}

server <- function(input, output) {
    
    initializeSession()
    
    x <- eventReactive(input$go, {
        tolower(input$caption)
    })
    
    output$nextword <- renderPrint({
        predictNextWord(x())
        # paste0(x(), dim(trifreq))
    })
}