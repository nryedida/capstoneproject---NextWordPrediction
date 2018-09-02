#script that will load the n-gram file(s) into data table and provides a function that can predict the next word given a string

#read each of the n-gram file in to an n-gram data frame

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
    print(paste0("toMatch is ", toMatch))
    # print(sv)
    # print(paste("length(sv) = ", length(sv)))
    # print(paste("sv = ", sv))
    # length(strsplit(s," ", fixed = TRUE))
    stm <- paste(toMatch,collapse="|")
    # print(paste0("stm before is ", stm))
    stm <- paste0("^", paste0(stm,"_"))
    print(paste0("searching trigrams for ", stm))
    result <<- head(trifreq[grep(stm,trifreq$feature),1],3)
    print(paste0("class of result = ", result))
    s <- result$feature
    # print(paste0("s= ",s))
    # print(s)
    pred <- vector(length=2)
    for (i in 1:length(s)) {
        sl <- strsplit(s[i],"_")
        v <- unlist(sl)
        # print(paste0("v= ",v[length(v)]))
        pred[i] <- v[length(v)]
        # len <- length(v)
        # for (j in len:len-2) {
        #     print(paste0("j = ", j))
        #     print(v[j])
        # }
    }
    pred
}

# predictNextWord("how are you")
print(predictNextWord("how are to the"))
