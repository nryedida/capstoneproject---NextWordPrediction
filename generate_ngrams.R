require(readtext)
require(quanteda)
require(stringi)
# library(ggplot2)

sampleAFile <- function(bigFile, sampleFile, proportion)
{
    conn <- file(bigFile, "r")
    txt <- readLines(conn, warn = FALSE)
    close(conn)
    tdf <-  as.data.frame(txt, stringsAsFactors=FALSE)
    nlines <- length(txt)
    
    selection <- ifelse(rbinom(nlines, 1, proportion) == 1, TRUE, FALSE)
    
    conn <- file(sampleFile, "w")
    cat(tdf[selection,], file=conn, sep = "\n")
    close(conn)
}

Sys.time()
sampleAFile("en_US.twitter.txt", "sampletweets.txt", 0.1)
sampleAFile("en_US.news.txt", "samplenews.txt", 0.1)
sampleAFile("en_US.blogs.txt", "sampleblogs.txt", 0.1)

mytxt1 <- readtext("sampletweets.txt")
mycorpustwitter <- corpus(mytxt1)
rm(mytxt1)

mytxt2 <- readtext("sampleblogs.txt")
mycorpusblogs <- corpus(mytxt2)
rm(mytxt2)

mytxt3 <- readtext("samplenews.txt")
mycorpusnews <- corpus(mytxt3)
rm(mytxt3)

mycorpus <- mycorpustwitter + mycorpusblogs + mycorpusnews
rm(mycorpustwitter, mycorpusblogs, mycorpusnews)

#convert non-ASCII characters to blanks
texts(mycorpus) <- iconv(texts(mycorpus), "latin1", "ASCII", sub="")
texts(mycorpus) <- stri_replace_all_regex(texts(mycorpus), "[^[:alnum:][:space:]\'\\\\?]", " ")

bigrams <- tokens_ngrams(tokens(mycorpus, remove_symbols = TRUE, remove_url = TRUE), n = 2L)

bidfm <- dfm(bigrams, remove_punct = TRUE)
bifreq <- textstat_frequency(bidfm)
rm(bigrams, bidfm)
bifreq <- bifreq[bifreq$frequency > 4]

#write the bigrams into bigram.Rds
saveRDS(bifreq, file = "bigram.Rds")
bifreq <- readRDS("bigram.Rds")

Sys.time() 
trigrams <- tokens_ngrams(tokens(mycorpus, remove_symbols = TRUE, remove_url = TRUE), n = 3L)
tridfm <- dfm(trigrams, remove_punct = TRUE)
trifreq <- textstat_frequency(tridfm)
rm(trigrams, tridfm)
trifreq <- trifreq[trifreq$frequency > 4]
saveRDS(trifreq, file = "trigram.Rds")
Sys.time() 


#trifreq

toMatch <- c("the_players", "the_defense", "the_crowd", "the_referees")

adf <- data.frame(c("a","b","c"), 1:3)
names(adf) <- c("name", "id")
adf <- adf[1:2,]
adf <- adf[adf$id>2,]
adf
?as.data.frame

?strsplit
strsplit(paste(c("ab","cd","ef"),collapse=""), NULL)
length(strsplit(paste(c("ab","cd","ef"))," " ))
s <- c("how are you")
sl <- strsplit(s, " ")
sl[[1]]
sl[1]
sl[2]
sl
unlist(sl)[1]
v <- unlist(sl)
length(unlist(sl))

l.ex <- list(a = list(1:5, LETTERS[1:5]), b = "Z", c = NA)
unlist(l.ex, recursive = FALSE)
unlist(l.ex, recursive = TRUE)


sentences <- c("Jane saw a cat", "Jane sat down")

word(sentences, 1)
word(sentences, 2)
word(sentences, -1)
word(sentences, 2, -1)

mystr <- stri_replace_all_regex("Challenge_(_", "[^[:alnum:][:space:]\'\\\\?]", " ")
mystr
