######################################################
# require libraries
######################################################
require(readtext)
require(quanteda)
require(stringi)

# mytxt <- readtext("sampletweets.txt")
mycorpus <- corpus(readtext("sampletweets.txt"))

######################################################
# clean up non ascii characters - ? still left - clear
######################################################
#convert non-ASCII characters to blanks
texts(mycorpus) <- iconv(texts(mycorpus), "latin1", "ASCII", sub="")

my_tokens <- tokens(mycorpus, 
                    remove_symbols = TRUE, 
                    remove_numbers = TRUE,
                    remove_separators = TRUE,
                    remove_twitter = TRUE,
                    remove_hyphens = TRUE,
                    remove_url = TRUE,
                    remove_punct = TRUE
)

bidfm <- dfm(tokens_ngrams(my_tokens, n = 2L))
merged_bifreq <- textstat_frequency(bidfm)
rm(bidfm, my_tokens, mycorpus)
# bifreq <- bifreq[bifreq$frequency > 4]
merged_bifreq <- bifreq[bifreq$frequency > 4, c(1,2)]

        #~~~~
        mycorpus <- corpus(readtext("sampleblogs.txt"))
        
        #convert non-ASCII characters to blanks
        texts(mycorpus) <- iconv(texts(mycorpus), "latin1", "ASCII", sub="")
        
        my_tokens <- tokens(mycorpus, 
                            remove_symbols = TRUE, 
                            remove_numbers = TRUE,
                            remove_separators = TRUE,
                            remove_twitter = TRUE,
                            remove_hyphens = TRUE,
                            remove_url = TRUE,
                            remove_punct = TRUE
        )
        
        bidfm <- dfm(tokens_ngrams(my_tokens, n = 2L))
	tmpdf <- textstat_frequency(bidfm)
	tmpdf <- tmpdf[tmpdf$frequency > 4, c(1,2)]
	merged_bifreq <- merge(merged_bifreq, tmpdf, by = c("feature", "frequency"), all = TRUE)
rm(bidfm, my_tokens, mycorpus, tmpdf)
	merged_bifreq <- aggregate(frequency ~ feature, data = merged_bifreq, FUN = sum)
        #~~~~


        #~~~~
        mycorpus <- corpus(readtext("samplenews.txt"))
        
        #convert non-ASCII characters to blanks
        texts(mycorpus) <- iconv(texts(mycorpus), "latin1", "ASCII", sub="")
        
        my_tokens <- tokens(mycorpus, 
                            remove_symbols = TRUE, 
                            remove_numbers = TRUE,
                            remove_separators = TRUE,
                            remove_twitter = TRUE,
                            remove_hyphens = TRUE,
                            remove_url = TRUE,
                            remove_punct = TRUE
        )
        
        bidfm <- dfm(tokens_ngrams(my_tokens, n = 2L))
	tmpdf <- textstat_frequency(bidfm)
	tmpdf <- tmpdf[tmpdf$frequency > 4, c(1,2)]
	merged_bifreq <- merge(merged_bifreq, tmpdf, by = c("feature", "frequency"), all = TRUE)
rm(bidfm, my_tokens, mycorpus, tmpdf)
	merged_bifreq <- aggregate(frequency ~ feature, data = merged_bifreq, FUN = sum)
        #~~~~

###############################################################################
# https://stackoverflow.com/questions/42966535/combine-two-data-frames-and-aggregate
###############################################################################

df1 <- data.frame(gram = c("a", "b", "c"), freq = c(2, 4, 9))
df2 <- data.frame(gram = c("c", "d", "b"), freq = c(1, 2, 3))
# df4 <- merge(df1, df2, by = c("gram", "freq"), all = TRUE, sort = TRUE)
# df4
# # z <- aggregate(freq ~ gram, data = z, FUN = sum)
# # z
# df3 <- data.frame(gram = c("c", "a", "b"), freq = c(3, 1, 2))
# # df3
# # attach(df3)
# # df3 <- df3[order(gram),]
# # detach(df3)
# df3
# # library(data.table)
df1 <- rbindlist(mget(paste0('df', 1:2)))[, lapply(.SD, sum), by = gram]
df1
df2 <- data.frame(gram = c("c", "a", "b"), freq = c(3, 1, 2))
df1 <- rbindlist(mget(paste0('df', 1:2)))[, lapply(.SD, sum), by = gram]
df1
# z <- merge(z, df3, by = c("gram"), all = TRUE, sort = TRUE)	
###############################################################################
	
df1 <- data.frame(gram = c("a", "b", "c"), freq = c(2, 4, 9))
df1
df2 <- data.frame(gram = c("d", "c", "a"), freq = c(1, 2, 3))
df2
z <- merge(df1, df2, by = c("gram", "freq"), all = TRUE, sort = TRUE)
z
# z <- aggregate(freq ~ gram, data = z, FUN = sum)
# z
df3 <- data.frame(gram = c("b", "c", "a"), freq = c(3, 1, 2))
df3
z <- merge(z, df3, by = c("gram", "freq"), all = TRUE, sort = TRUE)
z
z <- aggregate(freq ~ gram, data = z, FUN = sum)
z






##############################################################################
#copied below text from generate ngrams
##############################################################################

# library(ggplot2)

######################################################
# sample file
######################################################
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

######################################################
## call sample files
######################################################
# Start the clock!
ptm <- proc.time()
sampleAFile("en_US.twitter.txt", "sampletweets.txt", 0.01)
sampleAFile("en_US.news.txt", "samplenews.txt", 0.01)
sampleAFile("en_US.blogs.txt", "sampleblogs.txt", 0.01)
print("time taken to sample files")
# Stop the clock
proc.time() - ptm

######################################################
# read text, make corpus and combine corpus
######################################################

# Start the clock!
ptm <- proc.time()


######################################################
# generate unigrams and save Rds
######################################################
# Start the clock!
print("starting unigrams")
ptm <- proc.time()

unigrams <- tokens_ngrams(my_toke, n = 1L)

bidfm <- dfm(unigrams)

unifreq <- textstat_frequency(bidfm)
rm(unigrams, bidfm)
# unifreq <- unifreq[unifreq$frequency > 4]
# unifreq <- unifreq[unifreq$frequency > 4, c(1,2)]
# unifreq <- unifreq[,c(1,2)]

#write the unigrams into unigram.Rds
saveRDS(unifreq, file = "unigram.Rds")
print("saved unigrams")
# Stop the clock
proc.time() - ptm

######################################################
# generate bigrams, trim count < 4 and save Rds
######################################################
# Start the clock!
print("starting bigrams")
ptm <- proc.time()

# bigrams <- tokens_ngrams(my_tokens, n = 2L)

bidfm <- dfm(tokens_ngrams(my_tokens, n = 2L))

bifreq <- textstat_frequency(bidfm)
# rm(bigrams, bidfm)
rm()
# bifreq <- bifreq[bifreq$frequency > 4]
bifreq <- bifreq[bifreq$frequency > 4, c(1,2)]
# bifreq <- bifreq[,c(1,2)]

# #write the bigrams into bigram.Rds
# saveRDS(bifreq, file = "bigram.Rds")
# print("saved bigrams")
# # Stop the clock
proc.time() - ptm

# bifreq <- readRDS("bigram.Rds")

######################################################
# generate trigrams , trim count < 4 and save Rds
######################################################
# Sys.time() 
# Start the clock!
print("starting trigrams")
ptm <- proc.time()

trigrams <- tokens_ngrams(tokens(mycorpus, remove_symbols = TRUE, remove_url = TRUE), n = 3L)
tridfm <- dfm(trigrams, remove_punct = TRUE)
trifreq <- textstat_frequency(tridfm)
rm(trigrams, tridfm)
trifreq <- trifreq[trifreq$frequency > 4, c(1,2)]

saveRDS(trifreq, file = "trigram.Rds")

print("saved trigrams")
# Stop the clock
proc.time() - ptm
# Sys.time() 

print("starting quadragrams")
ptm <- proc.time()

######################################################
# generate trigrams , trim count < 4 and save Rds
######################################################
quadragrams <- tokens_ngrams(tokens(mycorpus, remove_symbols = TRUE, remove_url = TRUE), n = 4L)
quadfm <- dfm(quadragrams, remove_punct = TRUE)
quadfreq <- textstat_frequency(quadfm)
rm(quadragrams, quadfm)
quadfreq <- quadfreq[quadfreq$frequency > 4, c(1,2)]

saveRDS(quadfreq, file = "quadragram.Rds")

print("saved quadgrams")
# Stop the clock
proc.time() - ptm


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

##############################################################################
# z <- rbind(df1,)
# z <- merge(x = df1, y = df2, by = "gram");z
# 
# z <- merge(df1, df2, by = c("gram", "freq"), all = TRUE)
# merge(df1, df2, by = c("gram", "freq"))
# z
# 
# ## example of using 'incomparables'
# x <- data.frame(k1 = c(NA,NA,3,4,5), k2 = c(1,NA,NA,4,5), data = 1:5)
# y <- data.frame(k1 = c(NA,2,NA,4,5), k2 = c(NA,NA,3,4,5), data = 1:5)
# merge(x, y, by = c("k1","k2")) # NA's match

