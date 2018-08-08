#quanteda sample code test####
summary(my_corpus)
typeof(my_corpus)
names(my_corpus)
my_corpus$tokens[[1]]
nchar(my_corpus$documents$texts[1])
dim(my_corpus$documents$texts)
tvar <- my_corpus$documents$texts[1]

#do the dfm operations on the first data set in quanteda quick start guide####
my_dfm <- dfm(my_corpus)
my_dfm[, 1:2]
my_dfm <- dfm(my_corpus, remove = stopwords("english"), remove_punct = TRUE)
my_dfm
topfeatures(my_dfm, 20)

# test scan and pushback ####
zz <- textConnection(LETTERS)
txt <- readLines(zz, 2)
txt
# paste0("txt = ",txt)
txt <- scan(zz, "", 4)
txt
# paste0("txt = ",txt)
pushBack(c("aa", "bb"), zz)
txt <- scan(zz, "", 4)
txt
# paste0("txt = ",txt)
close(zz)

#rbinom test####
bv1 <- rbinom(1, 10, 0.2)
bv1
bv2 <- rbinom(10, 1, 0.2)
bv2
rbinom(10, 1, 0.9)

# read lines attempt ####
# conn <- file("en_US.twitter.txt", "r")
conn <- file("tweets.txt", "r")
txt <- readLines(conn)
close(conn)
tdf <-  as.data.frame(txt, stringsAsFactors=FALSE)
# row.names = c(1:length(txt))
# tdf <- as.data.frame(txt, row.names = NULL)
# tdf <- as.data.frame(as.list(txt))

# data.frame(lapply(txt, type.convert), stringsAsFactors=FALSE)

# nlines <- round(length(txt) * 0.2)
nlines <- length(txt)

selection <- ifelse(rbinom(nlines, 1, 0.2) == 1, TRUE, FALSE)

conn <- file("tweetsample.txt", "w")
cat(tdf[selection,], file=conn, sep = "\n")
close(conn)

sampledf <- as.data.frame(tdf[selection,], stringsAsFactors=FALSE)
names(sampledf) = "text"
# summary(corpus(as.character(sampledf$text)))
# mycorpus <- corpus(sampledf)
# summary(mycorpus)

paste0("nlines of txt is ", length(txt))
close(conn)
rm(conn, txt)


# plot - hist ####
library(ggplot2)
features_dfm <- textstat_frequency(mydfm, n = 20)
features_dfm$feature <- with(features_dfm, reorder(feature, -frequency))
g <- ggplot(features_dfm, aes(x=feature, y = frequency)) + geom_bar(stat='identity',size=1) + theme(axis.text.x=element_text(angle=60))
# + coord_flip()
g

# dot plot ####
features_dfm_inaug <- textstat_frequency(mydfm, n = 40)
# Sort by reverse frequency order
features_dfm_inaug$feature <- with(features_dfm_inaug, reorder(feature, -frequency))
ggplot(features_dfm_inaug, aes(x = feature, y = frequency)) +
    geom_point() + 
    # theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip()

quanteda_options("threads")
options()
