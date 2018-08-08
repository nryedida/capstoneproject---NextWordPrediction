#Exploratory data analysis of the ENGLISH data set of corpora
# cd C:/ds-cap/final/tmp
# 34  wc -l *.txt
# 35  shuf --help
# 36  date; shuf -n 449644 en_US.blogs.txt -o blogs.txt; date
# 38  date; shuf -n 505121 en_US.news.txt -o news.txt; date
# 41  date; shuf -n 1180074 en_US.twitter.txt -o tweets.txt; date

#read text from the files
require(readtext)
require(quanteda)
library(parallel)
library(doParallel)

########Cluster
library(parallel)
library(doParallel)

# parallel - not useful ####
# cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
# registerDoParallel(cluster)
########Cluster

# read text ####
#sample files
sampleAfile <- function (in_file, out_file, proportion)
{
    #read the lines
    conn <- file(in_file, "r")
    buf <- readLines(conn)
    nlines <- length(buf) * proportion
    
    conn
}

# read text ####
# mytxt1 <- readtext("en_US.twitter.txt")
mytxt1 <- readtext("tweets.txt")
mycorpustwitter <- corpus(mytxt1)
rm(mytxt1)

# mytxt2 <- readtext("en_US.blogs.txt")
mytxt2 <- readtext("blogs.txt")
mycorpusblogs <- corpus(mytxt2)
rm(mytxt2)

# mytxt3 <- readtext("en_US.news.txt")
mytxt3 <- readtext("news.txt")
mycorpusnews <- corpus(mytxt3)
rm(mytxt3)

# merge corpuses ####
system.time(mycorpus <- mycorpustwitter + mycorpusblogs + mycorpusnews)
# nsentence(mycorpus)
rm(mycorpustwitter, mycorpusblogs, mycorpusnews)

texts(mycorpus) <- iconv(texts(mycorpus), "latin1", "ASCII", sub="")

system.time(bigrams <- tokens_ngrams(tokens(mycorpus, remove_symbols = TRUE, remove_url = TRUE), n = 2L))

# build dfm ####
# quanteda_options(threads = 2)
system.time(mydfm <- dfm(mycorpus, remove = stopwords("english"), remove_punct = TRUE , stem = TRUE, verbose = TRUE))

# system.time(mydfm <- dfm(mycorpus, remove = stopwords("english"), remove_punct = TRUE , verbose = TRUE))

# stopCluster(cluster)
# registerDoSEQ()

topfeatures(mydfm, 40)
#build and combine the corpus
#From corpus generate single word frequencies
#to get the relationship [CORRELATION ?]

#EXPLORATORY DATA ANALYSIS
# Tasks to accomplish
# 1. Exploratory analysis - perform a thorough exploratory analysis of the data,
# understanding the distribution of words and relationship between the words in the corpora.
# 2. Understand frequencies of words and word pairs - build figures and tables to
# understand variation in the frequencies of words and word pairs in the data.
# Questions to consider
# 1. Some words are more frequent than others - what are the distributions of word
# frequencies?
#     2. What are the frequencies of 2-grams and 3-grams in the dataset?
#     3. How many unique words do you need in a frequency sorted dictionary to cover 50% of all
# word instances in the language? 90%?
#     4. How do you evaluate how many of the words come from foreign languages?
#     5. Can you think of a way to increase the coverage -- identifying words that may not be in the
# corpora or using a smaller number of words in the dictionary to cover the same number of
# phrases?