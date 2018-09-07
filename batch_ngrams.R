######################################################
# require libraries
######################################################
require(readtext)
require(quanteda)
require(stringi)

myfiles <- c("sampleblogs.txt", "sampletweets.txt", "samplenews.txt")
        mytxt <- readtext(myfiles[1])
mycorpus <- corpus(mytxt)

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
df1 <- textstat_frequency(bidfm)
rm(bidfm, my_tokens, mycorpus)
# bifreq <- bifreq[bifreq$frequency > 4]
df1 <- df1[df1$frequency > 0, c(1,2)]

        #~~~~
for (i in 2:length(myfiles)) {
	mytxt <- readtext(myfiles[i])
	mycorpus <- corpus(mytxt)

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
	df2 <- textstat_frequency(bidfm)
	rm(bidfm, my_tokens, mycorpus)
	df2 <- df2[df2$frequency > 0, c(1,2)]

	#line below only works if dataframes are named df1, df2
	df1 <- rbindlist(mget(paste0('df', 1:2)))[, lapply(.SD, sum), by = feature]
}
#         #~~~~
# 
# 
#         #~~~~
# 	    mytxt <- readtext("samplenews.txt")
#         mycorpus <- corpus(mytxt)
#         
#         #convert non-ASCII characters to blanks
#         texts(mycorpus) <- iconv(texts(mycorpus), "latin1", "ASCII", sub="")
#         
#         my_tokens <- tokens(mycorpus, 
#                             remove_symbols = TRUE, 
#                             remove_numbers = TRUE,
#                             remove_separators = TRUE,
#                             remove_twitter = TRUE,
#                             remove_hyphens = TRUE,
#                             remove_url = TRUE,
#                             remove_punct = TRUE
#         )
#         
#         bidfm <- dfm(tokens_ngrams(my_tokens, n = 2L))
# 	df2 <- textstat_frequency(bidfm)
# 	df2 <- df2[df2$frequency > 0, c(1,2)]
#     rm(bidfm, my_tokens, mycorpus)
# 	df1 <- rbindlist(mget(paste0('df', 1:2)))[, lapply(.SD, sum), by = feature]
	df1 <- df1[df1$frequency > 4, c(1,2)]
	#~~~~
# 
# ###############################################################################
# # https://stackoverflow.com/questions/42966535/combine-two-data-frames-and-aggregate
# ###############################################################################
# 
# df1 <- data.frame(gram = c("a", "b", "c"), freq = c(2, 4, 9))
# df2 <- data.frame(gram = c("c", "d", "b"), freq = c(1, 2, 3))
# # df4 <- merge(df1, df2, by = c("gram", "freq"), all = TRUE, sort = TRUE)
# # df4
# # # z <- aggregate(freq ~ gram, data = z, FUN = sum)
# # # z
# # df3 <- data.frame(gram = c("c", "a", "b"), freq = c(3, 1, 2))
# # # df3
# # # attach(df3)
# # # df3 <- df3[order(gram),]
# # # detach(df3)
# # df3
# # # library(data.table)
# df1 <- rbindlist(mget(paste0('df', 1:2)))[, lapply(.SD, sum), by = gram]
# df1
# df2 <- data.frame(gram = c("c", "a", "b"), freq = c(3, 1, 2))
# df1 <- rbindlist(mget(paste0('df', 1:2)))[, lapply(.SD, sum), by = gram]
# df1
# # z <- merge(z, df3, by = c("gram"), all = TRUE, sort = TRUE)	
# ###############################################################################
# 	
# df1 <- data.frame(gram = c("a", "b", "c"), freq = c(2, 4, 9))
# df1
# df2 <- data.frame(gram = c("d", "c", "a"), freq = c(1, 2, 3))
# df2
# z <- merge(df1, df2, by = c("gram", "freq"), all = TRUE, sort = TRUE)
# z
# # z <- aggregate(freq ~ gram, data = z, FUN = sum)
# # z
# df3 <- data.frame(gram = c("b", "c", "a"), freq = c(3, 1, 2))
# df3
# z <- merge(z, df3, by = c("gram", "freq"), all = TRUE, sort = TRUE)
# z
# z <- aggregate(freq ~ gram, data = z, FUN = sum)
# z
# 
# 
# 
# 
# 
# 
# ##############################################################################
# #copied below text from generate ngrams
# ##############################################################################
# 
# # library(ggplot2)
# 
# ######################################################
# # sample file
# ######################################################
# sampleAFile <- function(bigFile, sampleFile, proportion)
# {
#     conn <- file(bigFile, "r")
#     txt <- readLines(conn, warn = FALSE)
#     close(conn)
#     tdf <-  as.data.frame(txt, stringsAsFactors=FALSE)
#     nlines <- length(txt)
#     
#     selection <- ifelse(rbinom(nlines, 1, proportion) == 1, TRUE, FALSE)
#     
#     conn <- file(sampleFile, "w")
#     cat(tdf[selection,], file=conn, sep = "\n")
#     close(conn)
# }
# 
# ######################################################
# ## call sample files
# ######################################################
# # Start the clock!
# ptm <- proc.time()
# sampleAFile("en_US.twitter.txt", "sampletweets.txt", 0.01)
# sampleAFile("en_US.news.txt", "samplenews.txt", 0.01)
# sampleAFile("en_US.blogs.txt", "sampleblogs.txt", 0.01)
# print("time taken to sample files")
# # Stop the clock
# proc.time() - ptm
# 
# ######################################################
# # read text, make corpus and combine corpus
# ######################################################
# 
# # Start the clock!
# ptm <- proc.time()
# 
# 
# ######################################################
# # generate unigrams and save Rds
# ######################################################
# # Start the clock!
# print("starting unigrams")
# ptm <- proc.time()
# 
# unigrams <- tokens_ngrams(my_toke, n = 1L)
# 
# bidfm <- dfm(unigrams)
# 
# unifreq <- textstat_frequency(bidfm)
# rm(unigrams, bidfm)
# # unifreq <- unifreq[unifreq$frequency > 4]
# # unifreq <- unifreq[unifreq$frequency > 4, c(1,2)]
# # unifreq <- unifreq[,c(1,2)]
# 
# #write the unigrams into unigram.Rds
# saveRDS(unifreq, file = "unigram.Rds")
# print("saved unigrams")
# # Stop the clock
# proc.time() - ptm
# 
# ######################################################
# # generate bigrams, trim count < 4 and save Rds
# ######################################################
# # Start the clock!
# print("starting bigrams")
# ptm <- proc.time()
# 
# # bigrams <- tokens_ngrams(my_tokens, n = 2L)
# 
# bidfm <- dfm(tokens_ngrams(my_tokens, n = 2L))
# 
# bifreq <- textstat_frequency(bidfm)
# # rm(bigrams, bidfm)
# rm()
# # bifreq <- bifreq[bifreq$frequency > 4]
# bifreq <- bifreq[bifreq$frequency > 4, c(1,2)]
# # bifreq <- bifreq[,c(1,2)]
# 
# # #write the bigrams into bigram.Rds
# # saveRDS(bifreq, file = "bigram.Rds")
# # print("saved bigrams")
# # # Stop the clock
# proc.time() - ptm
# 
# # bifreq <- readRDS("bigram.Rds")
# 
# ######################################################
# # generate trigrams , trim count < 4 and save Rds
# ######################################################
# # Sys.time() 
# # Start the clock!
# print("starting trigrams")
# ptm <- proc.time()
# 
# trigrams <- tokens_ngrams(tokens(mycorpus, remove_symbols = TRUE, remove_url = TRUE), n = 3L)
# tridfm <- dfm(trigrams, remove_punct = TRUE)
# trifreq <- textstat_frequency(tridfm)
# rm(trigrams, tridfm)
# trifreq <- trifreq[trifreq$frequency > 4, c(1,2)]
# 
# saveRDS(trifreq, file = "trigram.Rds")
# 
# print("saved trigrams")
# # Stop the clock
# proc.time() - ptm
# # Sys.time() 
# 
# print("starting quadragrams")
# ptm <- proc.time()
# 
# ######################################################
# # generate trigrams , trim count < 4 and save Rds
# ######################################################
# quadragrams <- tokens_ngrams(tokens(mycorpus, remove_symbols = TRUE, remove_url = TRUE), n = 4L)
# quadfm <- dfm(quadragrams, remove_punct = TRUE)
# quadfreq <- textstat_frequency(quadfm)
# rm(quadragrams, quadfm)
# quadfreq <- quadfreq[quadfreq$frequency > 4, c(1,2)]
# 
# saveRDS(quadfreq, file = "quadragram.Rds")
# 
# print("saved quadgrams")
# # Stop the clock
# proc.time() - ptm
