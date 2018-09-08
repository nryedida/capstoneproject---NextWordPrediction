######################################################
# require libraries
######################################################
require(readtext)
require(quanteda)
require(stringi)
require(data.table)

myfiles <- c("baa","bab","bac","bad","bae","baf","bag","bah","bai","baj","taa","tab","tac","tad","tae","taf","tag","tah","tai","taj")
#myfiles <- c("sampleblogs.txt")
ngramsVector <- c(1L, 2L, 3L)

for (gramSize in 1:length(ngramsVector))
{
    ptm <- proc.time()
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

	bidfm <- dfm(tokens_ngrams(my_tokens, n = ngramsVector[gramSize]), verbose = TRUE)
	df1 <- textstat_frequency(bidfm)
	rm(bidfm, my_tokens, mycorpus)
	# bifreq <- bifreq[bifreq$frequency > 4]
	df1 <- df1[df1$frequency > 0, c(1,2)]

    if (length(myfiles) > 1)
    {
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
    
    		bidfm <- dfm(tokens_ngrams(my_tokens, n = ngramsVector[gramSize]), verbose = TRUE)
    		# bidfm <- dfm(tokens_ngrams(my_tokens, n = 2L))
    		df2 <- textstat_frequency(bidfm)
    		rm(bidfm, my_tokens, mycorpus)
    		df2 <- df2[df2$frequency > 0, c(1,2)]
    
    		#line below only works if dataframes are named df1, df2
    		df1 <- rbindlist(mget(paste0('df', 1:2)))[, lapply(.SD, sum), by = feature]
		}
    }
	df1 <- df1[df1$frequency > 1, c(1,2)]
	if (gramSize == 1L)
		saveRDS(df1, file = "unigrams.Rds")
	else if (gramSize == 2L)
		saveRDS(df1, file = "bigrams.Rds")
	else if (gramSize == 3L)
		saveRDS(df1, file = "trigrams.Rds")
	cat("time taken for gramSize = ", gramSize)
	cat(proc.time()-ptm)
}