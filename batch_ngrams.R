df1 <- data.frame(gram = c("a", "b", "c"), freq = c(2, 4, 9))
df2 <- data.frame(gram = c("d", "b", "e"), freq = c(1, 2, 3))
z <- merge(df1, df2, by = c("gram", "freq"), all = TRUE)
aggregate(freq ~ gram, data = z, FUN = sum)

z <- rbind(df1,)
z <- merge(x = df1, y = df2, by = "gram");z

z <- merge(df1, df2, by = c("gram", "freq"), all = TRUE)
merge(df1, df2, by = c("gram", "freq"))
z

## example of using 'incomparables'
x <- data.frame(k1 = c(NA,NA,3,4,5), k2 = c(1,NA,NA,4,5), data = 1:5)
y <- data.frame(k1 = c(NA,2,NA,4,5), k2 = c(NA,NA,3,4,5), data = 1:5)
merge(x, y, by = c("k1","k2")) # NA's match

