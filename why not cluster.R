
b <- data.frame(scale(df[,c(4,7,8)]))
c <- dist(b)  ##,method=""            ##default : Euclidean
hc <- hclust(c) ##,method=""          ##default : Complete
plot(hc)

km <- kmeans(c,3,iter.max = 10)

df$kmeans <- km$cluster

View(df)


table(H = df$cluster,K= df$kmeans)
