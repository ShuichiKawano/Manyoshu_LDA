######################################################
# On-in Latent Dirichlet Allocation
# Copyright (C) Kohei Yoshikawa and Shuichi Kawano, Graduate School of Informatics and Engineering, The University of Electro-Communications, Japan. 

######################################################
# Load library 
######################################################
library(topicmodels)
#library(ldatuning)

######################################################
# Load dataset
######################################################
load("./data/master.id.author.Rdata")
Data <- read.table("./data/dataset.txt", header=T, sep="\t", row.names=1)
colnames_ <- colnames(Data)
Data[is.na(Data)] <- 0 

######################################################
# LDA
######################################################
topic_num <- 9 # the number of topics
filename <- "result"
#lda <- LDA(Data, k=topic_num, method="Gibbs", control= list(burnin=200000, iter = 1000000))
lda <- LDA(Data, k=topic_num, method="Gibbs", control= list(burnin=200, iter = 1000))
lda_inf <- posterior(lda, Data)

######################################################
# calculate On-in distribution in each topic
Onin_dist <- NULL
for(i in 1:topic_num){
  sorted_terms <- sort(terms(lda_inf)[i,], decreasing=TRUE)
  Onin_dist <- cbind(Onin_dist, names(sorted_terms), sorted_terms)
}
terms_filename <- sprintf(paste0(filename, "_terms_%d.txt"), topic_num)
write.table(Onin_dist, file=terms_filename, sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding="CP932")

######################################################
# Write On-in distribution in each topic
topics <- cbind(master.id.author["id"], master.id.author["author"], lda_inf$topics)
topics_filename <- sprintf(paste0(filename,"_topics_%d.txt"), topic_num)
write.table(topics, file=topics_filename, sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding="UTF-8")
