# Separating Words Algorithms

library(foreign)
setwd("C:/Users/Steve/Desktop/SoDA/Governors/Governor_Text_Modeling/Gov_Models")
uni_raw<- read.csv("unigrams.csv")
tri_raw<- read.csv("trigrams.csv")

#convert frequencies to rates
#take off id label and filenames
uni<- subset(uni_raw[,-c(1,2)])
tri<- subset(tri_raw[,-c(1,2)])

#calculate rates. 
for (i in 1:nrow(uni)){
    s<- sum(uni[i,])
    uni[i,]<- uni[i,]/s
}


#much faster, but returns a different type of data structure which requires a little more work to use
#norm<- function(x){
#    x<- x/sum(x)
#    return(x)
#}

#m<- t(apply(uni,1, function(x) norm(x)))

#add id back on
uni<-cbind(uni_raw$file, uni_raw$id, uni)
colnames(uni)[1:2]<- c("file", "id")

gov.pid <- read.csv('handcode_gov_party.csv')
colnames(gov.pid)[which(names(gov.pid) == "Governor")] <- "id"
gov.pid <- gov.pid[,3:5]
gov.pid <- gov.pid[,-2]
gov.pid <- unique(gov.pid[,1:2])
uni.pid <- merge(uni, gov.pid, by = c('id')) # should merge by year as well
uni <- uni.pid[which(uni.pid$party == 'democrat' | uni.pid$party == 'republican'),]
uni$party <- as.character(uni$party)




#same for trigrams
for (j in 1:nrow(tri)){
    s<- sum(tri[i,])
    tri[i,]<- tri[i,]/s
}
tri<-cbind(tri_raw$file, tri_raw$id, tri)
colnames(tri)[1:2]<- c("file","id")

tri.pid <- merge(tri, gov.pid, by = c('id'))
tri <- tri.pid[which(tri.pid$party == 'democrat' | tri.pid$party == 'republican'),]
tri.pid$party <- as.character(tri.pid$party)

#we will need these for our discriminant methods
#number of sessions and shelby docs
GOPN <- table(uni$party)[1]
DemN <- table(uni$id)[2]
DemNT <- table(tri$party)[1]
GOPNT <- table(tri$party)[4]

#unigrams
muGOPU <- colMeans(uni[uni$party == "republican",-c(1,2,1003)], na.rm=T)
muDemU <- colMeans(uni[uni$party == "democrat",-c(1,2,1003)], na.rm=T)
varGopU <- apply(uni[uni$party == "republican", -c(1,2,1003)], 2, var)
varDemU <- apply(uni[uni$party == "democrat", -c(1,2,1003)], 2, var)




#trigrams
muDemT <- colMeans(tri[tri$party=="democrat",-c(1,2,503)], na.rm=T)
muGOPT<- colMeans(tri[tri$party=="republican",-c(1,2,503)], na.rm=T)
varDemT<- apply(tri[tri$party=="democrat", -c(1,2,503)], 2, var)
varGOPT<- apply(tri[tri$party=="republican", -c(1,2,503)], 2, var)

#unigrams
ldWeightsU <- (muGOPU - muDemU) / (varGopU + varDemU)

#trigrams
ldWeightsT <- (muGOPT - muDemT) / (varGOPT + varDemT)

#trim weights - not necessary here
#ldWeightsU[abs(ldWeightsU)< 0.025]<- 0
#ldWeightsT[abs(ldWeightsT)< 0.025]<- 0

#ldWeightsT!="-Inf"]
ldWeightsU <- sort(ldWeightsU)
ldWeightsT <- sort(ldWeightsT)

#get 20 most discriminating words
LDmostGOPU <- head(ldWeightsU, 10)
LDmostDemU <- tail(ldWeightsU, 10)
LDmostGOPT <- head(ldWeightsT, 10) 
LDmostDemT <- tail(ldWeightsT, 10)


#plot
LDdiscrimU <- c(LDmostGOPU, LDmostDemU)
LDdiscrimT <- c(LDmostGOPT, LDmostDemT)

#unigrams
numU <- muGOPU - muDemU
denomU <- sqrt((varGopU/GOPN) + (varDemU/DemN))
stdDiffU <- numU / denomU

#trigrams
numT <- muGOPT - muDemT
denomT <- sqrt((varDemT/DemNT) + varGOPT/GOPNT)
stdDiffT <- numT/denomT

sdWeightsU <- sort(stdDiffU[stdDiffU != "-Inf"])
sdWeightsT <- sort(stdDiffT[stdDiffT !="-Inf"])

SDmostGOPU <- head(sdWeightsU,10)
SDmostDemU <- tail(sdWeightsU, 10)
SDmostGOPT <- head(sdWeightsT, 10) # might need to reverse
SDmostDemT <- tail(sdWeightsT, 10)

SDdiscrimU <- c(SDmostGOPU, SDmostDemU)
SDdiscrimT <- c(SDmostGOPT, SDmostDemT)

# Bring in partisanship to uni_raw dataset
uni_raw <- merge(uni_raw, gov.pid, by = c('id'))
uni_raw <- uni_raw[which(uni_raw$party == 'democrat' | uni_raw$party == 'republican'),]
uni_raw$party <- as.character(uni_raw$party)

#unigrams
totDemU<- sum(colSums(uni_raw[uni_raw$party == "democrat", -c(1,2,1003)]))
piDemU<- (colSums(uni_raw[uni_raw$party == "democrat", -c(1,2,1003)]) + 1)/(totDemU + (ncol(uni_raw)-1))
totGOPU<- sum(colSums(uni_raw[uni_raw$party == "republican", -c(1,2,1003)])) 
piGOPU<- (colSums(uni_raw[uni_raw$party == "republican", -c(1,2,1003)]) + 1)/(totGOPU + (ncol(uni_raw)-1))

logOddsRatioU<- log(piDemU/ (1- piDemU)) - log(piGOPU/ (1-piGOPU))
varLogOddsU<- (1/(colSums(uni_raw[uni_raw$party=="democrat", -c(1,2,1003)]) + 1)) + (1/(colSums(uni_raw[uni_raw$party=="republican", -c(1,2,1003)]) + 1)) 

stdLogOddsU <- logOddsRatioU/ sqrt(varLogOddsU)

# Bring in partisanship to tri_raw dataset
tri_raw <- merge(tri_raw, gov.pid, by = c('id'))
tri_raw <- tri_raw[which(tri_raw$party == 'democrat' | tri_raw$party == 'republican'),]
tri_raw$party <- as.character(tri_raw$party)

#trigrams
totDemT<- sum(colSums(tri_raw[tri_raw$party=="democrat", -c(1,2,503)]))
piDemT<- (colSums(tri_raw[tri_raw$party=="democrat", -c(1,2,503)]) + 1)/(totDemT + (ncol(tri_raw)-1))
totGOPT<- sum(colSums(tri_raw[tri_raw$party=="republican", -c(1,2,503)])) 
piGOPT<- (colSums(tri_raw[tri_raw$party=="republican", -c(1,2,503)]) + 1)/(totGOPT + (ncol(tri_raw)-1))

logOddsRatioT<- log(piDemT/ (1- piDemT)) - log(piGOPT/ (1-piGOPT))
varLogOddsT<- (1/(colSums(tri_raw[tri_raw$party=="democrat", -c(1,2,503)]) + 1)) + (1/(colSums(tri_raw[tri_raw$party=="republican", -c(1,2,503)]) + 1)) 

stdLogOddsT<- logOddsRatioT/ sqrt(varLogOddsT)

sloWeightsU <- sort(stdLogOddsU)
sloWeightsT <- sort(stdLogOddsT)

sloGOPMostU <- head(sloWeightsU,10) #this might have to be reversed
sloDemMostU <- tail(sloWeightsU, 10)
sloGOPMostT <- head(sloWeightsT, 10)
sloDemMostT <- tail(sloWeightsT, 10)

sloDiscrimU <- c(sloGOPMostU, sloDemMostU)
sloDiscrimT <- c(sloGOPMostT, sloDemMostT)



### Plotting Unigrams
index <- seq(1, 20, 1)

# Linear Discriminant Analysis
plot(LDdiscrimU, index, pch="", xlab="weight", ylab="", yaxt="n", main="Most discriminating words\n Linear Discriminant Analysis")
text(LDdiscrimU, index , label=names(LDdiscrimU), cex=.7)
axis(2, at=c(2, 18), labels=c("democrat", "republican"), tcl=0, las=1, cex.axis=.9)

#plot(LDdiscrimU, pch="", xaxt="n", xlab="", ylab="Weight", main="Most discriminating words\n Linear Discriminant Analysis")
#text(LDdiscrimU, label=names(LDdiscrimU), cex=.7)

# Standard Mean Difference
plot(SDdiscrimU, index, pch="", xlab="weight", ylab="", yaxt="n", main="Most discriminating words\n Standard Mean Difference")
text(SDdiscrimU, index, label=names(SDdiscrimU), cex=.7)
axis(2, at=c(2, 18), labels=c("democrat", "republican"), tcl=0, las=1, cex.axis=.9)

# Standardized Log Odds Ratio
plot(sloDiscrimU, index, pch="", xlab="weight", ylab="", yaxt="n", main="Most discriminating words\n Standardized Log Odds Ratio")
text(sloDiscrimU, index, label=names(sloDiscrimU), cex=.7)
axis(2, at=c(2, 18), labels=c("democrat", "republican"), tcl=0, las=1, cex.axis=.9)


### Plotting Trigrams

# Linear Discriminant Analysis
plot(LDdiscrimT, index, pch="", xlab="weight", ylab="", yaxt="n", main="Most discriminating words\n Linear Discriminant Analysis")
text(LDdiscrimT, index, label=names(LDdiscrimT), cex=.7)
axis(2, at=c(2, 18), labels=c("democrat", "republican"), tcl=0, las=1, cex.axis=.9)

# Standard Mean Difference
plot(SDdiscrimT, index, pch="", xlab="weight", ylab="", yaxt="n", main="Most discriminating words\n Standard Mean Difference")
text(SDdiscrimT, index, label=names(SDdiscrimT), cex=.7)
axis(2, at=c(2, 18), labels=c("democrat", "republican"), tcl=0, las=1, cex.axis=.9)

# Standardized Log Odds Ratio
# # This is wrong; GOP and dem are flipped in the calc. -> GOP should be in numerator
plot(sloDiscrimT, index, pch="",xlab="weight", ylab="", yaxt="n", main="Most discriminating words\n Standardized Log Odds Ratio")
text(sloDiscrimT, index, label=names(sloDiscrimT), cex=.7)
axis(2, at=c(2, 18), labels=c("democrat", "republican"), tcl=0, las=1, cex.axis=.9)


### PCA of State of the State Addresses
#
library(stats)
PCA.all <- prcomp(uni_raw[, -c(1,2,1003)], scale = T) #uni_raw$party=="democrat"

scoresGov <- PCA.all$x
eigen(scoresGov)
plot(PCA.all$x, eigen(scoresGov))
plot(PCA.all$x[,1:2])
biplot(PCA.all, scale = 0)

# Combine first two components with unigram dataframe
uni.PCA <- data.frame(uni, PCA.all$x)
uni.PCA <- uni.PCA[,1:1005]
uni.PCA1 <- uni.PCA[1003:1005]
uni.PCA1$party <- ifelse(uni.PCA1$party == 'republican', 1, 0)
class(uni.PCA1$party)

#SVM
prop <- 0.50
set.seed(24519)
index <- sample(1:nrow(uni.PCA1), round(prop*nrow(uni.PCA1)))
train.gov <- uni.PCA1[index,]
test.gov <- uni.PCA1[-index,]
install.packages('svm')
library(e1071)
svm.model <- svm(party~., data = train.gov)
summary(svm.model)
pred <- predict(svm.model, test.gov[,2:3])
table(pred, test.gov[,1])


#udv <- svd(uni_raw[, -c(1,2,1003)]); names(udv)
#plot(udv$d)
#plot(udv$d, xlim = c(0,10))
#pairs(udv$u[,1:4]); cor(udv$u[,1:4])


# Variance-Covariance Matrix of DT matrix
S <- cov(uni_raw[, -c(1,2,1003)])

# Total Variation of data
sum(diag(S))

# Compute the eigenvalues and corresponding eigenvectors of var-cov matrix
s.eigen <- eigen(S)
s.eigen

# Eigenvectors represent the principal components of S. The eigenvalues of S are
# used to find the proportion of the total variance explained by the components.
# First two components account for about 29 % of variance
for (s in s.eigen$values[1:10]) {
    print(s / sum(s.eigen$values))
}

# Scree Plot
plot(s.eigen$values, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', main = 'Scree Graph')
lines(s.eigen$values)

# Elements of the eigenvectors of S are the ‘coefficients’ or ‘loadings’ of the
# principal components.
dim(s.eigen$vectors)
s.eigen$vectors[,1:2]




### Comparing Document Similarity

# Sample 100 documents
Dem100<- sample(which(uni_raw$party=="democrat"), 100, replace=F)
GOP100<- sample(which(uni_raw$party=="republican"), 100, replace=F)
distSample<- c(Dem100, GOP100)
ds <- uni_raw[distSample,]
ids <-subset(ds, select = c('id', 'file', 'party'))
ids[,1]<- as.character(ids[,1])
ids[,2]<- as.character(ids[,2])
ids[,3]<- as.character(ids[,3])

#now take off ids
ds <- as.matrix(ds[,-c(1,2,1003)])

#need to do a little preprocessing- if a unigram doesn't appear in any of our selected documents, it will cause problems, so we'll need to drop those columns
ds<-ds[,-c(which(colSums(ds)==0))]

# Euclidean Distance
ed <- matrix(nrow=200, ncol=200)

euclidean<- function(x, y){
    return(sqrt(sum((x-y)^2)))        
}

for (m in 1:nrow(ds)){
    for (n in 1:nrow(ds)){
        if (is.na(ed[m,n])==T){
            a<- euclidean(ds[m,], ds[n,])
            ed[m,n]<- a
            ed[n,m]<- a
        }
    }
}


#could also just do the upper triangle and then copy to the lower triangle
#ed[lower.tri(ed)]<- t(ed)[lower.tri(ed)]

m <- NULL
n <- NULL

#you could also do this
#ed2<- as.matrix(dist(ds, method="euclidean"))

write.csv(ed, "euclidean.csv")

#Many matches; (diags are all 0 in addition to "true 0s"). Can use the following hacky method to get only the nondiagonal minimums
# a<-which(ed == min(ed), arr.ind=T)
# a<- as.data.frame(a)  #because you can't use $ on atomic vectors in R.
# a[which(a$row != a$col)]
#alternatively, just do this:
diag(ed) <- NA


#max distance
which(ed == max(ed, na.rm=T), arr.ind=T)


# Euclidean w/ tf-idf weights
edWithTfidf<-matrix(nrow=200, ncol=200)

calcIDF<- function(x){
    return(log(200/length(which(x>0))))
}

idf<- apply(ds, 2, calcIDF)

ds_idf<- as.matrix(t(apply(ds, 1, function(x) x*idf)))

edWithTfidf<- as.matrix(dist(ds_idf, method="euclidean"))
diag(edWithTfidf)<- NA

write.csv(edWithTfidf, "edTFIDF.csv")

# Cosine Similarity
cs<- matrix(nrow=200, ncol=200)
cosineSim<- function(x, y){
    return( sum(x*y)/ sqrt( sum(x^2)* sum(y^2)))
}

#this function is faster so we can just make the whole thing
for (m in 1:nrow(ds)){
    for (n in 1:nrow(ds)){
        a<- cosineSim(ds[m,], ds[n,])
        cs[m, n]<- a
        cs[n, m]<- a
    }
}
m<-NULL
n<-NULL


#you could also use the cosine function from the lsa package
#cs2<- matrix(nrow=200, ncol=200)
#for (m in 1:nrow(ds)){
#    for (n in 1:nrow(ds)){
#        a<- cosine(ds[m,], ds[n,])
#        cs2[m, n]<- a
#        cs2[n, m]<- a
#    }
#}

diag(cs)<-NA
write.csv(cs, "cosineSim.csv")

#most similar
#which(cs == max(cs, na.rm=T), arr.ind=T)

#most dissimilar
#which(cs == min(cs, na.rm=T), arr.ind=T)


# Cosine similarity with tf-idf weights
csWithTfidf<- matrix(nrow=200, ncol=200)

for (m in 1:nrow(ds_idf)){
    for (n in 1:nrow(ds_idf)){
        a <- cosineSim(ds_idf[m,], ds_idf[n,])
        csWithTfidf[m, n]<- a
        csWithTfidf[n, m]<- a
    }
}

diag(csWithTfidf)<- NA

write.csv(csWithTfidf, "csTFIDF.csv")
#most similar
#which(csWithTfidf == max(csWithTfidf, na.rm=T), arr.ind=T)

#most dissimilar
#which(csWithTfidf == min(csWithTfidf, na.rm=T), arr.ind=T)


# Normalize word counts, apply Gaussian kernel
normed<-ds
for (i in 1:nrow(normed)){
    normed[i,]<- normed[i,]/sum(normed[i,])
}

#choose sigma
sigma = 100
gauss<- exp(-(as.matrix(dist(normed)))/sigma)

diag(gauss)<- NA

write.csv(gauss, "gaussian.csv")

#most similar
#which(gauss == max(gauss, na.rm=T), arr.ind=T)

#most different
#which(gauss == min(gauss, na.rm=T), arr.ind=T)


# Gaussian with tf-idf weights
idf<- apply(normed, 2, calcIDF)
normed_idf<- as.matrix(t(apply(normed, 1, function(x) x*idf)))
gaussNorm<- exp(-(as.matrix(dist(normed_idf)))/sigma)
write.csv(gaussNorm, "gaussNorm.csv")

diag(gaussNorm)<- NA
#most similar
#which(gaussNorm == max(gaussNorm, na.rm=T), arr.ind=T)

#most different
#which(gaussNorm == min(gaussNorm, na.rm=T), arr.ind=T)
