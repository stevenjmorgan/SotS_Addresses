### Steven Morgan ###
### Initial Modeling of State of the State Address Corpus ###
### Code Last Updated 2/4/18 ###


setwd("C:/Users/Steve/Desktop/SoDA/Governors/Governor_Text_Modeling/SOTS_Modeling")

# install.packages("austin", repos = "http://R-Forge.R-project.org", 
#                 dependencies = "Depends", type = "source")

suppressWarnings(
    suppressMessages({
        library(tm)
        library(NLP)
        library(rio)
        library(austin)
        library(ggplot2)
    })
)

library(tm)

### Import .csv with State of the State Address metadata
gov.meta <- read.csv("governor_party.csv", stringsAsFactors = FALSE)

# Remove speeches prior to 2000 and non-GOP/Dem.
gov.meta <- gov.meta[which(gov.meta$Year >= 2000),] #make this 2001 later
dim(gov.meta)
#gov.meta <- gov.meta[which(gov.meta$party == "democrat" | gov.meta$party == "republican"),]
dim(gov.meta)


### Import corpus of state of the state addresses from Dropbox file
ds <- DirSource(
    directory = "C:\\Users\\Steve\\Dropbox\\IGERT\\Governor_Transcripts", 
    pattern = ".*20\\d\\d.*txt", 
    mode = "text")

corpus <- VCorpus(ds)
summary(corpus)

# Put all text in lower case
corpus <- tm_map(corpus, content_transformer(tolower))

# Remove English stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, c("must", "will", "can", "make"))

# Stem the text
corpus <- tm_map(corpus, stemDocument)

# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)

# Remove numbers
corpus <- tm_map(corpus, removeNumbers)

# Create document term matrix
dtm <- DocumentTermMatrix(corpus)

# Remove rare words
dtm <- removeSparseTerms(dtm, sparse = 0.4) #play around with sparsity level; probably too hight

inspect(dtm)

# Find words used more than 400 times
findFreqTerms(dtm, 400)

### Implement WORDSCORE Algorithm
# Set reference texts a prior for most liberal and conservative governor-year
# text: Brownback = most conservative, Wolf = most liberal
# Note: same year is utilized but there are pros and cons to doing so
reference.texts <- c(
    which(names(corpus) == "KS_2016_Brownback.txt"),
    which(names(corpus) == "PA_2016_Wolf.txt")
)

reference.scores <- c(100, -100)

ws <- classic.wordscores(as.wfm(dtm[reference.texts, ]), reference.scores)

head(ws$pi[, "Score"], 40)

# Distribution of word scores
hist(ws$pi, col = "blue", border = 0)

# Use estimated word scores to classify remaining texts
scores <- predict(ws, newdata = as.wfm(dtm[-reference.texts, ]))
plot(scores$Score)
hist(scores$Score)


# Most liberal (Wolf-like) speeches
scores[order(scores$Score, decreasing = FALSE), ][1:5, ]

# Most conservative (Brownback-like) speeches
scores[order(scores$Score, decreasing = TRUE), ][1:5, ]


### Implement WORDFISH algorithm
# Instead of a priori selecting ends of the continuum, this method is less 
# constrained by a priori decisions and only requires specifying direction
# by "keeping" one text to the left of another.

# Utilize Brownback and Wolf as reference texts, asserting that Brownback
# must lie to the right of Wolf (a very reasonable prior)

wf <- wordfish(as.wfm(dtm), reference.texts, 
               control = list(tol = .003)) # this tolerance is high and should be played with (also slows estimation)
plot(wf)


print.wordfish <- function(x, digits=max(3,getOption('digits')-3), ...){
    cat("Call:\n\t")
    print(x$call)
    cat("\nDocument Positions:\n")
    pp <- predict(x, se.fit=TRUE, interval='confidence')
    colnames(pp) <- c("Estimate", "Std. Error", "Lower", "Upper")
    print(pp, digits=digits)
}

print.wordfish(wf)


plot.wordfish <- function(x, truevals=NULL, level=0.95, pch=20, ...){
    ord      <- order(x$theta)
    theta    <- x$theta[ord]
    ci.theta <- (x$se.theta * qnorm(1-(1-level)/2))[ord]
    upper    <- theta + ci.theta
    lower    <- theta - ci.theta
    name.theta <- x$docs[ord]
    
    if (!is.null(truevals)){
        truevals <- truevals[ord]
        lims <- c(min(c(theta, truevals, lower)), max(c(theta, truevals, upper)))
        dotchart(theta, labels=name.theta, xlim=lims, pch=pch, ...)
        segments(lower, 1:length(theta), upper, 1:length(theta))
        points(truevals, 1:length(theta), col=rgb(139/255,0,0,0.75), pch=pch)
        title(paste('r =', format(cor(truevals, x$theta), digits=4)))
    } else {
        lims <- c(min(c(theta, lower)), max(c(theta, upper)))
        dotchart(theta, labels="", xlim=lims, pch=pch, ...) #name.theta
        segments(lower, 1:length(theta), upper, 1:length(theta))
        title(paste("Distribution of Speeches: WordFish"))
    }
}

plot.wordfish(wf)

z.critical <- qnorm(.975)

lower <- wf$theta - z.critical * wf$se.theta
upper <- wf$theta + z.critical * wf$se.theta


ggplot(mapping = aes(y = wf$theta, x = gov.meta$Year, color = gov.meta$party)) + geom_point() + 
    geom_smooth() +
    geom_segment(aes(x = gov.meta$Year, y = lower, xend = gov.meta$Year, yend = upper)) +
    labs(x = "Year", y = "Wordfish theta") + guides(col = guide_legend(title = "Party"))

# Assess which words were the most influential in determining the output
beta <- wf$beta
names(beta) <- wf$words

cat(names(beta[order(beta, decreasing = TRUE)][1:30])) # Conservative

cat(names(beta[order(beta, decreasing = FALSE)][1:30])) # Liberal

ggplot(mapping = aes(x = wf$beta, y = wf$psi, label = wf$words)) + 
    geom_text(size = 3) + 
    labs(x = "Beta", y = "Psi") +
    guides(size = "none", color = guide_legend(""))

plot(rowSums(as.matrix(dtm)) ~ wf$alpha, xlab = "Alpha",  #[-reference.texts,]
     ylab = "Document length", pch = 19, main = "Document length by alpha")

#plot(rowSums(as.matrix(dtm)[-reference.texts,]) ~ wf$alpha, 
#     subset = rowSums(as.matrix(dtm)[-reference.texts,]) < 5000, 
#     xlab = "Alpha", ylab = "Document length", pch = 19, 
#     main = "Document length by alpha (removed one document)")

plot(colSums(as.matrix(dtm)[-reference.texts,]) ~ wf$psi, xlab = "Psi", 
     ylab = "Overall frequency", pch = 19, main = "Word frequency by psi")


ggplot(mapping = aes(y = wf$theta, x = gov.meta$party, color = gov.meta$party)) + geom_point() +
    labs(x = "Claimed Ideology", y = "Wordfish theta") + guides(col = guide_legend(title = "Claimed Ideology"))

# Plot weights by frequencies for words that have both positive beta and psi
imptntwords.wf3 <- which(wf$beta > 0 & wf$psi > 0)
ggplot(mapping = aes(x = wf$beta[imptntwords.wf3], y = wf$psi[imptntwords.wf3], label = wf$words[imptntwords.wf3])) + 
    geom_text(size = 3) + labs(x = "Beta", y = "Psi")
