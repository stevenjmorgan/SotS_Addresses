### Steven Morgan
### Create Document-Term Matrix from State of the State Addresses
### Unigram and Trigram DT Matrices

import os, re, nltk, urllib2, csv
from nltk.corpus import stopwords

##make nested dictionary of press releases and metadata
wd = "C:/Users/Steve/Desktop/SoDA/Governors/SS_Addresses/"


os.chdir(wd)

folders = [wd]

pressReleases = {}

for folder in folders:
    files = os.listdir(folder)
    for pr in files:
        pressReleases[pr] = {}
        #pressReleases[pr]["day"] = pr[:2]
        #pressReleases[pr]["month"] = pr[2:5]
        pressReleases[pr]["year"] = pr[3:7]
        pressReleases[pr]["author"] = re.sub("[0-9]+.txt", "", pr[8:-4])
        with open(folder + "/" + pr, "r") as fileIn:
            pressReleases[pr]["text"] = fileIn.read()

#get stopwords, store as set for quick lookup. apply porter stemmer
porter = nltk.stem.PorterStemmer()

stopwords =  set(stopwords.words('english'))
#stopwords = {x.strip("\n") for x in stopwords}
#toAdd = {"shelby", "sessions", "richard", "jeff", "email", "press", "room", "member", "senate"}
#stopwords.update(toAdd) 
stopwords = {porter.stem(w) for w in stopwords}


#to count unigrams and trigrams, create dictionaries linking the -grams to their counts.
#For unigram and each trigram in each doc, check whether it already exists in the dictionary.
#If not, add it and give it a value of 1. If it does, increment the value by 1

unigrams = {}
trigrams = {}

for pr in pressReleases:
    txt = pressReleases[pr]["text"]
    txt = re.sub("\W", " ", txt)
    txt = txt.lower()
    tokens = nltk.word_tokenize(txt)
    tokens = [porter.stem(w) for w in tokens]
    tokens = [x for x in tokens if x not in stopwords]
    pressReleases[pr]["numUnigrams"] = len(tokens)
    #add another layer of dictionary to record the frequencies of unigrams in each document
    pressReleases[pr]["doc_unigrams"] = {}
    for i in set(tokens):
        count = tokens.count(i)
        #for each unigram, add a count to the new inner dictionary
        pressReleases[pr]["doc_unigrams"][i] = count
        #add to overall unigrams dictionary, with count
        if i in unigrams:
            unigrams[i] += count
        else:
            unigrams[i] = count
    #now to deal with trigrams
    trigList = list(nltk.trigrams(tokens))
    pressReleases[pr]["numTrigrams"] = len(trigList)
    pressReleases[pr]["doc_trigrams"] = {}
    for j in set(trigList):
        count = trigList.count(j)
        pressReleases[pr]["doc_trigrams"][j] = count
        if j in trigrams:
            trigrams[j] += count
        else:
            trigrams[j] = count 

#To sort by values:
#sorted(a, key=a.get, reverse=True)
#for i in topUnigrams:
#   print i, unigrams[i]

topUnigrams = sorted(unigrams, key=unigrams.get, reverse=True)[:1000]
topTrigrams = sorted(trigrams, key=trigrams.get, reverse=True)[:500]

#write unigrams file
#if you want to calculate your rates out of total words, add a column for this
#headerUni = ["file"] + ["id"] + topUnigrams + ["allUnigrams"] 
headerUni =  ["file"]+ ["id"] + topUnigrams


with open("unigrams.csv", "wb") as csvfile:
    writer = csv.writer(csvfile, delimiter= ",")
    writer.writerow(headerUni)
    for i in pressReleases:
        toWrite = []
        toWrite.append(i)
        toWrite.append(pressReleases[i]["author"])
        #toWrite.append(pressReleases[i]["year"])
        for j in topUnigrams:
            if j in pressReleases[i]["doc_unigrams"]:
                toWrite.append(str(pressReleases[i]["doc_unigrams"][j]))
            else:
                toWrite.append(str(0))
        #if you want to calculate your rates out of total words, you can add this to your tdm
        #toWrite.append(str(pressReleases[i]["numUnigrams"]))
        writer.writerow(toWrite)

#write trigrams file

#define funciton that takes in tuple, converts to dot delimited string
def tupToString(tup):
    return ".".join(tup)

stringifiedTris = [tupToString(x) for x in topTrigrams]

#if you want to calculate your rates out of total , add a column for this
#headerTri = ["file"] + ["id"] + stringifiedTris + ["allTrigrams"]
headerTri = ["file"] + ["id"] + stringifiedTris

with open("trigrams.csv", "wb") as csvfile:
    writer = csv.writer(csvfile, delimiter= ",")
    writer.writerow(headerTri)
    for i in pressReleases:
        toWrite = []
        toWrite.append(i)
        toWrite.append(pressReleases[i]["author"])
        #toWrite.append(pressReleases[i]["year"])
        for j in topTrigrams:
            if j in pressReleases[i]["doc_trigrams"]:
                toWrite.append(str(pressReleases[i]["doc_trigrams"][j]))
            else:
                toWrite.append(str(0))
        #if you want to calculate your rates out of total words, you can add this to your tdm
        #toWrite.append(str(pressReleases[i]["numTrigrams"]))
        writer.writerow(toWrite)
