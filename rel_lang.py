### Steven Morgan
### Ad Hoc "Dictionary" Approach
### Finding religious language in SotS Addresses

import os, re, csv, string, urllib2, nltk
from urllib import urlopen
from collections import Counter
from nltk.corpus import stopwords



#conda install pandas
#import pandas as pd

mydir = "C:/Users/Steve/Desktop/SoDA/Governors/"

#import stemmers
porter = nltk.stem.PorterStemmer()

stop_words = set(stopwords.words('english'))
pos = urllib2.urlopen("http://www.unc.edu/~ncaren/haphazard/positive.txt").read().split("\n")
neg = urllib2.urlopen("http://www.unc.edu/~ncaren/haphazard/negative.txt").read().split("\n")
posPorter = {porter.stem(w) for w in pos}
negPorter = {porter.stem(w) for w in neg}

#stop_stemmed = map(st.stem, stop_words)


# .csv file where extracted metadata will be stored
fout = open(mydir + "gov.csv", "wb")
outfilehandle = csv.writer(fout,
                           delimiter=",",
                           quotechar='"',
                           quoting=csv.QUOTE_NONNUMERIC)
						   
# Create your own label for each column of the metadata .csv file
localrow = []
localrow.append("State")
localrow.append("Year")
localrow.append("Governor")
localrow.append("God")
localrow.append("bless")
localrow.append("rel_fig")
localrow.append("rel_lang")
localrow.append("total_rel")
localrow.append("total_words")
localrow.append("ratio")
localrow.append("pos")
localrow.append("neg")
localrow.append("net_sent")
outfilehandle.writerow(localrow)

# Name of folder where all cases are located (and nothing else)
dirname = mydir + "SS_Addresses/"
dirlist = os.listdir(dirname)
cleandirlist = []
for entry in dirlist:
    matchresult = re.match('.+\\.txt$', entry)
    if matchresult != None:
        cleandirlist.append(matchresult.group())

url = 'https://raw.githubusercontent.com/CivilServiceUSA/us-governors/master/source/us-governors.csv'
response = urllib2.urlopen(url)
cr = csv.reader(response)
		
for entry in cleandirlist:
    infilepath = dirname + entry
    infilehandle = open(infilepath)
    txtlines = infilehandle.readlines()
    god = 0.0
    bless = 0.0
    rel_fig = 0.0
    rel_lang = 0.0
    all_rel = 0.0
    state_abbr = ""
    year = ""
    governor = ""
    localrow = []
    total_words = 0.0
    total_nonstop = 0.0
    ratio = 0.0
    net_sent = 0.0
    state_abbr = entry[0:2]
    digit = lambda x: int(filter(str.isdigit, x) or 0)
    year = digit(entry)
    governor = (entry.split("_", 2)[2]).split(".")[0]
    #print governor

    print(type(txtlines))
    with open(infilepath, 'r') as f:
        p = f.read()
        words = p.split()
        non_stop = [x for x in words if x not in stop_words]
        #total_words = len(words)
        total_nonstop = len(non_stop)
        pos_gov = len([x for x in non_stop if x in posPorter])
        neg_gov = len([x for x in non_stop if x in negPorter])
        net_sent = (pos_gov - neg_gov) / float(total_nonstop)
    print net_sent
    #print total_words	
    #print total_nonstop
	
    for txtline in txtlines:
        #print txtline
        if(re.search("god|GOD|God", txtline)):
            god = god + 1 
        if(re.search("bless|BLESS|Bless", txtline)):
            bless = bless + 1
        if(re.search("reverend|Reverend|REVEREND|Pastor|pastor|Priest|priest", txtline)):
            rel_fig = rel_fig + 1
        if(re.search("Pastor|pastor|Priest|priest", txtline)):
            rel_fig = rel_fig + 1
        if(re.search("Priest|priest|PRIEST", txtline)):
            rel_fig = rel_fig + 1
        if(re.search("reconciliation|Reconciliation|reconcile|Reconcile", txtline)):
            rel_lang = rel_lang + 1
        if(re.search("Devotion|devotion|devote|Devote", txtline)):
            rel_lang = rel_lang + 1
        if(re.search("religious freedom|Religious freedom", txtline)):
            rel_lang = rel_lang + 1
        if(re.search("grace|Grace", txtline)):
            rel_lang = rel_lang + 1
        if(re.search("devout|Devout", txtline)):
            rel_lang = rel_lang + 1
        if(re.search("faith|Faith", txtline)):
            rel_lang = rel_lang + 1
        if(re.search("Golden Rule", txtline)):
            rel_lang = rel_lang + 1
        if(re.search("worship|Worship", txtline)):
            rel_lang = rel_lang + 1
        if(re.search("Almighty|almighty", txtline)):
            rel_lang = rel_lang + 1
        if(re.search('religion|religious|Religion|Religious', txtline)):
            rel_lang = rel_lang + 1
        if(re.search('beliefs|Beliefs|belief|Belief', txtline)):
            rel_lang = rel_lang + 1
        if(re.search('servant|Servant', txtline)):
            rel_lang = rel_lang + 1
        if(re.search('church|Church', txtline)):
            rel_lang = rel_lang + 1
        all_rel = god + bless + rel_fig + rel_lang
    ratio = all_rel/total_nonstop
	
    #for row in cr:
        #match = column(cr)[7]
        #if match == governor:
        #    print "YAS"
#        print row

    localrow = []
    localrow.append(state_abbr)
    localrow.append(year)
    localrow.append(governor)
    localrow.append(god)
    localrow.append(bless)
    localrow.append(rel_fig)
    localrow.append(rel_lang)
    localrow.append(all_rel)
    localrow.append(total_nonstop)
    localrow.append(ratio)
    localrow.append(pos_gov)
    localrow.append(neg_gov)
    localrow.append(net_sent)
    outfilehandle.writerow(localrow)

infilehandle.close()
fout.close()
