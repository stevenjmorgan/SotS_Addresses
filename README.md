# SotS_Addresses

This repository houses code used for a project funded by the NSF BDSS IGERT housed at Penn State. After collecting over 800 state of the state addresses from 2000-2017, I take a few approaches to explore the data. This is part of a larger project utilizing a transfer learning approach to estimate ideological ideal points for US governors. The goal of these measures is to improve on previous measures of governor ideologies utilizing veto records and Wordfish/Wordscore approaches to text data.

create_dtm.py
Creates two document-term matrices: one for unigrams and one for trigrams.

ideology_scores.R
Replicates prior approaches to modeling state of the state addresses (Wordfish or Wordscore). These measures exhibit substantial face validity (specificially Wordscore results) but lack generalizations across time periods (due to a priori assumptions required by both models).

Word_Sep_Algorithms.R
Applies multiple word separation algorithms to the document-term matrices produced by create_dtm.py.

rel_lang.py
Ad hoc "dictionary" based analysis of partisan differences in religious language in SotS addresses. Produces a .csv of the count and normalized frequency of religious language for each speech in the dataset. 

rel_lang_poisson.R
Runs Poisson-based regressional analyses (count, negative binomial, zero-inflated Poisson, zero-inflated negative binomial, hurdle, and random-effects Poisson models) to predict the count of anti-LGBTQ bills with religious signals in governor state of the state addresses (with appropriate controls for institutions and public opinion).
