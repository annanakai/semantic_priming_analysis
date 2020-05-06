# Semantic Priming Analysis using word2vec and WordNet

#### Note on running the code
To run the code first go to this [link](https://code.google.com/archive/p/word2vec/) and download the Google News word embeddings. This file is too large to include in this Github repository.

#### Descriptions of the files
* aggregated_clean.csv - a csv file storing pairs of primes and targets
* word2vec_wordnet_similarity_scores.ipynb - a jupyter notebook that reads in aggregated_clean.csv and computes Jiang-Conrath and cosine similarities. 
* distances_data.csv - the data produced by running word2vec_wordnet_similarity_scores
* 200SOA.csv and 1200SOA.csv - trial data from the semantic priming project
* semantic_priming_analysis.R - R script that merges the similarity scores into the trial data and runs the linear mixed effects models. Also runs the ANOVA and AIC evaluations of the models.

#### How to run the code
After getting the word embeddings from Google News and placing them in the same directory as the rest of the files, open word2vec_wordnet_similarity_scores.ipynb and run all of the code.
Then open semantic_priming_analysis.R and run. Note that this file takes around an hour to run as linear mixed effects models are slow on large datasets.
