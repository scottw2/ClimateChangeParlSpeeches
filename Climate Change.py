# import packages
import pandas
import nltk
from nltk import RegexpTokenizer
import gensim
from nltk.stem.wordnet import WordNetLemmatizer
import pprint
import logging
import numpy

# enable logging
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)

# import big dataset and slim down
# filename = "hansard-speeches-v301.csv"
# Debates1 = pandas.read_csv(filename)
# DebatesCCSP = Debates1[Debates1['speech'].str.contains('climate change', case = False, na = False)]
# DebatesCCSP.to_csv("hansard-climatechangeSP-v301.csv",index=False)
# probably want to clear memory after this

# import data
CC1 = pandas.read_csv("hansard-climatechangeSP-v301.csv")
CC1.head

# probably will want to slim down the dataframe a bit first
# remove columns "mnis_id", "colnum", "oral_heading", "hansard_membership_id", "speakerid", "person_id", "url"
CC1 = CC1.drop(["mnis_id", "colnum", "oral_heading", "hansard_membership_id", "speakerid", "person_id", "url"], axis=1)

# import stopwords
nltk.download('stopwords')
from nltk.corpus import stopwords
stop_words = stopwords.words('english')

# removing punctuation and un-useful words
# remove unknown, remove anything in brackets or square brackets remove /n, and remove noisy words like hon and member
CC1unk = CC1[CC1['display_as'].str.contains('unknown', case=False, na=False)]
CC1 = CC1[~CC1['display_as'].str.contains('unknown', case=False, na=False)]
CC1['speech'] = CC1['speech'].str.replace("\n", "")
CC1['speech'] = CC1['speech'].str.replace(r"\s*\(.*\)\s*", "")
CC1['speech'] = CC1['speech'].str.replace(r"\s*\[.*\]\s*", "")
pattern = '|'.join(["hon.", "hon", "member", "minister", "friend", "lady", "right hon.", "gentleman", "right hon"])
CC1['speech'] = CC1['speech'].str.replace(pattern, "", case=False)
CC1['speech'] = CC1['speech'].str.replace("'s", "", case=False)
CC1['speech'] = CC1['speech'].replace("", numpy.nan).dropna()
CC1data = CC1.speech.values.tolist()

# tokenise and remove stopwords
tokeniser = RegexpTokenizer(r'\w+')
for item in range(len(CC1data)):
    CC1data[item] = CC1data[item].lower()
    CC1data[item] = tokeniser.tokenize(CC1data[item])
    CC1data[item] = [w for w in CC1data[item] if not w in stop_words]

# remove numbers
CC1data = [[w for w in doc if not w.isnumeric()] for doc in CC1data]

# Lematise - could make this very tricky if I wanted to but I will just do a simple one for now
from nltk.corpus import wordnet
def get_wordnet_pos(treebank_tag):
    if treebank_tag.startswith('J'):
        return wordnet.ADJ
    elif treebank_tag.startswith('V'):
        return wordnet.VERB
    elif treebank_tag.startswith('N'):
        return wordnet.NOUN
    elif treebank_tag.startswith('R'):
        return wordnet.ADV
    else:
        return wordnet.NOUN

lemmatiser = WordNetLemmatizer()
# CC1data = [[lemmatiser.lemmatize(w) for w in doc] for doc in CC1data] - other not as good method
for item in range(len(CC1data)):
    CC1data[item] = [lemmatiser.lemmatize(w, get_wordnet_pos(w)) for w in CC1data[item]]

# bigrams
from gensim.models import Phrases
bigram = Phrases(CC1data, min_count=10)
for item in range(len(CC1data)):
    for w in bigram[CC1data[item]]:
        if '_' in w:
            CC1data[item].append(w)

# make dictionary
from gensim.corpora import Dictionary
dictionary = Dictionary(CC1data)

# filter out words in less than 20 documents and in more than 50% of documents
dictionary.filter_extremes(no_below=20, no_above=0.75)

# make bag of words corpus
corpus = [dictionary.doc2bow(doc) for doc in CC1data]

# print count of words and docs
print('Number of unique tokens: %d' % len(dictionary))
print('Number of documents: %d' % len(corpus))

# make LDA model
from gensim.models import LdaModel

# model training parameters
num_topics = 20
chunksize = 4000
passes = 25
iterations = 400
eval_every = None

# index for loading dictionary
temp = dictionary[0]
id2word = dictionary.id2token

# run model
model = LdaModel(
    corpus=corpus,
    id2word=id2word,
    chunksize=chunksize,
    alpha='auto',
    eta='auto',
    iterations=iterations,
    num_topics=num_topics,
    passes=passes,
    eval_every=eval_every
)

# show top words and average topic coherence
top_topics = model.top_topics(corpus)  # , num_words=20)
avg_topic_coherence = sum([t[1] for t in top_topics]) / num_topics
print('Average topic coherence: %.4f.' % avg_topic_coherence)
from pprint import pprint
pprint(top_topics)

# saving model
model.save('lda.modelccsp2')

# saving dataframe of topics and top words per topic
top_words_per_topic = []
for t in range(model.num_topics):
    top_words_per_topic.extend([(t,) + x for x in model.show_topic(t, topn=100)])
top_words = pandas.DataFrame(top_words_per_topic, columns=['Topic', 'Word', 'P'])
top_words.to_csv("top_wordsCC1LDAmodelSP2hundred.csv")

# map df of CC1 data back to CC1 and then add in topic identifier - for each topic give % of the doc 
get_document_topics = [model.get_document_topics(doc) for doc in corpus]
document_topics_dict = [dict(x) for x in get_document_topics]
document_topics_df = pandas.DataFrame(document_topics_dict).fillna(0)
document_topics_df_reorder = document_topics_df[
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]]  # set this based on n topics

# need to set index to start from 0 again
CC1wTopics = pandas.concat([CC1.reset_index(drop=True), document_topics_df_reorder], axis=1)

# joins and the words in CC1data into one list each and then makes df
CC1processedtext = list(map(' '.join, CC1data))
CC1processedtext_df = pandas.DataFrame(CC1processedtext)

# joins the two dfs together
CC1wTopics = pandas.concat([CC1wTopics, CC1processedtext_df], axis=1)

# make csv with topics and processed text
CC1wTopics.to_csv("CC1TopicsPTSP2.csv")

# gets coherence score
from gensim.models.coherencemodel import CoherenceModel# get coherence score
CoherenceScore = CoherenceModel(model=model, corpus=corpus, dictionary=dictionary, coherence='u_mass')
print(CoherenceScore)
print(CoherenceScore.get_coherence())
print(CoherenceScore.get_coherence_per_topic())

# for new method model 12 topics umass = -1.517
# also tested other numbers of topics
# 20 topics umass = -1.6806
# not going above 20 topics as 20 seems to give good interpretable ones

