
# coding: utf-8

# In[18]:

import wikipedia as wp
from wordcloud import WordCloud, STOPWORDS
from os import path
import numpy as np
from PIL import Image

def get_wiki(query):
    title= wp.search(query, results=1)
    page= wp.page(title)
    return page.content

def printing_top_word_frequencies():
    split_contents= contents.split(" ")

    relevant_words= list(filter(lambda x: x not in STOPWORDS,split_contents))

    word_count = dict()

    for i in relevant_words:
        if i in word_count.keys():
            word_count[i]=word_count[i]+1
        else:
            word_count[i]=1
    final_unique_values= sorted(set(list(word_count.values())),reverse=True)
    print("Words appeared the most")
    for y in final_unique_values[0:5]:
        for (a,b) in word_count.items():
            if b==y:
                print((a,b))
    


# In[ ]:

if __name__ == "__main__":
    search_term=input("Enter the search term to form a wordcloud for: ")
    contents=get_wiki(search_term)
    printing_top_word_frequencies()
    create_wordcloud(contents)


# In[17]:

def create_wordcloud(text):
    # create numpy araay for wordcloud mask image
    mask = np.array(Image.open(path.join("cloud.png")))

    # create set of stopwords
    stopwords = set(STOPWORDS)

    # create wordcloud object
    wc = WordCloud(background_color="white",
                    max_words=200, 
                    mask=mask,
                    stopwords=stopwords)

    # generate wordcloud
    wc.generate(text)

    # save wordcloud
    wc.to_file(path.join("wc.png"))

