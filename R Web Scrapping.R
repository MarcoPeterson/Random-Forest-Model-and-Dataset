## Run the libraries

library("rvest")
library("xml2")
library("RSelenium")
library("reshape2")
library("igraph")
library("ggraph")
library("NLP")
library("glue")
library("readr")
library("dplyr")
library("tidytext")
library("textdata")
library("stringr")
library("tm")
library("wordcloud")
library("syuzhet")
library("tibble")
library("tidyverse")
library("tm")
library("widyr")
library("ggplot2")
library("tidyr")
library("igraph")
library("ggraph")
library("stringr")
library("gender")
library("genderdata")

## This is useful to kill a particular script

system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)


## This is to use the driver and scrape with a remote server

rmDr=rsDriver(browser=c("firefox"), chromever="latest",version = "latest", phantomver = "2.1.1", verbose = TRUE,
              check = TRUE)
myclient= rmDr$client
myclient$navigate("https://www.google.com/search?q=Colaw+Fitness+of+Arlington+TX+Gyms&hl=en&sxsrf=ALeKk00Kg-aXG0U1y4dAUwc-2x3bACvuag%3A1624045413964&source=hp&ei=ZffMYMTwNo7S-wSpiIyQAg&iflsig=AINFCbYAAAAAYM0FdR4Z7OnHq4OpA26qsJnQoh3JAmQH&oq=Colaw+Fitness+of+Arlington+TX+Gyms&gs_lcp=Cgdnd3Mtd2l6EAMyCwguEMcBEK8BEJMCMgIIJjoHCCMQ6gIQJzoNCC4QxwEQrwEQ6gIQJ1DkCVjkCWCaD2gBcAB4AIABrAGIAawBkgEDMC4xmAEAoAECoAEBqgEHZ3dzLXdperABCg&sclient=gws-wiz&ved=0ahUKEwiEt_rP-KHxAhUO6Z4KHSkEAyIQ4dUDCAk&uact=5#lrd=0x864e639e1c9997e7:0xcc73f3a9ae148e16,1,,,")
#click on the snippet to switch focus----------

webEle <- myclient$findElement(using = "css",value = ".review-snippet")
webEle$clickElement()

webElem <- myclient$

#loop and simulate clicking on all "click on more" elements-------------

webEles <- myclient$findElements(using = "css",value = ".review-more-link")
for(webEle in webEles){
  tryCatch(webEle$clickElement(),error=function(e){print(e)}) # trycatch to prevent any error from stopping the loop
}

## Create a source for review documents

pagesource <- myclient$getPageSource()[[1]]
# this should get you the full review, including translation and original text-------------
reviews <- read_html(pagesource) %>%
  html_nodes(".review-full-text") %>%
  html_text()

# number of stars
stars <- read_html(pagesource) %>%
  html_node(".review-dialog-list") %>%
  html_nodes("g-review-stars > span") %>%
  html_attr("aria-label")

# number of post over a certain period of time

post_time <- read_html(pagesource) %>%
  html_node(".review-dialog-list") %>%
  html_nodes(".dehysf") %>%
  html_text()


# create datasets from the prior sets


Fitness_Connection_Huelen_df <- reviews
Fitness_Connection_Huelen_df <- as.tibble(Fitness_Connection_Huelen_df)
Fitness_Connection_Huelen_df$gym_membership <- 'FC_Huelen'
Fitness_Connection_Huelen_df$gym_membership <- 'FC_Huelen'


Fox_fitness_df <- reviews
Fox_fitness_df <- as.tibble(Fox_fitness_df)
Fox_fitness_df$gym_membership <-'Fox_Fitness'

Fitness_Connection_Bedford_df <- reviews
Fitness_Connection_Bedford_df <- as.tibble(Fitness_Connection_Bedford_df)
Fitness_Connection_Bedford_df$gym_membership <- 'FC_Bedford'


Metroplex_Arlington_df <- reviews
Metroplex_Arlington_df <- as.tibble(Metroplex_Arlington_df)
Metroplex_Arlington_df$gym_membership <- 'Metroplex_Arlington'


Colaw_Fitness_df <- reviews
Colaw_Fitness_df <- as.tibble(Colaw_Fitness_df)
Colaw_Fitness_df$gym_membership <- 'Colaw_Fitness'




# Combine datasets to perform various text mining exercises


combineddataset = rbind(Fitness_Connection_Huelen_df, Fox_fitness_df, La_fitness_Irving_df, Fitness_Connection_Bedford_df, Metroplex_Arlington_df,Colaw_Fitness_df)

glimpse(combineddataset)

prop.table(table(combineddataset$gym_membership))


tidy_df <- combineddataset %>%
  group_by(gym_membership) %>%
  mutate(
    linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, value)


nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_df %>%
  filter(gym_membership == "FC_Huelen") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

tidy_df %>%
  filter(gym_membership == "LA_Fitness_Irving") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

tidy_df %>%
  filter(gym_membership == "FC_Bedford") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

tidy_df %>%
  filter(gym_membership == "Fox_Fitness") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

tidy_df %>%
  filter(gym_membership == "Metroplex_Arlington") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


tidy_df %>%
  filter(gym_membership == "Colaw_Fitness") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


bing_df_word_count <- tidy_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_df_word_count


bing_df_word_count %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

tidy_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 80)



book_words <- combineddataset %>%
  unnest_tokens(word, value) %>%
  count(gym_membership, word, sort = TRUE)

book_words

total_words <- book_words %>% 
  group_by(gym_membership) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words

book_tf_idf <- book_words %>%
  bind_tf_idf(word, gym_membership, n)

book_tf_idf

book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_tf_idf %>%
  group_by(gym_membership) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = gym_membership)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~gym_membership, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

df_words <- combineddataset %>%
  unnest_tokens(word, value) %>%
  count(gym_membership, word, sort = TRUE)

df_words

plot_df_word <- df_words %>%
  bind_tf_idf(word, gym_membership, n) %>%
  mutate(gym_membership = factor(gym_membership, levels = c("FC_Bedford",
                                            "FC_Huelen", 
                                            "Fox_Fitness",
                                            "LA_Fitness_Irving", "Metroplex_Arlington", "Colaw_Fitness")))
plot_df_word %>% 
  group_by(gym_membership) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = gym_membership)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~gym_membership, ncol = 2, scales = "free")


###############################################################################

df_bigram <- combineddataset %>%
  unnest_tokens(bigram, value, token = "ngrams", n = 2)

df_bigram %>%
  count(bigram, sort = TRUE)


df_separated <- df_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

df_filtered <- df_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
df_counts <- df_filtered %>% 
  count(word1, word2, sort = TRUE)

df_counts

df_united <- df_filtered %>%
  unite(bigram, word1, word2, sep = " ")

df_united


combineddataset %>%
  unnest_tokens(trigram, value, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)



df_filtered %>%
  filter(word2 == "pass") %>%
  count(gym_membership, word1, sort = TRUE)


df_tf_idf <- df_united %>%
  count(gym_membership, bigram) %>%
  bind_tf_idf(bigram, gym_membership, n) %>%
  arrange(desc(tf_idf))

df_tf_idf

AFINN <- get_sentiments("afinn")
AFINN

not_words <- df_separated %>%
  filter(word1 == "to") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")



df_graph <- df_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

df_graph


set.seed(2017)

ggraph(df_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(df_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()











