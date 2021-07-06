#------------------------------------------------------------------------------#
#                                                                              #
#    NLP Text Analysis on Security and Policy                                                                ####
#                                                                              #
#------------------------------------------------------------------------------#

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "tidymodels", "tidytext", "httr", "jsonlite", "rvest", "robotstxt", "RSelenium", "seleniumPipes", "forcats", "lubridate")

ipak(packages)


y#------------------------------------------------------------------------------#
# Load Packages                                                                  ####
#------------------------------------------------------------------------------#

library(tidyverse)
library(tidytext)
library(tidymodels)
library(httr)
library(jsonlite)
library(rvest)
library(robotstxt)
library(RSelenium)
library(seleniumPipes)
library(forcats)
library(lubridate)



#------------------------------------------------------------------------------#
# Scrape Data Fromt Lowy Site - Rvest                                                                  ####
#------------------------------------------------------------------------------#

## Set the url to get the data from
base_url <- "https://www.lowyinstitute.org/search?search_text=china"

## read in the url

h <- read_html(base_url)
h

lowy_dates = NULL
lowy_titles = NULL
lowy_urls = NULL
lowy_intro = NULL
counter = 0

lowy_urls = as_tibble(lowy_urls)
lowy_dates = as_tibble(lowy_dates)
lowy_titles = as_tibble(lowy_titles)
lowy_intro = as_tibble(lowy_intro)

for (i in 0:172){
  # loop to get the article key items from the teaser pages
  counter = i
  print(counter)
  Sys.sleep(3)
  pg = paste0(base_url,"&page=",i) %>% read_html()
  urls <- as_tibble(html_nodes(pg, css = '.right-content h2 a') %>% html_attr('href'))
  date <- as_tibble(html_nodes(pg, css = '.right-content .date') %>% html_text())
  titles <- as_tibble(html_nodes(pg,css = '.right-content h2') %>% html_text())
  intro <- as_tibble(html_nodes(pg, css = '.right-content .summary_content') %>% html_text2())
  lowy_urls <- bind_rows(lowy_urls, urls)
  lowy_dates <- bind_rows(lowy_dates, date) 
  lowy_titles <- bind_rows(lowy_titles, titles)
  lowy_intro <- bind_rows(lowy_intro, intro)
}

# bind the elements together into a tibble
lowy_articles_teaser <- bind_cols(lowy_dates, lowy_titles, lowy_intro, lowy_urls) 

# tidy the tibble 
lowy_art_teaser <- lowy_articles_teaser %>% rename(date = value...1,
                                title = value...2,
                                intro = value...3,
                                url = value...4) %>% 
  mutate(
    date = dmy(date)
  )

lowy_art_teaser 

write_csv(lowy_art_teaser, file = "data/lowy_art_teaser.csv")

#------------------------------------------------------------------------------#
# Scrape Data from site using Rselenium                                                                   ####
#------------------------------------------------------------------------------#

# set the remote driver and load it - firefox browser should open up here
rd <- rsDriver(browser = "firefox")
remdr <- rd[["client"]]
# Set the URL and navigate to it - check the browser to see how it looks
url <- "https://www.lowyinstitute.org/search?search_text=china"
remdr$navigate(url)

ticker = 0
# create empty tibbles for use in the following function
content_tib <- as_tibble(NULL)
date_tib <- as_tibble(NULL)
authour_tib <-  as_tibble(NULL)
title_tib <- as_tibble(NULL)
# find the article page elements and set the browser to click on them
while(cont_pg != cont_pg1) {
  ticker = ticker + 1
  print(ticker)
  # pause for 3 seconds for the page to load
  Sys.sleep(3)
  # get the url for the current page as a variable
  cont_pg <- remdr$getCurrentUrl() %>% unlist()
  # read in the elements from the page 
  title <- as_tibble(html_node(read_html(cont_pg), css = 'h1') %>% html_text2())
  content <- as_tibble(html_nodes(read_html(cont_pg), css = '.field-item p') %>% html_text2())
  date <- as_tibble(html_node(read_html(cont_pg), xpath = '/html/body/div[2]/div[2]/div/div/div/div/div/div[1]/div[3]/div[1]/span[1]') %>% html_text2())
  authour <- as_tibble(html_node(read_html(cont_pg), css = '.txt-dn a') %>% html_text2())
  # build the individual tibbles for combining when the loop is finished
  
  content <- content %>% nest()
  authour <- authour %>% nest()
  title_tib <- title_tib %>% bind_rows(title)
  content_tib <- content_tib %>% bind_rows(content)
  date_tib <- date_tib %>% bind_rows(date)
  authour_tib <- authour_tib %>% bind_rows(authour) 
  
  # find the page element
  page <- remdr$findElement(using = "xpath", "/html/body/div[2]/div[2]/div/div/div/div/div/div[3]/div[1]/div/a")
  # navigate to the next article
  page$clickElement()
  ## get next page to compare it with the current page to stop the loop
  cont_pg1 <- remdr$getCurrentUrl() %>% unlist()
}

title_tib$value %>% str_detect("China") %>% sum()
content_tib$data %>% str_detect("China") %>% sum()
date_tib %>% tail()
authour_tib
# be sure to stop the server at the end of the process
rd$server$stop()

#------------------------------------------------------------------------------#
# G-blogs                                                                ####
#------------------------------------------------------------------------------#

# set the remote driver and load it - firefox browser should open up here
rd <- rsDriver(browser = "firefox")
remdr <- rd[["client"]]
# Set the URL and navigate to it - check the browser to see how it looks
url <- ""
remdr$navigate(url)





# Stop the server at the end of the loop
rd$server$stop()

## Tidy the text for analysis
intro_count <- lowy_art_teaser %>% unnest_tokens(
  word, intro) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

intro_count

# Basic word count
intro_count %>% 
  filter(n>30) %>% 
  ggplot(aes(fct_reorder(word, n), n)) +
  geom_col() +
  coord_flip()
  
# Bigram word count

intro_bigram <- lowy_art_teaser %>% unnest_tokens(
  bigram, intro, token = "ngrams", n = 3) %>% 
  separate(bigram, c("word1", "word2", "word3"), sep = " ") 

#------------------------------------------------------------------------------#
# Basic text analysis - Bigram/ Trigram and word counts                                                                  ####
#------------------------------------------------------------------------------#

# add lowy and other words to stop words

stop_words <- stop_words %>% 
  add_row(word  = c("lowy", "fullilove", "NA", "dr", "senior", "michael"), lexicon = "SMART")

stop_words %>% tail()

intro_bigram <- intro_bigram %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>% 
  drop_na()

intro_bigram %>% count(word1, word2, word3, sort=TRUE)

intro_bigram <- intro_bigram %>% 
  unite(bigram, word1, word2, word3, sep = " ")

intro_bigram %>% 
  count(bigram, sort = TRUE) %>% 
  filter(n>3) %>% 
  ggplot(aes(fct_reorder(bigram, n), n)) +
  geom_col() +
  coord_flip()


#------------------------------------------------------------------------------#
# Article dates and frequency                                                                  ####
#------------------------------------------------------------------------------#

lowy_art_teaser %>% 
  group_by(year(date)) %>% 
  count()

lowy_art_teaser %>% 
  group_by(year(date)) %>% 
  filter(str_detect(intro, "published")) %>% 
  count()
  
  