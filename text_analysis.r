# Strategic Text Ana
#### James Murray
#### Jul2021
#### 1.0.0
#### The intent is to import and compare strategic policy with some key strategic commentary

## Load Packages ====

library(tidyverse)
library(tidymodels)
library(lubridate)
library(tidytext)
library(googledrive)
library(tm)

## Load and Tidy the Data ====

# Use google drive to download the ASPI files to a local drive
# Data file structure - each folderby year for commentary with policy in a separate folder

dir <- list.dirs('./data')

# List all.pdf files in a vector format for use in 'readPDF' below
files <- list.files(path = './data', pattern = ".pdf$", recursive = TRUE,full.names = TRUE) %>% as_vector()

### Read in the PDF to Txt with Meta-data ====
read <- readPDF(control = list(text = "-layout", engine = "xpdf")) # set the reader for use below

txt_docs <- Corpus(URISource(files), readerControl = list(reader = read))

# tidy the output for further analysis
pdf_dets <- txt_docs %>% tidy()

# Write the file to a csv for future use if required
write_csv(pdf_dets, file = 'data/pdf_dets.csv')

### Tidy the Text Data ====
# pdf_dets[2,]$text %>%str_squish() %>% str_wrap() %>%str_replace_all("\\s+", " ")

# remove new lines and extra white space from text files
df <- pdf_dets %>% rowwise() %>% mutate(
  text = text %>% str_squish()) %>%
  rename(pub_date = datetimestamp)
#tidy dates
df <- df %>% mutate(
  pub_date = date(pub_date)
)

#### Save the File to a .csv for future use ====
write_csv(df, "data/2002_2010_aspi.csv")

#### Tidy Text in tibble ====
## get the first sentence and put it a header column
df1 <- df %>%
  rowwise() %>%
  mutate (header = text %>% str_extract("^(.*?\\.)"), .before =description)

df1 <-  df1 %>% group_by(pub_date) %>% filter(!duplicated(id))  # remove any duplicates

# There is still some tidying to do - authour etc

txt_df <- df1 %>% ungroup() %>%mutate(
  key = c(1:258),
  pub_by = 'ASPI', .before = author
)

txt_df[248:258,] %>%
mutate(pub_by = 'Dept of Defence'))

txt_df[248:258,] <- txt_df[248:258,] %>% mutate(
  pub_by = 'Dept of Defence'
)

txt_df %>% tail(15)

## Prelim EDA ====
txt_df %>%
  filter(year(pub_date) == 2009) %>% group_by(pub_by) %>% count()


p1 <- txt_df %>% group_by(pub_by, year(pub_date)) %>%
  filter(year(pub_date) > 2001 & year(pub_date) <= 2010) %>%
  count() %>%
  ggplot(aes(`year(pub_date)`, n)) +
  geom_col(aes(fill = pub_by)) +
  geom_text(aes(label = n), vjust = -.8) +
  theme_light() +
  labs(title = "Number of Articles Published") +
  ylab("Number") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(2002:2010),
                     name = "Year")

p1




