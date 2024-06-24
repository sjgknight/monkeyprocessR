A set of scripts, with heavy support from stackoverflow and chatgpt, to process likert-style questions from surveymonkey into attractive stacked bar.

There are various packages for likert items, but many assume you have the raw data, and would require extracting unexported functions and some rewriting to process pre-summarised data.


#### chatGPT shiny request

Can you convert these Rmd chunks into a basic Shiny app with the following interactive elements:
1. takes as input an xlsx file which is processed using split_df function and extract_tables
2. displays the extract_tables output, and allows the user to input which tables from the q_tables list should be processed by likert_that 
3. once '2' input is set, a button to run likert_that and rbind, to obtain likert_combined
4. buttons to allow the user to download likert_combined and likert_long
5. some input boxes to set the HH features including 'main', cex, col
6. using the input in 5, a button to run HH::likertplot and display the output (with option to download in png or svg format)


```{r setup}
pacman::p_load(dplyr,magrittr,tidyr,ggplot2, purrr)

source('tidysheets.R')

table_raw<- readxl::read_excel("data/Survey71_singlesheet.xlsx",
                               skip = 1,
                               col_names = FALSE,
                               col_types = "text")

```

```{r process}

display_table_shape(table_raw)

# Get the tables roughly
split_table <- table_raw %>%
    split_df(complexity = 2) 

# Extract the tables broadly in a good format
q_tables <- extract_tables(split_table)

# Convert the tables to a wide tidy format
likert_wide <- map2(
    .x = q_tables[c(4:12)],
    .y = names(q_tables[c(4:12)]),
    .f = likert_that
  )


likert_combined <- do.call(rbind,likert_wide)
  
likert_long <- likert_combined %>%
  select(c(q:responses_strongly_agree)) %>%
  tidyr::gather(data, group, responses_strongly_disagree:responses_strongly_agree) %>%
  arrange(factor(q, levels = c("responses_strongly_disagree", "responses_disagree", "responses_neither_agree_nor_disagree", "responses_agree", "responses_strongly_agree"))) %>% 
  mutate(q=factor(q, levels=unique(q)))


```

```{r plot}

pacman::p_load(HH)

n_sizes <- get_n_sizes(likert_combined)


data_sum <- likert_combined %>%
  dplyr::rename(Item = q) %>%
    dplyr::select(Item:responses_strongly_agree) %>%
    mutate(across(c(-Item), ~as.numeric(.x)*100)) %>%
  dplyr::rename("Strongly Disagree" = responses_strongly_disagree,
                "Disagree" = responses_disagree,
                "Neither agree nor disagree" = responses_neither_agree_nor_disagree, 
                "Agree" = responses_agree,
                "Strongly Agree" = responses_strongly_agree)
  
data_sum <- data_sum %>%
  rowwise() %>%
  mutate(Item_orig = Item,
         Item = stringr::str_wrap(Item_orig, width = 50))


data_sum <- data_sum %>%
  mutate(Item = factor(Item, levels = c(Item)))

x <- HH::likertplot(data_sum[2:6], 
               horizontal=TRUE,
               main="NTEU member feedback",
               sub= list(paste0(n_sizes),
                         cex = .4,
                         col = "blue"),
               xlab = list("%",
                           cex = .4),
               #ylab = "Q",
               
               par.strip.text=list(cex=.4, lines=5, rotate=30),
               auto.key=list(space="right", 
                             columns=1,
                             cex = .5), 
               #padding.text=2),
               strip = FALSE,
               add.text = T,
               xlim = c(-100,100),
               scales = list(x = list(rot = 45,
                                      cex = .6,
                                      at=seq(-90,90,10)),
                             y = list(cex = .6,
                                      labels = rev(c(data_sum$Item))))
)

```
