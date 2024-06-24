# functions from https://github.com/yusuzech/tidyverse_notes/blob/master/utility/read_excel_tables.md
# which answers q at https://stackoverflow.com/questions/58251748/how-to-efficiently-import-multiple-excel-tables-located-in-one-sheet-into-an-r-l

# utility function to get rle as a named vector
vec_rle <- function(v){
  temp <- rle(v)
  out <- temp$values
  names(out) <- temp$lengths
  return(out)
}

# utility function to map table with their columns/rows in a bigger table
make_df_index <- function(v){
  table_rle <- vec_rle(v)
  divide_points <- c(0,cumsum(names(table_rle)))
  table_index <- map2((divide_points + 1)[1:length(divide_points)-1],
                      divide_points[2:length(divide_points)],
                      ~.x:.y)
  return(table_index[table_rle])
}

# split a large table in one direction if there are blank columns or rows
split_direction <- function(df,direction = "col"){
  if(direction == "col"){
    col_has_data <- unname(map_lgl(df,~!all(is.na(.x))))
    df_mapping <- make_df_index(col_has_data)
    out <- map(df_mapping,~df[,.x])
  } else if(direction == "row"){
    row_has_data <- df %>%
      mutate_all(~!is.na(.x)) %>%
      as.matrix() %>%
      apply(1,any)
    df_mapping <- make_df_index(row_has_data)
    out <- map(df_mapping,~df[.x,])
  }
  return(out)
}

# sometimes different tables are close to each other without empty space in between
# in thoses cases we need to manually insert rows and columns in the dataframe in
# order to make split_df() function to work
insert_row <- function(df,at,on = "below"){
  df <- as_tibble(df)
  if(!is.integer(at)) stop("at should be a integer")
  if(on == "below"){

  } else if(on == "above"){
    at <- at - 1
  } else {
    stop("on should be either 'below' or above")
  }

  if(at > 0){
    df1 <- df[1:at,]
    df2 <- df[at+1:nrow(df)-at,]
    out <- df1 %>%
      add_row() %>%
      bind_rows(df2)
  } else if(at == 0) {
    df1 <- df[0,]
    df2 <- df[at+1:nrow(df)-at,]
    out <- df1 %>%
      add_row() %>%
      bind_rows(df2)
  } else {
    stop("Positon 'at' should be at least 1")
  }
  return(out)
}

insert_column <- function(df,at,on = "right"){
  df <- as_tibble(df)
  if(!is.integer(at)) stop("at should be a integer")
  if(on == "right"){

  } else if(on == "left"){
    at <- at - 1
  } else {
    stop("on should be either 'right' or left")
  }

  if(at > 0){
    df1 <- df[,1:at]
    df1[,1+ncol(df1)] <- NA
    df2 <- df[,at+1:ncol(df)-at]
    out <- df1 %>%
      bind_cols(df2)
  } else if(at == 0) {
    df1 <- df[,0]
    df1[,1] <- NA
    df2 <- df
    out <- df1 %>%
      bind_cols(df2)
  } else {
    stop("Positon 'at' should be at least 1")
  }
  return(out)
}


# split a large table into smaller tables if there are blank columns or rows
# if you still see entire rows or columns missing. Please increase complexity
split_df <- function(df,showWarnig = TRUE,complexity = 1){
  if(showWarnig){
    warning("Please don't use first row as column names.")
  }

  out <- split_direction(df,"col")

  for(i in 1 :complexity){
    out <- out %>%
      map(~split_direction(.x,"row")) %>%
      flatten() %>%
      map(~split_direction(.x,"col")) %>%
      flatten()
  }
  return(out)

}

# set first row as column names for a data frame and remove the original first row
set_1row_colname <- function(df){
  colnames(df) <- as.character(df[1,])
  out <- df[-1,]
  return(out)
}

#display the rough shape of table in a sheet with multiple tables
display_table_shape <- function(df){
  colnames(df) <- 1:ncol(df)

  out <- df %>%
    map_df(~as.numeric(!is.na(.x))) %>%
    gather(key = "x",value = "value") %>%
    mutate(x = as.numeric(x)) %>%
    group_by(x) %>%
    mutate(y = -row_number()) %>%
    ungroup() %>%
    filter(value == 1) %>%
    ggplot(aes(x = x, y = y,fill = value)) +
    geom_tile(fill = "skyblue3") +
    scale_x_continuous(position = "top") +
    theme_void() +
    theme(legend.position="none",
          panel.border = element_rect(colour = "black", fill=NA, size=2))
  return(out)
}


########################################################
########################################################
########################################################
# Thanks chatGPT. Processing the extracted tables.
########################################################
########################################################
########################################################
# Function to process each dataframe
process_df <- function(df) {
  # Extract the label
  label <- df[1, 1] %>% as.character()

  # Remove the first row
  df <- df[-1, ]

  # Set the new column names
  col_names <- df[1, ] %>% as.character()
  df <- df[-1, ]
  names(df) <- col_names

  # Return a named list with the label and processed dataframe
  list(label = label, data = df)
}

extract_tables <- function(df_list = split_table){
  processed_list <- map(df_list, process_df)

  # Extract labels and dataframes
  labels <- map_chr(processed_list, "label")
  dataframes <- map(processed_list, "data")

  # Name the dataframes in the list with their corresponding labels
  names(dataframes) <- labels

  return(dataframes)

}


########################################################
########################################################
########################################################
# Converting each to a wide fromat
########################################################
########################################################
########################################################
likert_that <- function(likert_qs, likert_name){
  #convert each of the tables to wide, with the Q in col 1, and then likert
  # likert_q <- test[[1]]
  # likert_name <- names(test[1])

  likert_q <- likert_qs
  #likert_name <- names(likert_qs)

  likert_q <- likert_q %>%
    janitor::clean_names() %>%
    rename(n = na)

  # Step 2: Reshape and tidy the data
  # Filter out 'Answered' and 'Skipped' rows temporarily
  summary_info <- likert_q %>%
    dplyr::filter(is.na(`answer_choices`))
  response_data <- likert_q %>%
    dplyr::filter(!is.na(`answer_choices`))

  # Pivot the data to wide format
  response_data <- response_data %>%
    pivot_wider(
      names_from = answer_choices,
      values_from = c(responses, n),
      names_sep = "_"
    )

  # Combine the data back with 'Answered' and 'Skipped'
  response_data <- response_data %>%
    mutate(Answered = summary_info$n[summary_info$responses == "Answered"],
           Skipped = summary_info$n[summary_info$responses == "Skipped"])

  response_data <- response_data %>%
    mutate(Q = likert_name) %>%
    dplyr::relocate(Q) %>%
    rename(question = Q) %>%
    janitor::clean_names()

  response_data
}


#q unfortunately is also the base quit function so we'll backtick enclose it
get_n_sizes <- function(likert_combined){
  likert_combined %>%
  # Extract the unique question labels
  mutate(question = stringr::str_split_i(string = question, pattern = ". ", i = 1)) %>%
  # Group by the number of respondents
  group_by(answered) %>%
  # Summarize the questions with the same number of respondents
  summarise(
    question = paste(question, collapse = ", "),
    .groups = 'drop'
  ) %>%
  # Create a summary sentence
  mutate(summary = paste0(question, " have ", answered, " respondents")) %>%
  # Select the summary column
  pull(summary) %>%
  # Collapse into a single string with "; " as separator
  paste0(collapse = "; ")
}
