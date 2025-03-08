## -----------------------------------------------------------------------------
##
## Project: Summer Research Project
##
## Script purpose: Functions for text formatting and mining
##
## Date: 04/06/2024
## Author: BOD
##
## -----------------------------------------------------------------------------




## number_formatting
#' Format numbers in the data description returning the data with replaced text 
#' 
#' @param data the dataset 
#' 
#' 
number_formatting <- function(data){
  data <- data %>%  
    mutate(Description = gsub("(\\d+),\\s?(\\d{3})", "\\1\\2", Description)) %>% # replace d,ddd with dddd
    mutate(Description = str_replace_all(Description, "(\\d),(\\d{2})", "\\1.\\2")) %>% # replace d,dd with d.dd
    mutate(Description = gsub('”', '"', Description)) %>%
    mutate(Description = gsub('’', "'", Description)) %>%
    mutate(Description = gsub('€TM', 'm ', Description))
    return(data) 
} 

## Check function - Working 
number_data <- data.frame(Original = c('5,000', '52,002', '23,23', '1,23', '4, 234', '123, 512.4', '7’10” X 7’9”'))
number_data$Description <- number_data$Original
number_formatting(number_data)




## convert_feet_inches_numeric
#' Function to convert feet and inches (' '') to decimal, returing the replaced text in the description 
#' 
#' @param text free text containing room dimensions  
#' 
#' 
convert_feet_inches <- function(text) {
  
  # Ensure the input is a character vector
  if (is.data.frame(text)) {
    text <- as.character(text)
  }

  # Pattern to match feet and inches with various notation
  pattern1 <- "\\((\\d+)'(\\d+)\\s*x\\s*(\\d+)'(\\d+)\\)" # single mark dimensions 
  pattern2 <- "(\\d+)'\\s*(\\d+)(\"|''|’|')" # traditional ft inches marks 
  pattern3 <- "(\\d+)\\s*(ft)\\s*(\\d+)\\s*((in(ch(es)?)?)?|\")" # text feet inches 
  pattern4 <- "(\\d+)\\s*(\\\\')\\s*(\\d+)" # \' pickup
  pattern5 <- "(\\d+)\\s(\\d+)\\s*x\\s*(\\d+)\\s(\\d+)" # space between numbers dimension


  # Function to replace pattern1
  text <- str_replace_all(text, pattern1, function(x) {
    matches <- str_match(x, pattern1)
    feet1 <- as.numeric(matches[2])
    inches1 <- as.numeric(matches[3])
    decimal_feet1 <- feet1 + (inches1 / 12)
    feet2 <- as.numeric(matches[4])
    inches2 <- as.numeric(matches[5])
    decimal_feet2 <- feet2 + (inches2 / 12)
    return(sprintf("%.2fft x %.2fft ", decimal_feet1, decimal_feet2))
  })

  # Function to replace pattern2
  text <- str_replace_all(text, pattern2, function(x) {
    matches <- str_match(x, pattern2)
    feet <- as.numeric(matches[2])
    inches <- as.numeric(matches[3])
    decimal_feet <- feet + (inches / 12)
    return(sprintf("%.2fft ", decimal_feet))
  })
  
  # Function to replace pattern3
  text <- str_replace_all(text, pattern3, function(x) {
    matches <- str_match(x, pattern3)
    feet <- as.numeric(matches[2])
    inches <- ifelse(is.na(as.numeric(matches[4])), 0, as.numeric(matches[4]))
    decimal_feet <- feet + (inches / 12)
    return(sprintf("%.2fft ", decimal_feet))
  })
  
  # Function to replace pattern4
  text <- str_replace_all(text, pattern4, function(x) {
    matches <- str_match(x, pattern4)
    feet <- as.numeric(matches[2])
    inches <- ifelse(is.na(as.numeric(matches[4])), 0, as.numeric(matches[4]))
    decimal_feet <- feet + (inches / 12)
    return(sprintf("%.2fft ", decimal_feet))
  })
  
  # Function to replace pattern5
  text <- str_replace_all(text, pattern5, function(x) {
    matches <- str_match(x, pattern5)
    feet.1 <- as.numeric(matches[2])
    inches.1 <- as.numeric(matches[3])
    decimal_feet.1 <- feet.1 + (inches.1 / 12)
    feet.2 <- as.numeric(matches[4])
    inches.2 <- as.numeric(matches[5])
    decimal_feet.2 <- feet.2 + (inches.2 / 12)
    return(sprintf("%.2fft x %.2fft", decimal_feet.1, decimal_feet.2))
  })
  
  return(text)
}


    # ## Check functions 
    # room_txt_df <- read_excel('./Data/room_dimension_samples.xlsx')
    # room_txt_df <- number_formatting(room_txt_df)
    # room_txt_df$Description <- convert_feet_inches(room_txt_df$Description)



## remove_eircode
#' Remove eircodes from the description 
#' 
#' @param data data set with the descriprtion 
#' 
#' 
remove_eircode <- function(data){
  eircodes <- readr::read_csv('./Data/ericodes.csv')
  eircodes <- c(eircodes$eircodes_sf.EIRCOD)
  
  # create a pattern from all eircode (first 3 letters) followed by any 4 
  eircodes_ptn <- paste("(?i)(", paste(eircodes, collapse = "|"), ")\\s?\\w{4}") 

  # Use gsub to remove the matches from the description column
  data$Description <- gsub(eircodes_ptn, "", hd$Description, perl = TRUE)
  
  return(data)
  print("************************* EIRCODES REMOVED SUCCESSFULLY *************************")
}


## sqft_to_sqm
#' Convert sqm to sqft
#' 
#' @param size_in_ft data size variable in feet
#' 
#' 
sqft_to_sqm <- function(size_in_ft){
  size_in_m <- size_in_ft/10.7639
  return(size_in_m)
}


## property_size
#' Extract the sqm or sqft dimensions by adding columns sqft and sqm to the data
#' 
#' @param data data set with the descriprtion 
#' 
#' 
property_size <- function(data){
  sqft_pattern <- "(?i)(\\d+\\.?\\d*)\\s*(sq\\s*feet|sq\\.?\\s*ft|square\\s*feet|sq\\.\\s*ft|sq\\.ft|sq\\.\\s*feet|sq\\.?\\s*foot|sq\\s*ft|sq\\s*foot|Sq\\.\\s*Foot|square\\s*foot|sq\\s*/\\s*ft)"
  sqm_pattern <- "(?i)(\\d+\\.?\\d*)\\s*(square\\s*metre(s)?|square\\s*metre(s)?|metres|sq\\.?\\s*m|sq\\.?\\s*meters|sq\\.?\\s*mts|sq\\s*m|sq\\s*metres|sq\\.?\\s*mt|sq\\s*/m|m2|sqm|sq\\s*m|sq\\s*metres|sq\\.?\\s*mts|\\(m2\\))"
  
  # Extract the matches for ft 
  sqft_match <- str_match(data$Description, sqft_pattern)
  sqft_match <- sqft_match[,2]
  sqft_match <- as.numeric(sub("^\\.", "", sqft_match)) #remove leading dot for some observations 
  sqft_m_match <- round(sqft_match/10.7639,0) # convert ft to m
  
  # Extract the matches for m
  sqm_match <- str_match(data$Description, sqm_pattern)
  sqm_values <- as.numeric(sqm_match[,2])
  
  data <- data %>%
    mutate(sqft_m = sqft_m_match, sqm = sqm_values)
  return(data)
}




## property_size_mismatch
#' Return the df of the mimatches from property_size function
#' 
#' @param data data set with the descriprtion 
#' 
#' 
property_size_mismatch <- function(data){
  df <- data %>% select(Id, sqft_m, sqm)
  table(df$sqm == df$sqft_m)
  
  df <- df %>% filter(!sqm %in% c(sqft_m-10, sqft_m+10))
  return(df)
}



  # ## Check functions 
  # room_txt_df <- read_excel('./Data/room_dimension_samples.xlsx')
  # room_txt_df <- number_formatting(room_txt_df)
  # room_txt_df <- feet_converter(room_txt_df)
  # room_txt_df <- property_size(room_txt_df)
  # View(room_txt_df)



################## Room Size: Metres ################## 

room_pattern_no_m <- "(?i)(\\d*\\.?\\d+)\\s*m?\\w*\\.?\\s*x\\s*(\\d*\\.?\\d+)\\s*m?\\w*\\.?"
room_pattern_m <- "(?i)(\\d*\\.?\\d+)\\s*m.*?\\s*x\\s*(\\d*\\.?\\d+)\\s*m.*?"


## calculate_total_area_m
#' Return a column of total area calculations based on room dimensions in metres
#' 
#' @param description description containing room dimensions 
#' 
#' 
calculate_total_area_m <- function(description) {
  
  matches <- str_match_all(description, room_pattern_m)[[1]]
  if (length(matches) > 0) {
    areas <- as.numeric(matches[, 2]) * as.numeric(matches[, 3])
    total_area <- sum(areas)
  } else {
    total_area <- NA
  }
  return(total_area)
}


## calculate_total_area_no_m
#' Return a column of total area calculations based on room dimensions with no units (assumed metres)
#' 
#' @param description description containing room dimensions 
#' 
#' 
calculate_total_area_no_m <- function(description) {
  
  matches <- str_match_all(description, room_pattern_no_m)[[1]]
  if (length(matches) > 0) {
    areas <- as.numeric(matches[, 2]) * as.numeric(matches[, 3])
    total_area <- sum(areas)
  } else {
    total_area <- NA
  }
  return(total_area)
}


## review_total_area_m
#' Return the df of the mismatches from calculate_total_area_m and calculate_total_area_no_m functions
#' 
#' @param id data Id to investigate
#' 
#' 
review_total_area_m <- function(id) {
  text <- no_size[no_size$Id == id, ] # enter ID here to see extractions 
  
  contains_ft <- grepl('\\d+\\sft', text, ignore.case = TRUE)
  
  if (contains_ft){
    room_pattern <- room_pattern_m
  }else{
    room_pattern <- room_pattern_no_m
  }

  matches <- str_match_all(text$Description, room_pattern)[[1]]
  areas <- as.numeric(matches[, 2]) * as.numeric(matches[, 3])
  df <- data.frame(matches, areas)
  return(df)
}




room_pattern_ft <- "(?i)(\\d*\\.?\\d+)\\s*ft\\.?\\s*x\\s*(\\d*\\.?\\d+)\\s*ft\\.?"


## calculate_total_area_ft_m
#' Return a column of total area calculations based on room dimensions with no units (assumed metres)
#' 
#' @param description description containing room dimensions 
#' 
#' 
calculate_total_area_ft_m <- function(description) {
  matches <- str_match_all(description, room_pattern_ft)[[1]]
  if (length(matches) > 0) {
    areas <- as.numeric(matches[, 2]) * as.numeric(matches[, 3])
    total_area <- sum(areas)
  } else {
    total_area <- NA
  }
  total_area <- total_area/10.793
  return(total_area)
}


## review_total_area_ft_m
#' Return the df of the mismatches from calculate_total_area_ft_m function
#' 
#' @param id data Id to investigate
#' 
#' 
review_total_area_ft_m <- function(id) {
  text <- no_size[no_size$Id == id, ] # enter ID here to see extractions 
  matches <- str_match_all(text$Description, room_pattern_ft)[[1]]
  areas <- as.numeric(matches[, 2]) * as.numeric(matches[, 3])
  df <- data.frame(matches, areas)
  return(df)
}





## total_area_by_room
#' Append the data with total area in ft (as metres) and total area in metres  
#' 
#' @param data data with description of the room dimensions 
#' 
#' 
total_area_by_room <- function(data){
data <- data %>% 
  mutate(TotalArea_ft_m = sapply(Description, calculate_total_area_ft_m)) %>% # get overall size in ft (as metres)
  mutate(TotalArea_m = sapply(Description, calculate_total_area_m)) # get overall size in m

return(data)
}


    
    # Text the function, apply to each row of the test data 
    # room_txt_df <- total_area_by_room(room_txt_df)
    # View(room_txt_df)


