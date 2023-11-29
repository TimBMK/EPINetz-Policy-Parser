# Processing manually coded Samples for the Policy Parser Evaluation 

{
  library(tidyverse)
  library(vroom)
  library(data.table)
  library(openxlsx)
  library(scales)
  library(irrCAC)
  library(irr)
}

#### MASI distance function from https://gdmcdonald.github.io/multi-label-inter-rater-agreement/Multi-Label_Agreement.html ####

#' Parse string into a character vector
#'
#' @param x string, e.g. "label_1, label_2"
#' @param sep separator, e.g. ", "
#'
#' @return character vector of labels, e.g. c("label_1", "label_2")
#' @export
#'
#' @examples
#' elements_from_string("l1, l2, l3", sep = ", ")
elements_from_string <- function(x, sep = ", ") {str_split(x,sep,simplify = F)[[1]]}

#' Measuring Agreement on Set-valued Items (MASI) distance from text string
#' MASI Similarity or Distance (pairwise)
#'
#' @param x Person x string of labels such as "label_1, label_2, label_3"
#' @param y Person y string of labels such as "label_4, label_1, label_5, label_7"
#' @param sep Label separator in the string, default = ", "
#' @param jaccard_only Only return Jaccard index instead of MASI (default = FALSE)
#' @param type one of "dist" or "sim" (default) for a distance or similarity score.
#'
#' @return Jaccard Distance between the two sets
#' @export
#'
#' @examples
#' masi("l1, l2, l3", "l7, l2")
masi <- function(x,y,sep = ", ", jaccard_only = F, type = "sim"){
  # Define the labels for each rater
  lab_x <- elements_from_string(x)
  lab_y <- elements_from_string(y)
  
  # compute set diff and intersection size
  diff_xy_size <- length(setdiff(lab_x,lab_y)) # number of elements in set x but not in set y
  diff_yx_size <- length(setdiff(lab_y,lab_x)) # number of elements in set y but not in set x
  intersection_size <- length(intersect(lab_x,lab_y)) # number of elements in common between two sets
  
  # monotonicity simillarity coefficient, M, see http://www.lrec-conf.org/proceedings/lrec2006/pdf/636_pdf.pdf Rebecca Passonneau. 2006. Measuring Agreement on Set-valued Items (MASI) for Semantic and Pragmatic Annotation. In Proceedings of the Fifth International Conference on Language Resources and Evaluation (LREC’06), Genoa, Italy. European Language Resources Association (ELRA).
  m_sim <- case_when(
    (diff_xy_size == 0) & (diff_yx_size == 0) ~ 1, # the sets are identical, return 1
    (diff_xy_size == 0) | (diff_yx_size == 0) ~ 2/3, # one set is a subset of the other, return 2/3
    (diff_xy_size != 0) & (diff_yx_size != 0) & (intersection_size !=0) ~ 1/3, # some overlap, some non-overlap in each set, return 1/3
    intersection_size ==0 ~ 0 # disjoint sets, return 0
  )
  
  # Calculate Jaccard simmilarity; J=1 means same, J=0 means no overlap at all. See https://en.wikipedia.org/wiki/Jaccard_index
  jaccard_sim <- intersection_size/(length(lab_x) + length(lab_y) - intersection_size)
  
  #MASI sim is M*J; MASI dist is 1-M*J
  masi_sim <- if_else(jaccard_only,
                      jaccard_sim,
                      m_sim*jaccard_sim)
  
  return(if_else(type == "sim",
                 masi_sim,
                 1-masi_sim))
}


MASI_simmilarity_matrix <- function(df, sep = ", ") {
  labels_all_combos <- sort(unique(unlist(df))) # alphabetical sorted list of all strings of labels
  
  num_label_combos <- length(labels_all_combos) # number of combinations above
  
  masi_sim_mat <- matrix(nrow = num_label_combos,
                         ncol = num_label_combos,
                         dimnames = list(labels_all_combos,
                                         labels_all_combos))
  
  for(i in 1:num_label_combos){
    for(j in 1:num_label_combos)
    {
      masi_sim_mat[i,j] <- masi(x = labels_all_combos[i],
                                y = labels_all_combos[j],
                                sep = sep)
    }}
  
  return(masi_sim_mat)
}
#####


# Read results

## Twitter

twitter_sample_1_eval <- read.xlsx("evaluation_samples/evaluated/twitter_sample_1.xlsx") %>% 
  #filter(!is.na(`_source.text`)) %>% # drop unclassified due to missing text
  #bind_rows(read.xlsx("evaluation_samples/evaluated/twitter_sample_1_missing.xlsx")) %>% 
  select(!c(`_source.created_at`, `_source.real_name`, `_source.text`)) %>% # drop unnecessary variables
  mutate(across(!c(`_id`, intercoder_sample), ~ case_when(!is.na(.) ~ TRUE, .default = FALSE))) %>% # TRUE/FALSE indicator
  mutate(coder = 1) 

twitter_sample_2_eval <- read_csv("evaluation_samples/evaluated/twitter_sample_2.csv",
                                  col_types = list(`_id` = "c")) %>% 
  #filter(!is.na(`_source.text`)) %>% # drop unclassified due to missing text
  # bind_rows(read_csv("evaluation_samples/evaluated/twitter_sample_2_missing.csv",
  #                    col_types = list(`_id` = "c"))) %>% 
  select(!c(`_source.created_at`, `_source.real_name`, `_source.text`, `...24`)) %>% # drop unnecessary variables
  mutate(across(!c(`_id`, intercoder_sample), ~ case_when(!is.na(.) ~ TRUE, .default = FALSE))) %>% # TRUE/FALSE indicator
  mutate(coder = 2)

twitter_sample_3_eval <- read.xlsx("evaluation_samples/evaluated/twitter_sample_3.xlsx") %>% 
  #filter(!is.na(`_source.text`)) %>% # drop unclassified due to missing text
  #bind_rows(read.xlsx("evaluation_samples/evaluated/twitter_sample_3_missing.xlsx")) %>% 
  select(!c(`_source.created_at`, `_source.real_name`, `_source.text`)) %>% # drop unnecessary variables
  mutate(across(!c(`_id`, intercoder_sample), ~ case_when(!is.na(.) ~ TRUE, .default = FALSE))) %>% # TRUE/FALSE indicator
  mutate(coder = 3) 

# Calculate intercoder reliability

## Twitter

intercoder_sample <- bind_rows(twitter_sample_1_eval, twitter_sample_2_eval,
                               twitter_sample_3_eval) %>% 
  #filter(duplicated(.$`_id`)|duplicated(.$`_id`, fromLast = T)) # get docs in all coder samples, i.e. intercoder sample
  filter(intercoder_sample == TRUE)

### MASI distance measures
intercoder_masi_data <- intercoder_sample %>% # get data into expected shape
  pivot_longer(!c(`_id`, coder), names_to = "coding") %>% # concatenate the codings
  filter(value == TRUE) %>% 
  reframe(coding = paste(coding, collapse = ", "), .by = c(`_id`, coder)) %>% 
  pivot_wider(names_from = coder, values_from = coding, # get into expected data format
              names_glue = "coder_{coder}") 

intercoder_masi_data_wt <- MASI_simmilarity_matrix(intercoder_masi_data %>% # calculate MASI similarity as weight
                                                     select(!`_id`), 
                                                   sep = ", ")
  
intercoder_masi <- krippen.alpha.raw(ratings = intercoder_masi_data %>% 
                                       select(!`_id`), 
                                     weights = intercoder_masi_data_wt, 
                                     categ.labels = rownames(intercoder_masi_data_wt),
                                     conflev = 0.95
)
  
intercoder_masi$est$coeff.val
  

### by policy field
intercoder_fields <- tibble()
for (field in names(intercoder_sample) %>% .[!. %in% c("_id", "coder", "intercoder_sample")]){
  alpha <- intercoder_sample %>% select(`_id`, field, coder) %>% 
    pivot_wider(names_from = `_id`, values_from = field) %>% 
    select(!coder) %>% 
    as.matrix() %>% 
    kripp.alpha("nominal") # T/F data could be considered nominal or ordinal, but results are identical
  
  alpha_1_2 <- intercoder_sample %>% select(`_id`, field, coder) %>% 
    filter(coder %in% c(1,2)) %>% 
    pivot_wider(names_from = `_id`, values_from = field) %>% 
    select(!coder) %>% 
    as.matrix() %>% 
    kripp.alpha("nominal") # T/F data could be considered nominal or ordinal, but results are identical
  
  alpha_1_3 <- intercoder_sample %>% select(`_id`, field, coder) %>% 
    filter(coder %in% c(1,3)) %>% 
    pivot_wider(names_from = `_id`, values_from = field) %>% 
    select(!coder) %>% 
    as.matrix() %>% 
    kripp.alpha("nominal") # T/F data could be considered nominal or ordinal, but results are identical
  
  alpha_2_3 <- intercoder_sample %>% select(`_id`, field, coder) %>% 
    filter(coder %in% c(2,3)) %>% 
    pivot_wider(names_from = `_id`, values_from = field) %>% 
    select(!coder) %>% 
    as.matrix() %>% 
    kripp.alpha("nominal") # T/F data could be considered nominal or ordinal, but results are identical
  
  intercoder_fields <- intercoder_fields %>% 
    bind_rows(tibble(field = field, 
                     alpha = alpha$value,
                     alpha_1_2 = alpha_1_2$value,
                     alpha_1_3 = alpha_1_3$value,
                     alpha_2_3 = alpha_2_3$value))
}

intercoder_fields %>% pull(alpha) %>% mean()


## add text for qualitative checks

intercoder_eval <- intercoder_sample %>% # get data into expected shape
  select(!intercoder_sample) %>% 
  pivot_longer(!c(`_id`, coder), names_to = "coding") %>% # concatenate the codings
  filter(value == TRUE) %>% 
  reframe(coding = paste(coding, collapse = ", "), .by = c(`_id`, coder)) %>% 
  pivot_wider(names_from = coder, values_from = coding, # get into expected data format
              names_glue = "coder_{coder}") %>% 
  left_join(read.xlsx("evaluation_samples/evaluated/twitter_sample_1.xlsx") %>% 
              select(`_id`, `_source.text`), by = "_id")


## re-export lackluster fields for clarification and reevaluation
twitter_intercoder_sample <- read.xlsx("evaluation_samples/twitter_sample_intercoder.xlsx")

problematic_tweets <- intercoder_sample %>%
  filter(
    if_any(
      intercoder_fields %>% filter(alpha <= 0.66) %>%
        pull(field),
      ~ . == TRUE
    )
  ) %>% pull(`_id`)

twitter_intercoder_sample %>% filter(`_id` %in% problematic_tweets) %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% # utf8 conversion
  write.xlsx(file = "evaluation_samples/twitter_recode_sample.xlsx")
