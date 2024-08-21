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


# Read results

## Twitter

twitter_sample_1_eval <- read.xlsx("evaluation_samples/evaluated/twitter_sample_1.xlsx") %>% 
  #filter(!is.na(`_source.text`)) %>% # drop unclassified due to missing text
  #bind_rows(read.xlsx("evaluation_samples/evaluated/twitter_sample_1_missing.xlsx")) %>% 
  select(!c(`_source.created_at`, `_source.text`)) %>% # drop unnecessary variables
  mutate(across(!c(doc_id, intercoder_sample, policy_field), ~ case_when(!is.na(.) ~ TRUE, .default = FALSE))) %>% # TRUE/FALSE indicator
  mutate(coder = 1) 

twitter_sample_2_eval <- read_csv("evaluation_samples/evaluated/twitter_sample_2.csv",
                                  col_types = list(doc_id = "c", intercoder_sample = "l")) %>% 
  #filter(!is.na(`_source.text`)) %>% # drop unclassified due to missing text
  # bind_rows(read_csv("evaluation_samples/evaluated/twitter_sample_2_missing.csv",
  #                    col_types = list(doc_id = "c"))) %>% 
  select(!c(`_source.created_at`, `_source.text`)) %>% # drop unnecessary variables
  mutate(across(!c(doc_id, intercoder_sample, policy_field), ~ case_when(!is.na(.) ~ TRUE, .default = FALSE))) %>% # TRUE/FALSE indicator
  mutate(coder = 2)

twitter_sample_3_eval <- read.xlsx("evaluation_samples/evaluated/twitter_sample_3.xlsx") %>% 
  #filter(!is.na(`_source.text`)) %>% # drop unclassified due to missing text
  #bind_rows(read.xlsx("evaluation_samples/evaluated/twitter_sample_3_missing.xlsx")) %>% 
  select(!c(`_source.created_at`, `_source.text`)) %>% # drop unnecessary variables
  mutate(across(!c(doc_id, intercoder_sample, policy_field), ~ case_when(!is.na(.) ~ TRUE, .default = FALSE))) %>% # TRUE/FALSE indicator
  mutate(coder = 3) 

twitter_sample_1_eval <- twitter_sample_1_eval %>% # recode intercoder for sample where different field/ID combinations lead to 2 additional intercoder rows
  mutate(id = paste0(doc_id, policy_field)) %>% # temporary ID
  mutate(intercoder_sample = case_when(
    id %in% (twitter_sample_3_eval %>% 
               transmute(id = paste0(doc_id, policy_field)) %>% 
               pull()) ~ TRUE,
    .default = FALSE
  )) %>% select(!id)

# ## recoded samples
# 
# intercoder_sample_old <- bind_rows(twitter_sample_1_eval, twitter_sample_2_eval, # intercoder sample pre-recode
#                                    twitter_sample_3_eval) %>% 
#   #filter(duplicated(.$doc_id)|duplicated(.$doc_id, fromLast = T)) # get docs in all coder samples, i.e. intercoder sample
#   filter(intercoder_sample == TRUE)
# 
# twitter_sample_recode_1 <- read.xlsx("evaluation_samples/evaluated/twitter_recode_sample_1.xlsx") %>% 
#   #filter(!is.na(`_source.text`)) %>% # drop unclassified due to missing text
#   #bind_rows(read.xlsx("evaluation_samples/evaluated/twitter_sample_1_missing.xlsx")) %>% 
#   select(!c(`_source.created_at`, `_source.real_name`, `_source.text`)) %>% # drop unnecessary variables
#   mutate(across(!c(doc_id, intercoder_sample), ~ case_when(!is.na(.) ~ TRUE, .default = FALSE))) %>% # TRUE/FALSE indicator
#   mutate(coder = 1) 
# 
# twitter_sample_1_eval <- twitter_sample_1_eval %>% # replace old evaluation with recoding
#   filter(!(doc_id %in% twitter_sample_recode_1$doc_id)) %>% 
#   bind_rows(twitter_sample_recode_1)
# 
# 
# twitter_sample_recode_2 <- read_csv("evaluation_samples/evaluated/twitter_recode_sample_2.csv",
#                                     col_types = list(doc_id = "c")) %>% 
#   #filter(!is.na(`_source.text`)) %>% # drop unclassified due to missing text
#   # bind_rows(read_csv("evaluation_samples/evaluated/twitter_sample_2_missing.csv",
#   #                    col_types = list(doc_id = "c"))) %>% 
#   select(!c(`_source.created_at`, `_source.real_name`, `_source.text`)) %>% # drop unnecessary variables
#   mutate(across(!c(doc_id, intercoder_sample), ~ case_when(!is.na(.) ~ TRUE, .default = FALSE))) %>% # TRUE/FALSE indicator
#   mutate(coder = 2)
# 
# twitter_sample_2_eval <- twitter_sample_2_eval %>% # replace old evaluation with recoding
#   filter(!(doc_id %in% twitter_sample_recode_2$doc_id)) %>% 
#   bind_rows(twitter_sample_recode_2)
# 
# 
# twitter_sample_recode_3 <- read.xlsx("evaluation_samples/evaluated/twitter_recode_sample_3.xlsx") %>% 
#   #filter(!is.na(`_source.text`)) %>% # drop unclassified due to missing text
#   #bind_rows(read.xlsx("evaluation_samples/evaluated/twitter_sample_3_missing.xlsx")) %>% 
#   select(!c(`_source.created_at`, `_source.real_name`, `_source.text`)) %>% # drop unnecessary variables
#   mutate(across(!c(doc_id, intercoder_sample), ~ case_when(!is.na(.) ~ TRUE, .default = FALSE))) %>% # TRUE/FALSE indicator
#   mutate(coder = 3) 
# 
# twitter_sample_3_eval <- twitter_sample_3_eval %>% # replace old evaluation with recoding
#   filter(!(doc_id %in% twitter_sample_recode_3$doc_id)) %>% 
#   bind_rows(twitter_sample_recode_3)

# Calculate intercoder reliability

## Twitter

intercoder_sample <- bind_rows(twitter_sample_1_eval, 
                               twitter_sample_2_eval,
                               twitter_sample_3_eval) %>% 
  #filter(duplicated(.$doc_id)|duplicated(.$doc_id, fromLast = T)) # get docs in all coder samples, i.e. intercoder sample
  filter(intercoder_sample == TRUE)


### Across all fields
intercoder_sample %>% mutate(id = paste0(doc_id, "_", policy_field)) %>% 
  select(id, correct, coder) %>% 
  pivot_wider(names_from = id, values_from = correct) %>% 
  select(!coder) %>% 
  as.matrix() %>% 
  kripp.alpha("nominal")


### by policy field
intercoder_fields <- tibble()
for (field in (distinct(intercoder_sample, policy_field) %>% pull())){
  alpha <- intercoder_sample %>% select(!intercoder_sample) %>% 
    filter(policy_field == field) %>% 
    pivot_wider(names_from = doc_id, values_from = correct) %>% 
    select(!coder) %>% 
    as.matrix() %>% 
    kripp.alpha("nominal") # T/F data could be considered nominal or ordinal, but results are identical
  
  # alpha_1_2 <- intercoder_sample %>% select(doc_id, field, coder) %>% 
  #   filter(coder %in% c(1,2)) %>% 
  #   pivot_wider(names_from = doc_id, values_from = field) %>% 
  #   select(!coder) %>% 
  #   as.matrix() %>% 
  #   kripp.alpha("nominal") # T/F data could be considered nominal or ordinal, but results are identical
  # 
  # alpha_1_3 <- intercoder_sample %>% select(doc_id, field, coder) %>% 
  #   filter(coder %in% c(1,3)) %>% 
  #   pivot_wider(names_from = doc_id, values_from = field) %>% 
  #   select(!coder) %>% 
  #   as.matrix() %>% 
  #   kripp.alpha("nominal") # T/F data could be considered nominal or ordinal, but results are identical
  # 
  # alpha_2_3 <- intercoder_sample %>% select(doc_id, field, coder) %>% 
  #   filter(coder %in% c(2,3)) %>% 
  #   pivot_wider(names_from = doc_id, values_from = field) %>% 
  #   select(!coder) %>% 
  #   as.matrix() %>% 
  #   kripp.alpha("nominal") # T/F data could be considered nominal or ordinal, but results are identical
  # 
  # alpha_old <- intercoder_sample_old %>% select(doc_id, field, coder) %>% 
  #   pivot_wider(names_from = doc_id, values_from = field) %>% 
  #   select(!coder) %>% 
  #   as.matrix() %>% 
  #   kripp.alpha("nominal") # T/F data could be considered nominal or ordinal, but results are identical
  # 
  # intercoder_fields <- intercoder_fields %>% 
  #   bind_rows(tibble(field = field, 
  #                    alpha = alpha$value,
  #                    alpha_1_2 = alpha_1_2$value,
  #                    alpha_1_3 = alpha_1_3$value,
  #                    alpha_2_3 = alpha_2_3$value,
  #                    alpha_old = alpha_old$value))
  
  intercoder_fields <- intercoder_fields %>% 
    bind_rows(tibble(field = field, 
                     alpha = alpha$value))
}

intercoder_fields %>% filter(field != "wirtschaft_finanzen_merge") %>% pull(alpha_old) %>% mean()
intercoder_fields %>% filter(field != "wirtschaft" & field != "haushalt_finanzen") %>% pull(alpha_old) %>% mean()

intercoder_fields %>% filter(field != "wirtschaft_finanzen_merge") %>% pull(alpha) %>% mean()
intercoder_fields %>% filter(field != "wirtschaft" & field != "haushalt_finanzen") %>% pull(alpha) %>% mean()


## add text for qualitative checks

intercoder_eval <- intercoder_sample %>% # get data into expected shape
  select(!intercoder_sample) %>% 
  mutate(score = sum(correct), .by = c(doc_id, policy_field)) %>% 
  filter(score != 3, score != 0) %>% # no full agreement (3 = all T, 0 = all F)
  pivot_wider(names_from = coder, values_from = correct, # get into expected data format
              names_glue = "coder_{coder}") %>% 
  left_join(read.xlsx("evaluation_samples/evaluated/twitter_sample_3.xlsx") %>% 
              select(doc_id, `_source.text`), by = "doc_id")


## re-export lackluster fields for clarification and reevaluation
twitter_intercoder_sample <- read.xlsx("evaluation_samples/twitter_sample_intercoder.xlsx")

problematic_tweets <- intercoder_sample %>%
  filter(
    if_any(
      intercoder_fields %>% filter(alpha <= 0.66) %>%
        pull(field),
      ~ . == TRUE
    )
  ) %>% pull(doc_id)

twitter_intercoder_sample %>% filter(doc_id %in% problematic_tweets) %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% # utf8 conversion
  write.xlsx(file = "evaluation_samples/twitter_recode_sample.xlsx")
