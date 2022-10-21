library(tidyverse)
library(haven)

N <- nrow(pr_df)

penalty_df <- pr_df %>% 
  mutate(d1 = treat/pr_score, #0 cuando treat=0, grande cuando treat=1 y pr_score~0 (penaliza FN)
         d0 = (1-treat)/(1-pr_score)) #0 cuando treat=1, grande cuando treat=0 y pr_score~1 (penaliza FP)

# non-normalized weights --------------------------------------------------

wt_df <- penalty_df %>% 
  mutate(y1 = d1*re78, #d1 * re78 (penalty de FN mult por el salario resultante)
         y0 = d0*re78, #d0 * re78
         ht = y1 - y0) #ATT

wt_df %>% 
  pull(ht) %>% 
  mean()

# Normalized weights ------------------------------------------------------

s1 <- sum(penalty_df$d1)
s0 <- sum(penalty_df$d0)

wt_norm_df <- penalty_df %>% 
  mutate(y1 = (d1*re78)/(s1/N),
         y0 = (d0*re78)/(s0/N),
         norm = y1 - y0)


wt_norm_df %>% 
  pull(norm) %>% 
  mean()

# trimming propensity score -----------------------------------------------

penalty_trimmed <- pr_df %>% 
  filter(between(pr_score, 0.1,0.9))%>% 
  mutate(d1 = treat/pr_score, #0 cuando treat=0, grande cuando treat=1 y pr_score~0 (penaliza FN)
         d0 = (1-treat)/(1-pr_score))

# Non normalized
wt_df_trimmed <- penalty_trimmed %>% 
  mutate(y1 = d1 * re78, #d1 * re78 (penalty de FN mult por el salario resultante)
         y0 = d0 *re78, #d0 * re78
         ht = y1 - y0)

wt_df_trimmed %>% 
  pull(ht) %>% 
  mean()

#Normalized

N_tr <- nrow(penalty_trimmed)
s1_tr <- sum(penalty_trimmed$d1)
s0_tr <- sum(penalty_trimmed$d0)

wt_norm_df_trimmed <- penalty_trimmed %>% 
  mutate(y1 = (d1*re78)/(s1_tr/N_tr),
         y0 = (d0*re78)/(s0_tr/N_tr),
         norm = y1 - y0)


wt_norm_df_trimmed %>% 
  pull(norm) %>% 
  mean()


