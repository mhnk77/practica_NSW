library(tidyverse)
library(haven)
library(MatchIt)

cps <- read_dta("https://raw.github.com/scunning1975/mixtape/master/cps_mixtape.dta") #
psid <- read_dta("https://raw.github.com/scunning1975/mixtape/master/nsw_mixtape.dta") #nsw_dw

#NSW -> data_id == 'Dehejia-...' & treat == 1
#PSID ->  data_id == 'Dehejia-...' & treat == 0
#CPS ->  data_id == 'CPS' & treat == 0

cps %>% #summary()
  count(treat)

psid %>% 
  count(treat)


# Bind rows and add new feats ---------------------------------------------

nsw_dw_cpscontrol <- cps %>% 
  bind_rows(psid) %>% 
  mutate(agesq = age^2,
         agecube = age^3,
         educsq = educ^2,
         u74 = ifelse(re74 == 0,0,1),
         u75 = ifelse(re75 == 0,0,1),
         interaction1 = educ*re74#,
         #re74sq = re74^2,
         #re75sq = re75^2,
         #interaction2 = u74*hisp
         )

# Tabla 1 -----------------------------------------------------------------

(tabla1 <- nsw_dw_cpscontrol %>% 
  filter(data_id == "Dehejia-Wahba Sample") %>% 
  select(-matches('sq|cube|interaction')) %>% 
  group_by(treat = factor(treat, levels = c(1,0))) %>% 
  summarise(across(where(is.numeric), mean),
            sample_size = n()) %>% 
   pivot_longer(names_to = 'variable', values_to = 'values', cols = -treat) %>% 
   pivot_wider(names_from = treat, values_from = values))

#coincide con tabla 1 del paper

# 2 ATE ---------------------------------------------------------------------

tabla1 %>% 
  filter(variable == 're78') %>% 
  summarise(ATE = `1`-`0`)  

#Comparar con tabla 2 renglon NSW
tabla1 %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>% 
  data.frame()

#Comparar con tabla 2 renglon Full CPS
nsw_dw_cpscontrol %>% 
  filter(data_id == "CPS1") %>% 
  select(-matches('sq|cube|interaction')) %>% 
  group_by(treat = factor(treat, levels = c(1,0))) %>% 
  summarise(across(where(is.numeric), mean),
            sample_size = n()) %>% 
  pivot_longer(names_to = 'variable', values_to = 'values', cols = -treat) %>% 
  pivot_wider(names_from = treat, values_from = values)%>%
  mutate(across(where(is.numeric), ~round(., 2))) %>% 
  data.frame()

#todo coincide excepto las variables de U74 y U75
#En el paper es el complemento i.e. U74[paper] = 0.29 = 1-.708 = 1 - U74[codigo]
## edit: modifiqué las variables en el código para que coincida
# Define model variables  ------------------------------------

psid_cov <- nsw_dw_cpscontrol %>% 
  select(age:re75, agesq:interaction1) %>% 
  names() 

(frml <- paste0(psid_cov, collapse = '+') %>% 
  paste('treat', ., sep = '~') %>% 
  as.formula()) 


# Logit models --------------------------------------------------------

## NSW vs PSID (esto no lo pide )

# logit_nsw <- nsw_dw_cpscontrol %>%
#   filter(data_id == "Dehejia-Wahba Sample") %>%
#   glm(frml, family = binomial(link = 'logit'), data = .)
# 
# 
# logit_nsw %>% summary()
# 
# nsw_dw_cpscontrol %>%
#   filter(data_id == "Dehejia-Wahba Sample") %>%
#   mutate(pr_score = logit_nsw$fitted.values) %>%
#   group_by(treat) %>%
#   summarise(across(pr_score, mean), sample_size = n())


## NSW vs CPS

logit_cps <- nsw_dw_cpscontrol %>% 
  filter(!(data_id == 'Dehejia-Wahba Sample' & treat == 0)) %>% #quitar los PSID, i.e. comparar NSW vs CPS
  glm(frml, family = binomial(link = 'logit'),
            data = .)

logit_cps %>% summary()


pr_df <- nsw_dw_cpscontrol %>% 
  filter(!(data_id == 'Dehejia-Wahba Sample' & treat == 0)) %>% #quitar los PSID, i.e. comparar NSW vs CPS
  mutate(pr_score = logit_cps$fitted.values)


pr_df %>% 
  group_by(data_id,treat) %>% 
  summarise(across(pr_score, mean)) #esto NO coincide con el paper, si con el libro

## "mean value of the propensity score for the treatment group is 0.43,
### and the mean for the CPS control group is 0.007."

pr_df %>% 
  group_by(data_id) %>% 
  summarise(
    p01 = quantile(pr_score, 0.01),
    p05 = quantile(pr_score, 0.05),
    p10 = quantile(pr_score, 0.10),
    p25 = quantile(pr_score, 0.25),
    p50 = quantile(pr_score, 0.50),
    p75 = quantile(pr_score, 0.75),
    p90 = quantile(pr_score, 0.90),
    p95 = quantile(pr_score, 0.95),
    p99 = quantile(pr_score, 0.99)
    
    ) %>% 
  pivot_longer(names_to = 'percentiles', values_to = 'values', cols = -data_id) %>% 
  pivot_wider(names_from = data_id, values_from = values)


# Coincide con el libro (en el paper no presentan esta tabla): 
### "The 50th percentile for the treatment group is 0.4"

# Histogram ---------------------------------------------------------------

pr_df%>% 
  ggplot(aes(pr_score, after_stat(density)), fill = factor(treat)) +
  geom_histogram(binwidth = 0.02)+ #el valor del eje y cambia de acuerdo al valor de binwidth
  facet_wrap(~treat)+
  xlim(c(-0.1,1.02))

## Se asemeja al histograma del libro (fig 5.3)

# Filter minimum ----------------------------------------------------------

min_treated <- pr_df %>% 
  group_by(treat) %>% 
  summarise(min = min(pr_score)) %>% 
  filter(treat == 1) %>% 
  pull(min)

pr_df %>% 
   filter(pr_score >= min_treated) %>% ##excluimos 12136 con este filtro
  ggplot(aes(pr_score, after_stat(density), fill = factor(treat), group = factor(treat)))+
  geom_histogram(position = "dodge",bins = 19, alpha = 0.95)#+
  # facet_wrap(~treat, ncol = 1)


# Matching ----------------------------------------------------------------

new_dta <-nsw_dw_cpscontrol %>% 
  filter(!(data_id == 'Dehejia-Wahba Sample' & treat == 0)) 


# Nearest Neighbor --------------------------------------------------------

mod_match_nn <- matchit(frml, method = 'nearest', data = new_dta,
                        distance = 'glm', link = 'logit')

dta_matched_nn <- match.data(mod_match_nn)

dta_matched_nn %>% 
  group_by(treat = factor(treat, levels = c(1,0))) %>% 
  summarise(across(where(is.numeric), mean),
            sample_size = n()) %>% 
  select(treat, re78) %>% 
  summarise(ATE = re78 -lead(re78)) %>% 
  drop_na()

# Weighted pr score   --------------------------------------------------

#esto falta!
## todo lo necesario está en 02_pr_score_weighting.R

# Coarsened exact -----------------------------------------------

mod_match_cem <- matchit(frml, method = 'cem', data = new_dta,
                         distance = 'glm', link = 'logit', estimand = 'ATE')

dta_matched_cem <- match.data(mod_match_cem, distance = 'pr_score')

dta_matched_cem %>% 
  group_by(treat = factor(treat, levels = c(1,0))) %>% 
  summarise(across(where(is.numeric), mean),
            sample_size = n()) %>% 
  select(treat, re78) %>% 
  summarise(ATE = re78 -lead(re78)) %>% 
  drop_na()



matched_prs <- bind_cols(
  pr_score = predict(logit_nsw, type = 'response', newdata = dta_m),
  treat = dta_m$treat
)

matched_prs %>% 
  ggplot(aes(pr_score))+
  geom_histogram(binwidth = .005)+
  facet_wrap(~treat, ncol = 1, scales = 'free_y')
  
  

