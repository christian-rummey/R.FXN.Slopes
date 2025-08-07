# ---
# output:
#   word_document:
#     reference_docx: "template.docx"
# ---

# intro -------------------------------------------------------------------
#     reference_docx: !expr here::here("template.docx")

rm(list=ls())

require(skimr)
require(moments)
library(corrr)
options(digits = 5)

fxn. <- read_rds(here("DATA derived/fxn.rds"))


# . -----------------------------------------------------------------------

fxn.tmp <- fxn. %>% 
  filter( type != 'FXN-T' ) %>% 
  # filter( grepl( 'TQ', analysis.group )) %>% 
  select( -analysis.group ) %>% 
  mutate( type = paste (type, unit) ) %>% 
  select(-unit) %>% 
  spread( type, value) %>%
  rename(
    FXN.M.LF = `FXN-M %`,
    FXN.M    = `FXN-M ng/ml`,
    FXN.E    = `FXN-E ng/ml`,
    ) %>%
  mutate( ratio =  `FXN.M`/`FXN.E` ) %>%
  ungroup

fxn.tmp %<>% 
  filter( status == 'patient' ) %>% 
  filter( pm == 0) %>% 
  select( -study, -pm, -status ) %>% 
  mutate_at(vars('sex'), as.factor)

fxn.tmp %<>% 
  mutate(gaa1.log = log10(gaa1)) %>% 
  mutate(gaa2.log = log10(gaa2)) %>% 
  mutate(FXN.M.log = log10(FXN.M)) %>% 
  mutate(FXN.E.log = log10(FXN.E)) %>% 
  mutate(FXN.M.LF.log = log10(FXN.M.LF))
  
my_skim <- skim_with(
  numeric = sfl(
    mean     = ~ mean(.x, na.rm=TRUE),
    sd       = ~ sd(.x,   na.rm=TRUE),
    median   = ~ median(.x, na.rm=TRUE),
    IQR      = ~ IQR(.x,    na.rm=TRUE),
    skewness = ~ skewness(.x, na.rm=TRUE)
  ),
  append = FALSE
)

# 2. Pipe your data into it
fxn.tmp %>%
  filter(is.na(FXN.M.LF.log)) %>% 
  select(-contains('FXN.M.LF')) %>% 
  .purge.df() %>% 
  my_skim()

fxn.tmp %>%
  filter(!is.na(FXN.M.LF.log)) %>% 
  select(-c(FXN.E, FXN.M, FXN.E.log, FXN.M.log)) %>% 
  .purge.df() %>% 
  my_skim()

# correlations ------------------------------------------------------------

fxn.tmp %>% 
  select( aoo, gaa1, gaa2, starts_with('FXN')) %>%
  correlate(use = "pairwise.complete.obs", ) %>%
  shave() %>%     # hides lower triangle & diagonal
  fashion()    

# regression models -----------------------------------------------------

fxn.tmp %>% 
  mutate(gaa1 = gaa1/100) %>% 
  mutate(gaa2 = gaa2/100) %>% 
  select(-c('gaa1.log', 'gaa2.log', 'ratio')) %>% 
  gather(type, value, starts_with('FXN.')) %>% 
  filter(!is.na(value)) %>% 
  group_by(type) %>% 
  nest() %>% 
  mutate( model = map(data , ~ lm ( value ~ gaa1 + aoo + sex, data = . )) ) %>% 
  mutate( coefs = map(model, ~ broom::tidy(.))) %>% 
  unnest(coefs) %>% 
  select(-data, -model) %>% 
  .fixmod() %>% 
  fashion(decimals = 3) %>% 
  flextable() %>% 
  hline(i = c(4), part = 'all', border = fp_border(color = "gray")) %>% 
  theme_booktabs()






