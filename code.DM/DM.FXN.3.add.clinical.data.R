
# intro -------------------------------------------------------------------

rm(list = ls())

source('code.DM/DM.FXN.2.post.process.R')

fxn. %<>% 
  # filter(study %in% c())
  filter  (
    status         == 'patient'
    )

dt. <- readRDS ( '../R.RCR.Modeling/DATA derived/fars.forslope.rds' ) %>% 
  filter(!dupline) %>% 
  filter(study == 'UNIFAI') %>% 
  mutate(amb = factor(phase.n, levels = c(1,2), labels = c('ambulatory','non-amb.')))

# 56 pts have no fars data (most are dipstick)
fxn. %<>% 
  # group_by(sjid) %>% 
  filter( sjid %in% unique(dt.$sjid) )

# summarise FU data -------------------------------------------------------

dt.ind <- dt. %>% select( study, sjid, adt, amb ) %>% 
  unique %>% 
  group_by( study, amb, sjid ) %>% 
  summarise(
    visits = n(),
    period = as.numeric(max(adt)-min(adt))/365.25
  )

bind_rows(
  dt.ind %>% 
    mutate( group = 'all.visits' ),
  dt.ind %>% 
    filter( visits > 1 ) %>% 
    mutate( group = 'more than 1 visit' ),
  dt.ind %>% 
    filter( sjid %in% c(fxn.$sjid) ) %>% 
    mutate( group = 'all.visits, fxn' ),
  dt.ind %>% 
    filter( visits > 1 ) %>% 
    filter( sjid %in% c(fxn.$sjid) ) %>% 
    mutate( group = 'more than 1 visit, fxn.' )
  ) %>% 
  group_by(group) %>% 
  summarise( 
    subjects  = length(unique(sjid)),
    fu.visits = mean(visits),
    fu.time   = mean(period)
  ) %>% 
  arrange()

rm(dt.ind)


# study  ------------------------------------------------------------------

fxn. %<>% 
  mutate(study = 'UNIFAI') %>% 
  select(study, analysis.group, sjid, status, sev.o, sex, aoo, gaa1, gaa2, pm, type, value, unit)

# combine dataset ----------------------------------------------------------

fxn. %>% 
  group_by( analysis.group ) %>% 
  summarise(length(unique(sjid)))

dt. %<>% 
  filter(sjid %in% unique(fxn.$sjid)) %>% 
  group_by( study, sjid, paramcd, amb ) %>% 
  nest()

# 1899+1152+1152

fxn.dt. <- dt. %>% 
  right_join(
    fxn.,
    relationship = "many-to-many"
    )

fxn.dt. %<>% 
  filter(!is.na(pm))

# %>% 
#   filter(is.na(gaa1)) %>% 
#   .p




