
# intro -------------------------------------------------------------------

rm(list = ls())

# source('code.DM/DM.FXN.2.post.process.R')
source('code.DM/DM.FXN.3.add.clinical.data.R')

rm(fxn.dt., dt.)

fxn. %<>% 
  mutate( analysis.group = factor( analysis.group, 
                                   c('lcms fxn.m', 'lcms fxn.e', 'dipstick fxn.m'),
                                   c('LCMS FXN-M', 'LCMS FXN-E', 'Lateral Flow FXN-M')
  )) %>% 
  filter( !is.na(sev.o) )

fxn. %>%
  select( analysis.group, sev.o ) %>% .tab

fxn. %>% select(analysis.group, study) %>% .tab


# tte dataset (not only ppl with LoA) -------------------------------------

steps.LoA <- .dd('steps') %>%
  filter( study == 'UNIFAI') %>% 
  # filter(sjid ==32) %>% 
  select( study, sjid, avisitn, fds, amb ) %>% 
  filter(study == 'UNIFAI') %>%
  filter( !is.na( amb )) %>% 
  group_by(sjid)

steps.LoA %<>%
  arrange ( sjid, avisitn ) %>% 
  mutate  ( event = ifelse( amb == 'non-amb.', 1, 0 )) %>% 
  group_by( sjid ) %>% 
  mutate  ( event = cummax( event ) ) %>% 
  select(-amb, -fds)

# convert into ages at this stage - otherwise some ages will be mi --------
# convert visits to ages 

steps.LoA %<>% 
  left_join( 
    .ds.UNIFAI('vf') %>% 
      mutate( adt = if_else( ( adt == as.Date('2015-05-13') & sjid == 4769), as.Date('2021-05-13'), adt ) ) %>% 
      select( sjid, avisitn, adt )
  ) %>% 
  left_join(
    .dd('demo') %>% select( study, sjid, dob ) %>% 
      mutate( dob = if_else( ( dob == as.Date('2006-01-01') & sjid ==   32), as.Date('1959-10-30'), dob ) )
  ) %>% 
  mutate( age = as.numeric( adt-dob )/365.25 ) %>% 
  select( study, sjid, age, event )

steps.LoA %<>% 
  filter(!is.na(age)) %>% 
  arrange(sjid, age)

# LoA censoring/events ----------------------------------------------------
# interval!

# left censored 
LoA.left.cens <- steps.LoA %>%
  filter  ( min( event ) == 1 ) %>% 
  filter  ( age == min (age) ) %>% 
  mutate  ( event = 2 )

# LoA dates 
LoA.events <- steps.LoA %>%
  group_by(sjid) %>%
  filter(any(event == 0) & any(event == 1)) %>%  # keep only transitions
  filter(row_number() == max(which(event == 0)) | 
           row_number() == min(which(event == 1)))

# right censored
LoA.right.cens <- steps.LoA %>% 
  filter ( max ( event ) == 0 ) %>% 
  filter ( age == max( age ) )

tte. <- bind_rows(
  LoA.left.cens,
  LoA.events,
  LoA.right.cens
)

rm(steps.LoA)

# prepare interval censoring ----------------------------------------------

tte.i <- tte. %>%
  # filter(sjid == 11) %>% 
  # mutate( ctype = event ) %>%
  spread(event, age) %>%
  transmute(
    study,
    sjid,
    t1 = case_when(
      !is.na(`1`) ~ `0`,      # interval-censored → lower bound
      !is.na(`0`) ~ `0`,      # right-censored → lower bound
      !is.na(`2`) ~ 0,        # left-censored → lower bound = 0
      TRUE ~ NA_real_
    ),
    t2 = case_when(
      !is.na(`1`) ~ `1`,      # interval-censored → upper bound
      !is.na(`2`) ~ `2`,      # left-censored → upper bound
      !is.na(`0`) ~ Inf,      # right-censored → upper bound = Inf
      TRUE ~ NA_real_
    )
  )

# check tte.i -------------------------------------------------------------

tte.i %>% 
  # filter(sjid %in% LoA.events$sjid) %>% 
  filter(is.na(t1) | is.na(t2))

filter(tte.i, t1>t2)


# add fxn data ------------------------------------------------------------

tte.i.g <- fxn. %>% 
  select( study, sjid, analysis.group, sev.o, aoo, gaa1, type, fxn = value ) %>% 
  left_join(
    tte.i
  ) %>% 
  select( study, sjid, analysis.group, sev.o, aoo, gaa1, fxn, t1, t2 ) 

# tte.i.g %>%
#   mutate( age = rowMeans(across(c(t1, t2)), na.rm = T) ) %>%
#   mutate( dur = age - aoo )

# tte.i.g %>% 
#   .gs %>% 
#   filter(n()>2)


rm(fxn., LoA.events, LoA.left.cens, LoA.right.cens)
