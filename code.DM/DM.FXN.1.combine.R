
rm(list=ls())

fxn. <- bind_rows(
  read_rds('DATA derived/fxn.coms.rds') ,
  read_rds('DATA derived/fxn.child.rds') ,
  read_rds('DATA derived/fxn.lcms.rds') , 
  read_rds('DATA derived/fxn.track.rds'), 
  read_rds('DATA derived/fxn.controls.rds') 
) %>%
  mutate( status = tolower(status)) %>% 
  mutate( status = factor (status, c('patient', 'carrier', 'control','unknown'))) 
# filter( status == 'patient', is.na(gaa1))
# mutate( mutation = pm )

# mark external patients --------------------------------------------------

fxn. %<>% .ug %>% 
  mutate( study = ifelse( grepl('NoID', sjid), 'external', study )) %>% 
  mutate( study = ifelse( grepl('NAF-', sjid), 'external', study )) %>%
  mutate( study = ifelse( (grepl('^6', sjid) & is.na(sex) ), 'external', study )) %>%
  mutate( study = ifelse( sjid %in% c('M68155','M68157','82266'), 'external', study )) %>%
  mutate( study = ifelse( study == 'FACHILD' & status == 'control', 'external', study ) ) %>% 
  mutate( study = ifelse( study == 'FACHILD' & status == 'carrier', 'external', study ) ) %>% 
  droplevels()
# filter( study == 'external' ) %>%
# filter( status %in% c('patient') ) %>% select(-status) %>% 
# filter( !   ) %>% 
# filter( !)  %>%
# select( study, sjid, dob, sex, aoo, gaa1, gaa2, pm ) %>% 
# unique

fxn. %>%
  select( study, status ) %>% .tab

# remove wrong samples ----------------------------------------------------
# per email DL 2025-07-07

fxn. %<>% 
  filter  ( !sjid %in% c(   43) ) %>% 
  filter  ( !sjid %in% c(  418, 5056, 4759, 4240 ) ) %>% 
  filter  ( !sjid %in% c( 4535, 4735 ) ) # unidentified duplicate


# make sure no dups -------------------------------------------------------

duplicates <- .rt('../DATA other/FACOMS.duplicates.txt') %>% 
  filter(!is.na(sjid.d1))  %>%
  mutate(across(c(sjid, sjid.d1), as.character))

fxn. %>% 
  filter(sjid %in% duplicates$sjid.d1)

rm(duplicates)

# mutliple (identical) measures -------------------------------------------------------

fxn. %<>%
  group_by(sjid, tissue, type, value) %>%
  arrange(sjid, tissue, type, value) %>%
  # filter(n()>1) %>%
  # .p
  slice(1) 


# remove 2 duplicates (TRACKFA) -----------------------------------------
# and UNIFAI if it was already in FACHILD

fxn. %<>% 
  # filter(sjid == 136) %>% 
  group_by(across(-study)) %>% 
  # filter(n()>1) %>% 
  slice_min(order_by = factor(study, levels = c("FACHILD", "UNIFAI", "FACOMS")), with_ties = FALSE) %>% 
  ungroup

# don't average multiple values -------------------------------------------------
# at least not here
# there are a lot external people here

# fxn. %>% 
#   filter( study != "external" ) %>% 
#   group_by( study, sjid, status, type, tissue ) %>%
#   filter  ( n()>1 ) %>% 
#   arrange ( sjid, study, tissue, type ) %>% .p
#   # mutate  ( value = mean(value, na.rm = TRUE) ) %>%
#   # slice_head(n = 1)  # Or distinct(across(...)) if needed

# sites -------------------------------------------------------------------

fxn. %<>% 
  # filter( !grepl('external', study) ) %>% 
  mutate( study.save = study ) %>% 
  mutate( study = ifelse( study %in% c('FACHILD', 'FACOMS'), 'UNIFAI', study) ) %>% 
  left_join(
    .dd('demo.l', c = T) %>% 
      select( study, sjid, site )
  ) %>% 
  # filter(!study == 'TRACKFA') %>% 
  select( study, site, everything()) %>% 
  mutate( study = study.save ) %>% 
  select( -study.save )

# unify IDs ---------------------------------------------------------------

fxn. %<>%
  # filter(sjid %in% c('5150','UFL021')) %>%
  left_join(
    .rt('../DATA other/cross.study.ids.txt')
    ) %>% 
  # filter( !is.na(medrioid) ) %>%
  mutate( save.id = ifelse( is.na( medrioid ), NA, sjid ) ) %>%
  mutate( sjid    = ifelse( is.na( medrioid ), sjid, medrioid )) %>% 
  select( -medrioid )

# corrections -------------------------------------------------------------

fxn. %<>%
  mutate( pm  = ifelse( sjid %in% c(4741, 4491, 5179, 4881, 4942, 4973, 5478, 5515), 'G130V', pm)) %>% 
  mutate( pm  = ifelse( sjid %in% c(4972, 87), 'L106S', pm)) %>% 
  mutate( pm  = ifelse(sjid == 4502, '165+5 G>C', pm)) %>% 
  mutate( pm  = ifelse(sjid ==  210, 'c.483-1G>T', pm)) %>% 
  mutate( pm  = ifelse(sjid == 5147, 'C.385-1G>c', pm)) %>% 
  mutate( pm  = ifelse(sjid == 5248, "Exon1 - c.169 del T - p.Ser57fs", pm)) %>% 
  mutate( pm  = ifelse(sjid == 5474, "unknown", pm)) %>% 
  mutate( aoo = ifelse(sjid ==  162, 20, aoo) ) %>% 
  droplevels()

# fxn. %>%
#   filter(status == 'patient') %>% 
#   filter(is.na(pm)) %>% .p
#   # filter(sjid == 4972)
#   filter(pm == 1)


# factors -----------------------------------------------------------------

fxn. %<>% 
  # .ug %>% select(status) %>% .tab
  mutate( study  = factor( study  , c('UNIFAI','FACHILD','TRACKFA','FACOMS','external','external (Ian)') )) %>% 
  mutate( tissue = factor( tissue , c('blood','buccal') )) %>% 
  mutate( status = factor( status , c('patient','carrier','control','unknown') )) %>% 
  mutate( type   = factor( type   , c('fxn.e','fxn.m','fxn.t') ))





