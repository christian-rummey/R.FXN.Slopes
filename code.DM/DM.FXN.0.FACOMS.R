# require -----------------------------------------------------------------
# fxn.1 is the less organized sheet (althhough it may have more data)
# fxn.1 <- read_excel("DATA/Master FXN Sample Data DL112918(1).xlsx", sheet = "FXN Blood Data 2018", na=c("?","X")) %>% 
#   rename(
#     sjid    = 'FACOMS',
#     cdt     = "Date of Collection"
#   ) %>% 
#   mutate(cdt = as.Date(as.numeric(cdt), origin = as.Date('1899-12-30'))) %>% 
#   select(sjid, cdt, "Loaded", "FXN mABS", "GAM mABS", "FXN Corrected", "% of Average ctl", "Average...10")
# require 2 ---------------------------------------------------------------
# Only looking at patients from FA-COMS - to compare clinical data. 
 # using Average of % of average control (for plate differences) 
# I am using only frataxin columns, although there are more patients with data in here. 

rm(list=ls())

types <- rep('text', 27)
# types[3] <- 'date'

fxn.bucc   <- readxl::read_excel("../DATA/FACOMS FXN/Master FXN Sample Data DL112918(1).xlsx", sheet = "FXN BC Data 2018", na=c("?","X"), #) %>% 
                                 col_types = types) %>%
  select(
    sjid    = 'FACOMS ID',
    Initials = Initials,
    # cdt     = "Date of Collection",
    status  = 'Status',
    # pct.ctr = `% of Average Ctl`,
    fxn.pct = Average,
    n       = N
    ) %>% 
  filter( fxn.pct > 0)

types <- rep('text', 31)
# types[3] <- 'date'

fxn.blood   <- readxl::read_excel("../DATA/FACOMS FXN/Master FXN Sample Data DL112918(1).xlsx", sheet = "FXN Blood Data 2018", #) %>% 
                                  col_types = types) %>%
  select(
    sjid    = 'FACOMS',
    Initials = Initials,
    # cdt     = `Date of Collection`,
    status  = 'Status',
    # fxn.pct = `% of Average ctl`,
    fxn.pct = Average...10,
    n       = `N...11`
  ) %>% 
  filter( fxn.pct > 0)

fxn.coms <- bind_rows(
  fxn.bucc  %>% mutate( tissue = 'buccal'),
  fxn.blood %>% mutate( tissue = 'blood' )
) %>% 
  group_by( tissue )

rm(fxn.bucc, fxn.blood)

# fix initials -> ID ------------------------------------------------------

fxn.coms %<>% 
  mutate( sjid = ifelse (
    sjid %in% c('no ID','No ID'),
    'NoID', sjid )) %>%
  mutate( Initials = ifelse (
    is.na(suppressWarnings(as.numeric(sjid))) & sjid == 'NoID' & (is.na(Initials)|Initials=='?'),
    paste0('(', row_number(), ')'),
    Initials
    )) %>% 
  mutate( sjid = ifelse(
    sjid == 'NoID',
    paste0 ( sjid,'-', Initials ),
    sjid
  ))

# # look at this - IDs can not be the same across tissues
# fxn.coms %>%
#   filter(Initials %in% c('SMF', 'CW'))

# sometimes the average was done several times (same result, same n) -----
# fxn.bucc %>%
#   group_by( sjid ) %>%
#   filter( n()>1)
fxn.coms %<>%
  group_by( tissue, sjid, fxn.pct, n ) %>%
  arrange ( tissue, sjid, fxn.pct, n ) %>%
  slice   ( 1 )

# these are now real duplicate measures
fxn.coms %>% 
  group_by( tissue, sjid ) %>% 
  filter( n()>1 )

fxn.coms %<>% 
  mutate_at(vars('fxn.pct','n'), as.numeric)

# fix.carrier.control status ----------------------------------------------

fxn.coms %<>% 
  mutate( status = ifelse ( 
    sjid == 6061 & Initials == 'DPE', 'Carrier', status
    )) %>%
  mutate( status = ifelse ( 
    sjid %in% c('NoID-BK','NoID-DAM','NoID-DH','NoID-EB','NoID-JA','NoID-KT','NoID-LC') , 'Patient', status
  )) %>%
  mutate( status = ifelse ( 
    sjid %in% c(
      'NoID-AB','NoID-BG','NoID-CAM','NoID-CHS','NoID-CMC','NoID-CMS',
      'NoID-CS','NoID-DAT','NoID-DL','NoID-DP','NoID-ELZ','NoID-GG','NoID-JAT',
      'NoID-JES','6198','NoID-JRB','NoID-KAL','NoID-KG','NoID-KL','NoID-LAT','NoID-LG',
      'NoID-LM','NoID-LMP','NoID-MA','NoID-MD','NoID-MEB','NoID-MED','NoID-ML',
      'NoID-MPC','NoID-MPL','NoID-PP','NoID-RCK','NoID-RWM','NoID-SB','NoID-SH',
      'NoID-SL','NoID-TB'
      ) , 'Carrier', status
  )) %>%
  mutate( status = ifelse ( 
    sjid %in% c('6370','NoID-DERV','NoID-NAD','6191'), 'Control', status
  )) 
  

# fxn.coms %>% 
#   # more than one individual with same initials - leave as is
#   filter( !sjid %in% c('NoID-BB', 'NoID-CL','NoID-DD','NoID-DW','NoID-JB','NoID-JMP','NoID-KS','NoID-MM','NoID-RR','NoID-SH','NoID-SM','NoID-TM') ) %>% 
#   group_by( sjid, Initials ) %>% 
#   filter( length(unique(status))>1) %>% 
#   arrange ( Initials, sjid ) 

sjids <- fxn.coms %>% 
  filter( !is.na(suppressWarnings(as.numeric(sjid)) ) ) %>% 
  
  # these are all 6X numbers -ok
  filter( status != 'Control') %>%
  filter( status != 'Carrier') %>%
  filter( !(status == 'Unknown' & as.numeric(sjid)>6000) ) %>%
  
  # these need fixing
  group_by( sjid ) %>% 
  filter( status != 'Patient') %>% 
  .ug %>% select(sjid) %>% pull(sjid)

fxn.coms %<>% 
  mutate( status = ifelse ( 
    sjid %in% sjids , 'Patient', status
  ))

# correct duplicates ------------------------------------------------------

duplicates <- .rt('../DATA other/FACOMS.duplicates.txt') %>% 
  filter(!is.na(sjid.d1))  %>%
  mutate(across(c(sjid, sjid.d1), as.character))

fxn.coms %<>% 
  # filter(sjid %in% duplicates$sjid.d1) %>% 
  left_join(
    duplicates %>%
      filter(!is.na(study)) %>% 
      select(sjid = sjid.d1, duplicate.of = sjid)
  ) %>%
  mutate( sjid = ifelse( !is.na(duplicate.of), duplicate.of, sjid ) ) %>% 
  select( -duplicate.of )

rm(duplicates)

# add demo ----------------------------------------------------------------

fxn.coms %<>% 
  mutate( study = 'FACOMS' ) %>% 
  left_join(.dd('demo') ) %>% 
  mutate ( fxn.unit = '%' ) %>%
  rename ( fxn      = fxn.pct ) %>% 
  # select ( -fxn.mean ) %>% 
  select ( study, tissue, site, sjid, status, fxn.unit, everything())

fxn.coms %<>% 
  mutate( status = ifelse( status == 'control', 'Control', status ))

fxn.coms %>% 
  group_by(tissue, status) %>%
  summarise( n = n(), s = length(unique(sjid)))

fxn.coms %>% .ug %>% 
  mutate( type = 'fxn.m' ) %>% 
  select(study, status, sjid, dob, sev.o, sex, aoo, gaa1, gaa2, pm, tissue, type, value = fxn, unit = fxn.unit, n ) %>% 
  write_rds('DATA derived/fxn.coms.rds')




