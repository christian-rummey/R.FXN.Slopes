
# intro -------------------------------------------------------------------

rm(list=ls())

require(labelled)
options(digits = 5)

source('code.DM/DM.FXN.2.post.process.R')

rm(dt.)
rm(fxn.)

fxn. %>% 
  filter( pm == 0 | is.na(pm) ) %>% 
  # unnest( data ) %>% 
  # filter( !is.na(bl.age) ) %>% # only one pt 
  # filter( paramcd == 'mFARS' ) %>% 
  group_by( study, analysis.group, sjid, type ) %>% 
  slice(1) %>% 
  group_by( study, analysis.group, status, type, sev.o ) %>% 
  summarise(n()) %>% .p
  
  group_by( st)
  filter( )
  
  mutate( visits = map_int(data, nrow) ) %>% 
  filter( visits < 2 )
  filter( type == 'FXN-M') %>% 
  mutate( NoFU = ifelse( visits < 2, T, F) ) %>% 
  mutate( pm   = ifelse( pm == 0   , F, T) ) %>% 
  select( sjid, analysis.group, NoFU, pm, value ) %>% 
  spread( paramcd, value ) %>% 
  filter( !NoFU, !pm ) %>% 
  .gs %>% arrange(sjid) %>% 
  filter(n()>2)
            
  arrange( visits ) %>% 
  filter(is.na(visits))
arrange(data)
  filter(nrow(data)==0)
  mutate()
  filter( pm != 0) %>% .ug %>% 
  select(sjid, analysis.group) %>% group_by(analysis.group) %>% unique %>% count()

fxn.dt. %>% 
  filter( status != 'patient' )

# step is only to clarify the n's ---------------------------------------------------------
# only one visit per patient (unless in both cohorts)
# 562 pts, including no-FU
# excluding PM 
# 118 in both cohorts

bl.tmp <- fxn.dt. %>% 
  filter( pm      == 0) %>% 
  filter( paramcd == 'mFARS') %>% 
  filter( type    == 'FXN-M') %>% 
  unnest( data ) %>%
  filter( dupline == FALSE ) %>%
  filter(!is.na(bl.age)) %>% # fixes 4560
  group_by( sjid, analysis.group ) %>% 
  mutate( follow.up = ifelse( n()>1, 'FU', 'No-FU' ) ) %>% 
  filter( avisitn == min(avisitn) ) %>% # leaves in 116 > 1 lines (are in both cohorts)
  .ug

# Step 1: Total per cohort (including everyone)
step0 <- bl.tmp %>%
  count(analysis.group, name = "n_total")

# Step 2: Remove No-FU
step1 <- bl.tmp %>%
  filter(follow.up != "No-FU") %>%
  count(analysis.group, name = "n_after_fu")

# Step 3: Remove Phase 2
step2 <- bl.tmp %>%
  filter(follow.up != "No-FU", phase == 1) %>%
  count(analysis.group, name = "n_after_phase1")

# Step 4: Combine into summary table
step_summary <- step0 %>%
  left_join(step1, by = "analysis.group") %>%
  left_join(step2, by = "analysis.group") %>%
  mutate(
    dropped_no_fu   = n_total - n_after_fu,
    dropped_phase2  = n_after_fu - n_after_phase1,
    final_n         = n_after_phase1
  )

step_summary

rm(bl.tmp)

# Demo Table --------------------------------------------------------------

pars. <- c('mFARS','FARS.E','FARS.B')

# FU data is the same for analysis group
# take out pms

dt.bl <- fxn.dt. %>% 
  filter( pm      == 0) %>% 
  filter( paramcd == 'mFARS') %>% 
  filter( type    == 'FXN-M') %>% 
  unnest( data ) %>%
  filter( dupline == FALSE ) %>%
  filter(!is.na(bl.age)) %>% # fixes 4560
  group_by( sjid ) %>% 
  mutate( age = bl.age + time. ) %>% # just for FU calculation
  select( study, sjid, age, time., phase, status, sev.o, sex, aoo, gaa1, gaa2, pm ) %>% 
  unique( ) %>% 
  mutate( 
    fu_t = max(age) - min(age),
    fu_v = n()
  ) %>% 
  mutate( NoFU = ifelse( fu_v == 1 , T , F ) ) %>% 
  mutate( fu_t = ifelse( NoFU , NA, fu_t ) ) %>% 
  mutate( fu_v = ifelse( NoFU , NA, fu_v ) ) %>% 
  filter( age == min( age ) ) %>% 
  unique()

fxn.group.sjid <- fxn.dt. %>% 
  filter( pm == 0) %>% 
  filter( type == 'FXN-M' ) %>% 
  .ug %>% 
  select(
    study, sjid, analysis.group
  ) %>% unique

dt.bl <- fxn.group.sjid %>% 
  left_join(
    dt.bl
  ) %>% 
  # left_join( .dd('demo') %>% select(study, sjid, dob) ) %>% 
  # mutate( bl_age = as.numeric( adt-dob )/365.25 ) %>% 
  rename( bl_age = age ) %>% 
  select( study, analysis.group, phase, sjid, NoFU, fu_v, fu_t, bl_age, sex, sev.o, aoo, gaa1, gaa2 )

dt.bl %<>%   
  mutate_at('analysis.group', factor, c('LF Cohort [%] FXN-M','TQ Cohort [ng/ml] FXN-M') ) %>%
  group_by( analysis.group )

# re-add Baselines --------------------------------------------------------

dt.bl.fars <- fxn.dt. %>% 
  filter( pm == 0) %>% 
  filter( type == 'FXN-M') %>% 
  group_by( analysis.group, paramcd, sjid ) %>% 
  unnest  ( data ) %>%
  filter  ( !is.na( bl.age) ) %>% 
  filter  ( avisitn == min( avisitn ) ) %>% 
  group_by(sjid, avisitn, analysis.group) %>% 
  select  ( analysis.group, study, sjid, paramcd, aval ) %>% 
  spread ( paramcd, aval )

dt.bl %<>% 
  left_join( dt.bl.fars ) %>% 
  droplevels

# demo.table --------------------------------------------------------------

.tab1.sub <- function (df, strata = NA )  {
  tb <- tableone::CreateTableOne(
    vars       = c('sev.o','sex',
                   'bl_age',
                   'aoo', 'gaa1', 'gaa2', 
                   'NoFU','fu_v' , 'fu_t', 
                   'phase','mFARS', 'FARS.E','FARS.B'),
    factorVars = c('sev.o','sex','phase', 'NoFU','analysis.group' ),
    strata     = strata,
    test = T,
    data       = df
  ) %>% 
    print(
      varLabels = T,
      nonnormal = c('bl_age', 'aoo','gaa1','gaa2','fu_v' , 'fu_t'),
      contDigits = 1, catDigits = 1,
      missing = T,
      explain = F, dropEqual= F,
      add.rownames = T) %>%
    data.frame() %>% rownames_to_column() %>%
    flextable() %>% 
    width(width = 7.4/3) %>% 
    fontsize(size = 18, part = "all") %>% 
    height_all(7.10/13, part = "all") 
  return (tb)
}

my.ft <- .tab1.sub( 
  dt.bl %>% 
    mutate( phase = factor(phase, c('2','1', labels = c('non-amb.', 'ambulatory'))) ) %>% 
    filter( !NoFU ) %>% 
    droplevels(), 
  strata = c('analysis.group') 
)


my.ft %<>%
  theme_booktabs() %>%  # clean modern style
  autofit() %>%          # adjust column widths to content
  align(align = "left", part = "all") %>%
  valign(valign = "top", part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  fontsize(size = 10, part = "all") %>%
  padding(padding = 4, part = "all") %>%
  border_remove() %>%
  border_outer() %>%
  border_inner()


doc <- read_docx() %>%
  body_add_flextable(my.ft)

# .st("Table 1")



