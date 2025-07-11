# intro -------------------------------------------------------------------

source('DM.FXN.combine.post.process.R')

fxn. %<>% 
  filter( !is.na(sev.o) )

fxn. %>%
  select( assay, sev.o ) %>% .tab

# symp on x ---------------------------------------------------------------

# fxn.tmp <- fxn. %>% 
#   mutate(group = paste( study, tissue, type )) %>% 
#   # filter( (status %in% c('Patient','homozygous','point mut.'))) %>%
#   # filter( (origin %in% c('buccal','blood'))) %>% 
#   # filter(study == 'FACOMS') %>% 
#   droplevels()

fxn. %>% select(analysis.group, study) %>% .tab

# 339 LoAs ----------------------------------------------------------------
# thats all FACOMS
# UNIFAI 392 NOV 2024
# 443 LoAs ----------------------------------------------------------------
# UNIFAI JUL 2025

# last ambulatory date 
last.amb.age.LoA.known <- .dd('steps') %>%
  select(study, sjid, avisitn, fds, amb) %>% 
  filter(study == 'UNIFAI') %>%
  group_by(sjid) %>%
  
  # limit to LoA people 
  filter  ( length(unique(amb))>1 ) %>%
  filter  ( amb == 'ambulatory') %>% filter(avisitn==max(avisitn)) %>%
  left_join( 
    .ds.UNIFAI('vf') %>% 
      select( sjid, avisitn, adt )
    ) %>% 
  # .add.time(tm='age', keepadt = T) %>%
  left_join(
    .dd('demo') %>% select(study, sjid, dob)
  ) %>% 
  mutate( age = as.numeric( adt-dob )/365.25 ) %>% 
  # filter(is.na(age)) %>% arrange(sjid, adt) %>% .p
  mutate  ( LoA.age = age) %>% ungroup %>%
  select (study, sjid, LoA.age, adt) %>% 
  unique %>% 
  .gs

# last visit of all of them. ----------------------------------------------
# is last ambulatory visit ok? - NOV24
# should be. if they are non-amb, the are in LoA
# LoA is not only from events. so we can censor here 
# (enrolled non-amb is missing)

LoA.all <- .dd('steps') %>% 
  filter(study == 'UNIFAI') %>% 
  left_join( 
    .ds.UNIFAI('vf') %>% 
      select( sjid, avisitn, adt )
  ) %>% 
  select(study, sjid, avisitn, adt, amb) %>% 
  group_by(sjid) %>% 
  # filter  ( length(unique(amb))>1 ) %>% 
  filter  ( amb == 'ambulatory') %>% filter(avisitn==max(avisitn)) %>% 
  # .add.time(tm='age', keepadt = T) %>%
  left_join(
    .dd('demo') %>% select(study, sjid, dob)
  ) %>% 
  mutate( age = as.numeric( adt-dob )/365.25 ) %>% 
  unique %>% 
  left_join( last.amb.age.LoA.known ) %>% 
  mutate  ( last.age = age) %>% ungroup %>% 
  select(sjid, LoA.age, last.age)

LoA.all %<>% 
  mutate(event = ifelse(is.na(LoA.age), 0, 1))

# fxn.tmp %<>%
#   filter(aoo<25) %>%
#   left_join(LoA)

# add LoA dates to fxn file--------------------------------------------------

LoA.all %>% 
  .gs %>% 
  filter(n()>1)

# LoA.all -> no study

fxn. %<>%
  left_join(LoA.all)

# for TTE: add fxn to LoA.all ---------------------------------------------
# with(fxn., table( analysis.group) )
# 
# LoA.all %<>% 
#   left_join(
#     fxn. %>%
#       ungroup %>% 
#       select(sjid, group, fxn) %>% 
#       spread(group, fxn)
#   )
# 
# LoA Age vs fxn, all 4  --------------------------------------------------

fxn. %>%
  filter( pm==0 ) %>%
  # filter( grepl( 'lcms', analysis.group ) & value > 12.5)
  mutate( analysis.group = factor( analysis.group, c('lcms fxn.m', 'lcms fxn.e', 'dipstick fxn.m'))) %>% 
  filter(!is.na(LoA.age)) %>%
  group_by( sjid ) %>% 
  ggplot()+geom_point()+
  aes( LoA.age, value )+
  # aes(value, LoA.age)+
  # ggpubr::stat_cor( data = filter(fxn.tmp, status %in% c('Patient','homozygous')), output.type	= 'tex',show.legend = F)+
  # geom_smooth     ( data = filter(fxn.tmp, status %in% c('Patient','homozygous')), method = 'lm', se = F, aes(group= 1))+
  facet_wrap(~analysis.group, ncol = 2, scales = 'free_y')+
  .theme(base_size = 16)+
  ggpmisc::stat_correlation(mapping = ggpmisc::use_label(c("R2", "P", "n")))+
  geom_smooth     (  method = 'lm', se = F, aes(group= 1))+
  # .leg_lrh+
  # ylim(0,1350)+
  # xlim(0,25)+
  # ylab('gaa1 (repeat length shorter allele)')+
  # xlab('FXN.l')+
  ggtitle('Correlation of Age at LoA with Frataxin Level')+
  .box

# boxplot LoA by FXN Percentiles ------------------------------------------

fxn. %>%
  filter(!is.na(LoA.age)) %>%
  # filter(LoA.age>40, study == 'FACHILD') %>%
  group_by(analysis.group) %>% filter(!is.na(value)) %>%
  mutate( fxn.cat = ntile(value,6)) %>% 
  # filter(is.na(fxn.cat))
  ggplot()+geom_boxplot()+
  aes(y = LoA.age, x = factor(fxn.cat) )+
  # aes(fill = fxn.cat)+
  # aes(value, LoA.age)+
  facet_wrap(~ analysis.group, ncol = 2)+
  theme_minimal(base_size = 16)+
  .leg('none')+
  .box+
  xlab('FXN Quantiles')+
  ylab('Age at LoA')+
  ggtitle('Age at LoA, Frataxin Level Quartiles')
  
# hist(fxn.$LoA.age)

# cox model, right away from censored data --------------------------------
# NOV 24

# dataset <- 'FACOMS blood NA'
# dataset <- 'FACOMS buccal NA'

tte. <- fxn. %>%
  filter(analysis.group == 'lcms fxn.m') %>% 
  # filter(last.age>LoA.age)
  # filter(!is.na(status)) %>% select(status) %>% table
  rename( fxn = value ) %>% 
  select( sjid, LoA.age, fxn, last.age, event, analysis.group ) %>% 
  filter( !is.na( fxn )) %>% 
  mutate( tm. = last.age ) %>% 
  select( sjid, tm., event, fxn ) %>% 
  .ug

shapiro.test(log10(tte.$fxn))
boxcox_result <- MASS::boxcox(lm(fxn ~ 1, data = tte.), lambda = seq(-2, 2, 0.1))

coxph( Surv( tte.$tm., tte.$event) ~ fxn, data = tte. ) %>%
  broom::tidy(exp = T, conf.int = T) 

coxph( Surv( tte.$tm., tte.$event) ~ log10(fxn), data = tte. ) %>%
  broom::tidy(exp = T, conf.int= T) 

tte.tmp <- tte. %>% 
  left_join(.dd('demo') %>% filter(study == 'UNIFAI') %>% select(sjid, sev.o)) %>% 
  filter(!is.na(tm.)) %>% 
  # mutate(fxn = ntile(fxn,5)) %>% 
  # mutate(fxn = ntile(fxn,5)) %>% 
  droplevels()

fit <- survfit( Surv( tte.tmp$tm., tte.tmp$event) ~ sev.o + fxn, 
                data = tte.tmp  
                )

fit %>% 
  survminer::ggsurvplot(data = tte.tmp, pval = T, censor = T, legend = 'none')



# GPT2 --------------------------------------------------------------------

LoA.all <- .dd('steps') %>% 
  filter(study == 'UNIFAI') %>% 
  select(study, sjid, avisitn, adt, amb) %>% 
  group_by(sjid) %>% 
  # filter  ( length(unique(amb))>1 ) %>% 
  filter  ( amb == 'ambulatory') %>% filter(avisitn==max(avisitn)) %>% 
  .add.time(tm='age') %>% 
  left_join(LoA) %>% 
  mutate  ( last.age = age) %>% ungroup %>% 
  select(sjid, LoA.age, last.age)

LoA.all %<>% 
  mutate( event = ifelse(is.na(LoA.age), 0, 1) ) %>% 
  rename( tm. = last.age ) %>% 
  select( -LoA.age ) %>% 
  filter( !is.na( tm. ))

fxn.surv <- fxn.tmp %>% 
  select   ( sjid, fxn, group ) %>% 
  left_join( LoA.all ) %>% 
  filter( !is.na( tm. ))

table(fxn.surv$group)

# Run Cox models by group and extract model summary statistics
model_results <- fxn.surv %>%
  # Group and nest data by `group`
  group_by(group) %>%
  nest() %>%
  # Add number of observations and number of events for each group
  mutate(
    n = map_int(data, nrow),
    events = map_int(data, ~ sum(.x$event)),
    # Apply Cox model to each dataset
    model = map(data, ~ coxph(Surv(tm., event) ~ fxn, data = .x)),
    # Extract model statistics
    results = map(model, ~ tidy(.x, exp = T, conf.int = TRUE, conf.level = 0.95))
  ) %>%
  # Unnest results to get a flat dataframe with model outputs
  select(group, n, events, results) %>%
  unnest(results) %>%
  select(group, n, events, term, estimate, conf.low, conf.high, p.value) %>% 
  .fixmod()

model_results %>% .ct

# GPT 1 ---------------------------------------------------------------------

cox_fit <- coxph(Surv(tm., event) ~ fxn, data = tte.)

# Check the model summary for the effect of fxn
summary(cox_fit, exp = T)

# Plot survival curves for illustrative purposes by categorizing `fxn` into groups
tte.$fxn_group <- cut(tte.$fxn, breaks = quantile(tte.$fxn, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                      include.lowest = TRUE, labels = c("Q1", "Q2", "Q3", "Q4"))

# Fit the survival model with fxn grouped into quartiles for visualization
fit <- survfit(Surv(tm., event) ~ fxn_group, data = tte.)

# Plot the Kaplan-Meier survival curves
ggsurvplot(fit, data = tte., pval = TRUE, censor = TRUE, 
           ggtheme = theme_minimal(base_size = 20), 
           legend.title = "fxn Quartile",
           palette = viridis::viridis(5),  # Adjust (4) to match the number of groups
           xlab = "Age", ylab = "Survival Probability", legend = 'top')

tte. %>% 
  left_join(
    .dd('demo') %>% filter(study == 'UNIFAI')%>% select(sjid, sev.o) ) %>% 
  select(fxn_group, sev.o) %>% table(exclude =F)

tte. %>% 
  left_join(
    .dd('demo') %>% filter(study == 'UNIFAI')%>% select(sjid, sev.o) ) %>%
  select(fxn_group, sev.o) %>% crosstable::crosstable(by=fxn_group, percent_digits = 0)


# cox model? --------------------------------------------------------------

fxn.tmp$group %>% table

tte <- fxn.tmp %>%
  mutate(gaa1 = gaa1/100) %>% 
  filter(group %in% c('FACHILD blood isoform.e','FACOMS blood NA') )  %>%
  left_join(
    LoA.all
  ) %>% 
  # filter(event == 0) %>% 
  filter(!is.na(event)) %>% # all others were enrolled non-amb.
  select( group, sjid, aoo, gaa1, fxn, time = LoA.age, event )

fit <- survfit(Surv(tte$time, tte$event)~group, data = tte)

fit %>% 
  ggsurvplot(data = tte, pval = T, censor = T)

table(tte$group)

tte.H <- filter(tte, group != 'FACOMS blood NA') 
tte.O <- filter(tte, group == 'FACOMS blood NA') 

results <- bind_rows(
  coxph(Surv(tte.H$time, tte.H$event)~ aoo  , data = tte.H ) %>% broom::tidy(exp = T, conf.int = T),
  coxph(Surv(tte.H$time, tte.H$event)~ gaa1 , data = tte.H ) %>% broom::tidy(exp = T, conf.int = T),
  coxph(Surv(tte.H$time, tte.H$event)~ fxn  , data = tte.H ) %>% broom::tidy(exp = T, conf.int = T),
  # coxph(Surv(tte.H$time, tte.H$event)~ aoo  + gaa1, data = tte.H ) %>% broom::tidy(exp = T, conf.int = T),
  # coxph(Surv(tte.H$time, tte.H$event)~ aoo  + fxn , data = tte.H ) %>% broom::tidy(exp = T, conf.int = T),
  # coxph(Surv(tte.H$time, tte.H$event)~ fxn  + gaa1, data = tte.H ) %>% broom::tidy(exp = T, conf.int = T),
  # coxph(Surv(tte.H$time, tte.H$event)~ fxn  + gaa1 + aoo , data = tte.H ) %>% broom::tidy(exp = T, conf.int = T),
  coxph(Surv(tte.O$time, tte.O$event)~ aoo  , data = tte.O ) %>% broom::tidy(exp = T, conf.int = T),
  coxph(Surv(tte.O$time, tte.O$event)~ gaa1 , data = tte.O ) %>% broom::tidy(exp = T, conf.int = T),
  coxph(Surv(tte.O$time, tte.O$event)~ fxn  , data = tte.O ) %>% broom::tidy(exp = T, conf.int = T),
  # coxph(Surv(tte.O$time, tte.O$event)~ aoo  + gaa1, data = tte.O ) %>% broom::tidy(exp = T, conf.int = T),
  # coxph(Surv(tte.O$time, tte.O$event)~ aoo  + fxn , data = tte.O ) %>% broom::tidy(exp = T, conf.int = T),
  # coxph(Surv(tte.O$time, tte.O$event)~ fxn  + gaa1, data = tte.O ) %>% broom::tidy(exp = T, conf.int = T),
  coxph(Surv(tte.O$time, tte.O$event)~ fxn  + gaa1 + aoo , data = tte.O ) %>% broom::tidy(exp = T, conf.int = T)
)
results %>% .ct


bind_rows(
  coxph(Surv(tte$time, tte$event)~ aoo, data = tte ) %>% broom::tidy(exp = T, conf.int = T),
  coxph(Surv(tte$time, tte$event)~ gaa1, data = tte ) %>% broom::tidy(exp = F, conf.int = T),
  coxph(Surv(tte$time, tte$event)~ fxn, data = tte ) %>% broom::tidy(exp = T, conf.int = T)
) %>% .fixmod()

.Last.value %>% .ct

# correct COX model with censoring ----------------------------------------

amb.sjids <- filter(LoA.all, event == 0) %>% select(sjid) %>% deframe

sjids.amb.last.ages <- .dd('steps') %>% 
  filter(sjid %in% amb.sjids) %>% 
  group_by(sjid) %>% 
  filter(avisitn == max(avisitn)) %>% 
  # ungroup() %>% select(fds.act) %>% table %>% 
  .add.time(tm = 'age') %>% 
  select(sjid, age)

fxn.tmp$group %>% table

tte <- fxn.tmp %>%
  mutate(gaa1 = gaa1/100) %>% 
  filter(group %in% c('FACHILD blood isoform.e','FACOMS blood NA') )  %>%
  left_join(
    LoA.all
  ) %>% 
  # filter(event == 0) %>% 
  filter(!is.na(event)) %>% # all others were enrolled non-amb.
  select( group, sjid, aoo, gaa1, fxn, time = LoA.age, event )

tte %>% 
  left_join(sjids.amb.last.ages) %>%
  mutate(time = ifelse(is.na(time), age, time)) %>%
  select(-amb, -age)

fit <- survfit(Surv(tte$time, tte$event)~group, data = tte)
fit

fit %>% 
  ggsurvplot(data = tte, pval = T, censor = T)

table(tte$group)

tte.H <- filter(tte, group != 'FACOMS blood NA') 
tte.O <- filter(tte, group == 'FACOMS blood NA') 

results <- bind_rows(
  coxph(Surv(tte.H$time, tte.H$event)~ aoo  , data = tte.H ) %>% broom::tidy(exp = T, conf.int = T),
  coxph(Surv(tte.H$time, tte.H$event)~ gaa1 , data = tte.H ) %>% broom::tidy(exp = T, conf.int = T),
  coxph(Surv(tte.H$time, tte.H$event)~ fxn  , data = tte.H ) %>% broom::tidy(exp = T, conf.int = T),
  # coxph(Surv(tte.H$time, tte.H$event)~ aoo  + gaa1, data = tte.H ) %>% broom::tidy(exp = T, conf.int = T),
  # coxph(Surv(tte.H$time, tte.H$event)~ aoo  + fxn , data = tte.H ) %>% broom::tidy(exp = T, conf.int = T),
  # coxph(Surv(tte.H$time, tte.H$event)~ fxn  + gaa1, data = tte.H ) %>% broom::tidy(exp = T, conf.int = T),
  # coxph(Surv(tte.H$time, tte.H$event)~ fxn  + gaa1 + aoo , data = tte.H ) %>% broom::tidy(exp = T, conf.int = T),
  coxph(Surv(tte.O$time, tte.O$event)~ aoo  , data = tte.O ) %>% broom::tidy(exp = T, conf.int = T),
  coxph(Surv(tte.O$time, tte.O$event)~ gaa1 , data = tte.O ) %>% broom::tidy(exp = T, conf.int = T),
  coxph(Surv(tte.O$time, tte.O$event)~ fxn  , data = tte.O ) %>% broom::tidy(exp = T, conf.int = T),
  # coxph(Surv(tte.O$time, tte.O$event)~ aoo  + gaa1, data = tte.O ) %>% broom::tidy(exp = T, conf.int = T),
  # coxph(Surv(tte.O$time, tte.O$event)~ aoo  + fxn , data = tte.O ) %>% broom::tidy(exp = T, conf.int = T),
  # coxph(Surv(tte.O$time, tte.O$event)~ fxn  + gaa1, data = tte.O ) %>% broom::tidy(exp = T, conf.int = T),
  # coxph(Surv(tte.O$time, tte.O$event)~ fxn  + gaa1 + aoo , data = tte.O ) %>% broom::tidy(exp = T, conf.int = T)
)
results %>% .ct


bind_rows(
  coxph(Surv(tte$time, tte$event)~ aoo, data = tte ) %>% broom::tidy(exp = T, conf.int = T),
  coxph(Surv(tte$time, tte$event)~ gaa1, data = tte ) %>% broom::tidy(exp = F, conf.int = T),
  coxph(Surv(tte$time, tte$event)~ fxn, data = tte ) %>% broom::tidy(exp = T, conf.int = T)
) %>% .fixmod()

.Last.value %>% .ct














