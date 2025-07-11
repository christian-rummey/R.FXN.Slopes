
rm(list=ls())

fxn. <- bind_rows(
  read_rds('DATA derived/fxn.coms.rds') ,
  read_rds('DATA derived/fxn.child.rds') ,
  read_rds('DATA derived/fxn.lcms.rds') , 
  read_rds('DATA derived/fxn.track.rds') 
) %>%
  mutate( status = tolower(status)) %>% 
  mutate( status = factor (status, c('patient', 'carrier', 'control','unknown'))) 
# filter( status == 'patient', is.na(gaa1))
# mutate( mutation = pm )

# number of samples -------------------------------------------------------

fxn. %>% 
  group_by( study, status, type, tissue ) %>% 
  summarise(
    subjects = length(unique(sjid)), 
    samples  = n()
    ) %>% 
  mutate( N = paste( subjects, '(', samples, ')')) %>% 
  select( -subjects, -samples ) %>% 
  spread( status, N)

# more than 1 value -------------------------------------------------------

fxn.multiple <- fxn. %>% 
  group_by( study, sjid, status, type, tissue ) %>% 
  filter  ( n()>1 )

# within subject variability
repro_summary <- fxn.multiple %>% 
  summarise(
    n.test = n(),
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    cv = sd_value / mean_value,
    .groups = "drop"
  )

ggplot(
  repro_summary %>% 
    filter(study != 'FACOMS'), 
  aes(x = mean_value, y = cv)) +
  aes(color =status)+
  geom_point(alpha = 0.5) +
  facet_wrap(~study, ncol = 2)+
  labs(title = "Reproducibility by CV",
       x = "Mean FXN Value",
       y = "Coefficient of Variation")+
  .leg('lr')

repro_summary %>%
  group_by(study, status, type, tissue) %>%
  summarise(
    mean_cv = mean(cv, na.rm = TRUE),
    sd_cv = sd(cv, na.rm = TRUE),
    n.tests =mean(n.test) ,
    n.subjects = n()
  )

rm(repro_summary, fxn.multiple)
# average multiple values -------------------------------------------------

fxn. %<>% 
  group_by( study, sjid, status, type, tissue ) %>%
  mutate(value = mean(value, na.rm = TRUE)) %>%
  slice_head(n = 1)  # Or distinct(across(...)) if needed

# boxplots ----------------------------------------------------------

fxn. %>% 
  filter( type != 'fxn.t') %>% 
  filter(study != 'FACOMS') %>% 
  ggplot()+geom_boxplot()+
  aes( fill = status)+
  aes( x = status )+
  aes( y = value )+
  facet_wrap(type~study)+
  .theme()

fxn. %>% 
  filter( type != 'fxn.t') %>% 
  filter(study != 'FACOMS') %>% 
  ggplot()+geom_boxplot()+
  aes( fill = status)+
  aes( x = status )+
  aes( y = value )+
  facet_wrap(type~study)+
  .theme()+
  .leg('none')


fxn. %>% 
  filter( type != 'fxn.t') %>% 
  filter(study != 'FACOMS') %>% 
  filter(status == 'control') %>% .p
  group_by( study, type, status ) %>% 
  mutate( value = log10(value)) %>% 
  summarise(median = median(value)) %>% 
  spread(type, median) %>% 
  arrange(status, study )



