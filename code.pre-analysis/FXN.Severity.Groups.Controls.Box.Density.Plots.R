
# intro -------------------------------------------------------------------

source('code.DM/DM.FXN.2.post.process.R')
# source('code.DM/DM.FXN.3.add.clinical.data.R')
source('.project.settings.R')

fxn. %<>% 
  filter( type != 'FXN-T') %>% 
  droplevels()

# boxplot ---------------------------------------------------------

fxn.$assay %>% .tab
fxn. %>% select(assay, type, status) %>% .tab

fxn.tmp <- fxn. %>% 
  filter( type != 'FXN-T') %>% 
  filter( !is.na(sev.o) ) %>%
  filter( pm == 0  | is.na( pm )) %>%
  # filter( pm == 0 ) %>%
  mutate( fxn = value       ) %>%
  droplevels ()

fxn.tmp %>% 
  ggplot()+geom_boxplot(alpha = 1)+
  aes( y = fxn )+
  aes( x = sev.o )+
  aes(fill = sev.o)+ggsci::scale_fill_nejm()+
  facet_wrap( ~assay+type, ncol = 3, scales = 'free' )+
  scale_y_log10()+
  guides(color = guide_legend(nrow = 1, title = NULL))+
  guides(fill  = guide_legend(nrow = 1, title = NULL))+
  # xlab('Patient Subgroups, Carriers and Controls')+
  xlab('')+
  ylab('Frataxin Level (log 10)')+
  .leg('none')

# .sp('Supplementary Figure 1', l = '1s', i = 2)

# density -----------------------------------------------------------------

fxn.tmp %>% 
  filter(pm == 0) %>% 
  ggplot(aes(x = fxn)) +  # place aes here to apply globally
  geom_histogram(aes(y = ..density..), alpha = 0.4, bins = 30) +
  geom_density(alpha = 0.6, adjust = 1.2) +
  facet_grid(paste(assay, type)~., scales = 'free_x') +
  scale_x_log10() +
  guides(color = guide_legend(nrow = 1, title = NULL),
         fill  = guide_legend(nrow = 1, title = NULL))

# .sp('Histograms, log-scales')

fxn.tmp %>% 
  filter(pm == 0) %>% 
  ggplot(aes(x = fxn)) +  # place aes here to apply globally
  geom_histogram(aes(y = ..density..), alpha = 0.4, bins = 30) +
  geom_density(alpha = 0.6, adjust = 1.2) +
  facet_grid(paste(assay, type)~., scales = 'free_x') +
  .theme(base_size = 20) +
  guides(color = guide_legend(nrow = 1, title = NULL),
         fill  = guide_legend(nrow = 1, title = NULL))

# .sp('Histograms, normal scales')

