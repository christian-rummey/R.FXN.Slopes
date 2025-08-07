
# intro -------------------------------------------------------------------

# source('DM.FXN.combine.post.process.R')
fxn. <- read_rds('DATA derived/fxn.2.rds')


# density -----------------------------------------------------------------

fxn.tmp <- fxn. %>% 
  filter(!is.na(sev.o)) %>% 
  # mutate( fxn = log(value)) %>%
  mutate( fxn = value       ) %>%
  filter( !sev.o %in% c('control','carrier')) %>% 
  mutate( sev.o = NA) %>% 
  droplevels ()

fxn.sum <- fxn.tmp %>% 
  group_by (analysis.group, sev.o, assay, type) %>% 
  summarise(fxn = median(fxn)) 


fxn.tmp %>% 
  ggplot()+geom_density(alpha = 0.5)+
  aes( x = fxn )+
  # aes(fill = sev.o)+ggsci::scale_fill_nejm()+
  facet_wrap( ~assay+type, ncol = 1, scales = 'free' )+
  scale_x_log10()+
  geom_vline( aes(xintercept = fxn, color = sev.o), data = fxn.sum ) + ggsci::scale_color_nejm()+
  .theme(base_size = 10)+
  guides(color = guide_legend(nrow = 1, title = NULL))+
  guides(fill  = guide_legend(nrow = 1, title = NULL))
  # ylab('FXN, log10, [ng/ml]')+
  # .leg('none')

# boxplot ---------------------------------------------------------

fxn.tmp <- fxn. %>% 
  filter(!is.na(sev.o)) %>% 
  # mutate( fxn = log(value)) %>%
  mutate( fxn = value       ) %>%
  # filter( !sev.o %in% c('control','carrier')) %>% 
  # mutate( sev.o = NA) %>% 
  droplevels ()
# 
# fxn.sum <- fxn.tmp %>% 
#   group_by (analysis.group, sev.o, assay, type) %>% 
#   summarise(fxn = median(fxn)) 


fxn.tmp %>% 
  filter( !(status == 'patient' & pm!=0) ) %>% 
  ggplot()+geom_boxplot(alpha = 1)+
  aes( y = fxn )+
  aes( x = sev.o )+
  aes(fill = sev.o)+ggsci::scale_fill_nejm()+
  facet_wrap( ~assay+type, ncol = 3, scales = 'free' )+
  scale_y_log10()+
  # geom_vline( aes(xintercept = fxn, color = sev.o), data = fxn.sum ) + ggsci::scale_color_nejm()+
  .theme(base_size = 14)+
  guides(color = guide_legend(nrow = 1, title = NULL))+
  guides(fill  = guide_legend(nrow = 1, title = NULL))+
# ylab('FXN, log10, [ng/ml]')
  .leg('none')

# .sp(l = '1s', i = 2)
# .sp()

# gaa vs fxn --------------------------------------------------------------

# fxn.tmp %>% 
#   # filter( !(status == 'patient' & pm!=0) ) %>% 
#   ggplot()+geom_point(alpha = 1)+
#   aes( y = fxn )+
#   aes( x = gaa1 )+
#   # aes(size = pm)+
#   aes(color = pm)+scale_fill_brewer(palette = 'Set1')+
#   # aes(fill = sev.o)+ggsci::scale_fill_nejm()+
#   facet_wrap( ~assay+type, ncol = 3, scales = 'free' )+
#   scale_y_log10()+
#   # geom_vline( aes(xintercept = fxn, color = sev.o), data = fxn.sum ) + ggsci::scale_color_nejm()+
#   .theme(base_size = 14)+
#   guides(color = guide_legend(nrow = 1, title = NULL))+
#   guides(fill  = guide_legend(nrow = 1, title = NULL))



