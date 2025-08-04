
# intro -------------------------------------------------------------------

source('code.DM/DM.FXN.2.post.process.R')
# source('code.DM/DM.FXN.3.add.clinical.data.R')
source('.project.settings.R')

fxn. %<>% 
  filter( type != 'FXN-T') %>% 
  droplevels()



fxn.tmp %>% 
  filter(pm == 0) %>% 
  ggplot()+geom_density(alpha = 0.5)+
  geom_histogram(alpha = 0.5)+
  aes( x = fxn )+
  # aes(fill = sev.o)+ggsci::scale_fill_nejm()+
  facet_wrap( ~assay+type, ncol = 1, scales = 'free' )+
  scale_x_log10()+
  .theme(base_size = 20)+
  guides(color = guide_legend(nrow = 1, title = NULL))+
  guides(fill  = guide_legend(nrow = 1, title = NULL))



fxn.tmp <- fxn. %>% 
  filter(!is.na(sev.o)) %>% 
  mutate( fxn = log(value)) %>%
  droplevels ()

fxn.sum <- fxn.tmp %>% 
  filter(!is.na(fxn)) %>% 
  group_by (analysis.group, sev.o, assay, type, unit) %>% 
  summarise(
    n = n(),
    fxn = median(fxn, na.rm = TRUE),
    q1 = quantile(fxn, 0.25, na.rm = TRUE, type = 8),
    q3 = quantile(fxn, 0.75, na.rm = TRUE, type = 8),
    iqr = IQR(fxn, na.rm = TRUE),
    .groups = "drop"
  )

fxn.sum %>%.ug %>% 
  group_by( assay, type ) %>% 
  # group_by( analysis.group ) %>% 
  mutate  ( 
    control.value = max(fxn),
    severe.value = (min(fxn))
  ) %>% 
  mutate( pct_of_control = 100*fxn/control.value) %>% 
  mutate( group_mult     = fxn/severe.value) %>% 
  select( -control.value, -severe.value) %>% 
  select( -assay ) %>%
  .ct
# pivot_wider(
#   names_from = c(analysis.group),
#   values_from = c(pct_of_control, group_mult)
# )
# spread()





# .sp(l = '1s', i = 2)
# .sp()

# gaa vs fxn --------------------------------------------------------------

fxn.tmp %>% 
  # filter( !(status == 'patient' & pm!=0) ) %>% 
  ggplot()+geom_point(alpha = 1)+
  aes( y = fxn )+
  aes( x = gaa1 )+
  aes(size = pm)+
  aes(color = pm)+scale_fill_brewer(palette = 'Set1')+
  # aes(fill = sev.o)+ggsci::scale_fill_nejm()+
  facet_wrap( ~assay+type, ncol = 3, scales = 'free' )+
  scale_y_log10()+
  # geom_vline( aes(xintercept = fxn, color = sev.o), data = fxn.sum ) + ggsci::scale_color_nejm()+
  .theme(base_size = 14)+
  guides(color = guide_legend(nrow = 1, title = NULL))+
  guides(fill  = guide_legend(nrow = 1, title = NULL))



