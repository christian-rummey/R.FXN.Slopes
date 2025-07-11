
# symp on x ---------------------------------------------------------------

fxn.tmp <- fxn. %>% 
  filter(!is.na(aoo), !is.na(gaa1), !is.na(fxn), is.finite(fxn)) %>% 
  mutate(pm = ifelse(pm==0, 'homozygous', 'heterozygous')) %>% 
  mutate(type = ifelse(is.na(type), '', type)) %>% 
  filter(type   != 'mature') %>% 
  filter(!(origin == 'buccal' & study == 'FACOMS')) %>% 
  filter(status == 'patient')

fxn.tmp %>% 
  group_by(sjid, study) %>% filter(n()>1)

p1 <- fxn.tmp %>%
  ggplot()+geom_point()+
  aes(aoo, gaa1)+
  aes(color = pm)+scale_color_brewer( palette = 'Set1' )+
  ggpmisc::stat_correlation(mapping = ggpmisc::use_label(c("R2", "P", "n")), data = filter(fxn.tmp, pm=='homozygous'))+
  geom_smooth     ( data = filter(fxn.tmp, pm=='homozygous'), method = 'lm', se = F, aes(group= 1))+
  facet_wrap(~paste(study), ncol = 1)+
  theme_minimal(base_size = 16)+
  # .leg_lrh+
  ylim(0,1350)+
  # xlim(0,25)+
  ylab('gaa1 (repeat length shorter allele)')+
  xlab('age at onset')+
  .box+
  theme(legend.title = element_blank())
p1
# .ps(ti = title)

# symp on x, FACHILD ---------------------------------------------------------------

fxn.tmp <- fxn. %>% 
  filter(!is.na(aoo)) %>% 
  mutate(pm = ifelse(pm==0, 'homozygous', 'heterozygous')) %>% 
  mutate(type = ifelse(is.na(type), '', type)) %>% 
  filter(study == 'FACHILD') %>% 
  # filter(type   != 'mature') %>% 
  # filter(!(origin == 'blood' & study == 'FACOMS')) %>% 
  filter(status == 'patient')

p2 <- fxn.tmp %>% 
  filter(is.finite(fxn)) %>%
  ggplot()+geom_point()+
  aes(aoo, fxn)+
  aes(color = pm)+scale_color_brewer( palette = 'Set1' )+
  ggpmisc::stat_correlation(mapping = ggpmisc::use_label(c("R2", "P", "n")), data = filter(fxn.tmp, pm=='homozygous'))+
  geom_smooth     ( data = filter(fxn.tmp, pm=='homozygous'), method = 'lm', se = F, aes(group= 1))+
  facet_wrap(~paste(type), ncol = 1)+
  theme_minimal(base_size = 16)+
  # .leg_lrh+
  # ylim(0,1350)+
  xlim(0,40)+
  ylab('FXN')+
  xlab('age at onset')+
  theme(legend.title = element_blank())+
  .box
p2
# .sp(ti = 'title')

# symp on x, FACOMS ---------------------------------------------------------------

fxn.tmp <- fxn. %>% 
  filter(!is.na(aoo)) %>% 
  mutate(pm = ifelse(pm==0, 'homozygous', 'heterozygous')) %>% 
  mutate(type = ifelse(is.na(type), '', type)) %>% 
  filter(study != 'FACHILD') %>% 
  # filter(type   != 'mature') %>% 
  # filter(!(origin == 'blood' & study == 'FACOMS')) %>% 
  filter(status == 'patient')

p3 <- fxn.tmp %>% 
  filter(is.finite(fxn)) %>%
  ggplot()+geom_point()+
  aes(aoo, fxn)+
  aes(color = pm)+scale_color_brewer( palette = 'Set1' )+
  ggpmisc::stat_correlation(mapping = ggpmisc::use_label(c("R2", "P", "n")), data = filter(fxn.tmp, pm=='homozygous'))+
  geom_smooth     ( data = filter(fxn.tmp, pm=='homozygous'), method = 'lm', se = F, aes(group= 1))+
  facet_wrap(~paste(origin), ncol = 1)+
  theme_minimal(base_size = 16)+
  # .leg_lrh+
  # ylim(0,1350)+
  xlim(0,40)+
  ylab('FXN')+
  xlab('age at onset')+
  theme(legend.title = element_blank())+
  .box
p3
# .sp(ti = 'title')

# gaa1 on x, FACHILD ---------------------------------------------------------------

fxn.tmp <- fxn. %>% 
  filter(!is.na(aoo)) %>% 
  mutate(pm = ifelse(pm==0, 'homozygous', 'heterozygous')) %>% 
  mutate(type = ifelse(is.na(type), '', type)) %>% 
  filter(study == 'FACHILD') %>% 
  # filter(type   != 'mature') %>% 
  # filter(!(origin == 'blood' & study == 'FACOMS')) %>% 
  filter(status == 'patient')

p4 <- fxn.tmp %>% 
  filter(is.finite(fxn)) %>%
  ggplot()+geom_point()+
  aes(gaa1, fxn)+
  aes(color = pm)+scale_color_brewer( palette = 'Set1' )+
  ggpmisc::stat_correlation(mapping = ggpmisc::use_label(c("R2", "P", "n")), data = filter(fxn.tmp, pm=='homozygous'))+
  geom_smooth     ( data = filter(fxn.tmp, pm=='homozygous'), method = 'lm', se = F, aes(group= 1))+
  facet_wrap(~paste(type), ncol = 1)+
  theme_minimal(base_size = 16)+
  # .leg_lrh+
  # ylim(0,1350)+
  # xlim(0,40)+
  ylab('FXN')+
  xlab('GAA1')+
  theme(legend.title = element_blank())+
  .box
p4
# .sp(ti = 'title')

# gaa on x, FACOMS ---------------------------------------------------------------

fxn.tmp <- fxn. %>% 
  filter(!is.na(aoo)) %>% 
  mutate(pm = ifelse(pm==0, 'homozygous', 'heterozygous')) %>% 
  mutate(type = ifelse(is.na(type), '', type)) %>% 
  filter(study != 'FACHILD') %>% 
  # filter(type   != 'mature') %>% 
  # filter(!(origin == 'blood' & study == 'FACOMS')) %>% 
  filter(status == 'patient')

p5 <- fxn.tmp %>% 
  filter(is.finite(fxn)) %>%
  ggplot()+geom_point()+
  aes(gaa1, fxn)+
  aes(color = pm)+scale_color_brewer( palette = 'Set1' )+
  # scale_shape_manual(values= c(19,19,21,0,0))+
  ggpmisc::stat_correlation(mapping = ggpmisc::use_label(c("R2", "P", "n")), data = filter(fxn.tmp, pm=='homozygous'))+
  geom_smooth     ( data = filter(fxn.tmp, pm=='homozygous'), method = 'lm', se = F, aes(group= 1))+
  facet_wrap(~paste(origin), ncol = 1)+
  theme_minimal(base_size = 16)+
  # .leg_lrh+
  # ylim(0,1350)+
  # xlim(0,40)+
  ylab('FXN')+
  xlab('GAA1')+
  theme(legend.title = element_blank())+
  .box
p5
# .sp(ti = 'title')
