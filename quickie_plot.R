# This plots whatever counts and fractions are in the prelim table if it has
# the right column names
prelim <- import('local/out/prelim_frail_LOINC.xlsx');

prelim00 <- prelim[,c('Category','Code','Name','n_all','frc_all','all_lci'
                      ,'all_uci')] %>% mutate(Group='overall') %>%
  rename_all(function(xx) gsub('all','frail',xx)) %>%
  rbind(prelim[,c('Category','Code','Name','n_frail','frc_frail','frail_lci'
                  ,'frail_uci')] %>% mutate(Group='frail')) %>%
  rename_all(function(xx) gsub('_|frail','',xx)) %>%
  subset(Category!='TOTAL') %>%
  mutate(Group=factor(Group,levels=c('overall','frail')
                      ,labels=c('Overall\nN=151,215\n'
                                ,'Frail (eFI > 0.2)\nN=13,095\n')));

plots <- sapply(unique(prelim00$Category)
                ,function(ii) subset(prelim00,Category==ii) %>%
                  mutate(Name=sapply(paste(Name,coalesce(Code,''),sep=' ')
                                     ,splitLine,18)) %>%
                  ggplot(aes(x=Name,y=frc,fill=Group)) +
                  geom_col(width=0.6,position=position_dodge(width=0.6)) +
                  geom_errorbar(aes(ymin=lci,ymax=uci)
                                ,position=position_dodge(width=0.6),width=0.4) +
                  ylab('% Population') + xlab('') +
                  theme(text = element_text(size = rel(4))
                        ,legend.text = element_text(size=rel(3))) +
                  scale_y_continuous(labels =
                                       scales::percent_format(accuracy = 1))
                ,simplify=F);

for(ii in names(plots)) print(plots[[ii]]+xlab(ii));
