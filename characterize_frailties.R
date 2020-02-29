#' ---
#' title: "Frailty Proof of Concept"
#' css: "production.css"
#' output:
#'   html_document:
#'     toc: true
#' ---
#'
#' # TOC
#+ settings, echo=FALSE
.debug <- 0;
knitr::opts_template$set(standard=list(echo=.debug>0,message=.debug>1
                                       ,warning=.debug>1,error=.debug>0
                                       ,cache.comments=FALSE,fig.cap=' '))
.projpackages <- c('GGally','tableone','pander','dplyr','ggplot2','lubridate'
                   ,'survival','survminer','broom');
.deps <- c( 'dictionary.R' );
# do not edit the next two lines
.junk<-(source('./scripts/global.R',chdir=TRUE,echo=FALSE));
# Set some formatting options for this document
panderOptions('table.alignment.default','right');
panderOptions('table.alignment.rownames','right');
panderOptions('table.split.table',Inf);
panderOptions('missing','-')
panderOptions('p.wrap','');
panderOptions('p.copula',', and ');

.currentscript <- current_scriptname('characterize_frailties.R');
#load('dictionary.R.unsampled.rdata');
#for(ii in ls(rawdata)) assign(ii,rawdata[[ii]]);
#'
#+ prep_data, opts.label='standard', cache=TRUE
# This is the unfiltered but mutated one
set.seed(project_seed);
subsamples <- unique(dat01$patient_num) %>% subsample %>%
  setNames(c('training','validation'));

dat02 <- group_by(dat01,patient_num) %>%
  mutate(nvisit=n(),min_frail=min(nval_num,na.rm=TRUE)
         ,med_frail=median(nval_num,na.rm=TRUE)
         ,q3_frail=quantile(nval_num,.75,na.rm=TRUE)
         ,max_frail=max(nval_num,na.rm=TRUE)
         ,firstvis=min(age_at_visit_days,na.rm=TRUE)
         ,lastvis=max(age_at_visit_days,na.rm=TRUE)
         ,start_date = as.Date(parse_date_time(start_date,truncate=3
                                               ,orders=c('mdy_HMp','Ymd_HMS')))
         ,birth_date = as.Date(parse_date_time(birth_date,truncate=3
                                               ,orders=c('mdy_HMp','Ymd_HMS')))
         ,death_date = as.Date(parse_date_time(death_date,truncate=3
                                               ,orders=c('mdy_HMp','Ymd_HMS')))
         # censoring variable-- if dead, TRUE otherwise, FALSE
         # at first used VITAL_STATUS_CD, except that there are patients with
         # apparently that incorrectly coded, so directly checking for
         # existence of DEATH_DATE now
         ,cens00=any(!is.na(death_date))
         ,lastdate=as.Date(coalesce(death_date,start_date))
         # days until death or last available followup, depending on cens00
         # this 'units' argument is key! Otherwise, the default unit can be chosen
         # as something other than you expect (e.g. seconds when the difference = 0)
         # and you get huge values because the entire result is converted to numeric
         # in seconds rather than days
         ,time00=as.numeric(lastdate-start_date,units='days')
         ,disc_frail=case_when(max_frail==0~'All eFI 0'
                               ,nval_num<=0.06~'eFI<=0.06'
                               ,TRUE~'eFI>0.06'));

# for each patient, select a random visit
set.seed(project_seed);
dat02a <- group_modify(dat02,function(xx,yy,...){
  xx[sample(seq_len(nrow(xx)),1),]});

# some aggregate values have to be recalculated due to filtering out certain
# visits
dat03 <- subset(dat02,age_at_visit_days>0 &
                  # our EMR goes back to this date at the earliest. Earlier
                  # records exist but are questionable, but in either case,
                  # does not greatly change concordanc,e
                  start_date > as.Date('2007-01-01') &
                  !(is.na(death_date) & vital_status_cd == 'y') &
                  ifelse(is.na(death_date), TRUE, death_date > start_date)) %>%
  mutate(nvisit=n(),min_frail=min(nval_num,na.rm=TRUE)
         ,med_frail=median(nval_num,na.rm=TRUE)
         ,q3_frail=quantile(nval_num,.75,na.rm=TRUE)
         ,max_frail=max(nval_num,na.rm=TRUE)
         ,firstvis=min(age_at_visit_days,na.rm=TRUE)
         ,lastvis=max(age_at_visit_days,na.rm=TRUE)
         # first time eFI goes up
         ,firstrise=age_at_visit_days[coalesce(match(TRUE,diff(nval_num)>0)
                                               ,1L)+1]
         ,maxrise=age_at_visit_days[c(which.max(diff(nval_num)),1L)[1]+1]
         ,lastdate=max(as.Date(coalesce(death_date,start_date)))
         ,time00=as.numeric(lastdate-start_date,units='days')
         ,disc_frail=case_when(max_frail==0~'All eFI 0'
                               ,nval_num<=0.06~'eFI<=0.06'
                               ,TRUE~'eFI>0.06'));


set.seed(project_seed);
dat03a <- group_modify(dat03,function(xx,yy,...){
  xx[sample(seq_len(nrow(xx)),1),]}) %>% subset(nvisit > 2);
# create training subsamples
dat03_tr <- subset(dat03,patient_num %in% subsamples$training);
dat03a_tr <- subset(dat03a,patient_num %in% subsamples$training);

#' # Data dictionary
#'
#' Here are some useful characteristics of the variables in `dat01`
#+ tblinfo, opts.label='standard',cache=TRUE
pander(attr(dat01,'tblinfo') %>% select(-c('nn','md5')));

#'
#' # Preliminary Results
#'
#' ## Survival, stratified by raw frailty score
#'
#' For each patient in a random sample of `r nrow(dat03a_tr)` a random visit was
#' selected between 2007 and 2019. For each such visit an eFI was calculated
#' over all visits during the previous two years. This was then used to
#' predict all-cause mortality (as reported in the EHR) during an eight year
#' period following the index visit. In the plot below the orange group
#' represents patients who never have a score greater than 0 despite
#' having visits.
#'
#' We need a principled way to distinguish patients for whom there is
#' insufficient data from patients who genuinely are non-frail-- all their
#' labs were normal and the only diagnoses they had were ones that are not
#' included in the eFI calculation.
#'
#+ survfit, opts.label='standard'
survfit(Surv(pmin(time00,2922)/365.25,cens00 & time00 <= 2922)~disc_frail
        ,dat03a_tr #,subset=nvisit>10
        ) %>%
  ggsurvplot(ylim=c(0.9,1),xlab='Years',break.time.by=1,conf.int=TRUE
             #,legend.labs=c('Low Frailty','High Frailty')
             ,palette=c('orange','blue','darkgreen')
             ,risk.table = TRUE
             ,ggtheme=theme_survminer(base_family='Times',base_size=11)
             ,font.family='Times',fontsize=3.5);
#'
#' ## Cox proportional hazard model using eFI
#+ cph00, opts.label='standard'
cph00 <- coxph(Surv(pmin(time00,2922)/365.25
                    ,cens00 & time00 <= 2922)~nval_num,dat03a_tr,subset=nvisit>10);
#'
#' Here we see that there is a strong association between eFI and
#' mortality risk, and high level of concordance--
#' `r round(concordance(cph00)$concordance,2)`.
#+ cph00plot, opts.label='standard'
pander(cph00);

pander(concordance(cph00));

coxph2survfit(cph00,bins=2,labels=c('Low Frailty','High Frailty')
              ,addcols='nvisit') %>%
  ggsurvplot(.,attr(.,'data'),ylim=c(.9,1),xlab='Years',break.time.by=1
             ,conf.int=TRUE,risk.table=TRUE,palette=c('orange','darkgreen')
             ,ggtheme=theme_survminer(base_family='Times',base_size=11)
             ,font.family='Times',fontsize=3.5);

#'
#' ***
#'
#' ## Cox proportional hazard model using birth date
#'
#' The most reliable predictor of mortality remains chronological age. However,
#' the obvious problems are that it is not subject to intervention and does not
#' reflect individual variation.
#+ cph01, opts.label='standard'
cph01 <- update(cph00,.~age_at_visit_days);
pander(cph01);
pander(concordance(cph01));
coxph2survfit(cph01,bins=2,labels=c('Young','Old'),addcols='nvisit') %>%
  ggsurvplot(.,attr(.,'data'),ylim=c(.9,1),xlab='Years',break.time.by=1
             ,conf.int=TRUE,risk.table=TRUE,palette=c('orange','darkgreen')
             ,ggtheme=theme_survminer(base_family='Times',base_size=11)
             ,font.family='Times',fontsize=3.5);

#'
#' ***
#'
#' ## Does eFI add any new information not covered by birth date?
#'
#+ cph02, opts.label='standard'
cph02 <- update(cph01,.~.+nval_num);
#' Above we saw that that eFI has a good concordance and fits the data well,
#' while age has an even better concordance and fit. But it turns out that we
#' can further improve on the predictive accuracy by including _both_ age and
#' eFI in the model as shown by the concordance, now
#' `r round(concordance(cph02)$concordance,2)`, and the analysis of
#' deviance table with a statistically significant improvement in goodness of
#' fit.
#+ cph02table, opts.label='standard'
pander(cph02);
pander(concordance(cph02));
#+ cph02anova, opts.label='standard'
anova(cph01,cph02) %>% set_rownames(c('Age Only','Age + eFI')) %>% pander;
#'
#'
#+ cph02plot, opts.label='standard'
coxph2survfit(cph02,bins=2,labels=c('Low Risk','High Risk')
              ,addcols='nvisit') %>%
  ggsurvplot(.,attr(.,'data'),ylim=c(.9,1),xlab='Years',break.time.by=1
             ,conf.int=TRUE,risk.table=TRUE,palette=c('orange','darkgreen')
             ,ggtheme=theme_survminer(base_family='Times',base_size=11)
             ,font.family='Times',fontsize=3.5);
#'
#' This means that eFI is not redundant with age and that in turn means it may
#' provide a quantitative metric by which one could identify patients who are
#' unusually frail and unusually robust _relative to their age_. Furthermore,
#' because eFI is calculated over a moving time window, the effect on frailty of
#' medical and lifestyle interventions can be monitored over time. The length
#' of the time-window used here is the same as that in Pajewski et al., but it
#' can be adjusted to the desired level of granularity.
#'
#'
#' ## Distribution of censored vs deceased (blue) frailties
#+ frail_dist, opts.label='standard'
ggplot(subset(dat03a_tr,nvisit>10 & disc_frail!='All eFI 0')
       ,aes(x=med_frail,fill=cens00,color=cens00)) +
  geom_density(adjust=4,alpha=0.5) + labs(color='Deceased',fill='Deceased') +
  xlab("Patient Median eFI")+ylab('Frequency of Patients');
#'
#+ oldcode, opts.label='standard'
# #' Start date vs frailty score
# #+ start_vs_frail, opts.label='standard'
# ggplot(subset(dat01,start_date > as.POSIXct('2007-01-01'))
#        ,aes(x=start_date,y=log1p(nval_num),group=patient_num
#             ,color=age_in_years_num)) + geom_step(alpha=0.5);
# #'
# #'
# #' Age vs frailty score for deceased patients
# #'
# #+ age_vs_frail, opts.label='standard'
# subset(dat01,!is.null(death_date) & death_date>=start_date) %>%
#   filter(patient_num %in% sample(unique(patient_num),100)) %>%
#   mutate(patid=as.numeric(factor(patient_num)),patid=patid/max(patid)/10) %>%
#   group_by(patient_num) %>% mutate(hislen=n()) %>% arrange(hislen,patient_num) %>%
#   ggplot(aes(x=age_at_visit_days/365.25,y=log1p(nval_num)+rank(hislen)/1000,group=patient_num
#             ,color=age_in_years_num)) +
#   geom_step(alpha=0.5);
#+ save, opts.label='standard', results='hide', cache=TRUE
# save results
save(file=paste0(.currentscript,'.rdata')
     ,list=c(setdiff(ls(),.origfiles)));
c()
