#' ---
#' title: "Frailty Proof of Concept"
#' css: "production.css"
#' output:
#'   html_document:
#'     toc: true
#' ---
#'
#' ### Settings
#+ echo=FALSE
.debug <- 0;
knitr::opts_template$set(standard=list(echo=.debug>0,message=.debug>1
                                       ,warning=.debug>1,error=.debug>0
                                       ,cache.comments=FALSE))
.projpackages <- c('GGally','tableone','pander','dplyr','ggplot2','lubridate'
                   ,'survival','survminer');
.deps <- c( 'dictionary.R' );
# do not edit the next two lines
.junk<-(source('./scripts/global.R',chdir=TRUE,echo=FALSE));
load('dictionary.R.unsampled.rdata');
# Set some formatting options for this document
panderOptions('table.alignment.default','right');
panderOptions('table.alignment.rownames','right');
panderOptions('table.split.table',Inf);
panderOptions('p.wrap','');
panderOptions('p.copula',', and ');

.currentscript <- current_scriptname('characterize_frailties.R');
load('dictionary.R.unsampled.rdata');
#'
#+ prep_data, opts.label='standard'
dat01[,c('START_DATE','DEATH_DATE','BIRTH_DATE')] <- lapply(dat01[
  ,c('START_DATE','DEATH_DATE','BIRTH_DATE')],parse_date_time
  ,orders=c('mdy_HMp','Ymd_HMS'));

set.seed(project_seed);
dat01a <- group_by(dat01,PATIENT_NUM) %>%
  subset(AGE_AT_VISIT_DAYS>0 & START_DATE > as.POSIXct('2007-01-01') &
           !(is.na(DEATH_DATE) & VITAL_STATUS_CD == 'y') &
           ifelse(is.na(DEATH_DATE),TRUE
                  ,as.Date(DEATH_DATE) > as.Date(START_DATE))) %>%
  mutate(lastdate=as.Date(coalesce(DEATH_DATE,START_DATE))
         ,START_DATE = as.Date(START_DATE)) %>%
  group_modify(function(xx,yy,...){
  out <- cbind(xx[sample(seq_len(nrow(xx)),1),]
               # confirmed that all patient_num/age_at_visit_days combos unique
               ,nvisit = nrow(xx), lastvis = max(xx$AGE_AT_VISIT_DAYS)
               ,med_frail=median(xx$NVAL_NUM,na.rm = TRUE)
               ,q3_frail=quantile(xx$NVAL_NUM,.75,na.rm=TRUE)
               ,max_frail=max(xx$NVAL_NUM,na.rm=TRUE)
        # censoring variable-- if dead, TRUE otherwise, FALSE
        # had used VITAL_STATUS_CD, except that there are patients with
        # apparently that incorrectly coded, so directly checking for
        # existence of DEATH_DATE now
        ,cens00 = any(!is.na(xx$DEATH_DATE)));
  #if(out$VITAL_STATUS_CD=='y') browser();
  # days until death or last available followup, depending on cens00
    # this 'units' argument is key! Otherwise, the default unit can be chosen
    # as something other than you expect (e.g. seconds when the difference = 0)
    # and you get huge values because the entire result is converted to numeric
    # in seconds rather than days
  out$time00 <- as.numeric(max(xx$lastdate) - out$START_DATE
                                 ,units='days');
  out;
  }) %>% subset(nvisit > 2);




#' ### Data dictionary
#'
#' Here are some useful characteristics of the variables in `dat01`
#+ tblinfo, opts.label='standard'
pander(attr(dat01,'tblinfo') %>% select(-c('nn','md5')));

#'
#' ### Plot the data
#'
#' Survival curve
#'
#' Death during eight years after a randomly selected visit for each patient,
#' as predicted by the frailty index calculated over the two years prior to
#' that visit.
#'
#+ survfit, opts.label='standard'
survfit(Surv(pmin(time00,2922)/365.25
             ,cens00 & time00 <= 2922)~NVAL_NUM>median(NVAL_NUM),dat01a
        ,subset=nvisit>10) %>%
  ggsurvplot(ylim=c(0.9,1),xlab='Years',break.time.by=1,conf.int=TRUE
             ,legend.labs=c('Low Frailty','High Frailty')
             ,palette=c('orange','darkgreen'),risk.table = TRUE);

#+ cph00, opts.label='standard'
cph00 <- coxph(Surv(pmin(time00,2922)/365.25
                    ,cens00 & time00 <= 2922)~NVAL_NUM,dat01a,subset=nvisit>10);
pander(cph00);

#' Distribution of censored vs deceased (blue) frailties
#+ frail_dist, opts.label='standard'
ggplot(subset(dat01a,nvisit>5),aes(x=NVAL_NUM,fill=cens00,color=cens00)) +
  geom_density(adjust=1.5,alpha=0.5);
#'
#' Start date vs frailty score
#+ start_vs_frail, opts.label='standard'
ggplot(subset(dat01,START_DATE > as.POSIXct('2007-01-01'))
       ,aes(x=START_DATE,y=log1p(NVAL_NUM),group=PATIENT_NUM
            ,color=AGE_IN_YEARS_NUM)) + geom_step(alpha=0.5);
#'
#'
#' Age vs frailty score for deceased patients
#'
#+ age_vs_frail, opts.label='standard'
ggplot(subset(dat01,!is.null(DEATH_DATE) & DEATH_DATE>=START_DATE)
       ,aes(x=AGE_AT_VISIT_DAYS,y=log1p(NVAL_NUM),group=PATIENT_NUM
            ,color=AGE_IN_YEARS_NUM)) + geom_step(alpha=0.5) +
  geom_point(data=subset(dat01a,cens00 & nvisit > 5)
             ,mapping=aes(x=time00+AGE_AT_VISIT_DAYS,y=(log1p(NVAL_NUM)))
             ,color='red',cex=0.5);
#' ### Save results
#'
#+ save, opts.label='standard', results='hide'
save(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles));
c()
