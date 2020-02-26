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
# Set some formatting options for this document
panderOptions('table.alignment.default','right');
panderOptions('table.alignment.rownames','right');
panderOptions('table.split.table',Inf);
panderOptions('p.wrap','');
panderOptions('p.copula',', and ');

.currentscript <- current_scriptname('characterize_frailties.R');
load('dictionary.R.unsampled.rdata');
for(ii in ls(rawdata)) assign(ii,rawdata[[ii]]);
#'
#+ prep_data, opts.label='standard'
dat01[,c('start_date','death_date','birth_date')] <- lapply(dat01[
  ,c('start_date','death_date','birth_date')],parse_date_time
  ,orders=c('mdy_HMp','Ymd_HMS'));

set.seed(project_seed);
dat01a <- group_by(dat01,patient_num) %>%
  subset(age_at_visit_days>0 & start_date > as.POSIXct('2007-01-01') &
           !(is.na(death_date) & vital_status_cd == 'y') &
           ifelse(is.na(death_date),TRUE
                  ,as.Date(death_date) > as.Date(start_date))) %>%
  mutate(lastdate=as.Date(coalesce(death_date,start_date))
         ,start_date = as.Date(start_date)) %>%
  group_modify(function(xx,yy,...){
    out <- cbind(xx[sample(seq_len(nrow(xx)),1),]
                 # confirmed all patient_num/age_at_visit_days combos unique
                 ,nvisit = nrow(xx)
                 , lastvis = max(xx$age_at_visit_days,na.rm = TRUE)
                 ,med_frail=median(xx$nval_num,na.rm = TRUE)
                 ,q3_frail=quantile(xx$nval_num,.75,na.rm=TRUE)
               ,max_frail=max(xx$nval_num,na.rm=TRUE)
        # censoring variable-- if dead, TRUE otherwise, FALSE
        # had used VITAL_STATUS_CD, except that there are patients with
        # apparently that incorrectly coded, so directly checking for
        # existence of DEATH_DATE now
        ,cens00 = any(!is.na(xx$death_date)));
  #if(out$VITAL_STATUS_CD=='y') browser();
  # days until death or last available followup, depending on cens00
    # this 'units' argument is key! Otherwise, the default unit can be chosen
    # as something other than you expect (e.g. seconds when the difference = 0)
    # and you get huge values because the entire result is converted to numeric
    # in seconds rather than days
  out$time00 <- as.numeric(max(xx$lastdate) - out$start_date
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
             ,cens00 & time00 <= 2922)~nval_num>median(nval_num),dat01a
        ,subset=nvisit>10) %>%
  ggsurvplot(ylim=c(0.9,1),xlab='Years',break.time.by=1,conf.int=TRUE
             ,legend.labs=c('Low Frailty','High Frailty')
             ,palette=c('orange','darkgreen'),risk.table = TRUE);

#+ cph00, opts.label='standard'
cph00 <- coxph(Surv(pmin(time00,2922)/365.25
                    ,cens00 & time00 <= 2922)~nval_num,dat01a,subset=nvisit>10);
pander(cph00);

#' Distribution of censored vs deceased (blue) frailties
#+ frail_dist, opts.label='standard'
ggplot(subset(dat01a,nvisit>5),aes(x=nval_num,fill=cens00,color=cens00)) +
  geom_density(adjust=4,alpha=0.5);
#'
#' Start date vs frailty score
#+ start_vs_frail, opts.label='standard'
ggplot(subset(dat01,start_date > as.POSIXct('2007-01-01'))
       ,aes(x=start_date,y=log1p(nval_num),group=patient_num
            ,color=age_in_years_num)) + geom_step(alpha=0.5);
#'
#'
#' Age vs frailty score for deceased patients
#'
#+ age_vs_frail, opts.label='standard'
ggplot(subset(dat01,!is.null(death_date) & death_date>=start_date)
       ,aes(x=age_at_visit_days/365.25,y=log1p(nval_num),group=patient_num
            ,color=age_in_years_num)) + geom_step(alpha=0.5) +
  geom_point(data=subset(dat01a,cens00 & nvisit > 5)
             ,mapping=aes(x=(time00+age_at_visit_days)/365.25,y=(log1p(nval_num)))
             ,color='red',cex=0.5);
#' ### Save results
#'
#+ save, opts.label='standard', results='hide'
save(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles));
c()
