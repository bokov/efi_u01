#' # Validate Frailty Scores
#'
#' Compare the frailty scores calculated within the database to independently
#' calculated ones to make sure that count distinct over rolling 2-year time
#' windows in the DB is correctly implemented.
#'
#' Add the names of packages (enclosed in quotes) you need to this vector
.projpackages <- c('dplyr','lubridate','pander');
#' If you want to reuse calculations done by other scripts, add them to `.deps`
#' below after `'dictionary.R'`.
.deps <- c( 'dictionary.R' );
#+ load_deps, echo=TRUE, message=TRUE, warning=TRUE,results='hide'
# Do not edit the next line
.junk<-capture.output(source('./scripts/global.R',chdir=TRUE,echo=TRUE));
load('dictionary.R.unsampled.rdata');
for(ii in ls(rawdata)) assign(ii,rawdata[[ii]]);
#' Edit the next line only if you copy or rename this file (make the new name the
#' argument to `current_scriptname()`)
.currentscript <- current_scriptname('validate_scores.R');
#'
#' Number of days prior to current visit over which to calculate the frailty
#' score
scr_timewindow <- 730;
#' ### Start
#'
#+ rename_cols
names(datgr) <- gsub('START_DATE|start_date','dt',names(datgr)) %>% tolower;
names(datfr) <- gsub('START_DATE|start_date','dt',names(datfr)) %>% tolower;
names(datfr) <- gsub('nval_num','frailty',names(datfr));
#' Format times
#+ format_times
datgr <- mutate(datgr,dt=parse_date_time(dt,c('mdy_HMp','Ymd_HMS'))) %>%
  arrange(patient_num,dt) %>% group_by(patient_num);
# if(!'dt' %in% names(datgr) && 'START_DATE' %in% names(datgr)){
#   datgr$dt <- datgr$START_DATE;
# }
datfr <- mutate(datfr[,c('patient_num','dt','frailty')]
                ,dt=parse_date_time(dt,c('mdy_HMp','Ymd_HMS'))) %>%
  arrange(patient_num,dt);

#+ datgr2fr, warning=FALSE
datgr2fr <- group_by(datgr,patient_num) %>% group_modify(function(aa,bb,...){
    out <- unique(data.frame(dt=aa$dt,fr=sapply(aa$dt,function(xx){
      #if(xx %in% xcheck$dt && bb[[1]] %in% xcheck$patient_num) browser()
      length(unique(na.omit(aa$grp[between(as.numeric(xx - aa$dt,units='days')
                                           ,0,scr_timewindow)])))
      })));
    #browser();
  },keep=TRUE);

# test code to find frailties that actually go UP... currently 0 patients have
# this
# baz <- group_by(foo90_2a,patient_num) %>% group_modify(function(aa,bb,...){if(any(diff(aa$fr)>0)) browser(); aa[F,]})
#
# creating datgr with an artificial increase in frailty score inserted at row 861
# (example only, in other version of the data the patient_num, dt, and grp will be different)
# datgr2 <- bind_rows(datgr[1:860,],data.frame(patient_num='1000727568',dt=parse_date_time('2018-01-07','Ymd'),grp=7:9,stringsAsFactors = F),datgr[-(1:860),])

# find denominator
if((dbfrmx<- max(datfr$frailty,na.rm=TRUE))<=1 &&
   (xcfrmx <- max(datgr2fr$fr,na.rm=TRUE)) > 1){
  denom <- xcfrmx/dbfrmx;
  if(denom == round(denom)) datgr2fr$fr <- datgr2fr$fr/denom else {
    warning('Mismatch between database version and crosscheck version.\n'
            ,'Could not find a conversion factor.')
  }
}

setNames(sapply(list(datgr,datfr,datgr2fr),function(xx){
  nrow(unique(xx))}),c('Patients/Dates/Groups'
                      ,'Patients/Dates/Frailties, DB'
                      ,'Patients/Dates/Frailties Crosscheck')) %>% cbind %>%
  pander(justify='right',col.names='Unique Rows');
#'
#' The following cases are discrepant between the database version and this
#' crosscheck:
xcheck <- merge(unique(datfr),(datgr2fr)) %>% subset(frailty != fr);
xcheck %>% pander;
#' ***
#' `r if(nrow(xcheck)==0) 'Excellent! No discrepancies.'`
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' ### Save results
#'
#' Now the results are saved and available for use by other scriports if you
#' place `r sprintf("\x60'%s'\x60",.currentscript)` among the values in their
#' `.deps` variables.
save(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles));
#' ### Finish
#+ echo=FALSE, results='hide'
c()
