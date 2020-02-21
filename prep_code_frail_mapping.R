#' ---
#' title: "Example Analysis"
#' css: "production.css"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---
#'
#' ### Settings
#'
#' In the below two lines are the minimum script-level settings you need.
#' The `.projpackages` object has the names of the packages you need installed
#' if necessary and then loaded for this scriport. The `.deps` object contains
#' other scriports on which this one _directly_ depends (you don't need to worry
#' about the indirect ones-- each scriport manages its own dependencies for
#' packages and scriports). The recommended value to start with is the one shown
#' here. You can add more if the need arises later. For more information, please
#' see the [overview](overview.html) scriport.
.projpackages <- c('GGally','tableone','pander','dplyr','ggplot2');
.deps <- c( 'dictionary.R' );
#+ load_deps, echo=FALSE, message=FALSE, warning=FALSE,results='hide'
# do not edit the next two lines
.junk<-capture.output(source('./scripts/global.R',chdir=TRUE,echo=FALSE));
#' Set some formatting options for this document
panderOptions('table.alignment.default','right');
panderOptions('table.alignment.rownames','right');
panderOptions('table.split.table',Inf);
panderOptions('p.wrap','');
panderOptions('p.copula',', and ');

.currentscript <- current_scriptname('prep_code_frail_mapping.R');
#' ### Clean up mapping table
code2grpout <- group_by(code2grpin,`No. `) %>% group_modify(function(xx,yy,...){
  #if(xx$Deficit[1]=='Peptic Ulcer') browser();
  diag10 <- xx$`ICD-10 Definition` %>% trimws %>%
    gsub('([0-9]) ([0-9])','\\1, \\2',.) %>% gsub('[ ]*-[ ]*','-',.) %>%
    strsplit(',[ ]*') %>% unlist %>% sapply(ranges,simplify=FALSE) %>%
    unlist %>% trimws %>% gsub('[.]?x[.]?$','',.) %>%
    gsub('[.]x(-|$)','\\1',.) %>% sapply(ranges,simplify=FALSE) %>%
    unlist %>% trimws %>% paste0('ICD10:',.);
  diag09 <- xx$`ICD-9 Definition` %>% trimws %>%
    gsub('(43[34])\\.x1',paste(paste0('\\1.',0:9,'1')
                               ,collapse=', '),.) %>%
    gsub('([0-9]) ([0-9])','\\1, \\2',.) %>% gsub('[ ]*-[ ]*','-',.) %>%
    strsplit(',[ ]*') %>% unlist %>% sapply(ranges,simplify=FALSE) %>%
    unlist %>% trimws %>% gsub('[.]?x[.]?$','',.) %>%
    gsub('[.]x(-|$)','\\1',.) %>% sapply(ranges,simplify=FALSE) %>%
    unlist %>% trimws %>% paste0('ICD9:',.);
  with(xx,data.frame(diag=c(diag10,diag09),deficit=Deficit[1],source=Source[1]
                     ,notes=Notes[1]))});
#' Standardize names
names(code2grpout)[1] <- 'grp';
#' Check for duplicates
table(code2grpout$diag) %>% `[`(.>1) %>% cbind %>%
  pander(justify='right',col.names='Number of Duplicates');
#' remove duplicates
code2grpout <- subset(code2grpout,!(grp==5 & diag %in% c('ICD9:404.03'
                                                         ,'ICD9:404.13'
                                                         ,'ICD9:404.93')) &
                        !(grp==17 & diag=='ICD9:412'));

export(code2grpout,'local/out/code_to_efigrp.csv');
with(code2grpout,paste0('\nunion select ',grp,", '",diag,"', '",deficit,"'
                        , '",source,"' from dual")) %>%
  cat(file='local/out/code_to_efigrp.sql');
#'
#'
#' ### Save results
#'
#' Now the results are saved and available for use by other scriports if you
#' place `r sprintf("\x60'%s'\x60",.currentscript)` among the values in their
#' `.deps` variables.
save(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles));
#+ echo=FALSE, results='hide'
c()
