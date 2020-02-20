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
code2grpin
code2grpout <- group_by(code2grpin,`No. `) %>% group_modify(function(xx,yy,...){
  with(xx,data.frame(diag=c(`ICD-10 Definition`,`ICD-9 Definition`) %>%
                       strsplit(',[ ]*') %>% unlist %>% sapply(ranges) %>%
                       unlist %>% gsub('x$','',.)
                     ,deficit=Deficit[1],source=Source[1],notes=Notes[1]))}) %>%
  select(1:5);
names(code2grpout)[1] <- 'grp';
export(code2grpout,'local/out/code_to_efigrp.csv');
with(code2grpout,paste0('\nunion select ',grp,", '",diag,"' from dual")) %>%
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
