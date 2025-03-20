####purpose/input parameters and what they control/
#AEdata:
#explain the arguments in the function
TODO:
#MQ is testing code on 19th March 2025
#install.packages("bookdown")
#install.packages("RODBC")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("data.table")
#install.packages("knitr")
#install.packages("lubridate")
#install.packages("kableExtra")
#install.packages("desctable")
#install.packages("flextable")
#install.packages("sur")
#install.packages("stringr")
#install.packages("readstata13")
#install.packages("survival") 
#install.packages("ggplot2")
#install.packages("ggfortify")
#install.packages("survminer")
#install.packages("anytime")
#install.packages("PropCIs")
#install.packages("reshape2")
#install.packages("zoo")
#install.packages("magrittr")
#install.packages("ggbreak")
#install.packages("patchwork")
#install.packages("gtsummary")
#install.packages("ftExtra")
#install.packages("officer")
#install.packages("officedown")
#install.packages("pandoc")
#install.packages("berryFunctions")
#install.packages("excel.link")
#install.packages("table1")
#install.packages("ggpubr")
#install.packages("gtools")
library(bookdown)
library(RODBC)
library(tidyverse)
library(dplyr)
library(data.table)
library(knitr)
library(lubridate)
library(kableExtra)
library(desctable)
library(flextable)
library(sur)
library(stringr)
library(readstata13)
library(survival)
library(ggplot2)
library(ggfortify)
library(survminer)
library(anytime)
library(PropCIs)
library(reshape2)
library(zoo)
library(magrittr)
library(ggbreak)
library(patchwork)
library(gtsummary)
library(ftExtra)
library(officer)
library(officedown)
library(pandoc)
library(berryFunctions)
library(excel.link)
library(table1)
library(ggpubr)
library(gtools)
####generating AE example data using ACE Trial
FIXME:
# MQ no access to data
d <- odbcConnectAccess2007("N:\\SNAPSHOTS\\DDU-FRAME\\Live Snapshot 20240503\\FRAME_LIVE_20240503.accdb")
#AE = sqlFetch(d,"dbo_R_FRAME_AdvRQG_aeRQG")
AE = sqlFetch(d,"dbo_R_FRAME_AdvRQG_aeRQG")

AE$z_usubjid = toupper(AE$z_usubjid)

AE = AE[!is.na(AE$z_usubjid) | !is.na(AE$rqaedet),]

AE_med = read.csv("N:\\ANALYSES\\FRAME\\SAR\\Phase I\\Data\\CSVexport_FRAME_AllSites 03May2024.csv")
#AE_med=read.csv("N:\\ANALYSES\\ACE\\SAR\\Phase l\\November 2022\\Manuscript\\Data\\Clinical Coding 06Feb2023\\ACE p1 Clinical Coding CSV export.csv")
AE_med$Subject.Label = toupper(AE_med$Subject.Label)
AE_med =AE_med[AE_med$ResponseValue!='',]

AE_med = AE_med[AE_med$eFormCode == "AdvRQG",c("Subject.Label","ResponseValue","eFormCycle", "QuestionCycle","System.Organ.Class","Preferred.Term")]



AE = merge(AE, AE_med, by.x =c("z_usubjid","CRFPageCycleNumber","RepeatNumber","rqaedet"), 
           by.y = c("Subject.Label","eFormCycle","QuestionCycle","ResponseValue") ,all = T)


AE = AE[!is.na(AE$Site),]

#reg = sqlFetch(d,"dbo_R_ACE_Reg")

reg = sqlFetch(d,"dbo_R_FRAME_reg")

reg$ptinits = toupper(reg$ptinits)

reg = distinct(reg,ptinits,birthdat,cohort,drugsched)
#Diag = sqlFetch(d,"dbo_R_ACE_Diag")

Diag = sqlFetch(d,"dbo_R_FRAME_Diag")
Diag$x_ptinits = toupper(Diag$x_ptinits)
Diag$x_usubjid = toupper(Diag$x_usubjid)

Diag =  Diag[,c(8:10,12,13)]
reg = merge(reg, Diag, by.x = c('ptinits','birthdat'),by.y = c('x_ptinits','x_birthdat'),all =T)

#esc = reg[reg$cohort=='Dose Escalation',]

AE = merge(AE,reg, by.x="z_usubjid", by.y="x_usubjid" , all.y = T)

AE = AE[AE$rqctcgrade!='Unobtainable',]

AE$grade = AE$rqctcgrade

AE1 = AE

AE1 = distinct(AE1,z_usubjid,System.Organ.Class,Preferred.Term,
               rqinvsda1rel,rqinvsda2rel,grade,cohort,drugsched)


#####ideally Expansion cohort will all be named as Dose Expansion (xxx) 
#   so we can identify and rename Expansion cohort automatically using the following code
AE1$group <- ifelse(
  grepl("Expansion", AE1$cohort, ignore.case = TRUE),
  sub(".*\\(([^)]+)\\).*", "\\1", AE1$cohort),
  AE1$cohort
)
AE1$cohort <- ifelse(grepl("Escalation", AE1$cohort),AE1$cohort,"Dose Expansion")
#AEdata: AE dataset for analysis

#group_var: Specifies how AEs are classified - either by "preferred term" or "system organ class".
#overall_var: Indicates whether to include an "Overall" column in the output. Set to TRUE to include it, or FALSE to exclude it.
#relation_var: Filters AEs based on their relatedness to the drug. If TRUE, only AEs that are at least "possibly related" to the drug will be included. If FALSE, all AEs are included regardless of relatedness.
#AE_cutoff: Sets the minimum threshold or cutoff value that present only AEs that affect >=x% of all patients, when AE_cutoff=0, all AEs will be presented
#relatedness_prefix: Specifies the common prefix used in column names that represent the relatedness of an AE to two drugs.
#dose_column: use AE1 as example, if by dose dose_column="drugsched" if by group dose_column="group"


create_AE_table1 <- function(AEdata, group_var = c("System.Organ.Class", "Preferred.Term"), overall_var = c(TRUE, FALSE), relation_var = TRUE, AE_cutoff = 10, relatedness_prefix = "rqinvsda", 
                             drug_labels = c("Drug A", "Drug B"), dose_column = "drugsched", grade_column = "grade",cohort_column="cohort",cohort_var = c("Escalation", "Expansion")) {
  
  # Ensure group_var is either "System.Organ.Class" or "Preferred.Term"
  group_var <- match.arg(group_var)
  
  # Exclude "Unobtainable"
  AEdata <- AEdata[AEdata[[grade_column]] != 'Unobtainable', ]
  #Extract relevant cohort
  cohort_regex <- paste(cohort_var, collapse = "|")
  
  # Filter AEdata based on cohort values
  AEdata <- AEdata[grepl(cohort_regex, AEdata[[cohort_column]], ignore.case = TRUE), ]
  
  # Capture relatedness columns
  relatedness_columns <- names(AEdata)[grepl(relatedness_prefix, names(AEdata))]
  
  # Ensure drug_labels length matches the relatedness_columns length
  if (length(drug_labels) != length(relatedness_columns)) {
    stop("Length of drug_labels does not match the number of relatedness columns.")
  }
  # Extract AEs at least possibly related to study drug
  if (relation_var) {
    AEdata <- AEdata %>%
      filter(
        if_any(all_of(relatedness_columns), ~ !grepl("Not related|Unlikely", .))
      )
  }
  # Select distinct rows by determined columns
  AEdata <- AEdata %>%
    distinct(z_usubjid, across(all_of(group_var)), across(all_of(relatedness_columns)), across(all_of(grade_column)), across(all_of(dose_column)))
  # Reclassify grades
  AEdata[[grade_column]][AEdata[[grade_column]] %in% c("Grade 1", "Grade 2")] <- 1
  AEdata[[grade_column]][AEdata[[grade_column]] %in% c("Grade 3", "Grade 4")] <- 2
  AEdata[[grade_column]][AEdata[[grade_column]] %in% c("Grade 5")] <- 3
  # Aggregate based on grade, group_var, and dose_column with dynamic columns
  AE_ag1 <- aggregate(as.formula(paste(grade_column, "~ z_usubjid +", group_var, "+", dose_column)), data = AEdata, FUN = max)
  AE_ag1 <- aggregate(as.formula(paste("z_usubjid ~", grade_column, "+", group_var, "+", dose_column)), data = AE_ag1, FUN = length)
  
  # Calculate N for each dose schedule
  schedule_summary <- AEdata %>%
    group_by(!!sym(dose_column)) %>%
    summarise(N = n_distinct(z_usubjid), .groups = 'drop')
  unique_drugsched <- unique(schedule_summary[[dose_column]])
  #unique_drugsched <- unique(AEdata[[dose_column]])
  #schedule_summary <- schedule_summary[match(unique_drugsched, schedule_summary[[dose_column]]), ] 
  # Merge N into AE_ag1 and calculate percentage
  AE_ag1 <- AE_ag1 %>%
    left_join(schedule_summary, by = setNames(dose_column, dose_column)) %>%
    mutate(
      percent = round((z_usubjid / N) * 100, 1),
      np = paste0(z_usubjid, " (", percent, "%) ")
    ) %>%
    arrange(desc(z_usubjid))
  
  # Create table for drugsched + grade combination
  AE_wide1 <- dcast(AE_ag1, as.formula(paste(group_var, "~", dose_column, "+", grade_column)), value.var = "np")
  AE_wide1[is.na(AE_wide1)] <- 0
  
  # Sort AE_wide1's columns based on dose order, note that this need to be changed for different trials 
  #dose_patterns <- unique(gsub(" .*", "", colnames(AE_wide1)[-c(1, length(colnames(AE_wide1)))]))
  
  # Extract the first numerical value before "mg" and sort dose patterns by this value
  #dose_order <- dose_patterns[order(as.numeric(sub(".*?(\\d+)(?=mg).*", "\\1", dose_patterns, perl = TRUE)))]
  
  # Initialize ordered_columns with the first column (group_var)
  #ordered_columns <- colnames(AE_wide1)[1]
  
  # Loop through each dose in the sorted order and find matching columns
  #for (dose in dose_order) {
   # dose_columns <- grep(paste0("^", dose), colnames(AE_wide1), value = TRUE)
  #  ordered_columns <- c(ordered_columns, dose_columns)
  #}

  
  # Reorder AE_wide1 based on ordered_columns
  #AE_wide1 <- AE_wide1[, ordered_columns]
  
  # Calculate overall if overall_var is TRUE
  if (overall_var) {
    AE_overall <- AE_ag1 %>%
      group_by(!!sym(group_var)) %>%
      summarise(
        overall_np = paste0(sum(z_usubjid, na.rm = TRUE), " (", 
                            round((sum(z_usubjid, na.rm = TRUE) / length(unique(AEdata$z_usubjid))) * 100, 1), "%) "),
        .groups = 'drop'
      )
    AE_wide1 <- AE_wide1 %>%
      left_join(AE_overall, by = setNames(group_var, group_var))
    # keep the AEs that affect >=x% of all patients, x=AE_cutoff 
    AE_wide1 <- AE_wide1 %>%
      mutate(percent_value = as.numeric(sub(".*\\((.*)\\%\\)", "\\1", overall_np))) %>%  # Extract the percentage value
      arrange(desc(percent_value)) %>% 
      filter(percent_value >= AE_cutoff) 
    AE_wide1 <- AE_wide1 %>%
      select(-percent_value)
  }
  

  
  header_labels <- colnames(AE_wide1) %>%
    setNames(colnames(AE_wide1))
  
  for (drugsched in unique_drugsched) {
    
    # Get the grades that actually exist for this drug schedule
    existing_grades <- unique(AE_ag1[AE_ag1[[dose_column]] == drugsched, grade_column])
    
    for (grade in existing_grades) {
      column_name <- paste0(drugsched, "_", grade)
      
      # Define the label based on the grade
      label <- if (grade == 1) {
        "Grade 1 or 2"
      } else if (grade == 2) {
        "Grade 3 or 4"
      } else {
        "Grade 5"
      }
      
      # Set the header label for columns that exist
      if (column_name %in% colnames(AE_wide1)) {
        header_labels[column_name] <- label
      }
    }
  }
  
  # Add the label for the overall column if it exists
  if ("overall_np" %in% colnames(AE_wide1)) {
    header_labels["overall_np"] <- "Overall"
  }
  
  # Adjust table caption based on group_var, this need to be further adjusted based on inputs
  caption_text <- if (group_var == "Preferred.Term") {
    "Total number and percentage of all adverse events occurring during dose escalation by preferred term, grade, and assigned dosage."
  } else {
    "Total number and percentage of all adverse events occurring during dose escalation by system organ class, grade, and assigned dosage."
  }
  
  # Generate the top row (dose schedules with sample sizes)
  top_row_values <- c(group_var, paste0(schedule_summary[[dose_column]], "\nN=", schedule_summary$N))
  # Calculate colwidths dynamically based on grades available for each drug schedule
  dose_grade_colwidths <- sapply(unique_drugsched, function(drug) {
    n_grades <- length(unique(AE_ag1[AE_ag1[[dose_column]] == drug, grade_column]))
    return(n_grades)
  })  
  # Add "Overall" to the header if overall_var is TRUE
  if (overall_var) {
    top_row_values <- c(top_row_values, "Overall")
    dose_grade_colwidths <- c(dose_grade_colwidths, 1)
  }

  
  # Create the flextable with dynamically generated header labels
  flextable(AE_wide1) %>%
    set_header_labels(values = header_labels) %>%
    add_header_row(
      values = top_row_values,
      colwidths = c(1, dose_grade_colwidths)
    ) %>%
    merge_v(part = "header") %>%
    align(align = "center", part = "all") %>%
    bold(part = "header") %>%
    fontsize(size = 10, part = "all") %>%
    set_caption(caption_text)
}

# Example of running the function with AE data
# Assuming 'AE1' is your dataframe and contains the relatedness columns and dose levels
# Here, 'drug_labels' specifies names like "Drug A", "Drug B"
#####By dose in escalation cohort
#by SOC
create_AE_table1(AE1, group_var = "System.Organ.Class", overall_var = TRUE, relation_var = FALSE, AE_cutoff = 10, relatedness_prefix = "rqinvsda", 
                 drug_labels = c("Drug A", "Drug B"), dose_column = "drugsched", grade_column = "grade",cohort_column="cohort",cohort_var="Dose Escalation")
#by PT
create_AE_table1(AE1, group_var = "Preferred.Term", overall_var = TRUE, relation_var = FALSE, AE_cutoff = 10, relatedness_prefix = "rqinvsda", 
                             drug_labels = c("Drug A", "Drug B"), dose_column = "drugsched", grade_column = "grade",cohort_column="cohort",cohort_var="Escalation")
#show all AE_cutoff
create_AE_table1(AE1, group_var = "System.Organ.Class", overall_var = TRUE, relation_var = FALSE, AE_cutoff = 0, relatedness_prefix = "rqinvsda", 
                 drug_labels = c("Drug A", "Drug B"), dose_column = "drugsched", grade_column = "grade",cohort_column="cohort",cohort_var="Escalation")
#show AE related with drugs 
create_AE_table1(AE1, group_var = "System.Organ.Class", overall_var = TRUE, relation_var = TRUE, AE_cutoff = 0, relatedness_prefix = "rqinvsda", 
                 drug_labels = c("Drug A", "Drug B"), dose_column = "drugsched", grade_column = "grade",cohort_column="cohort",cohort_var="Escalation")

#only AZD5069
create_AE_table1(AE1, group_var = "System.Organ.Class", overall_var = TRUE, relation_var = TRUE, AE_cutoff = 0, relatedness_prefix = "rqinvsda1rel", 
                 drug_labels = c("AZD5069"), dose_column = "drugsched", grade_column = "grade",cohort_column="cohort",cohort_var="Escalation")
###By group in all pts
#by SOC
create_AE_table1(AE1, group_var = "System.Organ.Class", overall_var = TRUE, relation_var = FALSE, AE_cutoff = 10, relatedness_prefix = "rqinvsda", 
                 drug_labels = c("Drug A", "Drug B"), dose_column = "group", grade_column = "grade",cohort_column="cohort",cohort_var= c("Escalation", "Expansion"))
#by PT 
create_AE_table1(AE1, group_var = "Preferred.Term", overall_var = TRUE, relation_var = FALSE, AE_cutoff = 10, relatedness_prefix = "rqinvsda", 
                 drug_labels = c("Drug A", "Drug B"), dose_column = "group", grade_column = "grade",cohort_column="cohort",cohort_var=c("Escalation", "Expansion"))
#show all AE_cutoff
create_AE_table1(AE1, group_var = "System.Organ.Class", overall_var = TRUE, relation_var = FALSE, AE_cutoff = 0, relatedness_prefix = "rqinvsda", 
                 drug_labels = c("Drug A", "Drug B"), dose_column = "group", grade_column = "grade",cohort_column="cohort",cohort_var=c("Escalation", "Expansion"))
#show AE related with drugs 
create_AE_table1(AE1, group_var = "System.Organ.Class", overall_var = TRUE, relation_var = TRUE, AE_cutoff = 0, relatedness_prefix = "rqinvsda", 
                 drug_labels = c("Drug A", "Drug B"), dose_column = "group", grade_column = "grade",cohort_column="cohort",cohort_var=c("Escalation", "Expansion"))

#only AZD5069
create_AE_table1(AE1, group_var = "System.Organ.Class", overall_var = TRUE, relation_var = TRUE, AE_cutoff = 0, relatedness_prefix = "rqinvsda1rel", 
                 drug_labels = c("AZD5069"), dose_column = "group", grade_column = "grade",cohort_column="cohort",cohort_var=c("Escalation", "Expansion"))


AE2=AE
AE2 = distinct(AE2,z_usubjid,rqinvsda1rel,rqinvsda2rel,rqsaeyn,rqsaeout,drugsched,cohort,rqstdrugact,rqstdrugact2)
###need to rename rqstdrugact to identify action taken with drug A and B
AE2=AE2%>%
  rename(rqstdrugact1=rqstdrugact)
#####ideally Expansion cohort will all be named as Dose Expansion (xxx) 
#   so we can identify and rename Expansion cohort automatically using the following code
AE2$group <- ifelse(
  grepl("Expansion", AE2$cohort, ignore.case = TRUE),
  sub(".*\\(([^)]+)\\).*", "\\1", AE2$cohort),
  AE2$cohort
)
AE2$cohort <- ifelse(grepl("Escalation", AE2$cohort),AE2$cohort,"Dose Expansion")
#data: SAE dataset for analysis

#group_var: Specifies how AEs are classified - either by "preferred term" or "system organ class".
#overall_var: Indicates whether to include an "Overall" column in the output. Set to TRUE to include it, or FALSE to exclude it.
#relation_var: Filters AEs based on their relatedness to the drug. If TRUE, only AEs that are at least "possibly related" to the drug will be included. If FALSE, all AEs are included regardless of relatedness.
#AE_cutoff: Sets the minimum threshold or cutoff value that present only AEs that affect >=x% of all patients, when AE_cutoff=0, all AEs will be presented
#relatedness_prefix: Specifies the common prefix used in column names that represent the relatedness of an AE to drugs.
#action_prefix = "rqstdrugact" pecifies the common prefix used in column names that represent the action to an AE to drugs.
##########SAE change with relatedness#################
create_SAE_table <- function(data, dose_column = "drugsched", sae_column = "rqsaeyn", 
                             type_column = "rqsaeout", relatedness_prefix = "rqinvsda", 
                             drug_labels = c("Drug A", "Drug B"),action_prefix = "rqstdrugact", relatedness_var= TRUE,
                             overall_var = TRUE,cohort_column="cohort",cohort_var=c("Escalation", "Expansion")) {
  library(dplyr)
  library(tidyr)
  library(flextable)
  cohort_regex <- paste(cohort_var, collapse = "|")
  
  # Filter AEdata based on cohort values
  data <- data[grepl(cohort_regex, data[[cohort_column]], ignore.case = TRUE), ]  #Calculate the number of distinct subjects per dose level
  freq_data <- data %>%
    group_by(!!sym(dose_column)) %>%
    summarise(N = n_distinct(!!sym("z_usubjid")), .groups = 'drop')
  
  # Ensure that all dose columns appear in freq_data with 0 if missing
  dose_levels <- sort(unique(data[[dose_column]]))
  freq_data <- freq_data %>%
    right_join(tibble(!!sym(dose_column) := dose_levels), by = dose_column) %>%
    replace_na(list(N = 0))
  
  #Filter data to include only rows where SAE column is "Yes"
  data <- data[!is.na(data[[sae_column]]),]
  data <- data[data[[sae_column]] == "Yes", ]
  #Capture relatedness columns
  relatedness_columns <- names(data)[grepl(relatedness_prefix, names(data))]
  
  # Ensure drug_labels length matches the relatedness_columns length
  if (length(drug_labels) != length(relatedness_columns)) {
    stop("Length of drug_labels does not match the number of relatedness columns.")
  }
  
  # Relatedness categories
  relatedness_categories <- c("Not related", "Unlikely", "Possible", "Probably", "Highly probable", "Unobtainable")
  relatedness_categories <- factor(relatedness_categories, levels = relatedness_categories)
  


  #if relatedness_var, exclude SAEs not related/unlikely related with the drug
  if (relatedness_var) {
    data <- data %>%
      filter(
        if_any(all_of(relatedness_columns), ~ !grepl("Not related|Unlikely", .))
      )
  }
  #Calculate the count of SAEs per dose level, filling missing levels with 0
  sae_counts <- data %>%
    group_by(!!sym(dose_column)) %>%
    summarise(count = n(), .groups = 'drop') %>%
    right_join(freq_data, by = dose_column) %>%
    replace_na(list(count = 0))%>%
    select(-N)
  #Count types in the "rqsaeout" column across dose levels, filling missing values with 0
  type_counts <- data %>%
    group_by(!!sym(dose_column), !!sym(type_column)) %>%
    summarise(count = n(), .groups = 'drop') %>%
    spread(!!sym(type_column), count, fill = 0) %>%
    right_join(freq_data, by = dose_column) %>%
    mutate(across(-!!sym(dose_column), ~ replace_na(., 0)))%>%
    select(-N) 

  # Calculate frequencies for each relatedness category for each drug
  relatedness_count <- list()
  for (i in seq_along(relatedness_columns)) {
    col <- relatedness_columns[i]
    data[[col]] <- factor(data[[col]], levels = levels(relatedness_categories))
    
    # Create a complete grid of dose levels and relatedness categories
    all_combinations <- expand_grid(
      !!sym(dose_column) := dose_levels,
      !!sym(col) := relatedness_categories
    )
    
    # Calculate counts with missing values as 0
    rel_count <- data %>%
      group_by(!!sym(dose_column), !!sym(col)) %>%
      summarise(count = n(), .groups = 'drop') %>%
      right_join(all_combinations, by = c(as.character(dose_column), col)) %>%
      replace_na(list(count = 0)) %>%
      spread(!!sym(col), count)
    
    relatedness_count[[i]] <- rel_count
    names(relatedness_count)[i] <- paste("Relatedness with", drug_labels[i])
  }
  
  #action columns similarly to relatedness columns
  action_columns <- names(data)[grepl(action_prefix, names(data))]
  action_count <- list()
  
  for (i in seq_along(action_columns)) {
    col <- action_columns[i]
    rel_count <- data %>%
      group_by(!!sym(dose_column), !!sym(col)) %>%
      summarise(count = n(), .groups = 'drop') %>%
      right_join(tibble(!!sym(dose_column) := dose_levels), by = dose_column) %>%
      spread(!!sym(col), count, fill = 0)
    
    action_count[[i]] <- rel_count
    names(action_count)[i] <- paste("Action taken with", drug_labels[i])
  }
  
  # Create row labels and category mappings
  type_labels <- unique(data[[type_column]])
  num_type <- length(type_labels)
  
  table_data <- data.frame(
    Category = c(
      "SAE", 
      rep("Type", num_type),
      unlist(lapply(names(relatedness_count), function(name) {rep(name, ncol(relatedness_count[[name]]) - 1)})),
      unlist(lapply(names(action_count), function(name) {rep(name, ncol(action_count[[name]]) - 1)}))  
    ),
    Row_Label = c(
      "SAE", 
      type_labels,
      unlist(lapply(relatedness_count, function(x) colnames(x)[-1])),
      unlist(lapply(action_count, function(x) colnames(x)[-1]))
    )
  )
  
  # Populate `table_data` with counts for each dose level
  for (dose in dose_levels) {
    sae_col <- sae_counts %>% filter(!!sym(dose_column) == dose) %>% select(count) %>% unlist(use.names = FALSE)
    type_col <- type_counts %>% filter(!!sym(dose_column) == dose) %>% select(-!!sym(dose_column)) %>% unlist(use.names = FALSE)
    relatedness_col <- unlist(lapply(relatedness_count, function(df) {
      df %>% filter(!!sym(dose_column) == dose) %>% select(-!!sym(dose_column)) %>% unlist(use.names = FALSE)
    }))
    action_col <- unlist(lapply(action_count, function(df) {
      df %>% filter(!!sym(dose_column) == dose) %>% select(-!!sym(dose_column)) %>% unlist(use.names = FALSE)
    }))
    
    # Assign data to the appropriate column in table_data
    table_data[[dose]] <- c(sae_col, type_col, relatedness_col, action_col)
  }
  
  #Add overall column if requested
  if (overall_var) {
    table_data$Overall <- rowSums(table_data[, 3:(2 + length(dose_levels))], na.rm = TRUE)
  }
  
  # Set column names for table_data based on dose levels
  colnames(table_data)[3:(2 + length(dose_levels))] <- paste0(dose_levels, " N=", freq_data$N)
  
  if (overall_var) {
    colnames(table_data)[ncol(table_data)] <- "Overall"
  }
  #Remove rows whose row label=NA
  table_data <- table_data %>%
    filter(Row_Label != "<NA>")
  #if present AEs related to tested drugs, remove Not related and Unlikely
  if (relatedness_var){
    table_data <- table_data %>%
      filter(Row_Label != "Not related" & Row_Label != "Unlikely")
  }
  # Create and format the flextable
  ft <- flextable(table_data) %>%
    set_header_labels(Category = "Category", Row_Label = "") %>%
    merge_v(j = 1) %>%
    align(align = "center", part = "all") %>%
    bold(part = "header") %>%
    fontsize(size = 10, part = "all")

  return(ft)
}
# Example of running the function with SAE data
# Assuming 'AE2' is your dataframe and contains the relatedness columns and dose levels
# Here, 'drug_labels' specifies names like "Drug A", "Drug B"
###by dose in escalation cohort
#print all SAE
create_SAE_table(AE2, dose_column = "drugsched",sae_column = "rqsaeyn",type_column = "rqsaeout", relatedness_pre = "rqinvsda", drug_labels = c("Drug A","Drug B"), relatedness_var= FALSE,overall_var = TRUE,cohort_column="cohort",cohort_var="Escalation")
#print those related with A or B and relevant actions
create_SAE_table(AE2, dose_column = "drugsched",sae_column = "rqsaeyn",type_column = "rqsaeout", relatedness_pre = "rqinvsda", drug_labels = c("Drug A","Drug B"), relatedness_var= TRUE,overall_var = TRUE,cohort_column="cohort",cohort_var="Escalation")
###by group in all cohort
create_SAE_table(AE2, dose_column = "group",sae_column = "rqsaeyn",type_column = "rqsaeout", relatedness_pre = "rqinvsda", drug_labels = c("Drug A","Drug B"), relatedness_var= FALSE,overall_var = TRUE,cohort_column="cohort",cohort_var=c("Escalation", "Expansion"))




