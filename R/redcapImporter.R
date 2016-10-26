#' REDCap importer function
#'                                             
#' This function takes patient data from HIVE Enrollment project on REDCap and processes it, transforming the data into that described by the REDCap import template
#' 
#' @param hive.enroll.file Data exported from the HIVE Enrollment project on REDCap as a file path ending in .csv, enclosed in quotes, defaults to "U:/EPID/Monto/HIVE_2016_2017/REDCap/Data Exports/HIVE enrollment export update.csv".
#' @param hive.study.file The output file name as a file path enclosed in quotes. Defaults to "U:/EPID/Monto/HIVE_2016_2017/REDCap/Data Exports/HIVE study import update.csv".
#' @param arm Specify the arm of the study. the function accepts 1, 2 and 3 for re-enrollees, new HIVE, and pediatric HIVE, respectively. Defaults to 2.
#' @export
redcap.importer <- function(hive.enroll.file="U:/EPID/Monto/HIVE_2016_2017/REDCap/Data Exports/HIVE enrollment export update.csv", 
                            hive.study.file="U:/EPID/Monto/HIVE_2016_2017/REDCap/Data Exports/HIVE study import update.csv", 
                            arm=2){
  
  try(if(!(arm %in% c(1,2,3))){
    stop("arm not one of 1, 2, or 3")})
  try(if(!exists("hive.enroll.file")){
    stop("hive.enroll.file does not exist or is not chosen")})
  try(if(!exists("hive.study.file")){
    stop("hive.study.file does not exist or is not chosen")})
  
  # Import data (extension = .csv) (default location will be from the downloads folder)
  hive.enrollment.data <- read.csv(file=hive.enroll.file, stringsAsFactors=FALSE)
  
  if(arm==1){
    arm.txt = "enrollment_arm_1"
    
    # variables to be moved
    pt.info.vars <- c("studyid","houseid_demhlth","fname","lname","dob","enroll_date")
    
    # reduce to variables described in pt.info.vars and subset, removing structual missing values
    hive.enroll.reduced <- hive.enrollment.data[ ,colnames(hive.enrollment.data) %in% pt.info.vars]
    hive.enroll.reduced.subset <- subset(hive.enroll.reduced, !is.na(hive.enroll.reduced$houseid_demhlth))
    
    # rename variables
    colnames(hive.enroll.reduced.subset) <- sub("houseid_demhlth","houseid",colnames(hive.enroll.reduced.subset))
    colnames(hive.enroll.reduced.subset) <- sub("enroll_date","enrolldate",colnames(hive.enroll.reduced.subset))
    
    hive.enroll.reduced.subset <- within(hive.enroll.reduced.subset,{
      # add patient_information_complete
      patient_information_complete <- 2
      # add recordid as studyid
      recordid <- studyid
      # add redcap_event_name
      redcap_event_name <- arm.txt
    })
    
    # add household e_mail
    email.by.hh <- hive.enrollment.data[,c("houseid","house_email")]
    colnames(email.by.hh) <- sub("house_email","email",colnames(email.by.hh))
    email.by.hh.subset <- subset(email.by.hh, !is.na(email.by.hh$houseid))
    hive.enroll.reduced.subset.final <- merge(email.by.hh.subset,
                                              hive.enroll.reduced.subset,
                                              by="houseid")
    temp <- hive.enroll.reduced.subset.final
    
    # reorder data columns
    temp <- temp[,c("recordid","redcap_event_name","studyid","houseid","fname", 
                    "lname","email","dob","enrolldate","patient_information_complete")]
    
    # write to file for import into REDCap
    write.csv(temp, file=hive.study.file, row.names=FALSE)
    
    # free memory
    rm(hive.enrollment.data,
       pt.info.vars,
       hive.enroll.reduced,
       hive.enroll.reduced.subset,
       email.by.hh,
       email.by.hh.subset,
       hive.enroll.reduced.subset.final,
       temp)
  }
  else if(arm==2){
    arm.txt = "enrollment_arm_2"
    
    # variables to be moved
    pt.info.vars <- c("studyid_a2","houseid_demhlth_a2","fname_a2","lname_a2","dob_a2","enroll_date_a2","email_followup")
    
    # reduce to variables described in pt.info.vars and subset, removing structual missing values
    hive.enroll.reduced <- hive.enrollment.data[ ,colnames(hive.enrollment.data) %in% pt.info.vars]
    hive.enroll.reduced.subset <- subset(hive.enroll.reduced, !is.na(hive.enroll.reduced$houseid_demhlth))
    
    # rename variables
    colnames(hive.enroll.reduced.subset) <- sub("houseid_demhlth_a2","houseid",colnames(hive.enroll.reduced.subset))
    colnames(hive.enroll.reduced.subset) <- sub("email_followup","email",colnames(hive.enroll.reduced.subset))
    colnames(hive.enroll.reduced.subset) <- sub("studyid_a2","studyid",colnames(hive.enroll.reduced.subset))
    colnames(hive.enroll.reduced.subset) <- sub("enroll_date_a2","enrolldate",colnames(hive.enroll.reduced.subset))
    colnames(hive.enroll.reduced.subset) <- sub("fname_a2","fname",colnames(hive.enroll.reduced.subset))
    colnames(hive.enroll.reduced.subset) <- sub("lname_a2","lname",colnames(hive.enroll.reduced.subset))
    colnames(hive.enroll.reduced.subset) <- sub("dob_a2","dob",colnames(hive.enroll.reduced.subset))
    
    hive.enroll.reduced.subset <- within(hive.enroll.reduced.subset,{
      # add patient_information_complete
      patient_information_complete <- 2
      # add recordid as studyid
      recordid <- studyid
      # add redcap_event_name
      redcap_event_name <- arm.txt
    })
    
    # reorder data columns
    hive.enroll.reduced.subset <- hive.enroll.reduced.subset[,c("recordid","redcap_event_name","studyid",
                                                                "houseid","fname", "lname","email","dob",
                                                                "enrolldate","patient_information_complete")]
    
    # write to file for import into REDCap
    write.csv(hive.enroll.reduced.subset, file=hive.study.file, row.names=FALSE)
    
    # free memory
    rm(hive.enrollment.data,
       pt.info.vars,
       hive.enroll.reduced,
       hive.enroll.reduced.subset)
  }
  else if(arm==3){
    arm.txt = "enrollment_arm_3"
    
    # variables to be moved
    pt.info.vars <- c("studyid_a3","houseid_demhlth_a3","fname_a3","lname_a3","dob_a3","enroll_date_a3")
    
    # reduce to variables described in pt.info.vars and subset, removing structual missing values
    hive.enroll.reduced <- hive.enrollment.data[ ,colnames(hive.enrollment.data) %in% pt.info.vars]
    hive.enroll.reduced.subset <- subset(hive.enroll.reduced, !is.na(hive.enroll.reduced$houseid_demhlth))
    
    # rename variables
    colnames(hive.enroll.reduced.subset) <- sub("houseid_demhlth_a3","houseid",colnames(hive.enroll.reduced.subset))
    colnames(hive.enroll.reduced.subset) <- sub("studyid_a3","studyid",colnames(hive.enroll.reduced.subset))
    colnames(hive.enroll.reduced.subset) <- sub("enroll_date_a3","enrolldate",colnames(hive.enroll.reduced.subset))
    colnames(hive.enroll.reduced.subset) <- sub("fname_a3","fname",colnames(hive.enroll.reduced.subset))
    colnames(hive.enroll.reduced.subset) <- sub("lname_a3","lname",colnames(hive.enroll.reduced.subset))
    colnames(hive.enroll.reduced.subset) <- sub("dob_a3","dob",colnames(hive.enroll.reduced.subset))
    
    hive.enroll.reduced.subset <- within(hive.enroll.reduced.subset,{
      # add patient_information_complete
      patient_information_complete <- 2
      # add recordid as studyid
      recordid <- studyid
      # add redcap_event_name
      redcap_event_name <- arm.txt
    })
    
    # add household e_mail
    email.by.hh <- hive.enrollment.data[,c("houseid","house_email")]
    colnames(email.by.hh) <- sub("house_email","email",colnames(email.by.hh))
    email.by.hh.subset <- subset(email.by.hh, !is.na(email.by.hh$houseid))
    hive.enroll.reduced.subset.final <- merge(email.by.hh.subset,
                                              hive.enroll.reduced.subset,
                                              by="houseid")
    temp <- hive.enroll.reduced.subset.final
    
    # reorder data columns
    temp <- temp[,c("recordid","redcap_event_name","studyid",
                    "houseid","fname", "lname","email","dob",
                    "enrolldate","patient_information_complete")]
    
    # write to file for import into REDCap
    write.csv(temp, file=hive.study.file, row.names=FALSE)
    
    # free memory
    rm(hive.enrollment.data,
       pt.info.vars,
       hive.enroll.reduced,
       hive.enroll.reduced.subset,
       hive.enroll.reduced.subset.final,
       temp)
  }
  print("File Done")
  print("See 'HIVE study import update.csv' in U:/EPID/Monto/HIVE_2016_2017/REDCap/Data Exports")
}
