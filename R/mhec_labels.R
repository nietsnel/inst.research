#' MHEC_variables function
#' @description A function to attach USM-MHEC variable labels to a file currently loaded to memory.
#' Specifically, this file renames database style names to those more appropriate for reading and or tables.
#' @param dataset = name of the object (file) that you wish to rename with USM labels.
#' @param labels_output = FALSE (default). TRUE outputs the labels used in the function.

#' @return Returns dataset with USM labels and optionally the raw labels used to name the file.
#' @examples MHEC_variables(dataset=my_dataset)
#' @keywords MHEC, University system of maryland, IR
#' @author Jordan L. Prendez, \email{jordanprendez@@gmail.com}
#' @export
#' @examples


MHEC_variables <- function(dataset       = NULL,
                           labels_output = FALSE) {

  dat <- dataset
  labels_output2 <- labels_output
  if("haven" %in% rownames(installed.packages()) == FALSE) {install.packages("haven")}
  if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}
  if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}

  library(stringr)
  library(haven)
  library(tidyverse)

  labels_matrix <- as.data.frame(matrix(data=c(
    "AcadStanding", 	"Acad Standing",
    "AcceptFlag", 	"Admit Accept Flag",
    "ACTcomp", 	"ACT Comp",
    "ACTenglish", 	"ACT English",
    "ACTmath", 	"ACT Math",
    "ACTread", 	"ACT Read",
    "ACTscience", 	"ACT Science",
    "AdmitExempt", 	"Admission Exempt",
    "AdmitTestReq", 	"Admission Test Flag",
    "AidAppFlag", 	"Fin Aid App Flag",
    "AidAward", 	"Fin Aid Award",
    "AidCat", 	"Aid Category",
    "AmerIndian", 	"Amer Indian/Nat Alaskan",
    "AppResidency", 	"Application Residency",
    "ApptStatus", 	"Appoint. Status",
    "ApptStatus", 	"Appoint. Status",
    "AppZip", 	"Apply Zip",
    "AppZip4", 	"Apply Zip+4",
    "Asian", 	"Asian",
    "AttendStatus", 	"Attend Status",
    "Birthdate", 	"Birthdate YYYYMMDD",
    "Black", 	"AA/Black",
    "Building", 	"Building",
    "CampusId", 	"Campus ID",
    "ClassroomType", 	"Classroom Type",
    "CollectTerm", 	"Collection Term",
    "CollectYear", 	"Collection Year",
    "CommuteStatus", 	"Commuter Status",
    "Contract", 	"Contract",
    "ContractRenew", 	"Contract Renew Flag",
    "CostAttend", 	"Cost to Attend",
    "CrHrsReq", 	"CHs Req for Award",
    "CrseDays", 	"Days Course Meets",
    "CrseEndDate", 	"Course End Date YYYYMMDD",
    "CrseEndTime", 	"Course End Time",
    "CrseEnrollment", 	"Course Enrollment",
    "CrseHours", 	"Course Hours",
    "CrseHoursType", 	"Course Hours Type",
    "CrseNum", 	"Course Number",
    "CrseOutcome", 	"Course Outcome",
    "CrseSection", 	"Section Num",
    "CrseShareFlag", 	"Course Share Flag",
    "CrseShareInst", 	"Course Share Institution",
    "CrseStartDate", 	"Course Start Date YYYYMMDD",
    "CrseStartTime", 	"Course Start Time",
    "CrseStatus", 	"Course Status",
    "CrseSubject", 	"Course Subject",
    "CrseTitle", 	"Course Title",
    "CumCrHrsAward", 	"Cum CHs Awarded",
    "CumDegCrHrsAward", 	"Cum Degree CHs Award",
    "CumGpa", 	"Cum GPA",
    "CumNatCrHrsEarn", 	"Cum Native CHs Earned",
    "CumNatCrHrsEarn", 	"Cum Native CHs Earned",
    "CurrZip", 	"Current Zip",
    "CurrZip4", 	"Current Zip+4",
    " ", 	"",
    "*Labels D-I.", 	"",
    "Variable Labels", 	"",
    "DadEdAttain", 	"Parent1 Educ Attainment",
    "Degree", 	"Degree",
    "Degree", 	"Degree",
    "DegreeDate", 	"Degree Award Date YYYYMMDD",
    "DependStatus", 	"Dependent Status",
    "DistEdFlag", 	"Dist Ed. Enroll Flag",
    "DistEdLocation", 	"Dist Ed. Location",
    "EFC", 	"EFC",
    "EmployDateCurrentRank", 	"Employ Date Current Fac Rank MMYY",
    "EmployStatus", 	"Employment Status",
    "EmployStatus", 	"Employment Status",
    "EnglRemedAssess", 	"English Remedial Assess Status",
    "EnrollFlag", 	"Admit Enroll Flag",
    "EnrollResidency", 	"Enrollment Residency",
    "EntryCreditEngl", 	"Entry Credit English Flag",
    "EntryCreditMath", 	"Entry Credit Math Flag",
    "EntryTerm", 	"Entry Term",
    "EntryYear", 	"Entry Year YYYY",
    "FacEmail", 	"Faculty Email",
    "FacFirstName", 	"Faculty First Name",
    "FacLastName", 	"Faculty Last Name",
    "FacPhone", 	"Faculty Phone",
    "FallAttendStatus", 	"Fall Attend Status",
    "FamilySize", 	"Family Size",
    "FirstEnglGrade", 	"First English Grade",
    "FirstMajorLink", 	"First Major Link",
    "FirstMathGrade", 	"First Math Grade",
    "FirstName", 	"First Name",
    "FirstTimeStatByLev", 	"First-Time Status by Level",
    "FirstTimeStatus", 	"First-Time Status",
    "FreezeFlag", 	"Fall Freeze Flag",
    "Gender", 	"Gender",
    "GeoOrigin", 	"Geo Origin",
    "GradAsstFlag", 	"Grad Assistant Flag",
    "HEGIS6", 	"HEGIS6",
    "HighDegree", 	"Highest Degree",
    "HighDegree", 	"Highest Degree",
    "Hispanic", 	"Hispanic",
    "HomeInstTerm", 	"Home Institution Term",
    "HsCode", 	"HS Code",
    "HsGpa", 	"HS GPA",
    "HsGradDate", 	"HS Grad Date YYYYMMDD",
    "IdType", 	"ID Type",
    "Income", 	"Adj Gross Income",
    "InitialEmployDate", 	"Initial Employ Date MMYY",
    "InitialEmployDate", 	"Initial Employ Date MMYY",
    "InstrLocation", 	"Instructional Location",
    "InstrLocation", 	"Instructional Location",
    "InstrSubCode", 	"Instruction Sub-Code",
    "InstrType", 	"Instruction Type",
    "", 	"",
    "*Labels L-Z.", 	"",
    "Variable Labels", 	"",
    "LastName", 	"Last Name",
    "MathRemedAssess", 	"Math Remedial Assess Status",
    "MaxEnroll", 	"Max Enrollment",
    "MiddleName", 	"Middle Name",
    "MilitaryStatus", 	"Military Status",
    "MomEdAttain", 	"Parent2 Educ Attainment",
    "NatHawaiian", 	"Nat Hawaiian/Pac Islander",
    "NewSATMath", 	"New SAT Math",
    "NewSATEBRW", 	"New SAT EBRW",
    "OccupationAssign", 	"Principal Occupation",
    "OccupationAssign", 	"Principal Occupation",
    "OPEID", 	"OPEID",
    "PriorDegFlag", 	"Prior Degree Flag",
    "ProgAssign", 	"Program Assignment",
    "PromoteFlag", 	"Promotion Flag",
    "Rank", 	"Rank",
    "RCProgFlag", 	"RC Program Flag",
    "ReadRemedAssess", 	"Reading Remedial Assess Status",
    "RemedEngl", 	"Remedial English",
    "RemedMath", 	"Remedial Math",
    "RemedRead", 	"Remedial Reading",
    "Residency", 	"Residency",
    "RevTransFlag", 	"Reverse Transfer Flag",
    "Room", 	"Room",
    "Salary", 	"Salary",
    "SASID", 	"SASID",
    "SATmath", 	"SAT Math",
    "SATverb", 	"SAT Verbal",
    "SATwrite", 	"SAT Writing",
    "SpringAttendStatus", 	"Spring Attend Status",
    "SsnId", 	"SSN",
    "StudentLevel", 	"Student Level",
    "SubCampusCode", 	"Sub-Campus Code",
    "Suffix", 	"Suffix",
    "TeachCandidate", 	"Teacher Candidate",
    "Tenure", 	"Tenure",
    "Tenure", 	"Tenure",
    "TermGpa", 	"Term GPA",
    "TermNatCrHrsAttempt", 	"Term Nat CHs Attempted",
    "TermNatCrHrsReg", 	"Term Nat CHs Registered",
    "TermNatDegCrHrsAttempt", 	"Term Nat Deg CHs Attempted",
    "TermNatDegCrHrsEarn", 	"Term Nat Deg CHs Earned",
    "TransCrHrsAward", 	"External CHs Awarded",
    "TransOPEID", 	"Source OPEID",
    "TransOPEID", 	"Transfer OPEID",
    "TuitionStatus", 	"Tuition Status",
    "UScitizen", 	"US Citizen",
    "White", 	"White"), ncol=2, byrow=TRUE), stringsAsFactors = FALSE)
# dat <- 1
  if(length(dat)>0){
    for(i in 1:dim(labels_matrix)[1]){
      old_var_name <- as.character(labels_matrix[i,1])
      new_old_name<-  as.character(labels_matrix[i,2])
      # print(i)
      names(dat)[names(dat) == old_var_name] <- new_old_name
    }
    output_file<<-dat
  }

  if(labels_output2==TRUE){
  labels_matrix<<-labels_matrix
  }
}
