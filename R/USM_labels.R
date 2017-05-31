#' usm_labels function
#' @description A function to attach USM-MHEC variable labels to a file currently loaded to memory.
#' Specifically, this file renames database style names to those more appropriate for reading and or tables.
#' @param dataset = name of the object (file) that you wish to rename with USM labels.
#' @param label_values = TRUE (default). TRUE outputs your dataset with MHEC value labels attached.
#' @param label_variables = TRUE (default). TRUE outputs your dataset with MHEC variable labels attached.
#' @param labels_output = FALSE (default). TRUE outputs the labels used in the function.
#' @param manual_label_input specify labels and definitions manually in R. [examples forthcoming]
#' @return Returns dataset with USM value labels and or variable names attached (depending on selected option).
#' @examples usm_labels(dataset=my_dataset, label_values = TRUE, label_variables = FALSE)
#' @examples data_def<- c("var.name_IDType"   , 1, "Student", 2, "faculty", 3, "staff","var.name_USCitizen", 1, "Yes", 2, "No", "var.name_Degree", 40, "BA", 60, "MA", 81, "AA")
#' @examples usm_labels(dataset=res2, label_variables = FALSE, label_values=FALSE, manual_label_input=data_def)
#' @keywords MHEC, University system of maryland, IR
#' @author Jordan L. Prendez, \email{jordanprendez@@gmail.com}
#' @seealso \href{https://github.com/nietsnel/inst.research/blob/master/README.md}{Online example}
#' @export



usm_labels <- function(dataset                 = NULL,
                           label_values        = TRUE,
                           label_variables     = TRUE,
                           labels_output       = FALSE,
                           label_matrix        = NULL,
                           manual_label_input = NULL){

  dat <- dataset
  label_values2 <- label_values
  label_variables2 <- label_variables
  labels_output2 <- labels_output
  label_matrix2 <- label_matrix
  manual_label_input2 <- manual_label_input
  dat <- data.frame(lapply(dat, as.character), stringsAsFactors=FALSE) ##Must be in character form.



  if("haven" %in% rownames(installed.packages()) == FALSE) {install.packages("haven")}
  if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}
  if("tidyr" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyr")}
  if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}


  library(stringr)
  library(haven)
  library(tidyverse)

  if(is.null(manual_label_input2)==FALSE){

    data_def <- manual_label_input2
    starting_vector <- matrix(data=data_def, byrow=TRUE)
    variable_column <- rep(NA,length(starting_vector))
    value_column <- rep(NA,length(starting_vector))

    values_label_matrix <- cbind(variable_column,starting_vector, value_column)
    for(iter in 1:length(starting_vector)){

      if(str_detect(string=starting_vector[iter], "var.name_")==TRUE){
        values_label_matrix[iter+1,1] <- values_label_matrix[iter,2]
        values_label_matrix[iter,2] <- NA
      }
    }
    values_label_matrix <- values_label_matrix[rowSums(is.na(values_label_matrix))!=3, ]
# browser()
    for(iter in 1:dim(values_label_matrix)[1]){
      values_label_matrix[iter-1,3] <- values_label_matrix[iter,2]

    }

    values_label_matrix[seq(2, dim(values_label_matrix)[1], 2),] <- NA
    values_label_matrix <- values_label_matrix[rowSums(is.na(values_label_matrix))!=3, ]

    for(i in 1:dim(values_label_matrix)[1]){
      if(is.na(values_label_matrix[i,1])==TRUE)
        values_label_matrix[i,1] <- values_label_matrix[i-1,1]
    }

    values_label_matrix[,1]<- str_match(string=values_label_matrix[,1], "var.name_(.*)")[,2]
    values_label_matrix <- as.data.frame(values_label_matrix)



    # dat <- data
    for(iter_data_var in 1:length(dat)){

      data_variable <- names(dat[iter_data_var])

      for(i in 1:dim(values_label_matrix)[1]){  ##Changes variable in df of interest.
        variable_name<- as.character((values_label_matrix[i,1]))

        if(variable_name==data_variable){

          data_value_vector <- (as.character(dat[,variable_name]))
          definition_value<- as.character(values_label_matrix[i,2])
          definition_label <- as.character(values_label_matrix[i,3])

          data_value_vector[data_value_vector==definition_value]<-definition_label

          dat[,iter_data_var] <- data_value_vector
        }
      }
    }
  }




  values_label_matrix <- as.data.frame(matrix(data=c(
    "AcceptFlag", 	"1", 	"Accepted",
    "AcceptFlag", 	"2", 	"Rejected",
    "AcceptFlag", 	"3", 	"No Action",
    "AdmitExempt", 	"NA", 	"N/A",
    "AdmitExempt", 	"1", 	"Admit Exempted",
    "AdmitTestReq", 	"0", 	"None Required",
    "AdmitTestReq", 	"1", 	"SAT",
    "AdmitTestReq", 	"2", 	"ACT",
    "AdmitTestReq", 	"3", 	"ACT or SAT",
    "AdmitTestReq", 	"4", 	"Institution Test",
    "AdmitTestReq", 	"5", 	"None - Admit Exempt",
    "AdmitTestReq", 	"6", 	"Transfer - Inst. Determines Test",
    "AidAppFlag", 	"0", 	"Not Applied",
    "AidAppFlag", 	"1", 	"Filed FAFSA",
    "AmerIndian", 	"4", 	"Amer. Indian",
    "AmerIndian", 	"NA", 	"Unknown",
    "AppResidency", 	"NA", 	"Unknown",
    "AppResidency", 	"1", 	"In-State",
    "AppResidency", 	"2", 	"Out-of-State",
    "ApptStatus", 	"NA", 	"Unassigned Fac.-CIS ONLY",
    "ApptStatus", 	"1", 	"Permanent",
    "ApptStatus", 	"2", 	"Temporary",
    "AppZip", 	"0", 	"Unknown",
    "AppZip", 	"99999", 	"Non-US Zip",
    "AppZip4", 	"NA", 	"Unknown",
    "Asian", 	"3", 	"Asian",
    "Asian", 	"NA", 	"Unknown",
    "AttendStatus", 	"1", 	"FT",
    "AttendStatus", 	"2", 	"PT",
    "Birthdate", 	"0", 	"Unknown",
    "Birthdate", 	"NA", 	"NA",
    "Black", 	"2", 	"AA/Black",
    "Black", 	"NA", 	"Unknown",
    "ClassroomType", 	"1", 	"Classroom",
    "ClassroomType", 	"2", 	"Computer Lab",
    "ClassroomType", 	"3", 	"Special Use Lab",
    "ClassroomType", 	"4", 	"Conference Room",
    "ClassroomType", 	"5", 	"Multipurpose Room",
    "ClassroomType", 	"6", 	"Other/Online",
    "CollectTerm", 	"1", 	"Fall",
    "CollectTerm", 	"2", 	"Winter",
    "CollectTerm", 	"3", 	"Spring",
    "CollectTerm", 	"4", 	"Summer",
    "CollectTerm", 	"9", 	"Annual",
    "CommuteStatus", 	"0", 	"Unknown",
    "CommuteStatus", 	"1", 	"On-Campus",
    "CommuteStatus", 	"2", 	"Off-Campus",
    "CommuteStatus", 	"3", 	"Lives w/ Parents",
    "Contract", 	"4", 	"9 Month",
    "Contract", 	"5", 	"10 Month",
    "Contract", 	"6", 	"11 Month",
    "Contract", 	"7", 	"12 Month",
    "Contract", 	"9", 	"<9 Month/Other",
    "ContractRenew", 	"1", 	"Multi-Year",
    "ContractRenew", 	"2", 	"Annual",
    "ContractRenew", 	"3", 	"Other",
    "CrseHoursType", 	"1", 	"Credit",
    "CrseHoursType", 	"2", 	"Non-Credit",
    "CrseHoursType", 	"3", 	"Credit, Non-Credit Student",
    "CrseOutcome", 	"P", 	"Passed",
    "CrseOutcome", 	"F", 	"Failed",
    "CrseOutcome", 	"A", 	"Audited",
    "CrseOutcome", 	"W", 	"Withdrew",
    "CrseOutcome", 	"I", 	"Incomplete",
    "CrseOutcome", 	"Z", 	"Unresolved",
    "CurrZip", 	"0", 	"Unknown",
    "CurrZip", 	"99999", 	"Non-US Zip",
    "CurrZip4", 	"NA", 	"Unknown",
    "DadEdAttain", 	"1", 	"Middle School",
    "DadEdAttain", 	"2", 	"High School",
    "DadEdAttain", 	"3", 	"College or Beyond",
    "DadEdAttain", 	"4", 	"Other/Unknown",
    "DadEdAttain", 	"NA", 	"NA",
    "Degree", 	"0", 	"Unknown",
    "Degree", 	"10", 	"Lower Cert.",
    "Degree", 	"20", 	"AA",
    "Degree", 	"30", 	"Upper Cert.",
    "Degree", 	"40", 	"Bachelors",
    "Degree", 	"47", 	"Non-Deg UG",
    "Degree", 	"50", 	"Post-Bacc. Cert.",
    "Degree", 	"60", 	"Masters",
    "Degree", 	"65", 	"Post-Masters Cert.",
    "Degree", 	"70", 	"Cert. Adv. Study",
    "Degree", 	"81", 	"Doc. R/S",
    "Degree", 	"85", 	"Doc. PP",
    "Degree", 	"86", 	"Doc. Other",
    "Degree", 	"87", 	"Non-Deg Grad",
    "Degree", 	"99", 	"Multi Major",
    "DependStatus", 	"0", 	"Unknown",
    "DependStatus", 	"1", 	"Dependent",
    "DependStatus", 	"2", 	"Independent",
    "DistEdFlag", 	"1", 	"Exclusively",
    "DistEdFlag", 	"2", 	"Some",
    "DistEdFlag", 	"3", 	"None",
    "DistEdLocation", 	"NA", 	"Unknown/Not All DE",
    "DistEdLocation", 	"1", 	"US, in MD",
    "DistEdLocation", 	"2", 	"US, not in MD",
    "DistEdLocation", 	"3", 	"US, State Unknown",
    "DistEdLocation", 	"4", 	"Outside US",
    "EFC", 	"9999999", 	"Unknown",
    "EFC", 	"0", 	"0",
    "EmployDateCurrentRank", 	"0", 	"Unknown",
    "EmployDateCurrentRank", 	"9", 	"N/A, Non-Fac.",
    "EmployStatus", 	"1", 	"FT",
    "EmployStatus", 	"2", 	"PT",
    "EmployStatus", 	"NA", 	"CIS ONLY",
    "EnglRemedAssess", 	"NA", 	"Not Assessed",
    "EnglRemedAssess", 	"1", 	"Assessed & Not Needed",
    "EnglRemedAssess", 	"2", 	"Assessed-Needs English",
    "EnglRemedAssess", 	"3", 	"Unknown Status/Took English",
    "EnrollFlag", 	"1", 	"Enrolled",
    "EnrollFlag", 	"2", 	"Not Enrolled",
    "EnrollResidency", 	"NA", 	"Unknown",
    "EnrollResidency", 	"1", 	"In-State",
    "EnrollResidency", 	"2", 	"Out-of-State",
    "EntryCreditEngl", 	"N", 	"Not Entry-Level Credit Course",
    "EntryCreditEngl", 	"Y", 	"Entry-Level Credit Course",
    "EntryCreditMath", 	"N", 	"Not Entry-Level Credit Course",
    "EntryCreditMath", 	"Y", 	"Entry-Level Credit Course",
    "EntryTerm", 	"1", 	"Fall",
    "EntryTerm", 	"2", 	"Winter",
    "EntryTerm", 	"3", 	"Spring",
    "EntryTerm", 	"4", 	"Summer",
    "FallAttendStatus", 	"0", 	"Not Enrolled-FAIS ONLY",
    "FallAttendStatus", 	"1", 	"FT",
    "FallAttendStatus", 	"2", 	"PT",
    "FamilySize", 	"0", 	"Unknown",
    "FirstEnglGrade", 	"NA", 	"Not Avail./Not Taken",
    "FirstEnglGrade", 	"A", 	"A",
    "FirstEnglGrade", 	"B", 	"B",
    "FirstEnglGrade", 	"C", 	"C",
    "FirstEnglGrade", 	"D", 	"D",
    "FirstEnglGrade", 	"P", 	"Pass",
    "FirstEnglGrade", 	"F", 	"Fail",
    "FirstMathGrade", 	"NA", 	"Not Aval./Not Taken",
    "FirstMathGrade", 	"A", 	"A",
    "FirstMathGrade", 	"B", 	"B",
    "FirstMathGrade", 	"C", 	"C",
    "FirstMathGrade", 	"D", 	"D",
    "FirstMathGrade", 	"P", 	"Pass",
    "FirstMathGrade", 	"F", 	"Fail",
    "FirstTimeStatByLev", 	"1", 	"UG - First-Time",
    "FirstTimeStatByLev", 	"2", 	"UG - Other",
    "FirstTimeStatByLev", 	"3", 	"Masters - First-Time",
    "FirstTimeStatByLev", 	"4", 	"Masters - Other",
    "FirstTimeStatByLev", 	"5", 	"Doc. PP - First-Time",
    "FirstTimeStatByLev", 	"6", 	"Doc. PP - Other",
    "FirstTimeStatByLev", 	"7", 	"Doc. R/S - First-Time",
    "FirstTimeStatByLev", 	"8", 	"Doc. R/S - Other",
    "FirstTimeStatByLev", 	"9", 	"Grad. Other - First-Time",
    "FirstTimeStatByLev", 	"0", 	"Grad. Other - Other",
    "FirstTimeStatus", 	"0", 	"Concurr. HS",
    "FirstTimeStatus", 	"1", 	"1st Time UG",
    "FirstTimeStatus", 	"2", 	"Continuing UG",
    "FirstTimeStatus", 	"3", 	"1st Time Grad",
    "FirstTimeStatus", 	"4", 	"Continuing Grad.",
    "FirstTimeStatus", 	"5", 	"UG New Transfer",
    "FreezeFlag", 	"1", 	"Include",
    "FreezeFlag", 	"NA", 	"Exclude",
    "Gender", 	"1", 	"Male",
    "Gender", 	"2", 	"Female",
    "Gender", 	"3", 	"Male Assigned",
    "Gender", 	"4", 	"Female Assigned",
    "GeoOrigin", 	"0", 	"Unknown",
    "GeoOrigin", 	"1", 	"Foreign",
    "GeoOrigin", 	"200", 	"MD Unknown County",
    "GeoOrigin", 	"201", 	"Allegany",
    "GeoOrigin", 	"202", 	"Anne Arundel",
    "GeoOrigin", 	"203", 	"Baltimore",
    "GeoOrigin", 	"204", 	"Calvert",
    "GeoOrigin", 	"205", 	"Caroline",
    "GeoOrigin", 	"206", 	"Carroll",
    "GeoOrigin", 	"207", 	"Cecil",
    "GeoOrigin", 	"208", 	"Charles",
    "GeoOrigin", 	"209", 	"Dorchester",
    "GeoOrigin", 	"210", 	"Frederick",
    "GeoOrigin", 	"211", 	"Garrett",
    "GeoOrigin", 	"212", 	"Harford",
    "GeoOrigin", 	"213", 	"Howard",
    "GeoOrigin", 	"214", 	"Kent",
    "GeoOrigin", 	"215", 	"Montgomery",
    "GeoOrigin", 	"216", 	"Prince George",
    "GeoOrigin", 	"217", 	"Queen Anne",
    "GeoOrigin", 	"218", 	"St. Mary",
    "GeoOrigin", 	"219", 	"Somerset",
    "GeoOrigin", 	"220", 	"Talbot",
    "GeoOrigin", 	"221", 	"Washington",
    "GeoOrigin", 	"222", 	"Wicomico",
    "GeoOrigin", 	"223", 	"Worcester",
    "GeoOrigin", 	"224", 	"Baltimore City",
    "GeoOrigin", 	"100", 	"US Unknown State",
    "GeoOrigin", 	"101", 	"Alabama",
    "GeoOrigin", 	"102", 	"Alaska",
    "GeoOrigin", 	"103", 	"Arizona",
    "GeoOrigin", 	"104", 	"Arkansas",
    "GeoOrigin", 	"105", 	"California",
    "GeoOrigin", 	"106", 	"Colorado",
    "GeoOrigin", 	"107", 	"Connecticut",
    "GeoOrigin", 	"108", 	"Delaware",
    "GeoOrigin", 	"109", 	"DC",
    "GeoOrigin", 	"110", 	"Florida",
    "GeoOrigin", 	"111", 	"Georgia",
    "GeoOrigin", 	"112", 	"Hawaii",
    "GeoOrigin", 	"113", 	"Idaho",
    "GeoOrigin", 	"114", 	"Illinois",
    "GeoOrigin", 	"115", 	"Indiana",
    "GeoOrigin", 	"116", 	"Iowa",
    "GeoOrigin", 	"117", 	"Kansas",
    "GeoOrigin", 	"118", 	"Kentucky",
    "GeoOrigin", 	"119", 	"Louisiana",
    "GeoOrigin", 	"120", 	"Maine",
    "GeoOrigin", 	"122", 	"Massachusetts",
    "GeoOrigin", 	"123", 	"Michigan",
    "GeoOrigin", 	"124", 	"Minnesota",
    "GeoOrigin", 	"125", 	"Mississippi",
    "GeoOrigin", 	"126", 	"Missouri",
    "GeoOrigin", 	"127", 	"Montana",
    "GeoOrigin", 	"128", 	"Nebraska",
    "GeoOrigin", 	"129", 	"Nevada",
    "GeoOrigin", 	"130", 	"New Hampshire",
    "GeoOrigin", 	"131", 	"New Jersey",
    "GeoOrigin", 	"132", 	"New Mexico",
    "GeoOrigin", 	"133", 	"New York",
    "GeoOrigin", 	"134", 	"North Carolina",
    "GeoOrigin", 	"135", 	"North Dakota",
    "GeoOrigin", 	"136", 	"Ohio",
    "GeoOrigin", 	"137", 	"Oklahoma",
    "GeoOrigin", 	"138", 	"Oregon",
    "GeoOrigin", 	"139", 	"Pennsylvania",
    "GeoOrigin", 	"140", 	"Rhode Island",
    "GeoOrigin", 	"141", 	"South Carolina",
    "GeoOrigin", 	"142", 	"South Dakota",
    "GeoOrigin", 	"143", 	"Tennessee",
    "GeoOrigin", 	"144", 	"Texas",
    "GeoOrigin", 	"145", 	"Utah",
    "GeoOrigin", 	"146", 	"Vermont",
    "GeoOrigin", 	"147", 	"Virginia",
    "GeoOrigin", 	"148", 	"Washington",
    "GeoOrigin", 	"149", 	"West Virginia",
    "GeoOrigin", 	"150", 	"Wisconsin",
    "GeoOrigin", 	"151", 	"Wyoming",
    "GeoOrigin", 	"152", 	"American Samoa",
    "GeoOrigin", 	"153", 	"Canal Zone",
    "GeoOrigin", 	"154", 	"Guam",
    "GeoOrigin", 	"155", 	"Puerto Rico",
    "GeoOrigin", 	"156", 	"Trust Terr. Pacific Island",
    "GeoOrigin", 	"157", 	"Virgin Islands",
    "GradAsstFlag", 	"NA", 	"Not a GA",
    "GradAsstFlag", 	"1", 	"Graduate Assistant",
    "HighDegree", 	"0", 	"N/A, Non-Fac.",
    "HighDegree", 	"1", 	"< Bachelors",
    "HighDegree", 	"2", 	"Bachelors",
    "HighDegree", 	"3", 	"Post-Bacc. Cert.",
    "HighDegree", 	"4", 	"Masters",
    "HighDegree", 	"5", 	"First-Prof.",
    "HighDegree", 	"6", 	"Cert. Adv. Study",
    "HighDegree", 	"7", 	"Doctorate",
    "HighDegree", 	"8", 	"Other",
    "HighDegree", 	"9", 	"Unknown",
    "Hispanic", 	"Y", 	"Hispanic",
    "Hispanic", 	"N", 	"Non-Hispanic",
    "Hispanic", 	"NA", 	"Unknown",
    "HsGpa", 	"NA", 	"Unknown",
    "HsGradDate", 	"NA", 	"Unknown/NA",
    "IdType", 	"1", 	"SSN",
    "IdType", 	"2", 	"Campus ID",
    "IdType", 	"3", 	"ITIN",
    "Income", 	"99999999", 	"Unknown",
    "Income", 	"0", 	"0",
    "InitialEmployDate", 	"0", 	"Unknown",
    "InstrLocation", 	"1", 	"Main Campus",
    "InstrLocation", 	"2", 	"Branch Campus",
    "InstrLocation", 	"11", 	"USG",
    "InstrLocation", 	"12", 	"USMH",
    "InstrLocation", 	"13", 	"HEAT",
    "InstrLocation", 	"14", 	"SMHEC",
    "InstrLocation", 	"15", 	"Waldorf",
    "InstrLocation", 	"16", 	"Arundel Mills",
    "InstrLocation", 	"17", 	"Laurel",
    "InstrLocation", 	"18", 	"Eastern Shore",
    "InstrLocation", 	"30", 	"Towson at HCC-USM Code",
    "InstrLocation", 	"31", 	"Other Off-Campus Site",
    "InstrLocation", 	"51", 	"Online",
    "InstrSubCode", 	"NA", 	"Not Primarily Instr.",
    "InstrSubCode", 	"1", 	"Credit Only",
    "InstrSubCode", 	"2", 	"Non-Credit Only",
    "InstrSubCode", 	"3", 	"Both Credit/Non-Credit",
    "InstrType", 	"A", 	"Traditional",
    "InstrType", 	"B", 	"Blended Hybrid",
    "InstrType", 	"C", 	"Online",
    "InstrType", 	"D", 	"Site-to-Site",
    "InstrType", 	"E", 	"Other Tech. Dist. Ed.",
    "InstrType", 	"F", 	"Correspondence",
    "InstrType", 	"G", 	"Other",
    "MathRemedAssess", 	"NA", 	"Not Assessed",
    "MathRemedAssess", 	"1", 	"Assessed & Not Needed",
    "MathRemedAssess", 	"2", 	"Assessed-Needs Math",
    "MathRemedAssess", 	"3", 	"Unknown Status/Took Math",
    "MilitaryStatus", 	"1", 	"Active duty",
    "MilitaryStatus", 	"2", 	"Veteran or former active duty",
    "MilitaryStatus", 	"3", 	"Reserve duty",
    "MilitaryStatus", 	"4", 	"Spouse or dependent child of active, rese",
    "MilitaryStatus", 	"5", 	"None of the above",
    "MomEdAttain", 	"1", 	"Middle School",
    "MomEdAttain", 	"2", 	"High School",
    "MomEdAttain", 	"3", 	"College or Beyond",
    "MomEdAttain", 	"4", 	"Other/Unknown",
    "MomEdAttain", 	"NA", 	"NA",
    "NatHawaiian", 	"5", 	"Nat. Hawaiian",
    "NatHawaiian", 	"NA", 	"Unknown",
    "OccupationAssign", 	"0", 	"Unknown",
    "OccupationAssign", 	"11", 	"Management",
    "OccupationAssign", 	"12", 	"Business & Financial Oper.",
    "OccupationAssign", 	"13", 	"Computer, Engineering & Sciences",
    "OccupationAssign", 	"14", 	"Comm. Service, Legal, Arts & Media",
    "OccupationAssign", 	"15", 	"Instruction",
    "OccupationAssign", 	"16", 	"Instruction w/ Res./Pub. Serv.",
    "OccupationAssign", 	"17", 	"Research",
    "OccupationAssign", 	"18", 	"Public Service",
    "OccupationAssign", 	"19", 	"Grad. Teaching Assts.",
    "OccupationAssign", 	"20", 	"Graduate Assistants – Research (I/R/PS)",
    "OccupationAssign", 	"21", 	"Archivists, Curators & Museum Techs",
    "OccupationAssign", 	"22", 	"Librarians",
    "OccupationAssign", 	"23", 	"Library Techs",
    "OccupationAssign", 	"24", 	"Other Teachers & Instr. Support Staff",
    "OccupationAssign", 	"25", 	"Healthcare Practitioners & Tech. Occup.",
    "OccupationAssign", 	"26", 	"Service Occup.",
    "OccupationAssign", 	"27", 	"Sales & Related Occup.",
    "OccupationAssign", 	"28", 	"Office & Admin. Support",
    "OccupationAssign", 	"29", 	"Nat. Resources, Construction & Maintenance",
    "OccupationAssign", 	"30", 	"Production, Transport. & Material Moving",
    "OccupationAssign", 	"31", 	"Military Staff",
    "OPEID", 	"206200", 	"BSU",
    "OPEID", 	"206800", 	"CSU",
    "OPEID", 	"207200", 	"FSU",
    "OPEID", 	"209100", 	"SU",
    "OPEID", 	"209900", 	"TU",
    "OPEID", 	"210200", 	"UB",
    "OPEID", 	"210300", 	"UMCP",
    "OPEID", 	"210400", 	"UMB",
    "OPEID", 	"210500", 	"UMBC",
    "OPEID", 	"210600", 	"UMES",
    "OPEID", 	"795900", 	"USMO",
    "OPEID", 	"1164400", 	"UMUC",
    "OPEID", 	"90795900", 	"UMCES",
    "PriorDegFlag", 	"NA", 	"NA",
    "PriorDegFlag", 	"Y", 	"Prior Degree/Cert.",
    "ProgAssign", 	"1", 	"Agric. & Nat. Res.",
    "ProgAssign", 	"2", 	"Arch. & Envtl. Des.",
    "ProgAssign", 	"3", 	"Area Studies",
    "ProgAssign", 	"4", 	"Biological Sciences",
    "ProgAssign", 	"5", 	"Business & Managmt.",
    "ProgAssign", 	"6", 	"Communications",
    "ProgAssign", 	"7", 	"Comp. & Info. Sci.",
    "ProgAssign", 	"8", 	"Education",
    "ProgAssign", 	"9", 	"Engineering",
    "ProgAssign", 	"10", 	"Fine & Applied Arts",
    "ProgAssign", 	"11", 	"Foreign Languages",
    "ProgAssign", 	"12", 	"Health Professions",
    "ProgAssign", 	"13", 	"Home Economics",
    "ProgAssign", 	"14", 	"Law",
    "ProgAssign", 	"15", 	"Letters",
    "ProgAssign", 	"16", 	"Library Science",
    "ProgAssign", 	"17", 	"Mathematics",
    "ProgAssign", 	"18", 	"Military Science",
    "ProgAssign", 	"19", 	"Physical Science",
    "ProgAssign", 	"20", 	"Psychology",
    "ProgAssign", 	"21", 	"Public Affairs",
    "ProgAssign", 	"22", 	"Social Sciences",
    "ProgAssign", 	"23", 	"Theology",
    "ProgAssign", 	"49", 	"Interdisciplinary St.",
    "ProgAssign", 	"50", 	"Business & Commer. Technologies",
    "ProgAssign", 	"51", 	"Data Processing Tech.",
    "ProgAssign", 	"52", 	"Paramedical Tech.",
    "ProgAssign", 	"53", 	"Mechanical & Engineering Tech.",
    "ProgAssign", 	"54", 	"Natural Sci. Tech.",
    "ProgAssign", 	"55", 	"Public Service  Tech.",
    "ProgAssign", 	"56", 	"Arts & Science",
    "ProgAssign", 	"96", 	"Continuing Education",
    "ProgAssign", 	"97", 	"Faculty--Unknown",
    "ProgAssign", 	"98", 	"Preclinical & Clinical Med.",
    "ProgAssign", 	"99", 	"Non-Faculty Emp.",
    "PromoteFlag", 	"1", 	"Yes",
    "PromoteFlag", 	"2", 	"No",
    "PromoteFlag", 	"3", 	"N/A, Non-Prof.",
    "PromoteFlag", 	"9", 	"Unknown",
    "Rank", 	"1", 	"Prof.",
    "Rank", 	"2", 	"Assoc. Prof.",
    "Rank", 	"3", 	"Asst. Prof.",
    "Rank", 	"4", 	"Instructor",
    "Rank", 	"5", 	"Lecturer",
    "Rank", 	"6", 	"Rank Unknown, Fac.",
    "Rank", 	"7", 	"N/A, Non-Fac.",
    "RCProgFlag", 	"11", 	"USG",
    "RCProgFlag", 	"12", 	"USMH",
    "RCProgFlag", 	"13", 	"HEAT",
    "RCProgFlag", 	"14", 	"SMHEC",
    "RCProgFlag", 	"15", 	"Waldorf",
    "RCProgFlag", 	"16", 	"Arundel Mills",
    "RCProgFlag", 	"17", 	"Laurel",
    "RCProgFlag", 	"18", 	"Eastern Shore",
    "RCProgFlag", 	"30", 	"Towson at HCC-USM Code",
    "RCProgFlag", 	"31", 	"Other Off-Campus Site",
    "RCProgFlag", 	"51", 	"Online",
    "ReadRemedAssess", 	"NA", 	"Not Assessed",
    "ReadRemedAssess", 	"1", 	"Assessed & Not Needed",
    "ReadRemedAssess", 	"2", 	"Assessed-Needs Reading",
    "ReadRemedAssess", 	"3", 	"Unknown Status/Took Reading",
    "RemedEngl", 	"0", 	"Non-Remedial",
    "RemedEngl", 	"1", 	"Remedial",
    "RemedMath", 	"0", 	"Non-Remedial",
    "RemedMath", 	"1", 	"Remedial",
    "RemedRead", 	"0", 	"Non-Remedial",
    "RemedRead", 	"1", 	"Remedial",
    "Residency", 	"1", 	"MD Resident",
    "Residency", 	"2", 	"Non-MD Resident",
    "RevTransFlag", 	"NA", 	"NA",
    "RevTransFlag", 	"Y", 	"Yes",
    "Salary", 	"1", 	"Unknown",
    "Salary", 	"7", 	"Fac. Leave w/Out Pay",
    "Salary", 	"8", 	"Military Pay Scale",
    "Salary", 	"9", 	"Paid by Bookkeeping",
    "Salary", 	"5", 	"Work-study",
    "SASID", 	"NA", 	"Unknown",
    "SpringAttendStatus", 	"0", 	"Not Enrolled-FAIS ONLY",
    "SpringAttendStatus", 	"1", 	"FT",
    "SpringAttendStatus", 	"2", 	"PT",
    "StudentLevel", 	"1", 	"UG Freshman",
    "StudentLevel", 	"2", 	"UG Sophomore",
    "StudentLevel", 	"3", 	"UG Junior",
    "StudentLevel", 	"4", 	"UG Senior+",
    "StudentLevel", 	"5", 	"Grad 1st Yr",
    "StudentLevel", 	"6", 	"Grad 2+ Yrs",
    "StudentLevel", 	"7", 	"UG Unclassified",
    "StudentLevel", 	"8", 	"Grad Unclassified",
    "TeachCandidate", 	"104", 	"Early Childhood Education PreK-Grade 3",
    "TeachCandidate", 	"249", 	"English for Speakers of Other Language",
    "TeachCandidate", 	"300", 	"Agriculture 7-12",
    "TeachCandidate", 	"334", 	"Special Education: Hearing Impaired No",
    "TeachCandidate", 	"340", 	"Special Education: Severely & Profound",
    "TeachCandidate", 	"346", 	"Special Education: Visually Impaired N",
    "TeachCandidate", 	"381", 	"Special Education: Generic Birth -grad",
    "TeachCandidate", 	"383", 	"Special Education: Generic 1-8",
    "TeachCandidate", 	"383", 	"Special Education: Generic 6-12",
    "TeachCandidate", 	"421", 	"Library Media Specialist Not Applicabl",
    "TeachCandidate", 	"428", 	"School Psychologist (Level II) Not App",
    "TeachCandidate", 	"428", 	"School Psychologist Not Applicable",
    "TeachCandidate", 	"517", 	"Administration Not Applicable",
    "TeachCandidate", 	"1213", 	"STEM FIELD:  Biology 7-12",
    "TeachCandidate", 	"1200", 	"French PreK-12",
    "TeachCandidate", 	"1202", 	"German PreK-12",
    "TeachCandidate", 	"1205", 	"Spanish PreK-12",
    "TeachCandidate", 	"1207", 	"Russian PreK-12",
    "TeachCandidate", 	"1214", 	"STEM FIELD Chemistry 7-12",
    "TeachCandidate", 	"1215", 	"STEM FIELD Earth/Space Science 7-12",
    "TeachCandidate", 	"1222", 	"STEM FIELD Physical Science 7-12",
    "TeachCandidate", 	"1223", 	"STEM FIELD Physics 7-12",
    "TeachCandidate", 	"1232", 	"History 7-12",
    "TeachCandidate", 	"1234", 	"Social Studies 7-12",
    "TeachCandidate", 	"1241", 	"Theater 7-12",
    "TeachCandidate", 	"1242", 	"English 7-12",
    "TeachCandidate", 	"1244", 	"STEM FIELD Mathematics 7-12",
    "TeachCandidate", 	"1250", 	"STEM FIELD Computer Science 7-12",
    "TeachCandidate", 	"1303", 	"Art PreK-12",
    "TeachCandidate", 	"1305", 	"Business Education 7-12",
    "TeachCandidate", 	"1311", 	"Health Education 7-12",
    "TeachCandidate", 	"1312", 	"Family and Consumer Sciences Education 7-12",
    "TeachCandidate", 	"1313", 	"Family and Consumer Sciences Education 7-12",
    "TeachCandidate", 	"1316", 	"Music PreK-12",
    "TeachCandidate", 	"1319", 	"Physical Education PreK-12",
    "TeachCandidate", 	"1331", 	"English Language Arts 4-9",
    "TeachCandidate", 	"1332", 	"STEM FIELD Mathematics 4-9",
    "TeachCandidate", 	"1335", 	"Dance Not Applicable",
    "TeachCandidate", 	"1338", 	"STEM FIELD Science 4-9",
    "TeachCandidate", 	"1339", 	"Social Studies 4-9",
    "TeachCandidate", 	"1557", 	"Gifted and Talented Specialist Not App",
    "TeachCandidate", 	"1559", 	"Elementary Education 1-6",
    "TeachCandidate", 	"1561", 	"American Sign Language PreK-12",
    "TeachCandidate", 	"1564", 	"STEM FIELD Mathematics Instructional L",
    "TeachCandidate", 	"1580", 	"Arabic PreK-12",
    "TeachCandidate", 	"1581", 	"Chinese PreK-12",
    "TeachCandidate", 	"1582", 	"Italian PreK-12",
    "TeachCandidate", 	"1583", 	"Japanese PreK-12",
    "TeachCandidate", 	"1584", 	"Latin PreK-12",
    "TeachCandidate", 	"9999", 	"Not Applicable",
    "TeachCandidate", 	"0104/0381", 	"DUAL CANDIDATE Early Child PreK-G",
    "TeachCandidate", 	"0104/1559", 	"DUAL CANDIDATE Early Child PreK-G",
    "TeachCandidate", 	"1331/1332", 	"DUAL CANDIDATE English Language A",
    "TeachCandidate", 	"1331/1338", 	"DUAL CANDIDATE English Language A",
    "TeachCandidate", 	"1331/1339", 	"DUAL CANDIDATE English Language A",
    "TeachCandidate", 	"1332/1338", 	"DUAL CANDIDATE STEM FIELD Mathema",
    "TeachCandidate", 	"1332/1339", 	"DUAL CANDIDATE STEM FIELD Mathema",
    "TeachCandidate", 	"1559/0104", 	"DUAL CANDIDATE Elementary Educati",
    "TeachCandidate", 	"1559/0383", 	"DUAL CANDIDATE Elementary Educati",
    "TeachCandidate", 	"0381/0340", 	"DUAL CANDIDATE Special Education",
    "TeachCandidate", 	"0383/0340", 	"DUAL CANDIDATE Special Education:",
    "TeachCandidate", 	"1232/1234", 	"DUAL CANDIDATE History 7-12 & Soc",
    "TeachCandidate", 	"1311/1319", 	"DUAL CANDIDATE Health Education 7",
    "TeachCandidate", 	"1338/1339", 	"DUAL CANDIDATE STEM FIELD Science",
    "Tenure", 	"1", 	"Tenured",
    "Tenure", 	"2", 	"Tenure Track",
    "Tenure", 	"3", 	"Non-Tenure Track",
    "Tenure", 	"4", 	"N/A, Non-Fac.",
    "Tenure", 	"NA", 	"Unknown-CIS ONLY",
    "TRANSOPEID", 	"210300", 	"UMCP",
    "TRANSOPEID", 	"210400", 	"UMB",
    "TRANSOPEID", 	"210500", 	"UMBC",
    "TRANSOPEID", 	"210600", 	"UMES",
    "TRANSOPEID", 	"1164400", 	"UMUC",
    "TRANSOPEID", 	"206200", 	"BSU",
    "TRANSOPEID", 	"206800", 	"CSU",
    "TRANSOPEID", 	"207200", 	"FSU",
    "TRANSOPEID", 	"209100", 	"SU",
    "TRANSOPEID", 	"209900", 	"TU",
    "TRANSOPEID", 	"210200", 	"UB",
    "TRANSOPEID", 	"205700", 	"Allegany CC",
    "TRANSOPEID", 	"205800", 	"AACC",
    "TRANSOPEID", 	"206300", 	"CCBC",
    "TRANSOPEID", 	"993500", 	"CCBC",
    "TRANSOPEID", 	"207000", 	"CCBC",
    "TRANSOPEID", 	"830800", 	"Cecil",
    "TRANSOPEID", 	"206400", 	"Col of Southern MD",
    "TRANSOPEID", 	"465000", 	"Chesapeake",
    "TRANSOPEID", 	"207100", 	"Frederick",
    "TRANSOPEID", 	"1001400", 	"Garrett",
    "TRANSOPEID", 	"207400", 	"Hagerstown",
    "TRANSOPEID", 	"207500", 	"Harford",
    "TRANSOPEID", 	"817500", 	"Howard",
    "TRANSOPEID", 	"691100", 	"MC",
    "TRANSOPEID", 	"2907400", 	"MC",
    "TRANSOPEID", 	"208200", 	"MC",
    "TRANSOPEID", 	"208100", 	"MC",
    "TRANSOPEID", 	"208900", 	"PGCC",
    "TRANSOPEID", 	"2073900", 	"Wor Wic",
    "TRANSOPEID", 	"206100", 	"BCCC",
    "TRANSOPEID", 	"208300", 	"Morgan",
    "TRANSOPEID", 	"209500", 	"St Marys",
    "TuitionStatus", 	"2", 	"MD Resident",
    "TuitionStatus", 	"3", 	"Non-MD Resident",
    "TuitionStatus", 	"5", 	"Empl/Dep. Tuit. Waiver",
    "UScitizen", 	"1", 	"US Citizen",
    "UScitizen", 	"2", 	"Foreign",
    "White", 	"1", 	"White",
    "White", 	"NA", 	"Unknown",
    "AidCat", 	"1101", 	"Pell Grants",
    "AidCat", 	"1102", 	"SEOG",
    "AidCat", 	"1103", 	"Other Federal Grants",
    "AidCat", 	"1104", 	"State Educational Assistance Grants",
    "AidCat", 	"1105", 	"State Guaranteed Access Grant",
    "AidCat", 	"1106", 	"State Part-Time Grant",
    "AidCat", 	"1107", 	"Private Grant Sources",
    "AidCat", 	"1108", 	"Institutional Grants",
    "AidCat", 	"1109", 	"State Tolbert Grant",
    "AidCat", 	"1110", 	"Academic Competitiveness Grant",
    "AidCat", 	"1111", 	"National SMART Grant",
    "AidCat", 	"1112", 	"Early College Access Grant",
    "AidCat", 	"1113", 	"TEACH Grant",
    "AidCat", 	"1114", 	"Irag & Afghanistan Service Grant",
    "AidCat", 	"1201", 	"Federal Perkins Loans",
    "AidCat", 	"1202", 	"Subsidized Stafford/Direct Loans",
    "AidCat", 	"1203", 	"Undsubsidized Stafford/Direct Loans",
    "AidCat", 	"1204", 	"PLUS Loans",
    "AidCat", 	"1205", 	"Other Federal Loans",
    "AidCat", 	"1206", 	"Institutional Loans",
    "AidCat", 	"1207", 	"Private Sources Loans",
    "AidCat", 	"1301", 	"Child Care Provider",
    "AidCat", 	"1302", 	"Dev Disabilities etc Tuition Assistanc",
    "AidCat", 	"1303", 	"Distinguished Scholar Award",
    "AidCat", 	"1304", 	"Delegate Scholarship",
    "AidCat", 	"1305", 	"Senatorial Scholarship",
    "AidCat", 	"1306", 	"HOPE CC Transfer Scholarship",
    "AidCat", 	"1307", 	"HOPE General Scholarship",
    "AidCat", 	"1308", 	"Distinguished Scholar Teacher",
    "AidCat", 	"1309", 	"MD Teacher Scholarship",
    "AidCat", 	"1310", 	"Christa McAuliffe Teacher Ed Award",
    "AidCat", 	"1311", 	"State Nursing Scholarship",
    "AidCat", 	"1312", 	"PT & OT Grants",
    "AidCat", 	"1313", 	"Science & Tech Scholarship",
    "AidCat", 	"1314", 	"Conroy Memorial Scholarship",
    "AidCat", 	"1315", 	"Diversity Grants",
    "AidCat", 	"1316", 	"Federal Scholarships",
    "AidCat", 	"1317", 	"Institutional Athletic Scholarships",
    "AidCat", 	"1318", 	"Other Institutional Scholarships",
    "AidCat", 	"1319", 	"Private Athletic Scholarships",
    "AidCat", 	"1320", 	"Other Private Scholarships",
    "AidCat", 	"1321", 	"Tuition Waivers Employees & Dependents",
    "AidCat", 	"1322", 	"Tuition Waivers Seniors Citizens",
    "AidCat", 	"1323", 	"Tuition Waivers Remission of Fees to S",
    "AidCat", 	"1326", 	"Distinguished Scholar Community Colleg",
    "AidCat", 	"1328", 	"William Donald Schaefer Scholarship",
    "AidCat", 	"1329", 	"GEAR UP Scholarship",
    "AidCat", 	"1330", 	"Workforce Shortage Student Assistance",
    "AidCat", 	"1331", 	"Venterans of Afghanistan & Iraq Confli",
    "AidCat", 	"1332", 	"Tuition Waivers of Fees to Disabled",
    "AidCat", 	"1333", 	"Tuition Waivers of Fees to Foster Care",
    "AidCat", 	"1334", 	"Tuition Waivers of Fees to Homeless Yo",
    "AidCat", 	"1335", 	"2+2 Transfer Scholarship Program",
    "AidCat", 	"1336", 	"Riley Firefighter, Ambulance, Rescue S",
    "AidCat", 	"1401", 	"Federal Work Study",
    "AidCat", 	"1402", 	"Institutional Work Study",
    "AidCat", 	"4601", 	"Grants Federal Sources",
    "AidCat", 	"4602", 	"Grad & Prof School Scholarship",
    "AidCat", 	"4603", 	"Grants from Private Sources",
    "AidCat", 	"4604", 	"Institutional Grants",
    "AidCat", 	"4605", 	"TEACH Grant",
    "AidCat", 	"4606", 	"Irag & Afganistan Service Grant",
    "AidCat", 	"4701", 	"Federal Perkins Loans",
    "AidCat", 	"4702", 	"Federal Subsidized Stafford/Direct Loa",
    "AidCat", 	"4703", 	"Federal Unsubsidized Stafford/Direct L",
    "AidCat", 	"4704", 	"Other Federal Loans",
    "AidCat", 	"4705", 	"Institutional Loans",
    "AidCat", 	"4706", 	"Loans from Private Sources",
    "AidCat", 	"4707", 	"PLUS Loans for Grad & Prof Students",
    "AidCat", 	"4801", 	"Dev Disabilities etc",
    "AidCat", 	"4802", 	"Delegate Scholarship",
    "AidCat", 	"4803", 	"Senatorial Scholarship",
    "AidCat", 	"4804", 	"MD Teacher Scholarship",
    "AidCat", 	"4805", 	"Christa McAuliffe Teacher Ed Award",
    "AidCat", 	"4806", 	"State Nursing Scholarship",
    "AidCat", 	"4807", 	"PT & OT grants",
    "AidCat", 	"4808", 	"Conroy Memorial Scholarship",
    "AidCat", 	"4809", 	"Diversity Grants",
    "AidCat", 	"4810", 	"Federal Scholarships",
    "AidCat", 	"4811", 	"Institutional Athletic Scholarships",
    "AidCat", 	"4812", 	"Other Institutional Scholarships",
    "AidCat", 	"4813", 	"Private Athletic Scholarships",
    "AidCat", 	"4814", 	"Other Private Scholarships",
    "AidCat", 	"4815", 	"Tuition Waivers of Fees to Employees &",
    "AidCat", 	"4816", 	"Tuition Waivers of Fees to Senior Citi",
    "AidCat", 	"4817", 	"Tuition Waivers of Fees to Students",
    "AidCat", 	"4819", 	"William Donald Schaefer Scholarship",
    "AidCat", 	"4820", 	"Graduate Nursing Faculty Scholarship",
    "AidCat", 	"4821", 	"Workforce Shortage Student Assistance",
    "AidCat", 	"4822", 	"Tuition Waivers of Fees to Disabled",
    "AidCat", 	"4823", 	"Tuition Waivers of Fees to Foster Care",
    "AidCat", 	"4824", 	"Tuition Waivers of Fees to Homeless Yo",
    "AidCat", 	"4901", 	"Assistantships",
    "AidCat", 	"4902", 	"Federal Work Study",
    "AidCat", 	"4903", 	"Institutional Work Study"),
    ncol=3, byrow = TRUE))

  # "White", 	"White"), ncol=2, byrow=TRUE), stringsAsFactors = FALSE)




  #  faster method --------------------------------------------------------------

  if(label_values2==TRUE){

    # dat <- data
    for(iter_data_var in 1:length(dat)){

      data_variable <- names(dat[iter_data_var])

      for(i in 1:dim(values_label_matrix)[1]){  ##Changes variable in df of interest.
        variable_name<- as.character((values_label_matrix[i,1]))

        if(variable_name==data_variable){

          data_value_vector <- (as.character(dat[,variable_name]))
          definition_value<- as.character(values_label_matrix[i,2])
          definition_label <- as.character(values_label_matrix[i,3])

          data_value_vector[data_value_vector==definition_value]<-definition_label

          dat[,iter_data_var] <- data_value_vector
        }
      }
    }
}


# external label matrix -------------------------------------------------------------


if(is.null(label_matrix2) == FALSE){

  values_label_matrix <- label_matrix2

  # if(label_values2==TRUE){

    # dat <- data
    for(iter_data_var in 1:length(dat)){

      data_variable <- names(dat[iter_data_var])

      for(i in 1:dim(values_label_matrix)[1]){  ##Changes variable in df of interest.
        variable_name<- as.character((values_label_matrix[i,1]))

        if(variable_name==data_variable){

          data_value_vector <- (as.character(dat[,variable_name]))
          definition_value<- as.character(values_label_matrix[i,2])
          definition_label <- as.character(values_label_matrix[i,3])

          data_value_vector[data_value_vector==definition_value]<-definition_label

          dat[,iter_data_var] <- data_value_vector
        }
      }
    }


}





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

  if(labels_output2==TRUE){
    labels_matrix<<-labels_matrix
  }

  if(label_variables2==TRUE){

    if(length(dat)>0){
      for(i in 1:dim(labels_matrix)[1]){
        old_var_name <- as.character(labels_matrix[i,1])
        new_old_name<-  as.character(labels_matrix[i,2])
        # print(i)
        names(dat)[names(dat) == old_var_name] <- new_old_name
      }
    }


  }

  if(label_variables2 == TRUE | label_values2 == TRUE | is.null(manual_label_input2)==FALSE | is.null(label_matrix2) == FALSE) {
        output_file<<-dat
  }
}



