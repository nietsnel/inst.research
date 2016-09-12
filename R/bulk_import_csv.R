#' bulk import function
#' @description A function to import multiple .txt or .csv files into R.
#'
#' @param folder_path = path of the file containing the raw data (i.e. 1, and 0's not "YES" and "NO"). The  file format currently accepted is .csv and .txt
#' @param merge: Can take on either two values. False: (the default) returns all imported datasets. TRUE: bulk_import attempts to return a single file containing all of the invidual datasets. This currently only works if all datasets contain all of the same variables.
#' @return Returns invidual loaded dataset OR a single combined dataset
#' @examples bulk_import(folder_path = "C:/Users/jprendez/Desktop/bulk_import2", merge=TRUE, file_type="txt")

#' @keywords IR
#' @author Jordan L. Prendez, \email{jordanprendez@@gmail.com}
#' @export
#' @examples
#' bulk_import()
#' Note:SPSS® is a registered trademarks of IBM in the United States.



bulk_import <- function(folder_path, merge=FALSE, file_type="txt"){

  if("devtools" %in% rownames(installed.packages()) == FALSE) {install.packages("devtools")}
  library(devtools)

  if("BH" %in% rownames(installed.packages()) == FALSE) {install.packages("BH")}
  library(BH)

  if("readr" %in% rownames(installed.packages()) == FALSE) {install.packages("readr")}
  library(readr)

  if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
  library(dplyr)


  original_wd<- getwd()
  setwd(folder_path)
  list.filenames<-list.files()
  file_name <- NA
  num_files<- length(list.filenames)
  file_type_temp <- as.character(file_type)

  if(merge==FALSE) {
    for(i in 1:num_files){

      file_name<-list.filenames[i]
      if(file_type=="csv"){
      file_temp<-read_csv(file_name)
      } else if(file_type=="txt"){
      file_temp<-read_delim(file=file_name, delim=",")
      }

      name <- paste(i,file_name,sep="_")
      assign(x=name, value=file_temp, envir = .GlobalEnv)
    } #end of loop
  } else if(merge==TRUE){
    combined_dataset <- data.frame(NA)
    for(i in 1:num_files){

      file_name<-list.filenames[i]
      file_temp<-read_csv(file_name)
      name <- paste(i,"imported_temp",sep="_")
      name <- assign(x=name, value=file_temp) #does not assign to global environment

      if(i==1){
        combined_dataset <- name
      }else if(i>1){
        combined_dataset<- rbind(combined_dataset, name) ##no clude to why bind_rows does not work.
      }

    }

  } #end of else (i.e. merge=TRUE)

  setwd(original_wd)
  combined_dataset<<-combined_dataset
}

