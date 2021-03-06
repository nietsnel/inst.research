#' import labels function
#' @description A function to import SPSS ®  style labels and attach them to a dataset in R. Formatting instructions can be found in the PDF provided in the "See Also" section.
#'
#' @param dataset = path of the file containing the raw data (i.e. 1, and 0's not "YES" and "NO"). The  file format currently accepted is .csv and .txt
#' @param definitions = path of the file containing definitions. The format of this file is shown below in table 1. File must be a .txt.
#' @return Returns dataset with attached labels
#' @examples import_labels(dataset="C:/Users/jprendez/Desktop/ic2015.csv", definitions="C:/Users/jprendez/Desktop/definitions.txt")
#' @seealso \url{https://github.com/nietsnel/inst.research/blob/master/import_labels_example1.pdf}
#' @keywords IR
#' @author Jordan L. Prendez, \email{jordanprendez@@gmail.com}
#' @export
#' @examples
#' import_labels()
#' Note:SPSS® is a registered trademarks of IBM in the United States.


import_labels <- function(dataset, definitions){
#blah
  # browser()

  if("haven" %in% rownames(installed.packages()) == FALSE) {install.packages("haven")}
  if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}
  library(stringr)
  library(haven)
  df <- read.delim(definitions, header=FALSE, sep="'")
  # df[,2] <- as.character(df[,2])

  df[,2][is.na(df[,2])] <- ""  #TEST

  # df[,2] <- as.integer(df[,2])

    # data[,2][is.na(data[,2])] <- " "

  ##added section that allows for variable levels to be entered in as strings
  df<- df[,colMeans(is.na(df)) == 0]

  df1 <-NA
  df2 <- NA
  rows <- nrow(df)
  for(i in 1:rows){
    if(df[i,1]!=""){
      vec <-c(NA)
      vec <- as.vector(t(df)[,i])
    }
    else{
      vec <- (NA)
      vec <- as.vector(t(df)[,i])
      vec <- c(vec,0)
      vec <- vec[-1]
    }
    df1<-rbind(df1,vec)
  }

  anumeric_dummy_numeric_vector <- "321321"
  df<-df1[-1,] #trims first row (not)


  df[, 1][df[,1] == " "] <- anumeric_dummy_numeric_vector ##This is dummy variable in place of a blank " " space in the R script.
  df[, 1][df[,1] == ""] <- anumeric_dummy_numeric_vector ##This is dummy variable in place of a blank " " space in the R script.

  # df<-df1[-1,] #trims first row (not)
  ##ENDS string formatting section

  data <- read.csv(dataset, as.is=TRUE, na.string="astring_that_doesnt_exist")
  data[is.na(data)] <- anumeric_dummy_numeric_vector

  data[data == ""] <- anumeric_dummy_numeric_vector #test
  data[data == " "] <- anumeric_dummy_numeric_vector #test



  definitions <- df[,1:2]


  nrow(df)
  variable_names <- c(NA)
  for(i in 1:nrow(df)){
    if(str_sub(df[i,1], start=1L, end=1L)=="/"){
      variable_names[i] <- as.matrix(df[i,1])
    }
  }
  variable_names <-variable_names
  number_of_rows <- nrow(definitions)
  df <- data.frame(nrow=(number_of_rows), ncol=3) #this determines how large the matrix containing the variable names needs to be
  for(i in 1:number_of_rows){
    df[i,1] <- variable_names[i]
    df[i,2] <- as.character(definitions[i,1])

    if(is.na(variable_names[i])==TRUE){
      df[i,1] <- df[i-1,1]
      df[i,3] <- as.character(definitions[i,2])
    }
  }
  df2 <- na.omit(df) ##this df contains all the labels and values in the correct format.

  number_of_rows <- nrow(df2)
  num_levels <- c(NA)
  for(i in 1:(number_of_rows-1)){
    #determine how many levels per variable
    if(df2[i,1]!=df2[i+1,1]){
      # print(i+1)
      num_levels[i] <- i
    }
    if(i==(number_of_rows-1)){
      num_levels[i] <- (i+1)
    }
  }
  num_levels <- as.numeric(na.omit(unique(num_levels)))
  num_levels <- c(0,num_levels)
  num_levels<- diff(num_levels)
  num_levels<-num_levels
  var_names<- names(data)
  def_loaded <- unique(variable_names) #shows the list of variables included in the definition file
  def_loaded<- na.omit(def_loaded)  #removes the NA
  def_loaded <- sub(pattern='/', replacement='',def_loaded)
  def_loaded <- str_trim(def_loaded, side=c("both"))
  data_set <- (NA)
  starting_value <- 0
  n <- 1
  for(ii in 1:length(num_levels)){
    levelz <- (NA)
    labelz <- (NA)
    # browser()
    for(i in 1:num_levels[ii]){
      levelz[i] <- as.numeric(df2[i+starting_value, 2])
      labelz[i] <- df2[i+starting_value, 3]
    }
    for(nn in 1:length(names(data))){
      if(def_loaded[n]==names(data[nn])){
        # browser()
        data[,nn] <- factor(data[,nn], levels=levelz, labels=labelz)
      }#change variable column in dataframe (i.e. searches through list of variables in dataset)
    } #change variable in variable list (once the dataset has been searched--we move to next variable)
    starting_value <- (i+starting_value)
    # browser()
    n <- n+1
  }
  #}
  data_set<<-data
}
