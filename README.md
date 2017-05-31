To install the latest version of *inst.research* from github:

``` r
# install.packages("devtools")
devtools::install_github("nietsnel/inst.research")
```

Functions Overview
------------------

-   `bulk_import()` A function to import multiple .txt or .csv files into R. (website example forthcoming).
-   `usm_labels()` labels variables and raw values using one of three different methods.

### usm\_labels() usage:

usm\_labels is a function to attach labels to a dataframe currently loaded in memory. This process can be facilitated using one of three methods: (1) by using the default MHEC labels included with the inst.research package; (2) by using custom user defined value labels in the R-console; or (3) by loading an external dataframe frame into R that contains the value-label pairings. These three methods are presented in the examples below.

#### 1. Attach default labels and variable names.

The "inst.research" package includes an unlabeled "example\_dataset" (*?example\_dataset* for more info). To attach the default MHEC label pairings to this dataset follow the example below.

``` r
##### Example 1.

library(inst.research) #Attach the inst.research package

print(head(example_dataset, 12), row.names=FALSE) #View the example dataset. 

 # IDType Degree Gender UScitizen
 #      2     60      2         1
 #      2     60      2         2
 #      2     40      2         1
 #      3     81      2         1
 #      2     81      1         2
 #      2     81      1         2
 #      2     60      1         1
 #      3     81      2         1
 #      1     40      1         2
 #      1     40      2         1
 #      1     60      2         2
 #      2     40      2         2


usm_labels(dataset = example_dataset, label_values = TRUE, label_variables = TRUE)  
##note that there are options to re-label the values and or the variables themselves.
##See usm_labels help to view all parameter options. 

print(head(output_file, 12), row.names=FALSE) #View the example dataset with labels attached. 

 # IDType    Degree Gender US Citizen
 #      1  Doc. R/S Female    Foreign
 #      2  Doc. R/S Female US Citizen
 #      2 Bachelors   Male US Citizen
 #      2   Masters   Male    Foreign
 #      2 Bachelors Female    Foreign
 #      1   Masters Female US Citizen
 #      1  Doc. R/S Female US Citizen
 #      1  Doc. R/S Female    Foreign
 #      2 Bachelors   Male US Citizen
 #      2   Masters   Male US Citizen
 #      1  Doc. R/S   Male    Foreign
 #      2 Bachelors   Male US Citizen
```

#### 2. Define value-label pairs in R.

Secondly, value-label pairs can be written directly in R. This is useful when the list of value-label pairings is short. This method can utilized by following the simple formatting shown in the example below.

``` r
##### Example 2. 
library(inst.research) #Attach the inst.research package


data_def<- c("var.name_IDType"   , 1, "Student", 2, "faculty", 3, "staff",
             "var.name_USCitizen", 1, "Yes", 2, "No",
             "var.name_Gender", 1, "male", 2, "female",
             "var.name_Degree", 40, "BA", 60, "MA", 81, "AA")


# Note: each variable name must follow the "var.name_" prefix. Secondly, each value (e.g., 1, 2, etc) must be 
# paired with a label (eg., "student"). Once the variables have been defined, call the object using the
# **manual_label_input** parameter in the usm_labels function as shown below. 


usm_labels(dataset=example_dataset, label_variables = FALSE, label_values=FALSE, manual_label_input=data_def)
##Attach user-defined labels to example dataset.


print(head(output_file, 12), row.names=FALSE) #View the example dataset with labels attached. 

 # IDType Degree Gender UScitizen
 # Student     AA female         2
 # faculty     AA female         1
 # faculty     BA   male         1
 # faculty     MA   male         2
 # faculty     BA female         2
 # Student     MA female         1
 # Student     AA female         1
 # Student     AA female         2
 # faculty     BA   male         1
 # faculty     MA   male         1
 # Student     AA   male         2
 # faculty     BA   male         1
```

#### 3. Define value-label pairs using an external data.frame

A dataframe containing value-label pairs can also be used for relabeling. This is useful when there are a large amount of value-label pairings stored in an external file (e.g, comma separated file.)

The value-label pairings must be in the following format.

    "Degree",        "86",  "Doc. Other",
    "Degree",        "87",  "Non-Deg Grad",
    "Degree",        "99",  "Multi Major",
    "DependStatus",  "0",   "Unknown",
    "DependStatus",  "1",   "Dependent",
    "DependStatus",  "2",   "Independent",
    "DistEdFlag",    "1",   "Exclusively",
    "DistEdFlag",    "2",   "Some",
    "Gender",        "1",   "Male", 
    "Gender,         "2",   "Female"

*Note*: Each line must begin with the variable name corresponding to the value-label pair.

The "inst.research" package includes an unlabeled "example\_dataset" (*?example\_dataset* for more info) which we can combine with a second included dataset called "example\_external\_labels". You can try this process using the procedure shown in the following example.

``` r
##### Example 3. 

##### Step 1.

# load the inst.research package and import your value-label pairings into R (e.g., read_csv()). Because 
# inst.research contains an example labels dataframe this step can be skipped. You can also view both of the 
# example datasets using the print() function.

library(inst.research) ##Attach inst.research library
print(head(example_dataset, 12), row.names=FALSE) #View the example dataset. 

 # IDType Degree Gender UScitizen
 #      2     60      2         1
 #      2     60      2         2
 #      2     40      2         1
 #      3     81      2         1
 #      2     81      1         2
 #      2     81      1         2
 #      2     60      1         1
 #      3     81      2         1
 #      1     40      1         2
 #      1     40      2         1
 #      1     60      2         2
 #      2     40      2         2

print(example_external_labels, row.names=FALSE) #View the example external labels. 

 #           V1 V2          V3
 #       Degree 40          BA
 #       Degree 60          MA
 #       Degree 81          AA
 # DependStatus  0     Unknown
 # DependStatus  1   Dependent
 # DependStatus  2 Independent
 #   DistEdFlag  1 Exclusively
 #   DistEdFlag  2        Some
 #       Gender  1        Male
 #       Gender  2      Female
 #    UScitizen  1         Yes
 #    UScitizen  2          No


##### Step 2.

# Label the example_dataset using the usm_labels() function. 

usm_labels(dataset=example_dataset, label_variables = FALSE, label_values=FALSE, 
           label_matrix=example_external_labels)


# You can then view the results below. 

print(head(output_file, 15), row.names=FALSE) #View the example dataset..

 # IDType Degree Gender UScitizen
 #      1     AA Female        No
 #      2     AA Female       Yes
 #      2     BA   Male       Yes
 #      2     MA   Male        No
 #      2     BA Female        No
 #      1     MA Female       Yes
 #      1     AA Female       Yes
 #      1     AA Female        No
 #      2     BA   Male       Yes
 #      2     MA   Male       Yes
 #      1     AA   Male        No
 #      2     BA   Male       Yes
 #      3     AA Female       Yes
 #      2     BA   Male       Yes
 #      3     BA   Male        No
```
