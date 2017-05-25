inst.research
=============

To install the latest version from github:

``` r
# install.packages("devtools")
devtools::install_github("nietsnel/inst.research")
```

### Using usm\_labels()

usm\_labels is a fucntion to attach default mhec variable labels to a dataframe currently in memory. Furthermore, usm\_labels can also be used to re-label variables using custom input from two additional methods.

#### 1. Define value-label pairs in R.

You can define value-label pairs directly in R. This is useful when there the list of value-label pairings are short. This can be accomplished using the following simple formatting.

``` r
data_def<- c("var.name_IDType"   , 1, "Student", 2, "faculty", 3, "staff",
             "var.name_USCitizen", 1, "Yes", 2, "No",
             "var.name_Gender", 1, "male", 2, "female",
             "var.name_Degree", 40, "BA", 60, "MA", 81, "AA")
```

Note: each variable name must follow the "var.name\_" handle. Secondly each value (e.g., 1, 2, etc) must be paired with a label (eg., "student").

Once the variable have been defined, call the object using the manual\_label\_input parameter in the usm\_labels function as shown below.

``` r
usm_labels(dataset=your_dataset, label_variables = FALSE, label_values=FALSE, manual_label_input=data_def)
```

#### 2. Define value-label pairs using a data.frame

A dataframe containing value-label pairs can be used for relabelling. This is useful when there are a large amount of value-label pairings stored in an external comma separated file.

Regardless of the file type, once imported, the dataframe must be in the following format.

    "Degree",        "86",  "Doc. Other",
    "Degree",        "87",  "Non-Deg Grad",
    "Degree",        "99",  "Multi Major",
    "DependStatus",  "0",   "Unknown",
    "DependStatus",  "1",   "Dependent",
    "DependStatus",  "2",   "Independent",
    "DistEdFlag",    "1",   "Exclusively",
    "DistEdFlag",    "2",   "Some",
    "Gender",        "1",   "Male", "2", "Female"

*Note*: Each line must begin with the variable name that the value-label pair corresponds to.

Load the definitions

``` r
usm_labels(dataset=your_dataset, label_variables = FALSE, label_values=FALSE, label_matrix=data_def_matrix)
```
