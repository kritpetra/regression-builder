### Loads packages ###
require(shiny)
if( !require(ggvis) ) install.packages("ggvis")
require(ggvis)
if( !require(magrittr) ) install.packages("magrittr")
require(magrittr)
if( !require(DT) ) install.packages("DT")
require(DT)


### Loads initial dataset ###
initialDataset <- read.csv("movies.csv", stringsAsFactors = FALSE)

### Specifies the column which has the unique names of each point
kIdentifyingColumnName <- "Movie"

### Specifies different required variables. Should work for all datasets.
initialIdColumn <- initialDataset[[kIdentifyingColumnName]]
initialVariables <- colnames(initialDataset)
initialNumericVariables <- initialVariables[sapply(initialDataset, class) == "numeric" |
																							sapply(initialDataset, class) == "integer"]

### Variable of current selected variables, used to keep track of changes in user input 
previousInputVars <- initialNumericVariables[1:2]
