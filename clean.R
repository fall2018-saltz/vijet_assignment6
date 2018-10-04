

no_arg <- function(){                                                             #Creating a function to make sure that all the columns except those mentioned in the vector 'drop'
  clean_data <- raw_data
  drop <- c("SUMLEV","REGION","DIVISION","STATE")                                 #are deleted from the data frame.
  clean_data <- clean_data[,!(colnames(clean_data) %in% drop)]                          # %%%%****I am having a doubt regarding this step and while calling the function, the doubt is mentioned below.
  clean_data <- clean_data[!(clean_data$NAME=="United States" | clean_data$NAME=="Puerto Rico Commonwealth"),]
}

dfStates <- no_arg() 
str(dfStates)
print(dfStates)

arrest <- USArrests
print(head(arrest))
merged <- merge(dfStates,arrest,sort=FALSE)
