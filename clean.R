

no_arg <- function(){                                                             #Creating a function to make sure that all the columns except those mentioned in the vector 'drop'
  drop <- c("SUMLEV","REGION","DIVISION","STATE")                                 #are deleted from the data frame.
  dfStates <- dfStates[,!(colnames(dfStates) %in% drop)]                          # %%%%****I am having a doubt regarding this step and while calling the function, the doubt is mentioned below.
}

dfStates <- no_arg() 
