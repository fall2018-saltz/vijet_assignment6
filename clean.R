
library(dplyr)
library(ggplot2)
no_arg <- function(){                                                             #Creating a function to make sure that all the columns except those mentioned in the vector 'drop'
  clean_data <- raw_data
  drop <- c("SUMLEV","REGION","DIVISION","STATE")                                 #are deleted from the data frame.
  clean_data <- clean_data[,!(colnames(clean_data) %in% drop)]                          # %%%%****I am having a doubt regarding this step and while calling the function, the doubt is mentioned below.
  clean_data <- clean_data[!(clean_data$NAME=="United States" | clean_data$NAME=="Puerto Rico Commonwealth"),]
}

dfStates <- no_arg() 
#------------------------------------------------------------------------------------------------------
#2).Copy the USArrests dataset into a local variable (similar to HW 2)
arrest <- USArrests

#------------------------------------------------------------------------------------------------------
#3)Create a merged dataframe -- with the attributes from both dataframes.
merged <- merge(dfStates, arrest, by.x="NAME",by.y="row.names",all=TRUE)                                         #Merging the 2 data sets into one. The state names for both the data sets are in 'NAME' for the 1st and are row names for the 2nd.So we combine/merge the data sets by those 2 columns.

#------------------------------------------------------------------------------------------------------
#Step B: Explore the Data – Understanding distributions
#------------------------------------------------------------------------------------------------------
#4). Create a histogram using GGPLOT for the population and a different histogram for the murder rate

#Before I start plotting, I feel the need to explain the role of 'ggplot'. It is essentially a way of filling up a 'blueprint'. Irrespective of the type of the plot, 'ggplot' will always be called
#ggplot takes the data frame to be plotted as an argument. In addition to that there is an 'aes' aspect to it(aesthetics). It handles what the plot will look as per, i.e. the values to be plotted and where 
#to plot them. The functions in addition to ggplot and following it define the type of plot (like histogram, box plot and scatter plot in our case).
#------------------------------------------------------------------------------------------------------
g_pop <- ggplot(merged, aes(POPESTIMATE2017))+geom_histogram(fill='red',colour='black')                          #'ggplot' and its importance is explained aove. The geom_histogram is used to plot a histogram for the data set in the ggplot and on the basis of the parameters in the ggplot function. In geom_histogram, 'fill' is the color/category to be filled in the column.
g_pop                                                                                                            #'g_pop' is the object in which ggplot(...)+geom_histogram(...) is returning the data regarding the plot. Calling this object will display the plot. This object can be used as an input to an image file as well, thus saving the output as an image.

g_pop1 <- ggplot(merged, aes(Murder))+geom_histogram(fill='red',colour='black')
g_pop1

#I felt no need to make nay adjustments o the scales in order to improve the quality of visualization with these histograms.

#------------------------------------------------------------------------------------------------------
#5). Create a boxplot for the population, and a different boxplot for the murder rate.

#As per my understanding, box plot can be used to describe just one variable or can be used to describe one variable with respect to another. However, from the question, it can be infered that
#we need to plot a box plot for just one variable. I tried passing just the X-axis variable to the function, however, it won't work with box plot function. So I tried plotting population with respect
#to populationitself, it works but didn't make any solid sense. So I took a vector with just one value, 1, like a count for number of boxes maybe and plotted it on X-axis.
#------------------------------------------------------------------------------------------------------

my_list <- factor(1)                                                                                             #Creating a vector with value 1, that'd act as an input for X-axis for boxplot.

g_box <- ggplot(merged,aes(x=my_list,y=POPESTIMATE2017))+geom_boxplot()                                          #After passing the data frame and parameters through ggplot, we are plotting a box plot using geom_boxplot(). As per the 'aes' in ggplot, 'my_list'(vector with 1) is to be plotted on X-axis and Population on Y-axis.
g_box

#g_murder <- merged %>% filter(!is.na(Murder))                                                                    #Only with this plot we won't get the warning of removal of a row, because we are explicitly removing the row with 'Murder' field equal to 'NA'.
g_box1 <- ggplot(merged,aes(x=my_list,y=Murder))+geom_boxplot()                                                 #Same as the previous box plot, just the difference is that the Y-axis now plots Murder rate and not population.
g_box1

#------------------------------------------------------------------------------------------------------
#I'd say that, personally, I'd prefer the box pot over histogram as I find it more informative. Box plot not only gives a range of the values
#but also shows the median value and the outliers. It is, in turn, a more wholesome visualization in my opinion. Although histogram has an 
#advantage over box plot and that is: histogram potrays a visualization of segregated data. Thus, we get a broader idea of the distribution of data. 
#------------------------------------------------------------------------------------------------------

#Step C: Which State had the Most Murders – bar charts
#------------------------------------------------------------------------------------------------------
#6). Calculate the number of murders per state.

merged["n_of_murders"] <- NA                                                                                     #Creating a new column in the 'merged' data set, named n_of_murders. The value of this column for every row is by default set to NA.
merged$n_of_murders <- round((merged$Murder*merged$POPESTIMATE2017)/100000)                                      #To calculate the number of murders, we multiply the rate by the population of that state and divide it by 100000, since the rate is measured per 100000 citizens of the state.
#------------------------------------------------------------------------------------------------------

#7). Generate a bar chart, with the number of murders per state.
g_bar <- ggplot(merged, aes(x=reorder(NAME, -n_of_murders), y=n_of_murders,fill=n_of_murders))                   #The X-axis has state names plotted on it and the states on X-axis are sorted in descending order of number of murders. The bars are fileld with respect to the murder rates
g_bar <- g_bar+geom_col()+ggtitle("Murder per state")+theme(plot.title=element_text(hjust=0.5,colour="blue",size=13))   #geom_col() is used to plot bar charts. ggtitle is used to define a title for the plot. 'theme' is used to customize the title(its theme basically). We are setting the height(from the plot), colour and font size of the title.
g_bar

g_bar1 <- ggplot(merged, aes(x=reorder(NAME, -n_of_murders), y=n_of_murders,fill=n_of_murders))
g_bar1 <- g_bar1+geom_col()+ggtitle("Total Muders")
g_bar1 <- g_bar1+theme(axis.text.x=element_text(angle=90,hjust=1))+xlab("States")+ylab("Number of Murders")+theme(plot.title=element_text(hjust=0.5,colour="black",size=14))   #Tilting the x-lables(state names) by 90 degrees and adjusting their heights.The rest is same as before.
g_bar1

g_bar2 <- ggplot(merged, aes(x=reorder(NAME, -Murder), y=n_of_murders,fill=n_of_murders))                        #Sorting by murder rate instead of number of murders. Rest is same as before.
g_bar2 <- g_bar2+geom_col()+ggtitle("Total Muders")
g_bar2 <- g_bar2+theme(axis.text.x=element_text(angle=90,hjust=1))+xlab("States")+ylab("Number of Murders")+theme(plot.title=element_text(hjust=0.5,colour="black",size=14))
g_bar2

g_bar3 <- ggplot(merged, aes(x=reorder(NAME, -Murder), y=n_of_murders,fill=PCNT_POPEST18PLUS))                   #Same as previous, just that the bars are filled by the % of population above 18 years of age
g_bar3 <- g_bar3+geom_col()+ggtitle("Total Muders")
g_bar3 <- g_bar3+theme(axis.text.x=element_text(angle=90,hjust=1))+xlab("States")+ylab("Number of Murders")+theme(plot.title=element_text(hjust=0.5,colour="black",size=14))
g_bar3
#------------------------------------------------------------------------------------------------------

scatter <- ggplot(merged,aes(x=POPESTIMATE2017,y=POPEST18PLUS2017,color=Murder,size=Murder))+geom_point()        #Scatter plot, with Population on X axis, population above 18 on Y axis and the circles(plot points) attributed in size and color by the murder rate of each state.
scatter

g_pop2 <- scatter
g_pop4 <- scatter
#------------------------------------------------------------------------------------------------------
#NOTE to the grader: Every time I run my plots(except for box plot for 'Murder rates' i.e. 'Murder' column of merged data frame) it will give a warning: 1 entry removed. The entry is for 'District of Columbia', for it had no row in the USarrest table. So in the merged data frame, 
#the values for the columns pertaining to the USarrest data set are set to NA for 'District of Columbia' and that is why that row is dropped while running plots.
