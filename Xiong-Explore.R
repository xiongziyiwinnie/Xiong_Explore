#Problem 1
#To find a way that can find all the numerica columns in a data frame
col_isnum=data_frame[sapply(data_frame,is.numeric)]
#extract all the numeric variables from the data_frame and name them with col_isnum. 
#The class of col_isnum is still data frame.


#Problem 2
require(ggplot2)
data("diamonds")                
#call in the data set "diamonds"
vp<-character()                 
corr<-numeric()
#create an empty character vector to store the numeric variable pairs' name
#create an empty numeric vector to store the mentioned pairs' Pearson correlation
col_isnum=diamonds[sapply(diamonds,is.numeric)]
#extract all the numeric variables from the data_frame and assign them to col_isnum 
vn<-colnames(col_isnum)
#extract the numeric variables' names and assign them to a vector vn
numcol<-length(col_isnum)
#obtain the number of numeric variable
for(i in 1:(numcol-1))
 {
   for(j in (i+1):numcol)
  {
    vp<-c(vp,paste(vn[i],vn[j],sep="-"))
    corr<-c(corr,cor(col_isnum[i],col_isnum[j]))
#start from the first numeric variable
#connect the current variable's name with each of the following's and add this to vp
#this way can avoid repeated pairs
#caculate the two variables' pearson correlation(default) and add the value to corr
   }
 }
prs_co<-data.frame(vp,corr)
#create a data frame with collumns vp,corr
print(prs_co)



#Problem 3
require(ggplot2)
data("diamonds")
#load the package we need to draw plot and the data we will use
col_isnum=diamonds[sapply(diamonds,is.numeric)]
#extract all the numeric variables from the data_frame and assign them to col_isnum 
vn<-colnames(col_isnum)
#extract the numeric variables' names and assign them to a vector vn
numcol<-length(col_isnum)
#obtain the number of numeric variable
for(i in 1:(numcol-1))
{
  for(j in (i+1):numcol)
  {
    c<-cor(col_isnum[i],col_isnum[j])
   p<-ggplot(col_isnum,aes_string(x=vn[i], y=vn[j]))+geom_point()+labs(title=paste("the correlation:",c))
   print(p)
   #start from the first numeric variable
   #do operations on current and each of the following variables
   #this way can avoid repeated pairs
   #caculate the two variables' pearson correlation(default)
   #create scatter plot of these two variables with their correlation as title
  }
}

