explore <- function(dataframe,binsize,threshold)
{
  require("ggplot2")
  data("diamonds")
  
  df_num=dataframe[sapply(dataframe,is.numeric)]
  #extract all the numeric variables from the dataframe passed by argument
  df_fac=dataframe[sapply(dataframe,is.factor)]
  #extract all the factor variables from the dataframe passed by argument
  df_log=dataframe[sapply(dataframe,is.logical)]
  #extract all the logical variables from the dataframe passed by argument
  #note:the class of df_num,df_fac,df_log is data frame
  data_nn=data.frame(df_fac,df_log)
  #combine the factor and logical data frame into one data frame
  
  #1.Plot a pair of blue histograms with a vertical red line at the mean (one using counts and the other density) 
  #for every numerical variable at each bin size specified in the bin sizes input parameter.
  if(length(df_num)!=0){#test that there is at least one numeric column in the dataframe
    col_n=colnames(df_num)
    #obtain the columns' name of df_num,that is all numeric columns' name
    for (i in 1:length(binsize))
      #go through the binsize we need to make plots with from the the first in the binsize vector
    {
      for (j in 1:length(col_n))
        #in each binsize loop through the columns in df_num
      {
        num_bins <- (max(df_num[[j]])-min(df_num[[j]]))/binsize[i]
        #calculate the binwidth value of histogram
        col_mean <- mean(df_num[[j]])
        #calculate the mean value of current variable
        mean_Label = sprintf("%8.2f ", col_mean)
        #set the format of the mean value
        p<-ggplot(df_num,aes_string(x=col_n[j]))+
          #obtain the column's name from col_n vector.use it to get the column data and apply them to ggplot.
          geom_histogram(colour="blue",fill="blue",binwidth=num_bins)+
          #draw a histogram graph with blue and then let width of bin equal to num_bins      
          geom_vline(xintercept=col_mean, colour='red')+
          #add a red verticle line whose intercept is the mean of the column
          annotate("text",x=col_mean,y=0,label=mean_Label,hjust=0)
        #add an annotation to this red line with the mean value as its label
        print(p)
        
        q<-ggplot(df_num,aes_string(x=col_n[j]))+
          #obtain the column's name from col_n vector.use it to get the column data and apply them to ggplot.
          geom_histogram(aes(y=..density..),colour="blue",fill="blue",binwidth=num_bins)+
          #use the density method to draw a histogram graph with blue and let width of bin equal to num_bins
          geom_vline(xintercept=col_mean, colour='red')+
          #add a red verticle line whose intercept is the mean of the column
          annotate("text",x=col_mean,y=0,label=mean_Label,hjust=0)+
          #add an annotation to this red line with the mean value as its label
          labs(y="density")
        #add label "density" to y-axis
        print(q)
      }
    }
  }
  else{
    print("There are no numeric columns in the dataframe")
  }
  
  
  #2.Plot a gray bar graph for every categorical and binary variable.
  if(length(data_nn)!=0){ #test that there is at least one factor or logical column in the dataframe
    col_nn=colnames(data_nn)
    #obtain the columns' name of the new data frame
    for (i in 1:length(col_nn))
      #loop through all the categorical and binary columns
    {
      pp<-ggplot(data_nn,aes_string(x=col_nn[i]))+
        #obtain the column's name from col_nn vector.use it to get the column data and apply them to ggplot.
        geom_bar(fill="gray")
      #draw a bar graph with grey
      print(pp)
    }
  } else{
    print("There are no factor or logical columns in the dataframe")
  }
  
  
  #3/4
  #(a)Return a frequency table for every categorical and binary variable
  if(length(data_nn)!=0){#check if there are any categorical or binary columns
    tablelist<-list() 
    #create an empty tablelist for a list of tables to store all tables
    for (i in 1:ncol(data_nn))
    {#go through all categoricals and binary variables in dataframe
      tablelist[[i]]<-as.data.frame(table((data_nn)[,i])) 
      #using table() to give the counts of ith column in fnb, convert it to a data frame and put into tablelist[[i]]
      names(tablelist[[i]])[1]=colnames(data_nn[i]) 
      #using names() to retain the variable name in the corresponding column name
    } 
  }
  else{
    tablelist <- NA #to make the subsequent call work
  }
  
  
  #3&4b
  
  if(length(df_num)!=0){
    
    #3.Calculate the r2 (r-square) value for every pair of numerical variables.
    r_square=numeric()
    #create an empty numeric list to store r-square value for every pair of numeric variables
    for (i in 1:(length(col_n)-1))
      #loop starts from the first numeric variable
    {
      for (j in (i+1):length(col_n))
        #calculate the r-square value between df_num[i] and its following variables 
        #each time add the r value to list r_square
      {
        r_square<-c(r_square,summary(lm(df_num[[i]]~df_num[[j]],df_num))$r.squared)
        #lm(df_num[[i]]~df_num[[j]]) will return a linear model of the ith and jth variable fo df_num. 
        #summary() will return a basic information fo this model
      }
    }

    
    #(b)
    #(i)A summary statistics table for each numerical variable
    s<-summary(df_num)
    #print(s)
    #summary() will return a statisticas table for each numerical variable 
    
    #(ii)A data frame that contains each pair of variable names and the associated r-square value
    pair_num=character()
    #create a new character list variable to store each numeric variables pairs' names
    for (i in 1:(length(col_n)-1))
    {#start from the first numeric variable
      for (j in (i+1):length(col_n))
      {#for each variable,connect its name with each of the following's and add this to pair_num
        pair_num<-c(pair_num,paste(col_n[i],col_n[j],sep="-"))
      }
    }
    r_num<-data.frame(pair_num,r_square)
    names(r_num)[1] <- "Pair" #name the first column Pair
    names(r_num)[2] <- "R Square Value" #name the second column Value
    #we have obtained the r-square value for each pair in problem3 in the same manner
    #combine these two list into a data frame
  
    
    #(iii) A data frame that contains each pair of variable names and correlation coefficient (Pearson)
    #for all coefficients whose absolute value is greater than the correlation threshold 
    
    cor_thr=numeric()
    pair_thr=character()
    #create a new numeric list variable to store each numeric variables pairs' correlation
    for (i in  1:(length(col_n)-1))
    {#the logic is the same as last problem
      for (j in (i+1):length(col_n))
      {#for each variable,calculate the correlation with each of the following variables
        if (cor(df_num[i],df_num[j])>threshold)
        {cor_thr<-c(cor_thr,cor(df_num[i],df_num[j]))
        pair_thr<-c(pair_thr,paste(col_n[i],col_n[j],sep="-"))
        }
        #if the current two variables' correlation is greater than threshold
        #add the correlation value to list cor_thr
        #connect their names with "-" and then add it to list pair_thr
      }
    }
    corr_n_thr=data.frame(pair_thr,cor_thr)
    #combine these two lists into a data frame
    names(corr_n_thr)[1] <- "Pair" #name the first column Pair
    names(corr_n_thr)[2] <- "Correlation" #name the second column Value
    #print(corr_n_thr)
    Rlist <-list(tablelist,s,r_num,corr_n_thr)
    return(Rlist)
  } else{
    print("There are no numeric columns in the dataframe")
  }
}

#5.Test
#Example1:Test your function by Using the diamonds data frame you extended to include the VS logical column,
#a vector of bin sizes (5, 20, 50), and a correlation threshold of 0.25.
data(diamonds)
data(mtcars)
#call in the dataset mtcars
data=diamonds
#create a new data frame variable and assign data diamonds to it
#I do this because I want to keep the data diamonds intact but I don't know it necessary or not
test=as.logical(rep(mtcars$vs,len=nrow(data)))
#get the data mtcars's column vs and repeat vs until the length equals that of columns in data
#change this new colunm into logical type and assign it to new variable test
diamonds1=data.frame(data,test)
#add this column test to the data frame data
explore(diamonds1,c(5,20,50),0.25)


#Example2:
explore(mtcars,c(5,20,50),0.25)
