library(tidyr)
library(dplyr)

path = "C:/Users/begel/OneDrive/Documents"
#TODO:
  #make a list of dataframes by turning files into dataframes
  #Use all(list, microanalyst) to get the dataframes you need
#EXAMPLE:
files1 = list.files(path, pattern = "files", all.files = TRUE, recursive = TRUE)
files<-read.table("files.txt", header=TRUE, as.is=1)
list<-list(files)


masterfile = list.files(path, pattern = "masterfile", all.files = TRUE, recursive = TRUE)

microanalyst = list.files(path, pattern = "MicroAnalyst_EC", all.files = TRUE, recursive = TRUE)

masterframe <- read.csv(masterfile, sep = ",", header = TRUE, as.is=2) 

microanalyst_df<-read.csv(microanalyst, sep = ",", header = TRUE, colClasses = "character")


#find the column number of the element
#return column number
find <- function(masterframe, element){
  result <-which(masterframe==element, arr.ind=TRUE)
  return(result[1])
}
#if you need to generalize it to any data tables you just need to change "function." and
# "description" to whatever columns you need
change <- function(files, masterframe){
  description <-c()
  for(i in 1:length(files$function.)){
  
  
  num<-find(masterframe, files$function.[i])
  
  element<-masterframe$description[num]
  description<- c(description, element)
      }
 
  dataframe<-data.frame(files$function., description)
  return(dataframe)
}
#return a dataframe
#only does one file
extract <- function(files, microanalyst){
  dataframe<-data.frame()
  for(i in 1:length(files[[1]])){
    row<-dplyr::filter(microanalyst, microanalyst[[1]]==files[[1]][i])
    dataframe<-rbind(dataframe, row)
    
  }
  
  return(dataframe)
}
#You need a list of dataframes
#returns a list of final dataframes
all <-function(mylist, microanalyst){
  final_list<-list()  #list of all dataframes from each file
  for (i in mylist){  #i is a dataframe(also is one of the files)
    df<-extract(i, microanalyst) #make the dataframe
    final_list<-append(final_list, df) #add the dataframe to the list
  }
  return(final_list)
}