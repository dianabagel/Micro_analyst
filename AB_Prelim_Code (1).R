#12.20.19 Renuka's Preliminary Plaque Counts = MPV w/CellProfiler

#Import all related files as data.tables (from .csv files)

path = "C:/Users/begel/OneDrive/Documents"

files = list.files(path, pattern = "FilterObjects2", all.files = TRUE, recursive = TRUE)

list1 <- list()

library(tidyr)
library(dplyr)
library(ggplot2)

export<-function(){
  df<-get_average_accurate(1)
  df2<-get_average_accurate(2)
  #write.csv(df, "C:/Users/begel/OneDrive/Documents\\FilterObjects2df.csv")
  write.csv(df2, "C:/Users/begel/OneDrive/Documents\\FilterObjects2_highdf1.csv")
}

#Import all files as tables
for(i in 1:length(files)){
  list1[[i]]= read.csv(files[i], sep = ",", header = TRUE) #data.frame

}
#Return the index of the file 
#input:"name of file"
getIndex <-function(filename){
  count<- 1
  for(i in files){
    if (i==filename){
      print(count)
    }
    else{
      count=count+1
    }
  }}
#Return the file name from index
#input: integer
getFileName <-function(index){
  if(index>=1){
    return (files[[index]])
  }
  else{
    message("Index out of bounds")
  }
  
}
#get a list of files
getFiles <- function(){
  return (files)
}

#Function Parameters: val: file number(refer to getIndex())
images <- function(val){
  listOfImages<<-list()
  for(i in 1:max(list1[[val]]$ImageNumber)){ #i=image number
    if (i %in% list1[[val]]$ImageNumber){
      image<-dplyr::filter(list1[[val]], ImageNumber==i)
      listOfImages <- append(listOfImages, list(image))
    }
    else{
      message(i, " Image Number is not found")
    }
  }
  return(listOfImages)
}
#Same as images(), but the imagenumber matches the mean intensity
images_accurate<-function(val){
  listOfImages<<-list()
  for(i in 1:max(list1[[val]]$ImageNumber)){ #i=image number
    if (i %in% list1[[val]]$ImageNumber){
      image<-dplyr::filter(list1[[val]], ImageNumber==i)
      listOfImages <- append(listOfImages, list(image))
    }
    else{
      image<-dplyr::filter(list1[[val]], ImageNumber==i)
      listOfImages <- append(listOfImages, list(image))
      message(i, " Image Number is not found")
    }
  }
  return(listOfImages)
}
#Same as get_average(), but uses accurate image numbers
get_average_accurate <-function(filenumber){
  listOfImages <- images_accurate(filenumber)
  imageNumber<-c()
  avgIntensity<-c()
  for(i in 1:length(listOfImages)){
    df<-as.data.frame(listOfImages[i])
    avg<-mean(df[["Intensity_MeanIntensity_DNA"]])
    imageNumber <- c(imageNumber, i)
    avgIntensity<- c(avgIntensity, avg)
  }
  final_df<-data.frame(ImageNumber=imageNumber, avg_Intensity=avgIntensity)
  return(final_df)
}
#find the average of mvps
#return average(int)
get_average <- function(filenumber){
  listOfImages <- images(filenumber)
  imageNumber<-c()
  avgIntensity<-c()
  for(i in 1:length(listOfImages)){
    df<-as.data.frame(listOfImages[i])
    avg<-mean(df[["Intensity_MeanIntensity_DNA"]])
    imageNumber <- c(imageNumber, i)
    avgIntensity<- c(avgIntensity, avg)
  }
  final_df<-data.frame(ImageNumber=imageNumber, avg_Intensity=avgIntensity)
  return(final_df)
  
}
avg_files<-function(){
  all_avg<-c()
  fileNumber<-c()
  #Find the average of all averages for each file
  for(i in 1:length(files)){
    df<-get_average(i)
    mean<-mean(df[["avg_Intensity"]])
    fileNumber<-c(fileNumber, i)
    all_avg<-c(all_avg, mean)
  }
  final_df<-data.frame(File_Number=fileNumber, AVG_Intensity_Per_File=all_avg)
  return(final_df)
}
#not done
plots<-function(){
  df<-avg_files()
  p<-ggplot(data=df, aes(x=File_Number, y=AVG_Intensity_Per_File), fill=supp)+
    geom_bar(stat="identity", position=position_dodge(), width=2.0, color="blue")+
    geom_text(aes(label=AVG_Intensity_Per_File), vjust=1.6, color="white", size=2.0)
  p
  p+labs(title = "AD Mice on VitD Def", x="Samples", y="Average mvp")+
    theme_classic()+
    geom_point(mapping = aes(x=File_Number), data = df)
}


















