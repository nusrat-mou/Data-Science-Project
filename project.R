print(getwd())
setwd("D:/USER/Documents/AIUB/12th semester/Data Science/Mid/Project")
print(getwd())

dataset<-read.csv("dataset.csv")          #reading csv
print(dataset)

#handling missing values
dataset$Murder[is.na(dataset$Murder)]<-mean(dataset$Murder,na.rm=TRUE)     
dataset

dataset$Assault[is.na(dataset$Assault)]<-mean(dataset$Assault,na.rm=TRUE)
dataset


dataset$Urban_Population[is.na(dataset$Urban_Population)]<-mean(dataset$Urban_Population,na.rm=TRUE)
dataset

#rounding decimal values
dataset$Murder =as.numeric(format(round(dataset$Murder, 0)))
dataset
dataset$Assault =as.numeric(format(round(dataset$Assault, 0)))
dataset





#adding column named 'Type'
dataset2=cbind(dataset,Type=NA)
dataset2


#data integration

for(i in 1:nrow(dataset2)) {                   
  

  
  if((dataset2$Urban_Population[i])<50){
    dataset2$Type[i]="Small"
  }else if((dataset2$Urban_Population[i])<60){
    dataset2$Type[i]="Medium"
    
  }else if((dataset2$Urban_Population[i])<70){
    dataset2$Type[i]="Large"
    
  }else{
    dataset2$Type[i]="Extra Large"
  }
}
 

dataset2

#data transformation using z-score normalization




dataset2$Murder_stand2a <- scale(dataset2$Murder)                                          # Standardize using scale()
dataset2$Murder_stand2a

dataset2$Assault_stand2a <- scale(dataset2$Assault)                                          # Standardize using scale()
dataset2$Assault_stand2a

dataset2$Urban_Population_stand2a <- scale(dataset2$Urban_Population)                                          # Standardize using scale()
dataset2$Urban_Population_stand2a

dataset2




#data Reduction using PCA
library(tidyverse)

dataset3 <- dataset2[,-c(1,5)]
dataset3

head(dataset3)
results <- prcomp(dataset3 , scale = TRUE)



results$rotation <- -1*results$rotation

results$rotation


results$x <- -1*results$x



head(results$x)


head(dataset3[order(-dataset3$Murder),])

results$sdev^2 / sum(results$sdev^2)

