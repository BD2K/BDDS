install.packages("rjson")
library("rjson", lib.loc="~/R/win-library/3.1")
data <- fromJSON( file="ppmi.json")
length(data)
names(data)
lapply(data, class)  # lapply function operates on the list piece-by-piece
lapply(data, length)
lapply(data, dim)
str(data)

# struct <- capture.output(str(data))
# write(struct, "C:\\Users\\Dinov\\Desktop\\ppmi.json.struc")
# head(data)
# head(data[[1]],n=3)

# Decide which data points to extract and use the c(), or concatenate to extract the data
str_data <- c(PATNO[[]], EDUCYRS[[]], BIOSPECIMEN[[]]$DIAGNOSIS, FAMHXPD[[]]$MATAU)

# sapply operated on the list piece-by-piece and returns an array instead of a list
dataNames<-sapply(data, function(x) x[[1]])
head(dataNames)

library(gdata) # for the trim function

grabInfo<-function(var){
  print(paste("Variable", var, sep=" "))
  sapply(data, function(x) returnData(x, var))
}

returnData<-function(x, var){
  if(!is.null( x[[var]])){
    return( trim(x[[var]]))
  }else{
    return(NA)
  }
}

# do the extraction and assembly
fmDataDF<-data.frame(sapply(1:5, grabInfo), stringsAsFactors=FALSE)