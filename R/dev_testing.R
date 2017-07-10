FunctionXform2 <- function (boxdata,origFieldName,newFieldName="newField",
                            newFieldDataType="numeric",formulaText,mapMissingTo=NA) {
  
  # boxdata = clickBox
  # origFieldName = 'dailyRate,analyticsReferencePrice'
  # formulaText = "100 * (dailyRate - analyticsReferencePrice)  / analyticsReferencePrice"
  
  boxdata$data$newFieldName <- NA
  
  parsed_text <- parse(text=formulaText)
  
  ## This loop makes it possible to apply an if-else formula to the new data column.
  for (n in 1:length(boxdata$data$newFieldName)) {
    boxrow <- boxdata$data[n,]
    boxdata$data$newFieldName[n] <- eval(parsed_text,boxrow)
  }
  #boxrow <- boxdata$data
  #shitty_fix = parse(text=gsub('min', 'pmin', gsub('max', 'pmax', formulaText)))
  #boxdata$data$newFieldName <- eval(shitty_fix, boxrow)
  #boxdata$data$newFieldName <- eval(parsed_text, boxrow)
  
  #boxdata$data$newFieldName <- apply(boxrow, 1, function(x) eval(parsed_text, x))
  
  names(boxdata$data)[names(boxdata$data)=="newFieldName"] <- newFieldName
  
  #new column for formula; only create if doesn't already exist; this is unnecessary if functionXform is already added by WrapData()
  if (!("functionXform" %in% colnames(boxdata$fieldData))) {
    boxdata$fieldData$functionXform <- "NA"
  }
  
  #make new row with "NA" entries
  temprow <- matrix(c(rep.int("NA",length(boxdata$fieldData))),nrow=1,ncol=length(boxdata$fieldData))
  newrow <- data.frame(temprow)
  colnames(newrow) <- colnames(boxdata$fieldData)
  boxdata$fieldData <- rbind(boxdata$fieldData,newrow)
  
  #add data to new row
  row.names(boxdata$fieldData)[nrow(boxdata$fieldData)] <- newFieldName
  
  levels(boxdata$fieldData$type)[2] <- "derived" #must create factor level first
  boxdata$fieldData[newFieldName,"type"] <- "derived"
  #correction
  levels(boxdata$fieldData$type)[1] <- "original"
  boxdata$fieldData[newFieldName,"dataType"] <- newFieldDataType #this could be string
  
  
  boxdata$fieldData[newFieldName,"functionXform"] <- formulaText
  
  #if origFieldName contains multiple fields, these will be combined into one string 
  boxdata$fieldData[newFieldName,"origFieldName"] <- paste(origFieldName,collapse=",")
  
  if(!is.na(mapMissingTo))
  {
    boxdata$fieldData[newFieldName,"missingValue"] <- mapMissingTo
  }
  
  return(boxdata)
}

require(pmmlTransformations)
require('XML')

#file.sources = list.files(pattern="*.R")
#for (i in 1:(length(file.sources)-1)){
#  source(file.sources[i])
#}

#source('pmml.datadictionary.R')
#source('pmml.miningschema.R')




set.seed(1992)
data = data.frame(x1=rnorm(30,0,1), x2=rnorm(30,0,1), x3=rnorm(30,0,1))

data2 = data
data2$x4 = data2$x1^2 
data2$x5 = data2$x2*data2$x4
lm = lm(x1~ x5, data=data2)

clickBox = WrapData(data)

clickBox <- FunctionXform2(clickBox,
                           origFieldName="x1,x3",
                           newFieldName="x4",
                           formulaText="x1*x3")

clickBox <- FunctionXform2(clickBox,
                           origFieldName="x2,x4",
                           newFieldName="x5",
                           formulaText="x2*x4")

xml = pmml(lm, transforms = clickBox)
xml


