# Note: we should be using packrat: http://rstudio.github.io/packrat/

# Getting started with exploring json parsing to enable EDA in R. There are 3 
# JSON parsing packages in R: RJsonio, rjson, and jsonlite.
# 
# based on 
# https://rstudio-pubs-static.s3.amazonaws.com/31702_9c22e3d1a0c44968a4a1f9656f1800ab.html,
# RJSONIO seems flexible and powerful. However, based on 
# http://cran.r-project.org/web/packages/jsonlite/vignettes/json-mapping.pdf and
# http://www.r-bloggers.com/new-package-jsonlite-a-smarter-json-encoderdecoder/,
# I'm inclined towards the jsonlite package, a fork of RJSONIO that emphasized 
# interoperability.

#install
install.packages("jsonlite", repos="http://cran.r-project.org")

#load library
library(jsonlite)

JSONFilePath <- "/Users/bheavner/Projects/bdds_center/data/ppmi.json"

PPMIdata <- fromJSON(JSONFilePath)
#Error in feed_push_parser(buf) : parse error: trailing garbage
#ll,"SITE_APRV":"08/2014"}]}  {"PATNO":3001,"ENROLL_STATUS":"
#                     (right here) ------^

#attempt to validate json by copying first couple of lines to the validator at
#http://jsonlint.com (on my mac, I did this in the shell: head
#../../data/ppmi.json | pbcopy) then pasted it there. The validator gives the
#following warning:

#Parse error on line 10678:
#...4"        }    ]}{    "PATNO": 3001,
#--------------------^
#Expecting 'EOF', '}', ',', ']'

# see 
# http://stackoverflow.com/questions/26519455/error-parsing-json-file-with-the-jsonlite-package
# - "...a "pseudo-JSON" file. I come across these in many naive API systems I
# work in. Each line is valid JSON, but the individual objects aren't in a JSON
# array. You need to use readLines and then build your own, valid JSON array
# from it and pass that into fromJSON:..."

PPMIdata <- fromJSON(sprintf("[%s]", paste(readLines(JSONFilePath), collapse=",")))
# Error in parse_string(txt, bigint_as_char) : 
#   parse error: unallowed token at this point in JSON text
# "06/2013"}],"WHOLEBLD":null},]
# (right here) ------^

#... hrm... Try a different R JSON Parser. Clear the environemnt, then...

#install
install.packages("rjson", repos="http://cran.r-project.org")

#load library
library(rjson)

JSONFilePath <- "/Users/bheavner/Projects/bdds_center/data/ppmi.json"

PPMIdata <- fromJSON(file = JSONFilePath) #PPMIdata is now a large list
names(PPMIdata) #106 elements

# but it appears to only import the first element:
PPMIdata$PATNO
#[1] 3000

#And with RJSONIO, Clear the environemnt, then...

#install
install.packages("RJSONIO", repos="http://cran.r-project.org")

#load library
library(RJSONIO)

JSONFilePath <- "/Users/bheavner/Projects/bdds_center/data/ppmi.json"

PPMIdata <- fromJSON(JSONFilePath) #PPMIdata is now a large list

# also appears to only import the first element:
PPMIdata$PATNO
#[1] 3000

#Okay, so I've got streaming JSON data. Return to jsonlite and try to read streaming.

# clear environment, restart R session, then:
library(jsonlite)
JSONFilePath <- "/Users/bheavner/Projects/bdds_center/data/ppmi.json"

PPMIdata <- stream_in(file(JSONFilePath)) # see http://demo.ocpu.io/jsonlite/man/stream_in/html
#opening file input connection.
#Found 812 lines...
#Error in parse_string(txt, bigint_as_char) : parse error: premature EOF

#(right here) ------^
#  closing file input connection.

# troubleshooting: http://stackoverflow.com/questions/26245188/importing-json-into-r-with-in-line-quotation-marks

readLines(JSONFilePath, n=1)
# looks like this: " 
# {\"PATNO\":3000,\"ENROLL_STATUS\":\"Enrolled\",\"RSNDEC\":null, ...
#\"QUERY\":null,\"SITE_APRV\":\"08/2014\"}]}"

PPMIdata <- stream_in(file(JSONFilePath),
                      pagesize=1) # still gives parse_string error after 812 lines

# in shell: sed -n 812p ppmi.json > line812.json
JSONFilePath <- "/Users/bheavner/Projects/bdds_center/data/line812.json"
PPMIdata <- stream_in(file(JSONFilePath)) #1 line, gives read error.

#trim last line with: sed -n 1,811p ppmi.json > shorter.json
# or sed '$d' <file>
JSONFilePath <- "/Users/bheavner/Projects/bdds_center/data/shorter.json"
PPMIdata <- stream_in(file(JSONFilePath))

#works! So, suppose I want to basic things: patient ID, diagnosis, and age.

colnames(PPMIdata)
#looks promising, but... 1) I don't know what most of these are, and 2) nested lists complicate, for example:
PPMIdata$BIOSPECIMEN[1]

# further, many look like they'll make good factors, or combine to be factors...

# which columns include numbers?
colnames(PPMIdata)[sapply(PPMIdata,is.numeric)]

#exploring more
lapply(PPMIdata, class) # many: character, ints, lists
lapply(PPMIdata, length) # each 811 long
lapply(PPMIdata, dim) # NULL
lapply(PPMIdata$BIOSPECIMEN, class) # mix of data.frame and NULL
lapply(PPMIdata$BIOSPECIMEN, length) # a range of values
lapply(PPMIdata$BIOSPECIMEN, dim) # a range... I see 7 x 14, 43 x 14, NULL
PPMIdata$BIOSPECIMEN[[810]]
names(PPMIdata$BIOSPECIMEN[[810]]) # 14 names
lapply(PPMIdata$BIOSPECIMEN, names) # null or 14

#the following is weird
PPMIdata$BIOSPECIMEN[[810]]$PATNO # 43 copies of "4139"
PPMIdata$BIOSPECIMEN[[810]]$GENDER # 43 copies of "Male"
PPMIdata$BIOSPECIMEN[[810]]$DIAGNOSIS # 43 copies of "Control"

# start making data dictionary to think about what data I want to extract - build data dictionary for colnames(PPMIdata) using str() and class()? For now, just start. Then, I'll think about reformatting for EDA (for example, lumping categorical factors)

PPMIdf <- data.frame(ID = PPMIdata$PATNO)

# Next, I'd like to add the diagnosis... I see it's repeated under each PPMIdata$BIOSPECIMEN element, for example:
PPMIdata$BIOSPECIMEN[[810]]$DIAGNOSIS
# If they're all the same within a biosepcimen, use it?

foo <- list()
for(index in 1:length(PPMIdata$BIOSPECIMEN)) {
  print(index)
  if(!is.null(PPMIdata$BIOSPECIMEN[[index]]$DIAGNOSIS)) {
    foo[[index]] <- unique(PPMIdata$BIOSPECIMEN[[index]]$DIAGNOSIS)
  }
}

foo[sapply(foo, is.null)] <- NA

cbind(PPMIdf, unlist(foo)) # length error. Not all samples have biospecimen...
# I think a data frame should be able to have different length entities. But I also want to make sure that the diagnosis is indexed to the right patient ID...

# do all patient IDs have a biospecimen?
foo <- list()
for(index in 1:length(PPMIdata$BIOSPECIMEN)) {
  print(index)
  if(!is.null(PPMIdata$BIOSPECIMEN[[index]]$PATNO)) {
    foo[[index]] <- unique(PPMIdata$BIOSPECIMEN[[index]]$PATNO)
  }
}

foo[sapply(foo, is.null)] <- NA

# Nope, there are NULL values for some BIOSPECIMIN entries (e.g. 808, 809)

# So... instead, add a collumn of NAs to the data frame, then populate it with diagnoses for those that exist in biospecimin (diagnosis may be somewhere else, so use BIOSPECIMIN.DIAGNOSIS for column name)

PPMIdf$BIOSPECIMIN.DIAGNOSIS <- rep(NA, length(PPMIdf$ID))

# get all biospecimin patient numbers - There must be a better way...
BIOSPECIMIN.PATNO <- list()
for(index in 1:length(PPMIdata$BIOSPECIMEN)) {
  if(!is.null(PPMIdata$BIOSPECIMEN[[index]]$PATNO)) {
    BIOSPECIMIN.PATNO[[index]] <- unique(PPMIdata$BIOSPECIMEN[[index]]$PATNO)
  }
}
BIOSPECIMIN.PATNO[sapply(BIOSPECIMIN.PATNO, is.null)] <- NA
BIOSPECIMIN.PATNO <-  unlist(BIOSPECIMIN.PATNO)

# get all biospecimin diagnoses
BIOSPECIMIN.DIAGNOSIS <- list()
for(index in 1:length(PPMIdata$BIOSPECIMEN)) {
  if(!is.null(PPMIdata$BIOSPECIMEN[[index]]$DIAGNOSIS)) {
    BIOSPECIMIN.DIAGNOSIS[[index]] <- unique(PPMIdata$BIOSPECIMEN[[index]]$DIAGNOSIS)
  }
}
BIOSPECIMIN.DIAGNOSIS[sapply(BIOSPECIMIN.DIAGNOSIS, is.null)] <- NA
BIOSPECIMIN.DIAGNOSIS <-  unlist(BIOSPECIMIN.DIAGNOSIS)

# now add the biospecimin.diagnosis to the DF, ordered by patient IDs in the data frame
PPMIdf$BIOSPECIMIN.DIAGNOSIS <- as.factor(BIOSPECIMIN.DIAGNOSIS[match(PPMIdf$ID, BIOSPECIMIN.PATNO)])

# Neat. So now I've got a way to populate the data frame from a BIOSPECIMIN field. I need to make a function to generalize it, and use that function to further populate the data frame.