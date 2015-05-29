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