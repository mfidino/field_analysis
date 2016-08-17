#####################
#
#
#
# Data cleaning script for 
# egg laying analysis
#
#
#
#

# function to load packages and download them if necessary

package_load<-function(packages = NULL, quiet=TRUE, verbose=FALSE, warn.conflicts=FALSE){
  
  # download required packages if they're not already
  
  pkgsToDownload<- packages[!(packages  %in% installed.packages()[,"Package"])]
  if(length(pkgsToDownload)>0)
    install.packages(pkgsToDownload, repos="http://cran.us.r-project.org", quiet=quiet, verbose=verbose)
  
  # then load them
  for(i in 1:length(packages))
    require(packages[i], character.only=T, quietly=quiet, warn.conflicts=warn.conflicts)
}

packs <- c("dplyr", "reshape2")

package_load(packs)


# read in the kestrel data
amke <- read.csv("kestrel_data.csv", header = TRUE,
                 stringsAsFactors = FALSE)

# read in the bluebird data
eabl <- read.csv("bluebird_data.csv", header = TRUE,
                 stringsAsFactors = FALSE)

# read in all of the other data
bird <- read.csv("arboretum_data.csv", header = TRUE,
                 stringsAsFactors = FALSE)

# Each of these datasets have their own issues, so
# we'll have to mess with them individually. The format
# we will need will be something like this...

### | species | jdate | year | migtype | ###

############################################
# kestrel_data.csv cleaning
############################################

# issues with date formatting
# we don't need the location stuff
# convert kestrel name to species code
# convert startling name to species code

###11111111111
### Date stuff
###11111111111

#change blank values in first egg to NA
amke$first_egg[amke$first_egg==""] <- NA

# remove NA stuff
amke <- amke[-which(is.na(amke$first_egg)),]

# date formatting stuff

# remove semicolon
eggs <- gsub(";", "/", amke$first_egg)

# change april to 4
eggs <- gsub("april", "4/", eggs, ignore.case = TRUE)

# remove empty spaces
eggs <- gsub(" ", "", eggs)

# make dashes slashes
eggs <- gsub("-", "/", eggs)

# change comma to slash
eggs <- gsub(",", "/", eggs)

# remove trailing period
eggs <- gsub("\\.", "", eggs)

# make a year column in amke
amke$year <- unlist(lapply(strsplit(eggs, "/"), FUN = function(x) x[3]))

# make eggs a date vector
edate <- as.Date(eggs, format = "%m/%d/%Y")

# convert it to julian date and put it in a jdate column in amke
amke$jdate <- format(edate, "%j")

#2222222222222222222
# species name stuff
#2222222222222222222

# replace KESTREL with AMKE
amke$species <- gsub("KESTRELS?", "AMKE", amke$species)

# replace STARLING with EUST
amke$species <- gsub("STARLINGS?", "EUST", amke$species)

#33333333333333333333
# prepare for binding
#33333333333333333333

# get only the columns we need
amke_tobind <- amke[,which(colnames(amke) %in% c("species", "jdate", "year"))]

# reorder the columns correctly
amke_tobind <- amke_tobind[,c(1,3,2)]

##############################################
# bluebird.csv cleaning
##############################################


