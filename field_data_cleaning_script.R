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
# kestrel_data.csv cleaning (object = amke)
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
# bluebird.csv cleaning (object = eabl)
##############################################

#111111111111111111111111111
# remove unnecessary columns
#111111111111111111111111111

# grab the ones we need
eabl <- eabl[,which(colnames(eabl) %in% c("COMMON.NAME", "First.Egg..Julian.date.",
                                          "Eggdate"))]

#change the column names
colnames(eabl) <- c("species", "jdate", "year")

#22222222222222222
# make year column
#22222222222222222

# the year stuff is seperated by a forward slash just like the
# amke data so we can use the same technique
eabl$year <- unlist(lapply(strsplit(eabl$year, "/"), FUN = function(x) x[3]))

#333333333333333333333
# change species names
#333333333333333333333

#eastern bluebird to EABL
eabl$species <- gsub("eastern bluebird", "EABL", eabl$species, ignore.case = TRUE)

#house wren to HOWR
eabl$species <- gsub("house wren", "HOWR", eabl$species, ignore.case = TRUE)

#tree swallow to TRES
eabl$species <- gsub("tree swallow", "TRES", eabl$species, ignore.case = TRUE)

#44444444444444444444
# prepare for binding
#44444444444444444444

# we actually have it ready now
eabl_tobind <- eabl

###########################################################
# clean arboretum.csv data (object = bird)
###########################################################

#111111111111111111111111111
# remove unnecessary columns
#111111111111111111111111111

# grab the ones we need
 bird <- bird[,which(colnames(bird) %in% c("totall.Ordinal", "Spp.", "Year"))]

#change the column names
colnames(bird) <- c("year", "jdate", "species")

# move the columns around
bird <- bird[,c(3,2,1)]

#222222222222222222222222222222222222222
# remove unnecessary rows and fix others
#222222222222222222222222222222222222222

# make blank values NA
bird$species[bird$species == ""] <- NA
bird$jdate[bird$jdate == ""] <- NA
bird$year[bird$year == ""] <- NA

# remove incomplete cases
bird <- bird[complete.cases(bird),]

# change TRSW to TRES
bird$species <- gsub("TRSW", "TRES", bird$species, ignore.case = TRUE)

# change REBL to RWBL
bird$species <- gsub("REBL", "RWBL", bird$species, ignore.case = TRUE)

# change RTHA to RTHU
bird$species <- gsub("RTHA", "RTHU", bird$species, ignore.case = TRUE)

# remove RUKI
bird <- bird[-which(bird$species=="RUKI"),]


####################

#33333333333333333333
# prepare for binding
#33333333333333333333

# nothing else needed
bird_tobind <- bird

#######################################################
# combine the three datasets (object = bd)
#######################################################

# rowbind the data
bd <- rbind(bird_tobind, amke_tobind, eabl_tobind)

# save these data 
write.csv(bd, "cleaned_bird_data_complete.csv", row.names = FALSE)

# figure out how many observations of each species we have
nobs <- data.frame(table(bd$species))
colnames(nobs) <- c("species", "nobs")

# sort by amount of data
nobs <- nobs[order(nobs$nobs, decreasing = TRUE),]
row.names(nobs) <- NULL

# write these data
write.csv(nobs, "number_of_observations_1994_onward.csv", row.names = FALSE)


