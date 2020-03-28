#!/usr/local/bin/Rscript
#' print current time and alternate timezone if specified.
#' allowed input timezone abbreviations specified in mytz
#' Add new timezones to tzlist: "abbrev,OlsonName"
#' 
suppressMessages(library(lubridate))
suppressMessages(library(data.table))

timeDirection <- function(difft) {
    return(switch((as.integer(difft)/abs(as.integer(difft)))+2,"behind","","ahead"))
}

make_tztab <- function(tzlist) {
    loc = sapply(strsplit(tzlist,','), function(x) x[[1]])
    tz = sapply(strsplit(tzlist,','), function(x) x[[2]])
    return(data.table(loc=loc, tz=tz))
}    
#'
#' add new zone abbreviations here
tzlist = c("ca,US/Pacific",
           "ny,US/Eastern",
           "co,US/Mountain",
           "dub,Europe/Dublin",
           "lon,Europe/London",
           "zur,Europe/Zurich",
           "syd,Australia/Sydney",
           "man,Singapore",
           "mad,Europe/Madrid",
           "rom,Europe/Rome",
           "ber,Europe/Berlin",
           "ams,Europe/Amsterdam",
           "hkg,Hongkong",
           "ice,Iceland")
    
mytz <- make_tztab(tzlist)
setkey(mytz, loc)

here <- now()
cat("date/time:", sprintf("%s",here), "\n")

args = commandArgs(trailingOnly=T)
if (length(args) > 0) {
    otherZone = mytz[args[1],tz]
    if (is.na(otherZone)) {
        stop('invalid timezone ', args[1])
    }
    d = here - force_tz(here, tz=otherZone)
    cat("time in", otherZone, sprintf("%s",with_tz(here, tz=otherZone)), d, "hrs", timeDirection(d), "\n")
}

