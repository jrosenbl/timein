#!/usr/local/bin/Rscript
#' print current time and alternate timezone if specified.
#' allowed input timezone abbreviations specified in mytz
#' Add new timezones to tzlist: "abbrev,OlsonName"
#' 
suppressMessages(library(lubridate))
suppressMessages(library(data.table))

timeDirection <- function(difft) {
    #' obtuse one-liner - just messs'in
    return(switch((as.integer(difft)/abs(as.integer(difft)))+2,"behind","","ahead"))
}

make_tztab <- function(tzlist) {
    abbr = sapply(strsplit(tzlist,","), function(x) x[[1]])
    tz = sapply(strsplit(tzlist,","), function(x) x[[2]])
    return(data.table(abbr=abbr, tz=tz, key="abbr"))
}    
#'
#' add new zone abbreviations (cities or states) here.
#' map to OlsonNames()
tzlist = c(
    "ams,Europe/Amsterdam",
    "ber,Europe/Berlin",
    "ca,US/Pacific",
    "co,US/Mountain",
    "dub,Europe/Dublin",
    "hfa,Israel",
    "hkg,Hongkong",
    "ice,Iceland",
    "lon,Europe/London",
    "mad,Europe/Madrid",
    "man,Singapore",
    "ny,US/Eastern",
    "par,Europe/Paris",
    "rom,Europe/Rome",
    "syd,Australia/Sydney",
    "zur,Europe/Zurich")

mytz <- make_tztab(tzlist)
here <- now()
cat("date/time:", sprintf("%s",here), "\n")

args = commandArgs(trailingOnly=T)
if (length(args) > 0) {
    if (args[1] == "?") {
        print(mytz)
        quit(save="no", status=0, runLast=FALSE)
    }
    otherZone = mytz[args[1],tz]
    if (is.na(otherZone)) {
        stop('invalid timezone ', args[1])
    }
    d = here - force_tz(here, tz=otherZone)
    cat("time in", otherZone, sprintf("%s",with_tz(here, tz=otherZone)), d, "hrs", timeDirection(d), "\n")
}

