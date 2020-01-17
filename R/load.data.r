# name: load.data
# purpose: read data for an analysis from a variety of external typical file types, and assign them
#       to archival and working datasets in R.
# input: a list of files, their names, R object names to assign them to, and the input director
#       in wihch these are found. NOTE the function an also be used to do a "scan" for ANY files
#       in this folder and load them without being specified. Filetypes supported include SAS xpt,
#       Microsoft Excel (xls and xlsx), Comma Separated Values (csv), and tab files (tab).
# output: R objects
# note:

# ROXYGEN Documentation
#' Loading data of various formats
#' @description Read data for an analysis from a variety of external typical file types, and assign them
#       to archival and working datasets in R. The function takes SAS transport (XPT), Microsoft Excel (XLS, XLSX), comma-delimited (CSV) and tab-delimited (ASCII) formats. NOTE the function an also be used to do a "scan" for ANY files in this folder and load them without being specified.
#' @param xptFiles vector of SAS transport (XPT) filenames
#' @param xlsFiles vector of Microsoft Excel (XLS, XLSX) filenames
#' @param csvFiles vector of comma-delimited (CSV) filenames
#' @param tabFiles vector of tab-delimited (ASCII) filenames
#' @param dir directory where the files reside
#' @param xptNames vector of object names for each SAS transport (XPT) file, in the same order.
#' @param xlsNames vector of object names for each Microsoft Excel (XLS, XLSX) file, in the same order.
#' @param csvNames vector of object names for each comma-delimited (CSV) file, in the same order
#' @param tabNames vector of object names for each tab-delimited (ASCII) file, in the same order
#' @param perl directory where the perl executable resides
#' @param xlsSheets a numeric vector of \code{length(xlsFiles)} defining which sheet in the workbook needs to be read. If you do not specify this, by default the first sheet of each workbook will be read.
#' @param doScan logical
#' @param target.env environment The environment into which the data files will be loaded
#' and attached.
#' @return Placeholder for return description
#' @export
#' @importFrom gdata read.xls
#' @importFrom foreign read.xport
#' @importFrom utils read.csv
#' @importFrom utils read.delim
#' @examples
#' \dontrun{
#' load.data(xlsFiles="ExampleInput.xlsx",
#' xlsNames="XLinput1",
#' xlsSheets=1, doScan=FALSE)
#' head(XLinput1)
#' head(iXLinput1)
#' load.data(xlsFiles="ExampleInput.xlsx",
#'           xlsNames="XLinput2",
#'           xlsSheets=2,            # NOTE We can specify which worksheet (tab) of the Excel file
#'           doScan=FALSE)
#' head(iXLinput2)
# Exammple of providing a LIST of files to import
#' load.data(csvFiles=c("ExampleInput2.csv","ExampleInput3.csv"),
#'           csvNames=c("CSVinput1", "CSVinput2"),
#'           doScan=FALSE)
#' head(CSVinput1)
#' load.data() # loading JUST doing automatic search for files ("scan")
#' }
load.data <- function(xptFiles, xlsFiles, csvFiles, tabFiles,
  dir = "../project input/client data/data",
  xlsNames, xptNames, csvNames, tabNames, perl="C:/Perl64/bin/perl.exe",
  xlsSheets, doScan=TRUE, target.env=parent.frame()) {

  if(!missing(xptNames)) {
    if(missing(xptFiles)|length(xptNames)!=length(xptFiles)) {
      stop(paste("xptNames cannot be defined unless a list of xptFiles of equal length",
                 "is also specified"))
    }
  }
  if(!missing(xlsNames)) {
    if(missing(xlsFiles)|length(xlsNames)!=length(xlsFiles)) {
      stop(paste("xlsNames cannot be defined unless a list of xlsFiles of equal length",
                 "is also specified"))
    }
    if(!missing(xlsSheets)) {
      if(length(xlsSheets)!=length(xlsFiles)) {
        stop(paste("xlsSheets cannot be defined unless it has equal length to xlsFiles"))
      }
    }
    else {
      xlsSheets = rep(1,length(xlsFiles))
    }
  }

  if(!missing(csvNames)) {
    if(missing(csvFiles)|length(csvNames)!=length(csvFiles)) {
      stop(paste("csvNames cannot be defined unless a list of csvFiles of equal length",
                 "is also specified"))
    }
  }

  if(!missing(tabNames)) {
    if(missing(tabFiles)|length(tabNames)!=length(tabFiles)) {
      stop(paste("tabNames cannot be defined unless a list of tabFiles of equal length",
                 "is also specified"))
    }
  }

  if (doScan) {   # do scan
    ## Scan directory if no file list was provided
    if(missing(xptFiles)) {
      xptFiles = list.files(dir)
      xptFiles = xptFiles[grep(".xpt", xptFiles)]
    }
    if(missing(xlsFiles)) {
      xlsFiles = list.files(dir)
      xlsFiles = xlsFiles[grep(".xls[x]*", xlsFiles)]
      ## remove files starting with "~" (temporary excel files)
      xlsFiles = xlsFiles[!grepl("^~",xlsFiles)]
      xlsSheets = rep(1,length(xlsFiles)) # default
    }
    if(missing(csvFiles)) {
      csvFiles = list.files(dir)
      csvFiles = csvFiles[grep(".csv", csvFiles)]
    }
    if(missing(tabFiles)) {
      tabFiles = list.files(dir)
      tabFiles = tabFiles[grep(".tab", tabFiles)]
    }

    if(missing(xptNames)) {
      xptNames = sub("(.*).xpt","\\1",xptFiles)
    }
    if(missing(xlsNames)) {
      xlsNames = sub("(.*).xls[x]*","\\1",xlsFiles)
    }
    if(missing(csvNames)) {
      csvNames = sub("(.*).csv","\\1",csvFiles)
    }
    if(missing(tabNames)) {
      tabNames = sub("(.*).tab","\\1",tabFiles)
    }
  }
  else {  # do not do scan,
          # but must establish variables if missing, for use below
    if (missing(xptFiles)) xptFiles = NULL
    if (missing(xlsFiles)) xlsFiles = NULL
    if (missing(csvFiles)) csvFiles = NULL
    if (missing(tabFiles)) tabFiles = NULL
  }


  if(length(xptFiles)>0) {  # LH
    for(i in 1:length(xptFiles)) {
#     fileData = read.xport(paste(dir,xptFiles[i], sep="/"),as.is=T)  KD 18th October 2013
      fileData = foreign::read.xport(paste(dir,xptFiles[i], sep="/"))
      names(fileData) = tolower(names(fileData))
      fileData$rec.id = 1:nrow(fileData)
      assign(paste("i", xptNames[i], sep=""), fileData,
             envir = target.env) # Archival copy of the data
      assign(xptNames[i], fileData,
             envir = target.env) # Working copy
      print(paste("Created data frame ",xptNames[i],sep=""))
    }
  }
  if(length(xlsFiles)>0) {  # LH
    for(i in 1:length(xlsFiles)) {
      fileData = read.xls(paste(dir,xlsFiles[i], sep="/"), perl=perl, sheet=xlsSheets[i])
      names(fileData) = tolower(names(fileData))
      fileData$rec.id = 1:nrow(fileData)
      assign(paste("i", xlsNames[i], sep=""), fileData,
             envir = target.env) # Archival copy of the data
      assign(xlsNames[i], fileData,
             envir = target.env) # Working copy
      print(paste("Created data frame ",xlsNames[i],sep=""))
    }
  }
  if(length(csvFiles)>0) {  # LH
    for(i in 1:length(csvFiles)) {
      fileData = read.csv(paste(dir,csvFiles[i], sep="/"), as.is=TRUE)
      names(fileData) = tolower(names(fileData))
      fileData$rec.id = 1:nrow(fileData)
      assign(paste("i", csvNames[i], sep=""), fileData,
             envir = target.env) # Archival copy of the data
      assign(csvNames[i], fileData,
             envir = target.env) # Working copy
      print(paste("Created data frame ",csvNames[i],sep=""))
    }
  }
  if(length(tabFiles)>0) {  # LH
    for(i in 1:length(tabFiles)) {
      fileData = read.delim(paste(dir,tabFiles[i], sep="/"), as.is=TRUE)
      names(fileData) = tolower(names(fileData))
      fileData$rec.id = 1:nrow(fileData)
      assign(paste("i", tabNames[i], sep=""), fileData,
             envir = target.env) # Archival copy of the data
      assign(tabNames[i], fileData,
             envir = target.env) # Working copy
      print(paste("Created data frame ",tabNames[i],sep=""))
    }
  }
}


if (F) {
  load.data(xlsFiles="ExampleInput.xlsx",
            xlsNames="XLinput1",
            xlsSheets=1, doScan=FALSE)
  #[1] "Created data frame XLinput1"

  head(XLinput1)
  #a         b c          d rec.id
  #1 29 10.779126 a  -9.962622      1
  #2 24 19.237118 a  15.411355      2
  #3 29 16.632331 b   7.596994      3
  #4 28  2.927655 c -33.517035      4
  #5 28 33.271880 c  57.515641      5
  #6 21 22.987656 c  26.662967      6

  head(iXLinput1)
  #a         b c          d rec.id
  #1 29 10.779126 a  -9.962622      1
  #2 24 19.237118 a  15.411355      2
  #3 29 16.632331 b   7.596994      3
  #4 28  2.927655 c -33.517035      4
  #5 28 33.271880 c  57.515641      5
  #6 21 22.987656 c  26.662967      6

  load.data(xlsFiles="ExampleInput.xlsx",
            xlsNames="XLinput2",
            xlsSheets=2,            # NOTE WE can specify which worksheet (tab) of the Excel file
            doScan=FALSE)

  head(iXLinput2)
  #   e        f g        h rec.id
  #   1 130 43.64459 a 83.24640      1
  #   2 127 44.20029 a 83.92436      2
  #   3 117 53.74561 b 95.56964      3
  #   4 101 44.77443 c 84.62481      4
  #   5 105 54.66527 c 96.69163      5
  #   6 117 52.22399 c 93.71326      6

  # Exammple of providing a LIST of files to import
  load.data(csvFiles=c("ExampleInput2.csv","ExampleInput3.csv"),
            csvNames=c("CSVinput1", "CSVinput2"),
            doScan=FALSE)
  #[1] "Created data frame CSVinput1"
  #[1] "Created data frame CSVinput2"

  head(CSVinput1)
  #   i          j k         l rec.id
  #   1 12  1.4278484 a  21.17344      1
  #   2 13 25.7748203 a 123.39071      2
  #   3 10 17.5700167 b  82.36330      3
  #   4 19 27.6167870 c 116.69581      4
  #   5 28 27.3402980 c 119.13643      5
  #   6 17  0.3835043 c  13.86611      6

  load.data() # loading JUST doing automatic search for files ("scan")
}
