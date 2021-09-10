## The Broad Institute
## SOFTWARE COPYRIGHT NOTICE AGREEMENT
## This software and its documentation are copyright (2021) by the
## Broad Institute/Massachusetts Institute of Technology. All rights are
## reserved.
##
## This software is supplied without any warranty or guaranteed support
## whatsoever. Neither the Broad Institute nor MIT can be responsible for its
## use, misuse, or functionality.

# Check whether common.R has been loaded using read.gct as an indicator.  If not,
# try to load it from the current directory.  This is just a workaround for using
# the script in gp-unit contexts.  This stuff should eventually reside in packages
# stored in one of the places R usually expects to find it.
if (! exists("read.gct")  && file.exists("common.R")) {
   source("common.R")
}

isRnk <- function(file.name) {
    return (regexpr("\\.rnk$", tolower(file.name))[[1]] != -1)
}


Diff.Datasets <- function(first.input.file, second.input.file, round.method, round.digits, comp.tolerance) {
   if (trim(first.input.file) == "") {
      stop("Missing required argument first.input.file")
   }

   if (trim(second.input.file) == "") {
      stop("Missing required argument second.input.file")
   }

   digits <- require.numeric.or.default(round.digits, "round.digits", 6)

   round.function <- NULL
   if (round.method == "round") {
      round.function <- round.function.factory(digits)
   }
   else if (round.method == "signif") {
      round.function <- signif.function.factory(digits)
   }
   else if (round.method != "none") {
      stop(paste("Received unknown rounding method '", round.method, "'", sep=""))
   }

   comp.tolerance <- require.numeric.or.default(comp.tolerance, "comp.tolerance", 0.00001)

   # Load up the datasets.  We'll use the first input file as a basis for 
   # determining whether to use read.dataset and assume that for both files
   # (and likewise for CSV files); if it's not true then they won't diff anyway.
   first.ds <- NULL
   second.ds <- NULL
   first.matrix <- NULL
   second.matrix <- NULL
   if (isGctOrRes(first.input.file)) {
      first.ds <- read.dataset(first.input.file)
      second.ds <- read.dataset(second.input.file)
      first.matrix <- first.ds$data
      second.matrix <- second.ds$data
   }
   else if (isCsv(first.input.file)) {
      # Assuming US-style CSVs with ',' separator and '.' decimal.
      first.matrix <- read.csv(first.input.file)
      second.matrix <- read.csv(second.input.file)
   }
   else if (isRnk(first.input.file)) {
      first.matrix <- read.table(first.input.file, fill=TRUE, quote="", sep="\t", row.names = 1)
      second.matrix <- read.table(second.input.file, fill=TRUE, quote="", sep="\t", row.names = 1)
      
      # For an RNK file, any ties may be in arbitrary order so we reorder the first to match the second.
      # Note that we also make sure both have the same dimensions beforehand.
      if (any(dim(first.matrix) != dim(second.matrix))) {
         stop(paste("The dimensions of these two datasets do not match, which is required for comparison"))
      }
      fixLabelOrder <- match(rownames(first.matrix), rownames(second.matrix))
      if (length(fixLabelOrder) != nrow(second.matrix)) {
         stop(paste("The labels of these two datasets do not match, which is required for comparison"))
      }
      first.matrix <- as.data.frame(first.matrix[fixLabelOrder,])
      colnames(first.matrix) <- colnames(second.matrix)
   }
   else {
      # otherwise we'll just take a stab at loading these as whatever else can be 
      # handled by read.table with the default settings.
      first.matrix <- read.table(first.input.file, fill=TRUE)
      second.matrix <- read.table(second.input.file, fill=TRUE)
   }
   
   if (any(dim(first.matrix) != dim(second.matrix))) {
      stop(paste("The dimensions of these two datasets do not match, which is required for comparison"))
   }
   
   # If provided, apply the round.function across the datasets (processing numerics only)
   if (!is.null(round.function)) {
       first.matrix <- apply(first.matrix, MARGIN=c(1,2), FUN=round.function)
       second.matrix <- apply(second.matrix, MARGIN=c(1,2), FUN=round.function)
   }

   # Output the two rounded datasets to text files to allow for visual inspection
   first.out.file <- paste(basename(first.input.file), ".rnd1", sep="")
   second.out.file <- paste(basename(second.input.file), ".rnd2", sep="")
   if (isGct(first.input.file)) {
      first.ds$data <- first.matrix
      second.ds$data <- second.matrix
      write.gct(first.ds, first.out.file, check.file.extension=FALSE)
      write.gct(second.ds, second.out.file, check.file.extension=FALSE)
   }
   else if (isRes(first.input.file)) {
      first.ds$data <- first.matrix
      second.ds$data <- second.matrix
      write.res(first.ds, first.out.file, check.file.extension=FALSE)
      write.res(second.ds, second.out.file, check.file.extension=FALSE)
   }
   else if (isRnk(first.input.file)) {
      write.table(first.matrix, first.out.file, col.names=FALSE, quote=FALSE, sep="\t")
      write.table(second.matrix, second.out.file, col.names=FALSE, quote=FALSE, sep="\t")
   }
   else {
      write.table(first.matrix, first.out.file, row.names=FALSE, col.names=FALSE, quote=FALSE)
      write.table(second.matrix, second.out.file, row.names=FALSE, col.names=FALSE, quote=FALSE)
   }

   # Check the row/column labels first
   labels.match <- FALSE
   if (isRes(first.input.file)) {
      labels.match <- all(first.ds$column.descriptions == second.ds$column.descriptions) &&
                      all(first.ds$row.descriptions == second.ds$row.descriptions)
   }
   else {
      labels.match <- all(colnames(first.matrix) == colnames(second.matrix)) &&
                      all(rownames(first.matrix) == rownames(second.matrix))
   }

   # Subtracting the second matrix from the first will result in a matrix considered a match
   # if all values are within the given tolerance.
   if (!labels.match) {
      stop(paste("The labels of these two datasets do not match, which is required for comparison"))
   }
   else if (all(abs(first.matrix - second.matrix) <= comp.tolerance)) {
      write("Both datasets are within tolerance.", stdout());
   }
   else {
      write(paste("Differences found in files '", first.input.file, "' and '", 
                  second.input.file, "'.", sep=""), stderr());
      write(paste("See '", first.out.file, "' and '", second.out.file, 
                  "' for the rounded data.", sep=""), stderr());
      q("no", status=1, runLast=FALSE)
   }
}

round.function.factory <- function(digits) { 
   function(x) {
      if (!is.numeric(x)) { return(x) }
      return(round(x, digits)) 
   }
}

signif.function.factory <- function(digits) { 
   function(x) {
      if (!is.numeric(x)) { return(x) }
      return(signif(x, digits))
   } 
}
