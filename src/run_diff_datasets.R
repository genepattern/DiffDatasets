## The Broad Institute
## SOFTWARE COPYRIGHT NOTICE AGREEMENT
## This software and its documentation are copyright (2015) by the
## Broad Institute/Massachusetts Institute of Technology. All rights are
## reserved.
##
## This software is supplied without any warranty or guaranteed support
## whatsoever. Neither the Broad Institute nor MIT can be responsible for its
## use, misuse, or functionality.

suppressMessages(suppressWarnings(library(getopt)))
suppressMessages(suppressWarnings(library(optparse)))

sessionInfo()

args <- commandArgs(trailingOnly=TRUE)

libdir <- args[1]

source(file.path(libdir, "common.R"))

option_list <- list(
  make_option("--first.input.file", dest="first.input.file"),
  make_option("--second.input.file", dest="second.input.file"),
  make_option("--round.method", dest="round.method"),
  make_option("--round.digits", dest="round.digits", default=NULL)
  )

opt <- parse_args(OptionParser(option_list=option_list), positional_arguments=TRUE, args=args)
print(opt)
opts <- opt$options

source(file.path(libdir, "diff_datasets.R"))

Diff.Datasets(opts$first.input.file, opts$second.input.file, opts$round.method, opts$round.digits)

sessionInfo()