## The Broad Institute
## SOFTWARE COPYRIGHT NOTICE AGREEMENT
## This software and its documentation are copyright (2021) by the
## Broad Institute/Massachusetts Institute of Technology. All rights are
## reserved.
##
## This software is supplied without any warranty or guaranteed support
## whatsoever. Neither the Broad Institute nor MIT can be responsible for its
## use, misuse, or functionality.

args <- commandArgs(trailingOnly=TRUE)

library(optparse)
option_list <- list(
  make_option("--first.input.file", dest="first.input.file"),
  make_option("--second.input.file", dest="second.input.file"),
  make_option("--round.method", dest="round.method"),
  make_option("--round.digits", dest="round.digits", default=NULL),
  make_option("--tolerance", dest="comp.tolerance", default=NULL)
  )

opt <- parse_args(OptionParser(option_list=option_list), positional_arguments=TRUE, args=args)
print(opt)
opts <- opt$options
sessionInfo()

all.args <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.dir <- dirname(sub(file.arg.name, "", all.args[grep(file.arg.name, all.args)]))
source(file.path(script.dir, "common.R"))
source(file.path(script.dir, "diff_datasets.R"))

Diff.Datasets(opts$first.input.file, opts$second.input.file, opts$round.method, opts$round.digits, opts$comp.tolerance)