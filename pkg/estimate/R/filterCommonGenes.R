###
### $Id$
###


##-----------------------------------------------------------------------------
filterCommonGenes <- function(input.f,
                              output.f=NULL,
                              id=c("GeneSymbol", "EntrezID"),
                              writeToDisk=FALSE) {

    ## Check arguments
    stopifnot((is.character(input.f) && length(input.f) == 1 && nzchar(input.f)) ||
              (inherits(input.f, "connection") && isOpen(input.f, "r")) ||
              inherits(input.f, "data.frame"))
    if (writeToDisk) {
      stopifnot((is.character(output.f) && length(output.f) == 1 && nzchar(output.f)))
    }
    id <- match.arg(id)
     
    ## Read input data
    if (inherits(input.f, "data.frame") == FALSE) {
      input.df <- read.table(input.f,
                             header=TRUE,
                             row.names=1,
                             sep="\t", 
                             quote="",
                             stringsAsFactors=FALSE)
    } else {
      input.df <- input.f
    }
     
    merged.df <- merge(common_genes, input.df, by.x=id, by.y="row.names")
    rownames(merged.df) <- merged.df$GeneSymbol
    merged.df <- merged.df[, -1:-ncol(common_genes)]
    print(sprintf("Merged dataset includes %d genes (%d mismatched).",
                  nrow(merged.df),
                  nrow(common_genes) - nrow(merged.df)))

    ## Output data
    if (writeToDisk) {
      outputGCT(merged.df, output.f)
    } else {
      return(merged.df)
    }
}

