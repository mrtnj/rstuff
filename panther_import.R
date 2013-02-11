library(stringr)
library(plyr)

read.panther <- function(filename="PTHR8.0_chicken") {
  ## Panther chicken classification file from
  ## ftp://chelsea.usc.edu/sequence_classifications/current_release/
  panther <- read.delim(filename, sep="\t", head=F,
                        stringsAsFactors=F)

  colnames(panther)[1] <- "gene.id"
  colnames(panther)[3:5] <- c("panther.id", "panther.family",
                            "panther.subfamily")
  colnames(panther)[6:8] <- c("go.mf", "go.bp", "go.cc")
  colnames(panther)[9:10] <- c("panther.ontology", "panther.pathway")


  ## accession numbers to Entrez, UniProt, Ensembl
  panther$ensembl.id <- str_match(panther$gene, "Gene=(.*)\\|")[,2]
  panther$uniprot.id <- str_match(panther$gene, "UniProtKB=(.*)$")[,2]
  panther$entrez.id <- str_match(panther$gene, "ENTREZ=(.*)\\|")[,2]

  ## function to handle the columns Gene Ontology terms
  parse.go <- function(go.column) {
    go.list <- str_match_all(go.column, "GO:([0-9]*)")
    names(go.list) <- panther$gene.id.string
    go.list <- llply(go.list, function(x) {if (!is.null(dim(x))) x[,1]})
    return(go.list)
  }

  go.mf <- parse.go(panther$go.mf)
  go.bp <- parse.go(panther$go.bp)
  go.cc <- parse.go(panther$go.cc)

  ## Panther pathway ids
  pathway.list <- str_match_all(panther$panther.pathway,
                                "#([G,P,S,U][0-9]*)>")
  names(pathway.list) <- panther$gene.id.string
  pathway.list <- llply(pathway.list,
                        function(x) {if (!is.null(dim(x))) x[,2]})

  ## Package it up as an object
  panther.classification <- list(data=panther,
                                 go.mf=go.mf,
                                 go.bp=go.bp,
                                 go.cc=go.cc,
                                 panther.pathway=pathway.list)
  class(panther.classification) <- "panther.classification"

  return(panther.classification)

}

## Extract pathway information from panther classification
get.pathways <- function(panther, acc) {
  
  ix.uni <- which(panther$data$uniprot.id == acc)
  ix.ens <- which(panther$data$ensembl.id == acc)
  ix.ent <- which(panther$data$entrez.id == acc)
  
  pathways <- character(0)
  
  if (length(ix.uni) > 0) {
    ix <- ix.uni
  } else if (length(ix.ens > 0)) {
    ix <- ix.ens
  } else if (length(ix.ent > 0)){
    ix <- ix.ent
  } 
  panther$panther.pathway[[ix]]
  
}
