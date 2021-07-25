## Plot showing the location of a few genomes on chromosomes

library(httr)
library(jsonlite)
library(ggplot2)
library(ggrepel)
library(magrittr)
library(purrr)


## Get an endpoint from the Ensembl REST api and return parsed JSON

get_from_rest_api <- function(endpoint_string,
                              server = "https://rest.ensembl.org/") {
  rest <- GET(paste(server, endpoint_string, sep = ""),
              content_type("application/json"))
  
  stop_for_status(rest)
  
  fromJSON(toJSON(content(rest)))
}


## Get chromosomes sizes from the Ensembl REST API

get_chromosome_sizes_from_ensembl <- function(species) {

  json <- get_from_rest_api(paste("info/assembly/", species, sep = ""))

  data.frame(name = as.character(json$top_level_region$name),
             length = as.numeric(json$top_level_region$length),
             stringsAsFactors = FALSE)
}


## Get coordinates from Ensembl ids

get_coordinates_from_ensembl <- function(ensembl_ids) {
 
  map_dfr(ensembl_ids,
                  function(ei) {
                    json <- get_from_rest_api(paste("lookup/id/", ei, sep = ""))
  
                    data.frame(position = (json$start + json$end)/2,
                               chr = json$seq_region_name,
                               display_name = json$display_name,
                               stringsAsFactors = FALSE)
                  })
}


plot_genes <- function(coordinates,
                       chromosome_sizes) {

  ## Restrict to chromosomes that are in data  
  chrs_in_data <- chromosome_sizes[chromosome_sizes$name %in% coordinates$chr,]
  chr_order <- order(as.numeric(chrs_in_data$name))
  
  ggplot() +
    geom_linerange(aes(x = name,
                       ymin = 1,
                       ymax = length/1e6),
                   size = 2,
                   colour = "grey",
                   data = chrs_in_data) +
    geom_text_repel(aes(x = chr,
                        y = position/1e6,
                        label = display_name),
                    nudge_x = 0.33,
                    data = coordinates) +
    scale_y_reverse() +
    ## Fix ordering of chromosomes on x-axis
    scale_x_discrete(limits = chrs_in_data$name[chr_order],
                     labels = chrs_in_data$name[chr_order]) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    xlab("Chromosome") +
    ylab("Position (Mbp)")
  
}


## Some Ensembl genes to test with

ensembl_genes <- c("ENSG00000125845", ## BMP2
                   "ENSG00000181690", ## PLAG1
                   "ENSG00000177508", ## IRX3
                   "ENSG00000140718") ## FTO

chr_sizes <- get_chromosome_sizes_from_ensembl(species = "homo_sapiens")

coords <- get_coordinates_from_ensembl(ensembl_genes)

plot_genes_test <- plot_genes(coords,
                              chr_sizes)
