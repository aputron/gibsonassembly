#' Nanograms conversion for DNA
#'
#' Convert Nanogram units to Femtomoles
#' @param ng The amount of DNA in solution as ng, as outputted by nanodrop/qubit
#' @param dna_len The length of DNA being used
#' @return The amount of DNA in a solution as Femtomoles
#' @examples
#' dna_fm <- ng_to_fm(8.8, 520);
#' dna_fm <- ng_to_fm(c(8.8, 10.1), c(520, 500));
#' dna_fm <- ng_to_fm(c(8.8, 10.1), 520);
#' @export
ng_to_fm <- function(ng, dna_len){
  femtomoles <- (ng)/((dna_len*617.96) + 36.04)*10^6
  return(femtomoles)
}


#' Femtomoles conversion for DNA
#'
#' Convert Femtomoles units to Nanograms
#' @param fm The amount of DNA in solution as fm
#' @param dna_len The length of DNA being used
#' @return The amount of DNA in a solution as Nanograms
#' @examples
#' dna_ng <- fm_to_ng(8.8, 520);
#' dna_ng <- fm_to_ng(c(8.8, 10.1), c(520, 500));
#' dna_ng <- fm_to_ng(c(8.8, 10.1), 520);
#' @export
fm_to_ng <- function(fm, dna_len){
  nanograms <- fm * ((dna_len*617.96) + 36.04) / 10^6
  return(nanograms)
}

#' Nanograms conversion table of DNA for experiments
#'
#' Convert Nanogram units to Femtomoles
#' @param ng The amount of DNA in solution as ng, as outputted by nanodrop/qubit
#' @param dna_len The length of DNA being used
#' @param required_fm The required amount of DNA in femtomoles for the experimental method
#' @return A table containing the ng, dna_len, DNA conc in fm, and DNA volume
#' @examples
#' dna_fm <- ng_to_fm(8.8, 520, 250);
#' dna_fm <- ng_to_fm(c(8.8, 10.1), c(520, 500), 250);
#' dna_fm <- ng_to_fm(c(8.8, 10.1), 520, c(250,100));
#' @export
ng_to_fm_table <- function(ng, dna_len, required_fm, total_vol){
  femtomoles <- ng_to_fm(ng, dna_len)
  dna_vol <- required_fm/femtomoles
  table <- data.frame(ng, dna_len, femtomoles, dna_vol)
  return(table)
}

#' Femtomoles conversion table of DNA for experiments
#'
#' Convert Femtomole units to Nanogram
#' @param fm The amount of DNA in solution as fm
#' @param dna_len The length of DNA being used
#' @param required_fm The required amount of DNA in nanograms for the experimental method
#' @return A table containing the fm, dna_len, DNA conc in ng, and DNA volume
#' @examples
#' dna_fm <- ng_to_fm(8.8, 520, 250);
#' dna_fm <- ng_to_fm(c(8.8, 10.1), c(520, 500), 250);
#' dna_fm <- ng_to_fm(c(8.8, 10.1), 520, c(250,100));
#' @export
fm_to_ng_table <- function(fm, dna_len, required_fm){
  nanograms <- fm_to_ng(fm, dna_len)
  dna_vol <- required_fm/femtomoles
  table <- data.frame(fm, dna_len, nanograms, dna_vol)
  return(table)
}
