#' Vector and Insert DNA volume calculations
#'
#' Calculates the amount of Vector and Insert DNA in required molar ratios. Uses the equation: required mass insert (g) = desired insert/vector molar ratio x mass of vector (g) x ratio of insert to vector lengths
#' @param v_dna_len The length of the vector in base pairs (bp)
#' @param v_ng The mass (ng) of vector per ul
#' @param i_dna_len The length of insert in base pairs (bp)
#' @param i_ng The mass (ng) of insert per ul
#' @param ratio The ratio of insert to vector that you require. It is recommended to have a ratio of 2:1 or 3:1. Default is 2.
#' @param tot_dna_vol The total DNA volume used for the assembly. Default is 5 (for 10ul reactions)
#' @return A table that includes the vector and insert final volumes and mass.
#' @examples
#' table <- gibson(4129, 200, 1787, 24.3)
#' table <- gibson(4129, 200, 1787, 24.3, 3)
#' table <- gibson(4129, 200, 1787, 24.3, 3, 10)
#' @export

gibson <- function(v_dna_len, v_ng, i_dna_len, i_ng, ratio = 2,tot_dna_vol = 5){
  # required DNA mass for equal molar ration
  req_i <- ratio * v_ng * (i_dna_len/v_dna_len)
  # volume of insert for 1 ul of vector
  dna_v <- req_i/i_ng
  # ratio of vector in DNA mix
  v_vol_ratio = 1/(dna_v+1)
  # volume of vector with respect to final volume
  v_vol = tot_dna_vol * v_vol_ratio
  # volume of insert with respect to final volume
  dna_vol = tot_dna_vol - v_vol
  # total mass of DNA of each part
  v_mass = v_vol * v_ng
  i_mass = dna_vol * i_ng
  # table containing the volumes and mass of each biopart
  table <- data.frame(v_vol, dna_vol, v_mass, i_mass)
  return(table)
}


