#' @rdname penguins_ext
#' 
#' @title 
#' Data on adult penguins near Palmer Station, Antarctica
#' 
#' @description 
#' Measurements of physical characteristics and nesting success of penguins of 
#' several species on islands in the Palmer Archipelago. Derived from the 
#' [palmerpenguins](https://allisonhorst.github.io/palmerpenguins/) package.
#' 
#' @usage 
#' penguins_ext
#' 
#' @details 
#' 
#' These data are an **ext**ended version of the \link[palmerpenguins]{penguins} 
#' data, using addition variables from \link[palmerpenguins]{penguins_raw}. 
#' Variables were chosen to represent several data types (character, factor, 
#' numeric, integer, logical, and Date) for use in examples for stubble (see e.g. [stub()]).
#' 
#' The data include the following variables:
#' \describe{
#'   \item{`id`}{a character string denoting the unique ID for each individual in dataset}
#'   \item{`species`}{a factor denoting penguin species (Adélie, Chinstrap and Gentoo)}
#'   \item{`island`}{a factor denoting island in Palmer Archipelago, Antarctica (Biscoe, Dream or Torgersen)}
#'   \item{`bill_length_mm`}{a numeric denoting bill length (millimeters)}
#'   \item{`bill_depth_mm`}{a numeric denoting bill depth (millimeters)}
#'   \item{`flipper_length_mm`}{an integer denoting flipper length (millimeters)}
#'   \item{`body_mass_g`}{an integer denoting body mass (grams)}
#'   \item{`sex`}{a factor denoting penguin sex (female, male)}
#'   \item{`clutch_completion`}{logical denoting whether the nest was observed with a full clutch (2 eggs)}
#'   \item{`date_egg`}{a Date denoting the date study nest observed with 1 egg (sampled)}
#' }
#' (Variable descriptions were adapted from the palmerpenguins package.)
#' 
#' @source {All data were derived from the [palmerpenguins](https://allisonhorst.github.io/palmerpenguins/) package, under that package's [CC0-1.0 license](https://creativecommons.org/publicdomain/zero/1.0/). Original sources are listed below.}
#' @source {Adélie penguins: Palmer Station Antarctica LTER and K. Gorman. 2020. Structural size measurements and isotopic signatures of foraging among adult male and female Adélie penguins (Pygoscelis adeliae) nesting along the Palmer Archipelago near Palmer Station, 2007-2009 ver 5. Environmental Data Initiative} \url{https://doi.org/10.6073/pasta/98b16d7d563f265cb52372c8ca99e60f}
#' @source {Gentoo penguins: Palmer Station Antarctica LTER and K. Gorman. 2020. Structural size measurements and isotopic signatures of foraging among adult male and female Gentoo penguin (Pygoscelis papua) nesting along the Palmer Archipelago near Palmer Station, 2007-2009 ver 5. Environmental Data Initiative} \url{https://doi.org/10.6073/pasta/7fca67fb28d56ee2ffa3d9370ebda689}
#' @source {Chinstrap penguins: Palmer Station Antarctica LTER and K. Gorman. 2020. Structural size measurements and isotopic signatures of foraging among adult male and female Chinstrap penguin (Pygoscelis antarcticus) nesting along the Palmer Archipelago near Palmer Station, 2007-2009 ver 6. Environmental Data Initiative} \url{https://doi.org/10.6073/pasta/c14dfcfada8ea13a17536e73eb6fbe9e}
#' @source {Originally published in: Gorman KB, Williams TD, Fraser WR (2014) Ecological Sexual Dimorphism and Environmental Variability within a Community of Antarctic Penguins (Genus Pygoscelis). PLoS ONE 9(3): e90081. doi:10.1371/journal.pone.0090081}
#' 
#' @examples 
#' summary(penguins_ext)
"penguins_ext"

