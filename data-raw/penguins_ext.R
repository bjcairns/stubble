## Construct a dataset based on the palmerpenguins package:
#
# Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer Archipelago 
# (Antarctica) penguin data. R package version 0.1.0. 
# https://allisonhorst.github.io/palmerpenguins/
#
# Used under CC0-1.0 license.


## Not run
# install.packages("palmerpenguins")

library(palmerpenguins)

p1 <- palmerpenguins:::penguins_df
p2 <- palmerpenguins:::penguins_raw_df[
  c("Individual ID", "Clutch Completion", "Date Egg")
]

names(p2) <- c("id", "clutch_completion", "date_egg")

p <- cbind(p1, p2)

p$clutch_completion <- (p$clutch_completion == "Yes")


# Selection of columns includes integer, numeric, factor, character, Date,
# and logical columns
penguins_ext <- p[
  c(
    "id",
    "species",
    "island",
    "bill_length_mm",
    "bill_depth_mm",
    "flipper_length_mm",
    "body_mass_g",
    "sex",
    "clutch_completion",
    "date_egg"
  )
]

save(penguins_ext, 
     file = "data/penguins_ext.RData", 
     compress = "xz", 
     version = 2
)
