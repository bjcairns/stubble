library(hexSticker)
library(ggplot2)
library(cowplot)
library(showtext)

### Some magic bits and pieces
#########################

## Load the font if installed, or install from Google Fonts
tryCatch(
  font_add(google_font, paste0(google_font, "-Bold.ttf")),
  error = function(err) font_add_google(google_font, google_font)
)

showtext_auto()

## Colours
bar_colour <- "#cca929"
pkg_name_colour <- "#444d3c"
sticker_bg_colour <- "#d6f2bd"
sticker_border_colour <- "#403200"
stub_motif_colour <- "#4d3f0f"

## Font
google_font <- "Roboto"

## Horizontal (x) and vertical (y) extent of the "stub" motif
x_cal <- sqrt(2)/2
y_cal <- 1/2

## Data frames for the bar plot components
df1 <- data.frame(
  cat = c(1,2,3),
  bar = c(1,5,4)
)
df2 <- data.frame(
  cat = c(1,2,3),
  bar = c(4,3,3)
)
df3 <- data.frame(
  cat = c(1,2,3),
  bar = c(3,5,2)
)


### Construct the bar plot components
#####################################

## Bar plot generating function to avoid repetition
bar_it <- function(df) {
  ggplot(df, aes(x = cat, y = bar)) + 
    geom_bar(stat = "identity", width = 0.65, color = bar_colour, fill = bar_colour) + 
    theme_void() + theme_transparent()
}

## Apply the bar plot generating function
p_bar1 <- bar_it(df1)
p_bar2 <- bar_it(df2)
p_bar3 <- bar_it(df3)

## Spacer plot object
p_placeholder <- ggplot() + geom_blank() + theme_void()

## Arrange the bar plots
p_bar1 <- plot_grid(p_placeholder, p_bar1, p_placeholder, ncol = 1, rel_heights = c(1,4,1))
p_bar2 <- plot_grid(p_bar2, p_placeholder, ncol = 1, rel_heights = c(2,1))


### ggplot object for the "stubble" name
########################################

## Construct the package name plot
p_title <- ggplot() + 
  annotate(
    "text", 
    x = 1, 
    y = 0.5, 
    hjust = 0.49,
    label = "stubble", 
    angle = 90, 
    color = pkg_name_colour, 
    size = 23, 
    family = google_font,
    fontface = "bold"
  ) + 
  theme_void()


### Create the stub motif for bottom part of the plot component of the sticker
##############################################################################

motif <- function(x, y) {
  ggplot() +
    geom_segment(aes(x = x-x_cal, y = y, xend = x+x_cal, yend = y), size = 1.5, lineend = "square", color = stub_motif_colour) +
    geom_segment(aes(x = x-x_cal, y = y, xend = x-x_cal, yend = y_cal+y), size = 1.5, lineend = "square", color = stub_motif_colour) +
    geom_segment(aes(x = x+x_cal, y = y, xend = x+x_cal, yend = y_cal+y), size = 1.5, lineend = "square", color = stub_motif_colour) +
    geom_segment(aes(x = x, y = y, xend = x, yend = y_cal+y), size = 1.5, lineend = "square", color = stub_motif_colour) +
    xlim(-2, 2) + ylim(0,1.5) + 
    theme_void() + theme_transparent()
}


### Put all the pieces together
###############################

## Bottom line for the plot component of the sticker
bottom_line <- motif(0,0.5)

## Top line for the plot component of the sticker (bar plots and pkg name)
top_line <- plot_grid(p_bar1, p_placeholder, p_bar2, p_placeholder, p_title, p_bar3, nrow = 1,
                      rel_widths = c(0.8,0.45,0.8,0.45,1,0.8))

## Construct the complete plot component7
p <- plot_grid(top_line, bottom_line, ncol = 1, rel_heights = c(4,1.5))


### Create and plot the sticker
###############################

s <- sticker(
  p,
  s_x = 1,
  s_y = 0.9,
  s_width = 1.4,
  s_height = 1.4,
  package = "",
  h_fill = sticker_bg_colour,
  h_color = sticker_border_colour,
  filename = "man/figures/stubble_hex.png"
)

print(plot(s))

