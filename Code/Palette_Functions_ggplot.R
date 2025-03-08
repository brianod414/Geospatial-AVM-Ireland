## Colour Palette for ggplot
# https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

# Setting up Palette
bin_colors <- c(`Midnight Green` = "#005266", 
                `Blue Sapphire` = "#00627A", 
                `Cyan Process` = "#00B4E0", 
                `Vivid Sky Blue` = "#1FD2FF", 
                `Blizzard Blue` = "#C2F3FF", 
                `Alice Blue` = "#E3F2FD", 
                `Cultured` = "#FAFAFA", 
                `Pale Pink` = "#FEDAD7", 
                `Vivid Tangerine` = "#FFA599", 
                `Bittersweet` = "#FA6D61", 
                `Persian Red` = "#D43835", 
                `Lava` = "#CC0022", 
                `Firebrick` = "#B8001F", 
                `Ruby Red` = "#A3001B", 
                `Forest Green Crayola` = "#6BAA75", 
                `Tyrian Purple` = "#5F0F40", 
                `Yellow Green Crayola` = "#D5E68D", 
                `Jasmine` = "#FADF7F", 
                `Safety Orange` = "#F96900",
                `Dark Olive` = "#65743A",
                `Hookers Green` = "#4E6E58", 
                `Spanish Green` = "#32965D", 
                `Emerald` = "#47C27C", 
                `Raspberry` = "#D32E5EFF", 
                `Coffee` = "#D9B48FFF", 
                `Eggshell` = "#EBE6D6",
                `Hot Pink` = "#F374AE", 
                `Brunswick Green` = "#32533D",
                `Thistle` = "#E8C7DE", 
                `Pale Silver` = "#CBBEB3",      # More like a beige
                `Burnt Sienna` = "#DD6E42",
                `Columbia Blue` = "#C0D6DF",
                `Quinacridone Magenta` = "#993955",
                `Jam` = "#67023F",
                `Orchid` = "#AF69EF",
                `Plum` = "#601A35",
                `Royal Purple` = "#7851A9", 
                `Tiger` = "#FC6A03",
                `Cantaloupe` = "#FDA172",
                `Merigold` = "#FCAE1E",
                `Midnight Blue` = "#191970",
                `Gold Web` = "#FFD700",
                `Caramel` = "#65350F",
                `Chocolate` = "#2E1503",
                `Paradise Pink` = "#EF476F",
                `Yellow Orange Crayola` = "#FFD166",
                `Royal Blue` = "#4169e1",
                `Non Photo Blue` = "#99EBFF",
                `Sky Blue Crayola` = "#5CDEFF",
                `Blue Green` = "#0093B8",
                `Cyclamen` = "#E27898",
                `Blush` = "#DB577E",
                `Orange Red` = "#FC6B22",
                `Funky Purple` = "#C22ED0",
                `Funky Blue` = "#5FFAE0",
                `Purple Grey` = "#9FA5D5",
                `Cream Green` = "#E8F5C8", 
                `North PCode` = "#EF476F", 
                `South PCode` = "#005266",
                `Ivory` = "#EFDCBE"
                )

#' Function to extract bin colors as hex codes
#'
#' @param ... Character names of bin_colors 
#'
bin_c_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (bin_colors)
  
  bin_colors[cols]
}

bin_palettes <- list(
  `main` = bin_c_cols("Ruby Red", "Eggshell", "Hookers Green"),
  `GrOg` = bin_c_cols("Forest Green Crayola", "Bittersweet"), 
  `OgPur` = bin_c_cols("Bittersweet", "Cultured", "Tyrian Purple"), 
  `RedYGreen` = bin_c_cols("Ruby Red", "Jasmine", "Spanish Green"), 
  `RedsYwGr` =  bin_c_cols("Ruby Red", "Safety Orange", "Jasmine", 
                           "Yellow Green Crayola", "Spanish Green"), 
  `PkBu` = bin_c_cols("Cyan Process", "Raspberry"), 
  `Brunch_IceCream` = bin_c_cols("Coffee", "Raspberry"),
  `MagEm` = bin_c_cols("Quinacridone Magenta", "Emerald"),
  `PurOrg` = bin_c_cols("Orchid", "Vivid Tangerine", "Tiger"),
  `BlYw` = bin_c_cols("Midnight Blue", "Alice Blue", "Gold Web"),
  `OrGr` = bin_c_cols("Safety Orange", "Jasmine", "Emerald"),
  `OrBl` = bin_c_cols("Safety Orange", "Jasmine", "Cultured", 
                      "Blizzard Blue", "Cyan Process"),
  `RdBl` = bin_c_cols("Ruby Red", "Bittersweet", "Cultured", "
                      Blizzard Blue", "Cyan Process"),
  `LightPinkBlue` = bin_c_cols("Pale Pink", "Orchid", "Royal Blue"),
  `YwPkBu` = bin_c_cols("Bumblebee", "Hot Pink", "Royal Blue"),
  `BlPr` = bin_c_cols("Funky Purple", "Funky Blue"),
  `PGCG` = bin_c_cols("Purple Grey", "Cream Green"),
  `MnSv`  = bin_c_cols("Maroony", "Silvery"), 
  `Postcodes` = bin_c_cols("North PCode", "South PCode"), 
  `WYOR` = bin_c_cols('white', 'Jasmine', 'Yellow Orange Crayola', 'Burnt Sienna', 'Lava')
) 

#' Return function to interpolate a bin color palette
#'
#' @param palette Character name of palette in bin_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
bin_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- bin_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#' Color scale constructor for bin colors
#'
#' @param palette Character name of palette in bin_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_bin <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bin_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("bin_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for bin colors
#'
#' @param palette Character name of palette in binj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_bin <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bin_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("bin_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
