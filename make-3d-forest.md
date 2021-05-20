Forest 3D scene
================
Eleanor Jackson
20 May, 2021

``` r
library("rayrender")
library("raybonsai")
library("tidyverse")
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.2     ✓ dplyr   1.0.6
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x ggplot2::arrow() masks rayrender::arrow()
    ## x dplyr::filter()  masks stats::filter()
    ## x dplyr::lag()     masks stats::lag()

## Create a forest of randomly placed individuals from three species

``` r
# define species
sp1 <- generate_tree(branch_depth = 8, branch_color = "tan", leaf_color = "chartreuse4",
                       leaf_depth_start = 5)
sp2 <- generate_tree(branch_angle_vert = c(-20, 0, 20), 
                     branch_depth = 8, branch_color = "burlywood4",leaf_color = "darkgreen",
                       leaf_depth_start = 3)
sp3 <- generate_tree(branch_angle_vert = seq(-30, 30, by = 5),
              branch_depth = 8, leaf_color = "pink", leaf_depth_start = 5)

par(mfrow=c(1,3))
render_tree(sp1)
render_tree(sp2)
render_tree(sp3)
```

![](figures/unnamed-chunk-2-1.png)<!-- -->

``` r
# create dataset of trees, 50 individuals of each species
trees_df <- tibble(sp = c("sp1", "sp2", "sp3"),
                   tree = list(sp1, sp2, sp3)) %>%
  slice(rep(1:n(), each = 30)) %>%
  mutate(id = row_number())

trees <- trees_df[["tree"]]

# init forest with one tree
one_tree <- trees[[1]]
remaining_trees <- trees[-1]

forest <- group_objects(
  one_tree,
  pivot_point = c(0, -10, 0), group_angle = c(0, 0, 10)
)

# build forest by adding trees at random locations
build_forest <- function(tree, angle_z) {
  forest <<- forest %>%
    add_object(
      group_objects(tree, pivot_point = c(0, -10, 0), group_angle = c(runif(1, -90, 90), runif(1, -90, 90), angle_z))
    )
}

walk2(remaining_trees, runif(length(remaining_trees), -90, 90), build_forest)

# render forest
# hdr file can be downloaded here: https://hdrihaven.com/hdri/?c=nature&h=kiara_3_morning
forest %>%
  render_tree(
    lights = FALSE, environment_light = here::here("kiara_3_morning_2k.hdr"),
    samples = 100,
    fov = 20, lookat = c(2, 4, 2), lookfrom = c(40, 4, 10),
    aperture = 0.5,
    width = 1200, height = 800,
    ground_color1 = "darkgreen", ground_color2 = "darkolivegreen",
    filename = "figures/random-forest.png"
  )
```
