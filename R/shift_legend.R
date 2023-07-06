library(gtable)
library(cowplot)
library(purrr)
library(lemon)

shift_legend <- function(p) {
  pnls <- cowplot::plot_to_gtable(p) %>% gtable::gtable_filter("panel") %>%
    with(setNames(grobs, layout$name)) %>% purrr::keep(~identical(.x, zeroGrob()))

  if( length(pnls) == 0 ) stop( "No empty facets in the plot" )

  lemon::reposition_legend( p, "center", panel=names(pnls) )
}


