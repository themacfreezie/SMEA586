addLegendCustom <- function(
    map, 
    colors, 
    labels, 
    sizes, 
    opacity = 0.5
){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0(
    "<div style='display: inline-block;height: ", 
    sizes, "px;margin-top: 4px;line-height: ", 
    sizes, "px;'>", 
    labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
}

addBivariateChoropleth <- function(
    map, map_data, var1_name, var2_name, ntiles = 3,
    var1_label = NA, var2_label = NA,
    label_arrows = TRUE,
    region_names = NA,
    add_legend = TRUE,
    paletteFunction = pals::stevens.pinkblue
){
  
  if(is.na(var1_label)) var1_label <- rlang::enexpr(var1_name)
  if(is.na(var2_label))  var2_label <- rlang::enexpr(var2_name)
  
  
  # separate labels for the palette and the map, if so desired
  var1_pal_label <- var1_label
  var2_pal_label <- var2_label
  
  # are we putting fun arrows on the labels?
  if (label_arrows){
    var1_pal_label <- paste0(var1_label, " \U2192")
    var2_pal_label <- paste0(var2_label, " \U2192")
  }
  
  # the pals package has many palettes for bivariate choropleths.
  # NOTE!! That all of them are 2x2 or 3x3. larger matrices will probably be
  # harder to interpret, and it looks like people don't use them
  bivar_pal <- function(x) paletteFunction(n = ntiles^2)[x]
  
  forplot <- map_data %>%
    dplyr::rename (var1 = {{var1_name}},
                   var2 = {{var2_name}}) %>%
    dplyr::mutate(var1_ntile = dplyr::ntile(var1, n = ntiles),
                  var2_ntile = dplyr::ntile(var2, n = ntiles),
                  pal_num = var1_ntile + (var2_ntile - 1)*ntiles,#(ntiles -var1_ntile)*3 + var2_ntile,
                  pal_colour = bivar_pal(pal_num))
  
  # FIXME: region names not working
  if (!is.na(region_names)) forplot <- dplyr::rename(forplot,
                                                     region_name_label = region_names)
  
  # set up some css for the html palette swatch
  palette_size_px <- 120
  swatch_size_px <- round(palette_size_px / ntiles)
  
  row_col_px <- rep(paste0(swatch_size_px,"px"), times = ntiles) %>%
    stringr::str_flatten(collapse = " ") %>%
    paste0(., ";")
  
  
  div_var1 <- paste0('<div class = "var1-label" style="grid-row-start:1; grid-row-end:',(ntiles+1),'; text-align: center; writing-mode: tb-rl;
        transform: rotate(-180deg);">',var1_pal_label,'</div>')
  div_var2 <- paste0('<div style="text-align:center; grid-column:2 / ',(ntiles+2),';">',var2_pal_label,'</div>')
  
  # set up the indices for the palette
  div_indices <- matrix((1 : ntiles^2),
                        nrow = ntiles,
                        ncol = ntiles,
                        byrow = TRUE)
  
  div_indices <- div_indices[, c(ntiles : 1)]
  
  # set up the divs for the palette squares
  divs <- paste0('<div style="background-color:',bivar_pal(div_indices),
                 '; color:', bivar_pal(div_indices),
                 ';">',div_indices,' </div>') %>%
    stringr::str_flatten()
  
  # combine the above bits with a css grid wrapper for the html palette
  palette_html <- paste0(
    '<style> .grid-container { display: grid;
    grid-template-columns: 40px ',row_col_px,
    'grid-auto-rows: ',row_col_px,' 40px;','}
    </style>
    <div class="grid-container">',
    div_var1,
    divs,
    div_var2,
    '</div>')
  
  labs <- paste0("<b>", var1_label,"</b>",
                 "<br>Ntile: ", forplot$var1_ntile,
                 "<br>Value: ", round(forplot$var1, 2),
                 "<br><b>", var2_label,"</b> ",
                 "<br>Ntile: ",forplot$var2_ntile,
                 "<br>Value: ", round(forplot$var2, 2)
  )
  
  # FIXME: region names not working
  if(!is.na(region_names)) labs <- paste0("<b>",forplot$region_name_label,"</b><br>",labs)
  
  labs <- purrr::map(labs, htmltools::HTML)
  
  # are we adding a legend? we may not want to, if e.g. we're being clever and
  # showing different bivariate choropleths (with the same palettes) at different
  # zoom levels
  if (add_legend){
    map <- map %>%
      leaflet::addControl(
        html = palette_html,
        position = "bottomleft"
      )
  }
  map %>%
    leaflet::addPolygons(data = forplot,
                         label = labs,
                         fillColor = ~pal_colour,
                         color = "grey30",
                         opacity = 0.5,
                         fillOpacity = 0.7)
  
}