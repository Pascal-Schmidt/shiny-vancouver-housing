base_map <- function(filtered_df, df) {
  
  pal2 <- RColorBrewer::brewer.pal(2, "Set1")
  
  # use initial df so we have the same color for each category
  # and we do not switch colors for categories based on filtered_df cats
  leaf_pal <- leaflet::colorFactor(palette = pal2, domain = df$above_mvalue)
  
  leafletProxy("map", data = filtered_df) %>%
    clearMarkerClusters() %>%
    clearControls() %>%
    addCircleMarkers(
      clusterOptions = markerClusterOptions(),
      stroke = FALSE, fill = TRUE, fillOpacity = .7,
      color = ~ leaf_pal(above_mvalue),
      popup = paste(
        "<strong>Price: </strong>", filtered_df$price %>%
          scales::dollar(), "<br/>",
        "<strong>", filtered_df$market_rate, "</strong>", filtered_df$rate, "<br/>",
        "<strong>", filtered_df$what, "</strong>", filtered_df$predicted, "<br/>",
        "<strong>Price to Rent Ratio: </strong>", filtered_df$price_to_rent, "<br/>",
        "<strong>Type: </strong>", filtered_df$type, "<br/>",
        "<strong>Bed, Bath, Square Feeet: </strong>", filtered_df %>%
          tidyr::unite(col = "united", bed, bath, sqft, sep = ", ") %>%
          dplyr::pull(united), "<br/>",
        "<strong>Address: </strong>", filtered_df$address, "<br/>",
        "<strong>URL: </strong>", "<a href =", filtered_df$url, "> Go to Real Estate Listing </a>", "<br/>"
      ),
      group = "Properties"
    ) %>%
    addLayersControl(baseGroups = c("Map 1", "Map 2", "Map 3"), 
                     overlayGroups = c("Properties"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addLegend("bottomright",
              pal = leaf_pal,
              values = ~above_mvalue, title = "Category"
    )
  
}