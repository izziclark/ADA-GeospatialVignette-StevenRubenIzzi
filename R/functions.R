#' @title mcp_map
#' @description Maps minimum convex polygons around a set of geographic points for one or more groups.
#' @param sp A SpatialPointsDataFrame {sp} containing track (relocation) points of groups or individuals. It should contain 3 columns: "id", "x", "y", where x and y are UTM point coordinates.
#' @param percent Percentage of points out of 100 to include in the MCP. Default is 95.
#' @param units Units to output area. Takes "km2", "m2", "ha" for hectares. Default is "km2".
#' @keywords MCP map
#' @export
#' @examples
#' mcp_map(sp = trackpoints, perc = 90, units = "m2")

mcp_map <- function(sp, perc = 95, units = "km2"){
  # Generate mcp
  sp.mcp <- adehabitatHR::mcp(sp, percent = perc, unout = units)
  # Convert to geographic coordinates
  spgeo <- sp::spTransform(sp, CRS("+proj=longlat"))
  mcpgeo <- sp::spTransform(sp.mcp, CRS("+proj=longlat"))
  # Generate basemap using bounding box from spgeo
  basemap <- ggmap::get_stamenmap(bbox = c(
    left = min(spgeo@coords[,1])-0.005,
    bottom = min(spgeo@coords[,2])-0.005,
    right = max(spgeo@coords[,1])+0.005,
    top = max(spgeo@coords[,2])+0.005),
    zoom = 12)
  # Turn the spatial data frame of points into a regular dataframe for plotting
  spgeo.df <- data.frame(spgeo@coords, id = spgeo@data$id)
  # Map using {ggmap}
  map.mcp <- ggmap::ggmap(basemap) + 
    # "fortify" polygon layer to add geometry to the dataframe
    ggplot2::geom_polygon(data = ggplot2::fortify(mcpgeo), ggplot2::aes(long, lat, colour = id, fill = id), alpha = 0.3) +
    ggplot2::geom_point(data = spgeo.df, ggplot2::aes(x = x, y = y, colour = id), alpha = 0.01, pch = 20)  + # set transparency using alpha and smaller point size use pch.
    labs(x = "Longitude", y = "Latitude")
  map.mcp
  print(sp.mcp)
}

#' @title kde_map
#' @description Maps polygons generated using kernel density estimates for one or more groups.
#' @param sp A SpatialPointsDataFrame {sp} containing track (relocation) points of groups or individuals. It should contain 3 columns: "id", "x", "y", where x and y are UTM point coordinates.
#' @param percent Percentage of estimated distribution to include in polygons. Default is 95.
#' @param units Units to output area. Takes "km2", "m2", "ha" for hectares. Default is "km2".
#' @keywords KDE kernel map
#' @export
#' @examples
#' kde_map(sp = trackpoints, perc = 90, units = "m2")

kde_map <- function(sp, perc = 95, units = "km2"){
  # Generate kernels
  sp.kernels <- adehabitatHR::kernelUD(sp, h = "href")
  # Convert kernels to SpatialPolygons
  sp.kde <- adehabitatHR::getverticeshr(sp.kernels, percent = perc, unout = units)
  # Convert to geographic coordinates
  spgeo <- sp::spTransform(sp, CRS("+proj=longlat"))
  kdegeo <- sp::spTransform(sp.kde, CRS("+proj=longlat"))
  # Generate basemap using bounding box from spgeo
  basemap <- ggmap::get_stamenmap(bbox = c(
    left = min(spgeo@coords[,1])-0.005,
    bottom = min(spgeo@coords[,2])-0.005,
    right = max(spgeo@coords[,1])+0.005,
    top = max(spgeo@coords[,2])+0.005),
    zoom = 12)
  # Turn the spatial data frame of points into a regular dataframe for plotting
  spgeo.df <- data.frame(spgeo@coords, id = spgeo@data$id)
  # Map using {ggmap}
  map.kde <- ggmap::ggmap(basemap) + 
    # "fortify" polygon layer to add geometry to the dataframe
    ggplot2::geom_polygon(data = ggplot2::fortify(kdegeo), ggplot2::aes(long, lat, colour = id, fill = id), alpha = 0.3) +
    ggplot2::geom_point(data = spgeo.df, ggplot2::aes(x = x, y = y, colour = id), alpha = 0.01, pch = 20)  + # set transparency using alpha and smaller point size use pch.
    labs(x = "Longitude", y = "Latitude")
  map.kde
  print(sp.kde)
}

#' @title fractal_plot
#' @description This function graphs a series of coordinates, and then conducts a fractal analysis.
#' @param d dataframe
#' @param latitude latitude 
#' @param longitude longitude 
#' @keywords 
#' @export
#' @examples
#' fractal_plot(d = data,latitude,longitude)

fractal_plot <- function(d,latitude,longitude) {
  plot1 <- ggplot2::ggplot(data = d,  ggplot2::aes(latitude,longitude)) + ggplot2::geom_point()
  fractalplot <- fractaldim::fd.estim.dctII(cbind(latitude,longitude),plot.loglog=TRUE, plot.allpoints=TRUE, nlags="auto")
  return(list(plot1, fractalplot))
}
