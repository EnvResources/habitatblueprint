#' Habitat Blueprint Browser
#'
#' This packages contains the Habitat Blueprint Browser shiny app. Data used 
#' by the app was generated using the \code{rremat} package.
#'
#' @name habitatblueprint-package
#' @docType package
#'
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom tidyr gather_
#' @importFrom tidyr spread_
#' @importFrom RColorBrewer brewer.pal
#' @import ggplot2
#' @importFrom shiny runApp
NULL

#' CTD Transect Meta Data
#'
#' Meta data for CTD transects. Structured as:
#' \itemize{
#'   \item \code{start} timestamp of transect commencement
#'   \item \code{end} timestamp of transect completion
#'   \item \code{note} additional note on e.g. mouth state
#' }
#' @details Transect meta data was collected from the Russian River Estuary 
#'   Circulation and Water Quality Data Reports (2011, 2012, 2013, 2014) 
#'   submitted to the Sonoma County Water Agency.
#' @docType data
#' @keywords datasets
#' @name ctdmeta
#' @usage data(ctdmeta)
#' @format A data frame with 3 variables
NULL

#' Interpolated CTD Grids
#' 
#' Interpolated CTD grids constructed using natural neighbor interpolation in 
#' Matlab. Structured as:
#' \itemize{
#'   \item \code{sa} salinity (PSU)
#'   \item \code{ta} temperature (degrees C)
#'   \item \code{oa} dissolved oxygen (mg/L)        
#'   \item \code{dist} longitudinal distance from mouth (km)
#'   \item \code{elev} elevation (m NAVD29)
#'   \item \code{date} cast date
#' }
#' @docType data
#' @keywords datasets
#' @name grids
#' @usage data(grids)
#' @format A data frame with 6 variables
#' @seealso ctdmeta ctd
NULL

#' Habitat Data
#'
#' Habitat evaluation of interpolated CTD transects. Same structure and data
#' as \code{grids}, but with the following additional columns:
#'
#' @docType data
#' @keywords datasets
#' @name habgrids
#' @usage data(habgrids)
#' @format A data frame with 6 variables
#' @seealso grids
NULL

#' CTD Cast Data
#' 
#' CTD cast data collected by the Largier Lab and processed using 
#' \code{rremat}. Structured as:
#' \enumerate{
#'   \item \code{date} cast date
#'   \item \code{dist} longitudinal distance from mouth (km)
#'   \item \code{surfelev} water surface elevation (m NAVD29)
#'   \item \code{elev} elevation (m NAVD29)
#'   \item \code{depth} cast depth (m)
#'   \item \code{ta} temperature (degrees C)
#'   \item \code{sa} salinity (PSU)
#'   \item \code{da}       
#'   \item \code{oa} dissolved oxygen (mg/L)        
#'   \item \code{sat} dissolved oxygen percent saturation (%)
#'   \item \code{fl} fluorescence (ug/L))
#'   \item \code{bt} beam transmission (%)
#'   \item \code{par} photosynthetically-active radiation (umol^-1 m^-2)
#'   \item \code{ph} pH
#' }
#' @docType data
#' @keywords datasets
#' @name ctd
#' @usage data(ctd)
#' @format A data frame with 14 variables
#' @seealso ctdmeta grids habgrids
NULL

#' Water Level Data
#' 
#' Pressure gauge water Level data collected by the Largier Lab and processed 
#' using \code{rremat}. Structured as:
#' \itemize{
#'   \item \code{site} gauge location
#'   \item \code{mtime} timestamp (UTC)
#'   \item \code{depth} water depth (m)
#' }
#' @docType data
#' @keywords datasets
#' @name wll
#' @usage data(wll)
#' @format A data frame with 3 variables
NULL

#' River Flow Data
#' 
#' River flow data managed by USGS. Structured as:
#' \itemize{
#'   \item \code{site} gauge location. Austin Creek near Cazadero and Russian 
#'         River near Guerneville are included.
#'   \item \code{datetime} timestamp (PST/PDT)
#'   \item \code{flow} flow rate (cfs)
#' }
#' @docType data
#' @keywords datasets
#' @name inflows
#' @usage data(inflows)
#' @format A data frame with 3 variables
NULL

#' Tide Height Data
#' 
#' Tide height data managed by NOAA. Structured as:
#' \itemize{
#'   \item \code{datetime} timestamp (UTC)
#'   \item \code{height} Tide height above MLLW (m)
#'   \item \code{sigma}
#' }
#' @docType data
#' @keywords datasets
#' @name tides
#' @usage data(tides)
#' @format A data frame with 3 variables
NULL

#' Volume Lookup Table
#' 
#' Volume lookup table constructed from RRE bathymetry. Structured as:
#' \itemize{
#'   \item \code{dist} distance along the thalweg marking the edge of each zone
#'     furthest from the mouth, e.g. \code{dist = 100} identifies the region
#'     spanning 0 meters and 100 meters along the thalweg.
#'   \item \code{elev} elevation marking the the top of the depth increment, 
#'     i.e. \code{elev = 0.1} identifies the region spanning 0 and 0.1 meters 
#'     elevation.
#'   \item \code{count} the total number of cells contained within each zone 
#'     and vertical increment.
#' }
#' @details The volume lookup table was generated from the RRE bathymetry DEM 
#'   and zone delineations based on a manual trace of the river thaleweg. The 
#'   volume cells are defined as having a length of 1 meter, width of 1 meter, 
#'   and height of 0.1 meters (cell volume of 0.1 cubic meters). Zones are 
#'   delineated in 100-meter increments along the thalweg, and each zone is 
#'   divided in the vertical into 0.1-meter increments from a minimum elevation 
#'   of -15.8 meters to a maximum elevation of 2.6 meters. The lookup table 
#'   lists the count of volume elements in each vertical increment in each 
#'   zone, i.e. each row lists the number of elements between river distances 
#'   \code{d} and \code{d - 100} and between depths \code{z} and 
#'   \code{z - 0.1}. 
#' @docType data
#' @keywords datasets
#' @name volumes
#' @usage data(volumes)
#' @format A data frame with 3 variables
NULL

#' Estuary Closures
#' 
#' Estuary closures identified from water level and photographic data. 
#' Structured as:
#' \enumerate{
#'   \item \code{id} (arbitary) closure id
#'   \item \code{initiation} date of closure initiation
#'   \item \code{breach} date of mouth breach
#' }
#' @docType data
#' @keywords datasets
#' @name closures
#' @usage data(closures)
#' @format A data frame with 3 variables
NULL

#' Habitat Blueprint Browser
#'
#' Start the Habitat Blueprint Browser.
#'
#' @examples
#' \dontrun{
#'   HabitatBrowser()
#' }
#' @export
HabitatBrowser = function(){
  runApp(system.file("shiny/", package = "habitatblueprint", mustWork = TRUE))
}
