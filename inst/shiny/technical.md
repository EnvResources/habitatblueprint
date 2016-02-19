## Technical Details

### Computing Estuary Volume

Estuary volume is computed using the 2009 bathymetry raster developed from the
Lower Russian River Bathymetric Analysis (Environmental Data Solutions, 2009).
Following manual delineation of the estuary thalweg using ArcMap, the estuary
was divided into 100-meter segments. Estuary volume was computed in 10-cm depth
slices for water surface elevations ranging from -15.8 meters to +2.6 meters
NGVD29. Volume was computed by counting the number of cells inundated in each
segment for each depth interval. These counts are then multiplied by the
bathymetry raster resolution to obtain water volumes within each depth slice.
These volume calculations are then sub-divided into the depth categories
developed by the Water Quality Parameter and Habitat Productivity Technical
Group to produce hypsometric curves for each segment that describe how total
estuary volume, and the volume of each depth category, changes with water
surface elevation.

### Interpolating Water Quality Data

Water quality data was interpolated using Natural Neighbor Interpolation with 
Matlab.
