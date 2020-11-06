# Data Notes

Tabular geospatial data in files `samples_spatial.csv` and `sites_spatial.csv`
were produced in the R Markdown notebook, `SWAT_data_spatial.RMD`.

## GIS Data

###  Point Locations of Sites and Samples
Geospatial data in the geodatabase "spatial_data"  was created in ArcGIS, as follows:
See R notebook "

1. load  CSV files as tables
2.  Create Event Layer
3.  Export data as feature to geodatabase

#  Near Impervious Cover Estimates
Nearby impervious cover estimates (calculated for sample locations) were
based on Maine IF&W one meter pixel impervious cover data, which is based
largely on data from 2007.  CBEP has a version of this impervious cover data for
the Casco Bay watershed towns in our GIS data archives. Analysis followed the
following steps (Some of these steps only speed up analysis). 

1. Town by town IC data in a Data Catalog were assembled into a large `tif` 
   file using the "Mosaic Raster Catalog"  item from the context menu from the
   ArcGIS table of contents.

2. We created a polygon that enclosed all of the Casco Bay sample locations and
   a 1000 meter buffer around each.  Because our version of the Impervious Cover
   layer is limited to Casco Bay Watershed Towns, we can not develop impervious
   cover statistics for sites outside the watershed towns. 

3. We used "Extract by Mask" to extract a smaller version of the impervious
   cover data for just this region.

4. We used "Aggregate" to reduce the resolution of the impervious cover raster
   to a 5 meter resolution, summing the total impervious cover within the
   5m x 5 m area, generating a raster with values from zero to 25. 

5. We used "Focal Statistics" to generate rasters that show the cumulative area
   of impervious cover (in meters) within 500 m. 

6. Finally, we extracted the values of the three rasters produced in step 4 at
   each of the sample locations.  We used  'Extract  Values to Points'.
   
7. We calculated a "percent imperviousness within 500m" by dividing the raster
   extracted value by the total area of a circe with radius 500m 
   $(\pi \times 500^2)$.
   
8. We exported the resulting attribute table in a (comma delimited) text file 
   `site_500m_imperious_area.txt`.
   
   
