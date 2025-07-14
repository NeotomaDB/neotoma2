# neotoma2 1.0.0

* Added a `NEWS.md` file to track changes to the package.


# neotoma2 1.0.3

## Remarks for Upgrade 1.0.3

Fixed errors in the API calls.

Fixed filter function for collection units and datasets. Changed \itemize for \describe

Updated plotLeaflet to not add provider tiles.

Removed mapview to avoid `sp` conflicts.

Updated README to reflect milestones with JOSS.

Updated maintainer.

# neotoma2 1.0.4

## Remarks for Upgrade 1.0.4

Fixed errors in the API calls.

Updated README.

# neotoma2 1.0.5

## Remarks for Upgrade 1.0.5

Fixed errors in the API calls. Added `tryCatch` statements in vignette
Changed some minor API calls to `eval=False`

# neotoma 1.0.6

Fixed RMarkdown so that when the API is not available, it will render a document with all eval=False and so, it will not fail.
Fixed passing `loc` so that a proper geojson is given to the API. At this point, the API cannot handle POST statements with complicated bodies. 

# neotoma 1.0.7
Started with parsing lists rather than sites objects. This helps handle better the site creation 
and speeds things up.
Removed temporarily functions such as specimens and speleothems to correct some potential bugs incoming from API parsing.
Added tests to get_downloads / datasets / sites
Increased the potential of POST statements to handle more complex queries from the package.
Will correct speleothems and specimens for Neotoma 1.0.8
