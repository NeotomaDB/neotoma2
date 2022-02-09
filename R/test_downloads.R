# Test that a downloads object with two collunits returns only one site

# Behaviour inherited from the API

# Reproduce error
# https://api.neotomadb.org/v2.0/data/downloads/14530,%2014531

# dw_sites <-get_downloads(14530, 14531)

# expected behaviour

# len(dw_sites) == 1