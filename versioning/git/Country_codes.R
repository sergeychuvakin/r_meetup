df <- read.csv('depression_by_country_WHO.csv')

# ## change 1
# 
# head(df)
# 
# ## change 2
# 
# df[['SpatialDim_full_name']] <- sapply(df[['SpatialDim']], function(x) {
#   countrycode::countrycode(x, origin = 'iso3c', destination = 'country.name')
# })