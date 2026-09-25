# Generate test data according to gaussian/poisson/binomial MLR
# Mainly used for debugging without reloading MASS package

gauss_df <- datasets::mtcars

pois_df <- MASS::ships[MASS::ships["service"] != 0, ] # Drop 0 values

bin_df <- datasets::mtcars

usethis::use_data(gauss_df, overwrite = TRUE)
usethis::use_data(pois_df, overwrite = TRUE)
usethis::use_data(bin_df, overwrite = TRUE)


