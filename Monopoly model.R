
# for every bug make a unit test

#Initialization parameters

#it would be better if patches were coordinates pulled from a space - and then summed to get n_patches
n_patches <- 10

n_bands <- 10

resources <- 10



#patch list

#might actually want to have patch ID based on actual coordinates

patches <- list()

patches$patch_ID <- 1:n_patches
patches$resources <- rep(resources, n_patches)


#create number of bands and band list

bands <- list()

bands$ID <- 1:n_bands
bands$payoff <- rep(0, n_bands)
bands$fitness <- rep(0, n_bands)
bands$patch_id <- sample(n_patches, n_bands, replace = TRUE) 
str(bands)

#need to add bands ID to patches





