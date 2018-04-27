
# for every bug make a unit test

#Initialization parameters

#it would be better if patches were coordinates pulled from a space - and then summed to get n_patches
n_patches <- 10

n_bands <- 10

resources <- 10



#patch list

#might actually want to have patch ID based on actual coordinates

patches <- list()

patches$patch_id <- 1:n_patches
patches$resources <- rep(resources, n_patches)
patches$bands_id <- list()


#create number of bands and band list

bands <- list()

bands$band_id <- 1:n_bands
bands$payoff <- rep(0, n_bands)
bands$fitness <- rep(0, n_bands)
bands$patch_id <- sample(n_patches, n_bands, replace = TRUE) 
str(bands)

#need to add bands ID to patches
index <- list()



for (i in 1:n_patches){
    index[[i]] <- which(bands$patch_id == i)
    if (length(index[[i]]) == 0) {
      index[[i]] <- "NA"
    }
    patches$bands_id[[i]] <- bands$band_id[index[[i]]]
}





