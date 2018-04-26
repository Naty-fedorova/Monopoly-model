

#create number of patches and patch list

n_patches <- 1

patch_ID <- 1:n_patches

patches <- list(patch_ID = patch_ID)



#create number of bands and band list
n_bands <- 10

bands <- list()

bands$ID <- 1:n_bands
bands$payoff <- rep(0, n_bands)
bands$fitness <- rep(0, n_bands)
bands$patch_id <- #random sample from the possible patches 
str(bands)




# for every bug make a unit test

