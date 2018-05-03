
# for every bug make a unit test

#Initialization parameters

#it would be better if patches were coordinates pulled from a space - and then summed to get n_patches
n_patches <- 10
n_bands <- 10
resources <- 10

#Fitness calculation parameteres
payoff_default <- 10
cooperative_benefit <- 0.5
sigma <- 1


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


#need to add bands ID to patches

#take some time to look over this
for (i in 1:n_patches){
  temp <- which(bands$patch_id == i)
  if (length(temp) == 0) {
    patches$bands_id[[i]] <- NA
  } else {
    patches$bands_id[[i]] <- bands$band_id[temp]
  }
}


#fitness calculation

#occupied index so that calculations are not done on irrelevant patches
index_occupied <- which(!is.na(patches$bands_id))

#groupsize calculation 
#do I need to track groupsize separate from the fitness calculation? because otherwise I can just put this into the payoff calculation
for (i in 1: length(index_occupied)){
    bands$groupsize[index_occupied[i]] <- length(patches$bands_id[[index_occupied[i]]])

      #payoff calculation
      bands$payoff[index_occupied[i]] <- rnorm( 1 , mean = payoff_default + (bands$groupsize[index_occupied[i]] - 1)^cooperative_benefit, sigma)
      #the 1 in rnorm is number of observations
      
          #fitness calculation 
          #directly calculating group foraging payoff and fitness 
          if((bands$payoff[index_occupied[i]]*bands$groupsize[index_occupied[i]]) > patches$resources[bands$patch_id[index_occupied[i]]]){
            bands$fitness[index_occupied[i]] <- patches$resources[bands$patch_id[index_occupied[i]]]/bands$groupsize[index_occupied[i]] 
          } else {
            bands$fitness[index_occupied[i]] <- (bands$payoff[index_occupied[i]]*bands$groupsize[index_occupied[i]])/bands$groupsize[index_occupied[i]]  
          }
  
}

#check things are working fine - because groupsize and patch id don't line up.

#birth and death process








