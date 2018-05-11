
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

#Birth and Death parameters
rep_rate <- 0.5
death_par_1 <- 0.8
death_par_2 <- 5


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


###FITNESS###


# calculate groupsizes for each agent
for(i in 1:length(bands$band_id)){
  
bands$group_size[i] <- length(patches$bands_id[[bands$patch_id[i]]])
  
}
  
#calculate each bands payoff  
for (i in 1: length(bands$band_id)){
    bands$payoff[i] <- rnorm(1, mean = payoff_default + (bands$group_size[i] -1)^cooperative_benefit, sigma)
    #the 1 in rnorm is number of observations
}

#FIX ME - below, removed the index_occupied because then payoff element would not match length of others - better way?
#index_occupied <- which(!is.na(patches$bands_id))
for (i in 1: n_patches){
      temp_2 <- unlist(patches$bands_id[i])
      patches$payoff[i] <- sum(bands$payoff[temp_2])
  
}

#calculate fitness based on density dependence
for (i in 1: length(bands$band_id)){
      if(patches$payoff[bands$patch_id[i]] >= patches$resources[bands$patch_id[i]]){
        bands$fitness[i] <- patches$resources[bands$patch_id[i]]/bands$group_size[i]
      } else {
        bands$fitness[i] <- patches$payoff[bands$patch_id[i]]/bands$group_size[i]
      }
}



###BIRTH AND DEATH PROCESS###

for (i in 1:length(bands$band_id)){

      #Birth
        
      #Get probability of birth for each band  
      #FIX ME - for now, storing birth/death prob in bands, but probably don't need to store it (make temp vector)  
      bands$birth_prob[i] <- rep_rate*(bands$fitness[i]/bands$payoff[i])
      
      #Get birth/not birth of each band
      #FIX ME - same here, doesn't need to be stored, just temp vector
      bands$birth[i] <- rbinom(1, size = 1, prob = bands$birth_prob[i])
      
      #Death
      
      #Get probability of death for each band
      #FIX ME - doesn't need to be stored
      bands$death_prob[i] <- 1/(1+exp(1)^((death_par_1*bands$fitness[i])-death_par_2))

      #Get death/not death of each band
      #FIX ME - doesn't need to be stored
      bands$death[i] <- rbinom(1, size = 1, prob = bands$death_prob[i])
      
}

#FIX ME - think about how to do it in one go, instead of adding births and then subtracting deaths
#this is actually quite a big problem because when they are already added, they will not be removed correctly by the deaths
#but by doing the below loop, we as if always kill off new borns - so if we do not clone them then it matters - DISCUSS

#An attempt at above
for (i in 1: length(bands$band_id)){
  #if both of those are true
  if((as.logical(bands$birth[i]))&(as.logical(bands$death[i])) == TRUE){
    #then they both have to become 0 i.e. they cancel each other out
    bands$birth[i] <- 0
    bands$death[i] <- 0
    #otherwise both stay the same 
  } else {
    bands$birth[i] <- bands$birth[i]
    bands$death[i] <- bands$death[i]
  }
  
}

#After the above for loop, I should only have unique birth and death events (not those that cancel each other out)


#Remove dead bands
#FIX ME - doesn't work because index repeats once its length finishes - fixed for now by putting death process first
bands$band_id[as.logical(bands$death)] <- NA 
bands$payoff[as.logical(bands$death)] <- NA 
bands$fitness[as.logical(bands$death)] <- NA 
bands$patch_id[as.logical(bands$death)] <- NA 
bands$group_size[as.logical(bands$death)] <- NA


#Clone band_id, payoff, fitness, patch_id
#FIX ME - for now, new bands are clones of old ones, but need to think about whether this works
bands$band_id <- append(bands$band_id, bands$band_id[as.logical(bands$birth)], after = length(bands$band_id)) 
bands$payoff <- append(bands$payoff, bands$payoff[as.logical(bands$birth)], after = length(bands$band_id))
bands$fitness <- append(bands$fitness, bands$fitness[as.logical(bands$birth)], after = length(bands$band_id))
bands$patch_id <- append(bands$patch_id, bands$patch_id[as.logical(bands$birth)], after = length(bands$band_id))



#NOTE - group size is not updated as of yet

#TO DO
#discuss what to do about cloning, and therefore whether to have loop getting unique birth/death
#recalculate group size after birth/death process
#rename groupsize group_size for consistency
#discuss and code not storing birth and death probabilities and vectors








