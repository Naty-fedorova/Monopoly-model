

This script has been written to test the integrity of the code so far. The loop has been removed. 


#Initialization parameters

#timesteps
timesteps <- 10

#it would be better if patches were coordinates pulled from a space - and then summed to get n_patches
n_patches <- 10
n_bands_ini <- 10
resources <- 200

#Fitness calculation parameteres
payoff_default <- 10
cooperative_benefit <- 0.5
sigma <- 1

#Birth and Death parameters
rep_rate <- 0.05
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

bands$band_id <- 1:n_bands_ini
bands$payoff <- rep(0, n_bands_ini)
bands$fitness <- rep(0, n_bands_ini)
bands$patch_id <- sample(n_patches, n_bands_ini, replace = TRUE) 

#Initialize matrix to hold loop results
loop_results <- list()





#Loop over timesteps
for (j in 1:timesteps){
  
  #(Re)calculate n_bands so that it adapts to the increasing/decreasing pop. size
  n_bands <- length(bands$band_id)
  
  
  #Add bands ID to patches list
  for (i in 1:n_patches){
    #Temporary vector that stores index of which patch_id is the same as i
    temp <- which(bands$patch_id == i)
    if (length(temp) == 0) {
      patches$bands_id[[i]] <- NA
    } else {
      #If the temp vector actually includes something, it is used to index band_id and put it into the patches list
      patches$bands_id[[i]] <- bands$band_id[temp]
    }
  }
  
  
  ###FITNESS###
  
  #Groupsize and payoff calculation 
  for(i in 1:length(bands$band_id)){
    # calculate groupsizes for each agent by indexing band id's in patches by patch ids in bands and getting the length of that (since its a vector, kinda)  
    bands$group_size[i] <- length(patches$bands_id[[bands$patch_id[i]]])
    #calculate each bands payoff using crema's equation and sampling from a normal distribution, the 1 in rnorm is number of observations
    bands$payoff[i] <- rnorm(1, mean = payoff_default + (bands$group_size[i] -1)^cooperative_benefit, sigma)
  }
  
  # tests
  # groupsizes should be higher when density is higher (fewer patches, more bands)
  # payoff should be higher when groupsize is higher, default payoff is higher, and when cooperative benefit is higher - deviation should be the same across conditions
  # groupsize of 1 should always only receive default payoff(with deviation)
  # So, in a sparce population, there should be more default payoffs than in a dense one
  # since there is no density regulation at this stage, higher groupsizes should indefinitely lead to higher payoffs 
  # when population is sparce (groupssizes = 1) or groupsizes are the same payoffs should be normally distributed
  
  #plot distribution of payoffs
  
  
  #Calculate patch payoff by summing payoff of bands in that patch 
  for (i in 1: n_patches){
    if(length(patches$bands_id) > 0){
      #If a patch has some bands, we store the index of those bands in a temp vector 
      temp_2 <- match(unlist(patches$bands_id[i]), bands$band_id)
      #Use the temp vector to index payoff in bands and get the sum of all the payoffs of the bands in that patch
      patches$payoff[i] <- sum(bands$payoff[temp_2]) 
    } else {
      # if that patch is not occupied, NA for payoff
      patches$payoff[i] <- NA
    }
  }
  
  # tests
  # basic check that payoff is a multiple of groupsize
  
  
  #Calculate fitness based on density dependence
  for (i in 1: length(bands$band_id)){
    #If patch payoff is more than patch resources, band fitness is resources/groupsize
    if(patches$payoff[bands$patch_id[i]] >= patches$resources[bands$patch_id[i]]){
      bands$fitness[i] <- patches$resources[bands$patch_id[i]]/bands$group_size[i]
    } else {
      #Otherwise, band fitness is patch payoff/groupsize
      bands$fitness[i] <- patches$payoff[bands$patch_id[i]]/bands$group_size[i]
    } 
    
  # tests
  # when resources are low, and population dense (few patches, many bands), fitness should be low 
  # when resources are lower than default payoff, all bands should have resources/groupsize
  # when resources are very high, all bands should have payoff/groupsize
  
    
    ###BIRTH AND DEATH PROCESS###
    #FIX ME - for now, storing birth/death prob in bands, but probably don't need to store it (make temp vector) 
    
    #Birth
    
    #Get probability of birth for each band, crema's function  
    bands$birth_prob[i] <- rep_rate*(bands$fitness[i]/bands$payoff[i])
    #Get birth/not birth of each band, bernoulli draw
    bands$birth[i] <- rbinom(1, size = 1, prob = bands$birth_prob[i])
    
    #Death
    #Get probability of death for each band, crema's function
    bands$death_prob[i] <- 1/(1+exp(1)^((death_par_1*bands$fitness[i])-death_par_2))
    #Get death/not death of each band, bernoulli draw
    bands$death[i] <- rbinom(1, size = 1, prob = bands$death_prob[i])
  }
  
  # tests
  # proportion of events should reflect probability of event, i.e. prob of 0.5 should result in half dead half alive  
  
  #Index of which bands died
  death_index <- which(bands$death == 1)
  
  #Remove dead from bands and birth index, if death index is more than 0, using c-() notation and death index
  if(length(death_index) > 0){
    bands$band_id <- bands$band_id[-(death_index)]  
    bands$payoff <- bands$payoff[-(death_index)]
    bands$fitness <- bands$fitness[-(death_index)]  
    bands$patch_id <- bands$patch_id[-(death_index)]
    bands$group_size <- bands$group_size[-(death_index)]
    #FIX ME - _prob won't be necessary once it isn't being stored, just now so it doesn't look confusing 
    bands$birth_prob <- bands$birth_prob[-(death_index)]
    bands$birth <- bands$birth[-(death_index)]
    bands$death_prob <- bands$death_prob[-(death_index)]
    bands$death <- bands$death[-(death_index)]
  } 
  
  # tests
  # check correct ids are being removed 
  
  #Index of bands that are reproducing
  birth_index <- which(bands$birth == 1)
  
  #Generate new id's, n = length of birth_index, i.e. n of new bands, using max id for this
  if(length(birth_index) > 0){
    #Repetitions where n=the lenght of the birth index, and the ids go from the max id +1 to the max id + length of birth index
    new_ids <- rep((max(bands$band_id)+1):(max(bands$band_id)+length(birth_index)))
  } else {
    new_ids <- NULL
  }
  
  # tests
  # check correct ids are being generated
  
  #Appends new ids to band_id, and clone value of reproduced bands for payoff, fitness, and patch_id
  bands$band_id <- append(bands$band_id, new_ids, after = length(bands$band_id)) 
  bands$payoff <- append(bands$payoff, bands$payoff[birth_index])
  bands$fitness <- append(bands$fitness, bands$fitness[birth_index])
  bands$patch_id <- append(bands$patch_id, bands$patch_id[birth_index])
  
  #FIX ME - need to first update patches list with band ID and recalculate groupsizes, before model goes on to fission fusion
  
  #Store temp loop output
  loop_results[[j]] <- patches$bands_id 
}      

#TO DO
#unit tests on each section 










