
# for every bug make a unit test

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
loop_results$band_id <- list()




#Loop over timesteps
for (j in 1:timesteps){

      #(Re)calculate n_bands so that it adapts to the increasing/decreasing pop. size
      n_bands <- length(bands$band_id)
      
        
      #Add bands ID to patches list
      for (i in 1:n_patches){
        temp <- which(bands$patch_id == i)
        if (length(temp) == 0) {
          patches$bands_id[[i]] <- NA
        } else {
          patches$bands_id[[i]] <- bands$band_id[temp]
        }
      }
      
      
      ###FITNESS###
      
      #Groupsize and payoff calculation 
      for(i in 1:length(bands$band_id)){
            
            # calculate groupsizes for each agent  
            bands$group_size[i] <- length(patches$bands_id[[bands$patch_id[i]]])
            
            #calculate each bands payoff 
            bands$payoff[i] <- rnorm(1, mean = payoff_default + (bands$group_size[i] -1)^cooperative_benefit, sigma)
            #the 1 in rnorm is number of observations
      }
        

      #Calculate patch payoff by summing payoff of bands in that patch 
      for (i in 1: n_patches){
          
            if(length(patches$bands_id) > 0){
            
            temp_2 <- match(unlist(patches$bands_id[i]), bands$band_id)
            
            patches$payoff[i] <- sum(bands$payoff[temp_2]) 
            
            #here i need what to do with occupied patches
            
            } else {
              # if that patch is not occupied, put in an na for payoff
              patches$payoff[i] <- NA
            }
      }
        

      #Calculate fitness based on density dependence
      for (i in 1: length(bands$band_id)){
            if(patches$payoff[bands$patch_id[i]] >= patches$resources[bands$patch_id[i]]){
              bands$fitness[i] <- patches$resources[bands$patch_id[i]]/bands$group_size[i]
            } else {
              bands$fitness[i] <- patches$payoff[bands$patch_id[i]]/bands$group_size[i]
            } 
              
                  ###BIRTH AND DEATH PROCESS###
                  #FIX ME - for now, storing birth/death prob in bands, but probably don't need to store it (make temp vector) 
                  
                  #Birth
                  
                  #Get probability of birth for each band  
                  bands$birth_prob[i] <- rep_rate*(bands$fitness[i]/bands$payoff[i])
                  
                  #Get birth/not birth of each band
                  bands$birth[i] <- rbinom(1, size = 1, prob = bands$birth_prob[i])
                  
                  #Death
                  
                  #Get probability of death for each band
                  bands$death_prob[i] <- 1/(1+exp(1)^((death_par_1*bands$fitness[i])-death_par_2))
                  
                  #Get death/not death of each band
                  bands$death[i] <- rbinom(1, size = 1, prob = bands$death_prob[i])
              
              
      }
      
      #Index of which bands died
      death_index <- which(bands$death == 1)
      
      
      #Remove dead from bands and birth index, if death index is more than 0
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
      
      
      #Index of bands that are reproducing
      birth_index <- which(bands$birth == 1)
      
      #Generate new id's, n = length of birth_index, i.e. n of new bands, using max id for this
      if(length(birth_index) > 0){
        new_ids <- rep((max(bands$band_id)+1):(max(bands$band_id)+length(birth_index)))
      } else {
        new_ids <- NULL
      }
      
      
      #Appends new ids to band_id, and clone value of reproduced bands for payoff, fitness, and patch_id
      bands$band_id <- append(bands$band_id, new_ids, after = length(bands$band_id)) 
      bands$payoff <- append(bands$payoff, bands$payoff[birth_index])
      bands$fitness <- append(bands$fitness, bands$fitness[birth_index])
      bands$patch_id <- append(bands$patch_id, bands$patch_id[birth_index])
      
      #FIX ME - need to first update patches list with band ID and recalculate groupsizes, before model goes on to fission fusion
      
      #Store temp loop output
      loop_results$band_id[j] <- patches$bands_id 
}      

#TO DO
#look at death probabilities - too high, everything is dying too soon 
#maybe its the fact that deaths are killing the also reproducing agents - i.e. death process happens before birth 








