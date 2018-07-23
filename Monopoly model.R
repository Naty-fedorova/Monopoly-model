#THE MONOPOLY MODEL

#Initialization parameters

#timesteps
timesteps <- 10

#it would be better if patches were coordinates pulled from a space - and then summed to get n_patches
patch_dim <- 4
n_patches <- patch_dim^2
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

#Fission fusion parameters
space_range <- 17
decision_prob <- 0.5
threshold <- 0
prop_sample <- 1


#patches
world <- matrix(1: patch_dim^2, nrow = patch_dim, ncol = patch_dim, byrow = TRUE)

#pad the matrix 
world_2 <- rbind(world, world[1:space_range, ])
world_3 <- rbind(world[(patch_dim-(space_range-1)):patch_dim, ], world_2)
world_6 <- cbind(world_3, world_3[, 1:space_range])
world_padded <- cbind(world_3[ , (patch_dim-(space_range-1)):patch_dim], world_6)

#set global condition
if(space_range < n_patches){
  
  #create list of id's and their neighbours (for local condition)
  patch_neighbours <- list()
  
  #FIX ME - I probably don't actually need this patch_id thing
  patch_neighbours$patch_id <- 1:n_patches
  
  #Initialize n_patches number of lists within neighbours
  patch_neighbours$neighbours <- vector("list", n_patches)
  
  #FIX ME - below works for different values of space range to get the outer neighbour set - but for small world's leads to duplicates that need to be removed
  for(i in (space_range+1):(patch_dim+space_range)){
    for(j in (space_range+1):(patch_dim+space_range)){
      for(x in -(space_range):space_range){
        for(y in -(space_range):space_range){
          if(x != 0 || y != 0){
            patch_neighbours$neighbours[[ world_padded[i, j] ]] <- c( patch_neighbours$neighbours[[ world_padded[i, j] ]] , world_padded[i+x, j+y])
          }
        }
      }
    }
  }
}

#Patch list
#FIX ME - should patch id be based on coordinates or directly related to world? 
patches <- list()

patches$patch_id <- 1:n_patches
patches$resources <- rep(resources, n_patches)
patches$bands_id <- list()


#Create number of bands and band list
bands <- list()

bands$band_id <- 1:n_bands_ini
bands$payoff <- rep(0, n_bands_ini)
bands$fitness <- rep(0, n_bands_ini)
bands$patch_id <- sample(n_patches, n_bands_ini, replace = TRUE) 

#Initialize matrix to hold loop results
loop_results <- list()


#############################################################################################################################################################MODEL


#Loop over timesteps
#for (j in 1:timesteps){

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
        

      #Calculate patch payoff by summing payoff of bands in that patch 
      for (i in 1: n_patches){
            if(length(patches$bands_id) > 0){
            #If a patch has some bands, we store the index of those bands in a temp vector 
            temp_2 <- match(unlist(patches$bands_id[i]), bands$band_id)
            #Use the temp vector to index payoff in bands and get the sum of all the payoffs of the bands in that patch
            patches$payoff[i] <- sum(bands$payoff[temp_2]) 
            } else {
              # if that patch is not occupied, put in an na for payoff
              patches$payoff[i] <- NA
            }
      }
        
      #Calculate fitness based on density dependence
      for (i in 1: length(bands$band_id)){
            #If patch payoff is more than patch resources, band fitness is resources/groupsize
            if(patches$payoff[bands$patch_id[i]] >= patches$resources[bands$patch_id[i]]){
              bands$fitness[i] <- patches$resources[bands$patch_id[i]]/bands$group_size[i]
            } else {
              #Otherwise, band fitness is patch payoff/groupsize
              bands$fitness[i] <- patches$payoff[bands$patch_id[i]]/bands$group_size[i]
            } 
              
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
      
      #Index of bands that are reproducing
      birth_index <- which(bands$birth == 1)
      
      #Generate new id's, n = length of birth_index, i.e. n of new bands, using max id for this
      if(length(birth_index) > 0){
        #Repetitions where n=the lenght of the birth index, and the ids go from the max id +1 to the max id + length of birth index
        new_ids <- rep((max(bands$band_id)+1):(max(bands$band_id)+length(birth_index)))
      } else {
        new_ids <- NULL
      }
      
      #Appends new ids to band_id, and clone value of reproduced bands for payoff, fitness, and patch_id
      bands$band_id <- append(bands$band_id, new_ids, after = length(bands$band_id)) 
      bands$payoff <- append(bands$payoff, bands$payoff[birth_index])
      bands$fitness <- append(bands$fitness, bands$fitness[birth_index])
      bands$patch_id <- append(bands$patch_id, bands$patch_id[birth_index])
      
      #Update band ids in patches list and update groupsize 
      
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
      
      #Groupsize and payoff calculation 
      for(i in 1:length(bands$band_id)){
        # calculate groupsizes for each agent by indexing band id's in patches by patch ids in bands and getting the length of that (since its a vector, kinda)  
        bands$group_size[i] <- length(patches$bands_id[[bands$patch_id[i]]])
      }
      
      ###FISSION FUSION###
      
       
      
      for(i in 1:length(bands$band_id)){
        
        #Fission fusion happens with probability decision_prob for each agent
        if((rbinom(1,1, decision_prob))==1){
          
          #Get index of the neighbours of agent i
          neigh_ind <- patch_neighbours$neighbours[[bands$patch_id[i]]]
          
          #Get index of bands that are present in those neighboring patches
          #Don't forget that set below is filled with INDEXES, not agent IDs
          set <- match(neigh_ind, bands$patch_id)
          set_pos <- set[!is.na(set)]
          
          #Select subset prop_sample from that
          #FIX ME - what do I do with odd numbers here i.e. when the proportion is not a whole number? 
          sample_model_ind <- sample(set_pos, size = (prop_sample * length(set_pos)))
          
          #fittest agent
          model <- sample_model_ind[which.max(bands$fitness[sample_model_ind])]
          
          
          
          #Decision tree
          #FIX ME - at the moment I'm not nesting the if statemements, but would be good to discuss pros/cons of that
          #seems like nesting will be required so that each agent doesn't have to go through all the conditions before moving 
          
          #FIX ME - after a few iterations the loop decision tree breaks with an error
          
          #Do nothing conditions
          #FIX ME - don't understand this null condition
          if(bands$group_size[i]==1 & is.null(bands$group_size[model])==TRUE){
            bands$patch_id[i] <- bands$patch_id[model]
          }
          if(bands$group_size[i]==1 & bands$group_size[model]==1 & bands$fitness[i]>=bands$fitness[model]){
            bands$patch_id[i] <- bands$patch_id[model]
          }
          if(bands$group_size[i]>1 & bands$group_size[model]>1 & bands$fitness[i]>=bands$fitness[model] & bands$fitness[i]>=payoff_default-threshold){
            bands$patch_id[i] <- bands$patch_id[model]
          }
          
          #Fission conditions (move to empty patch)
          if(bands$group_size[i]>1 & bands$group_size[model]>1 & bands$fitness[1]<=payoff_default-threshold & bands$fitness[model]<=payoff_default-threshold){
            #move to empty patch in neighbourhood
            #FIX ME - how I do this depends on what is done above with the neighbour index
            
            #find empty patches and randomly select one to move to 
            bands$patch_id[i] <- sample(neigh_ind[which(is.na(set))], 1)
            
          } 
          
          if(bands$group_size[i]>1 & bands$group_size[model]==1 & bands$fitness[i]<=bands$fitness[model]-threshold & bands$fitness[i]<=payoff_default-threshold){
            #find empty patches and randomly select one to move to 
            bands$patch_id[i] <- sample(neigh_ind[which(is.na(set))], 1)
          } 
          
          
          if(bands$group_size[i]>1 & is.null(bands$group_size[model])==TRUE & bands$fitness[i]<=payoff_default-threshold){
            #find empty patches and randomly select one to move to 
            bands$patch_id[i] <- sample(neigh_ind[which(is.na(set))], 1)
          } 
          
          #Migration conditions (join model's group)
          if((bands$group_size[i]>1 & bands$group_size[model]>1 & bands$fitness[model]>payoff_default-threshold) & bands$fitness[i] <= payoff_default-threshold | bands$fitness[i]<=bands$fitness[model]-threshold){
            bands$patch_id[i] <- bands$patch_id[model]
          } 
          
          #Fusion conditions (join group after being alone)
          if(bands$group_size[i]==1 & bands$group_size[model]>1 & bands$fitness[i]<= bands$fitness[model]-threshold){
            bands$patch_id[i] <- bands$patch_id[model]
          } 
          
          #Fussion conditions (group formation)
          #FIX ME - need to figure out how to then count this as a move for the model as well
          if(bands$group_size[i]==1 & bands$group_size[model]==1 & bands$fitness[i]<payoff_default & bands$fitness[model]<payoff_default){
            
            #find empty patches and randomly select one, assign to new variable new_patch
            new_patch <- sample(neigh_ind[which(is.na(set))], 1)
            
            #assign new patch as the patch for both the agent i and the model
            bands$patch_id[i] <- new_patch
            bands$patch_id[model] <- new_patch
          }
        }
      }
      
      
      #Store temp loop output
      loop_results[[j]] <- patches$bands_id 
#}      
      
      

      
      
      

#TO DO
#finish test file
#keep both local and global options in mind
#need to log each movement in the for loop going through the agents to see what is actually happening 
#write fission fusion conditions out nested?       










