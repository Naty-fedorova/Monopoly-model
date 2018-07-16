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
space_range <- 1
decision_prob <- 0.5
threshold <- 3
prop_sample <- 0.5


#patches
world <- matrix(1: patch_dim^2, nrow = patch_dim, ncol = patch_dim, byrow = TRUE)

#pad the matrix 
world_2 <- rbind(world, world[1:space_range, ])
world_3 <- rbind(world[(patch_dim-(space_range-1)):patch_dim, ], world_2)
world_6 <- cbind(world_3, world_3[, 1:space_range])
world_padded <- cbind(world_3[ , (patch_dim-(space_range-1)):patch_dim], world_6)

#create list of id's and their neighbours (for local condition)
#FIX ME - should I also have a list of neighbours for global condition?

patch_neighbours <- list()

#FIX ME - I probably don't actually need this patch_id thing
patch_neighbours$patch_id <- 1:n_patches
patch_neighbours$neighbours <- list()

#FIX ME - need to put in flexible coordinates in loop specification so it responds to changes to space_range - only does immediately vicinity as is
for(i in (space_range+1):(patch_dim+space_range)){
  for(j in (space_range+1):(patch_dim+space_range)){
    #this find neighbours in a radius of one patch (wider radius would require a change in padding too)
    patch_neighbours$neighbours[[ world_padded[i, j] ]] <- c(  world_padded[(i-space_range), (j-space_range)] ,
                                                               world_padded[(i), (j-space_range)] ,
                                                               world_padded[(i+space_range), (j-space_range)] ,
                                                               world_padded[(i-space_range), (j)] ,
                                                               world_padded[(i+space_range), (j)] ,
                                                               world_padded[(i-space_range), (j+space_range)] ,
                                                               world_padded[(i), (j+space_range)] ,
                                                               world_padded[(i+space_range), (j+space_range)])
  }
}



#patch list
#FIX ME - should patch id be based on coordinates or directly related to world? 
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
      
      for(i in 1:length(bands$bands_id)){
        
        #find a model
        
        
        #compare to model
        
        #Decision tree
        #FIX ME - at the moment I'm not nesting the if statemements, but would be good to discuss pros/cons of that
        
        #Do nothing conditions
        if(bands$group_size[i]=1 & model_groupsize=NULL){
          bands$patch_id[i] <- bands$patch_id
        }
        if(bands$group_size[i]=1 & model_groupsize=1 & bands$fitness[i]>=model_fitness){
          bands$patch_id[i] <- bands$patch_id
        }
        if(bands$group_size[i]>1 & model_groupsize>1 & bands$fitness[i]>=model_fitness & bands$fitness[i]>=payoff_default-threshold){
          bands$patch_id[i] <- bands$patch_id
        }
        
        #Fission conditions (move to empty patch)
        if(bands$group_size[i]>1 & model_groupsize>1 & bands$fitness[1]<=payoff_default-threshold & model_fitness<=payoff_default-threshold){
          #move to empty patch in neighbourhood
          which(neighbour groupsize == 0)
          bands$patch_id[i] <- randomly choose from (patch_id[above index])
        } else{
          bands$patch_id[i] <- bands$patch_id
        }
        if(bands$group_size[i]>1 & model_groupsize=1 & bands$fitness[i]<=model_fitness-threshold & bands$fitness[i]<=payoff_default-threshold){
          which(neighbour groupsize == 0)
          bands$patch_id[i] <- randomly choose from (patch_id[above index])
        } else{
          bands$patch_id[i] <- bands$patch_id
        }
        if(bands$group_size[i]>1 & model_groupsize=NULL & bands$fitness[i]<=payoff_default-threshold){
          which(neighbour groupsize == 0)
          bands$patch_id[i] <- randomly choose from (patch_id[above index])
        } else{
          bands$patch_id[i] <- bands$patch_id
        }
        
        #Migration conditions (join another group)
        if((bands$group_size[i]>1 & model_groupsize>1 & model_fitness>payoff_default-threshold) & bands$fitness[i] <= payoff_default-threshold | bands$fitness[i]<=model_fitness-threshold){
          bands$patch_id[i] <- model_patch_id
        } else{
          bands$patch_id[i] <- bands$patch_id
        }
        
        #Fusion conditions (join group after being alone)
        if(bands$group_size[i]=1 & model_groupsize>1 & bands$fitness[i]<= model_fitness-threshold{
          bands$patch_id[i] <- model_patch_id
        } else{
          bands$patch_id[i] <- bands$patch_id
        }
        
        #Fussion conditions (group formation)
        if(bands$group_size[i]=1 & model_groupsize=1 & bands$fitness[i]<payoff_default & model_fitness<payoff_default){
          #find empty path
          #both models move there
          #FIX ME - this one is a bit tricky because this is considered a move by both agents, and thus the model agents no longer gets a turn this round
          bands$patch_id[i] <- #new patch
          model_patch <- #new patch
        }
          
      
      #Store temp loop output
      loop_results[[j]] <- patches$bands_id 
#}      
      
      

      
      
      

#TO DO
#finish test file
#make list with all fission-fusion options
#code if statements for fission fusion options  
#keep both local and global options in mind










