
#Adding band ids to patch list

bands_in_patches <- function(bands, patches, n_patches){
  
  #This is a function which takes band ids from the band list and matches them with the correct patch id indice in patches list
  
  #argument: band list, patch list, n_patches
  
  #return: patch_band list of lists
  
  #initialize list of lists
  
  ids_in_patches <- list()
  
  #Add bands ID to patches list
  for (i in 1:n_patches){
    #Temporary vector that stores index of which patch_id is the same as i
    temp <- which(bands$patch_id == i)
    if (length(temp) == 0) {
      ids_in_patches[[i]] <- NA
    } else {
      #If the temp vector actually includes something, it is used to index band_id and put it into the patches list
      ids_in_patches[[i]]<- bands$band_id[temp]
    }
  }
  
  return(ids_in_patches)
  
}


#Groupsize function

calc_groupsize <- function(bands, patches){
  
  #this is a function which calculate groupsizes for each agent by indexing band id's in patches by patch ids in bands and getting the length
  
  #arguments: band list, patch list
  
  #return - vector of groupsizes
  
  groupsizes <- rep(0, length(bands$band_id))
  
  for(i in 1:length(bands$band_id)){
    groupsizes[i]<- length(patches$bands_id[[bands$patch_id[i]]])
  }
  return(groupsizes)
}

