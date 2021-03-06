#Groupsize function

calc_groupsize <- function(bands, patches){
  
  #this is a function which calculate groupsizes for each agent by indexing band id's in patches by patch ids in bands and getting the length
  
  #arguments: band list, patch list
  
  #return - vector of groupsizes
  
  groupsizes <- rep(0, length(bands$patch_id))
  
  for(i in 1:length(bands$patch_id)){
    groupsizes[i]<- length(patches$bands_id[[bands$patch_id[i]]])
  }
  return(groupsizes)
}

#bands$groupsize <- calc_groupsize(bands = bands, patches = patches)