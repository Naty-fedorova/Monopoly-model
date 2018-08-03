#Groupsize function

calc_groupsize <- function(bands, patches){
  
  #this is a function (type: method) which calculate groupsizes for each agent by indexing band id's in patches by patch ids in bands and getting the length
  
  #arguments: band list, patch list
  
  #return - nada, it should just create and write the bands$groupsize sublist
  
  for(i in 1:length(bands$band_id)){
    bands$group_size[i] <- length(patches$bands_id[[bands$patch_id[i]]])
  }
}

