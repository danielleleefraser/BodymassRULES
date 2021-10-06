

IUCN_synonyms<-read.csv("IUCN_Synonyms_clean.csv")
  
trait_data<-read.csv("Elton_traits3.0.csv")

species_names_column<-4
# Now to match up the names that need changing with the accepted name

Fun_IUCN_synonyms<-function(trait_data,species_names_column,IUCN_synonyms){
  trait_names<-trait_data[,species_names_column]
  new_names<-c()
  for(i in 1:length(trait_names)){
    trait_match<-which(IUCN_synonyms$synonym==trait_names[i])
    if(length(trait_match)>1){
      print("it's happened!")
    }
    if(length(trait_match)>0){
      new_names[i]<-IUCN_synonyms$accepted_name[trait_match]
    }else{
      new_names[i]<-trait_names[i]
    }
  }
  return(new_names)
}

replacements<-Fun_IUCN_synonyms(trait_data,4,IUCN_synonyms)

trait_data_new<-cbind(trait_data,replacements)
write.csv(trait_data_new,"Elton traits 3.0 with name changes.csv")

###