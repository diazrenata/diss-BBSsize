library(dplyr)

first <- read.csv(here::here("dat_to_include", "species_list_with_masses.csv"))
second <- read.csv(here::here("dat_to_include", "species_list_with_masses2ndentry.csv"))


second <- second %>% arrange(species_id)
first <- first %>% arrange(species_id)
for(i in 1:nrow(first)) {

  if(second$species_id[i] != first$species_id[i]) {
    print(i)
    break()
  }

}


second <- second %>% arrange(species_id, mass)
first <- first %>% arrange(species_id, mass)
for(i in 1:nrow(first)) {

  if(is.na(first$mass[i])) {
    if(is.na(second$mass[i])) {
      next()
    } else {
      print(i)
      break()
    }
  }

  if(second$mass[i] != first$mass[i]) {
    print(i)
    break()
  }

}



second <- second %>% arrange(species_id, mass, location)
first <- first %>% arrange(species_id, mass, location)
for(i in 1:nrow(first)) {

  if(is.na(first$sd[i])) {
    if(is.na(second$sd[i])) {
      next()
    } else {
      print(i)
      break()
    }
  }

  if(second$sd[i] != first$sd[i]) {
    print(i)
    break()
  }

}


for(i in 1:nrow(first)) {

  if(!(all_equal(first[i,],second[i,]))) {

    print(i)
    break()
  }

}

first[i,]
second[i,]
which(first[i,] != second[i,])

all_equal(first, second)
