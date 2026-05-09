carsCSV <- read.csv("Car_details_v3.csv", header=TRUE)
#str(carsCSV) 
# la colonna torque contiene dati in un formato non standard.
# sono presenti i valori di coppia (in Newton*metro o in kg*metro),
# ciascuno accompagnato dal valore (o dal range) di giri/minuto (RPM) dai quali quella coppia e' disponibile.
#paste("Numero di valori in Nm:", length(grep('Nm', carsCSV$torque, ignore.case=TRUE)))
#paste("Numero di valori in kg:", length(grep('kgm', carsCSV$torque, ignore.case=TRUE)))

clean_torque <- function(carsCSV, cleanedCSV) {
  
  torque <- gsub(" ", "", carsCSV$torque, fixed = TRUE)
  torque <- gsub("at", "@", torque, fixed = TRUE)
  
  # in che modi e' espresso il dato?
  # types <- unique(gsub('[[:digit:]]', '', torque))
  # print(types)
  # tutti iniziano con un numero (intero o decimale) ed e' sempre la coppia.
  torque <- gsub('[[:alpha:]]', '', torque)
  
  # sostituisco la prima occorrenza di @ con °, in modo da eliminare poi la seconda perche' inutile.
  torque <- sub('@', '°', torque)
  
  # elimino le parentesi ed il loro contenuto usando REGEXPR
  torque <- gsub("\\s*\\([^\\)]+\\)","", torque)
  
  # elimino la seconda @ e le virgole
  torque <- gsub('[@,]', "", torque)
  
  # tolti i +/- (inutili ai fini della coppia: l'auto con coppia max tra 3500 e 4500 avrà coppia max anche a 4000)
  torque <- gsub("./-...", "", torque)
  
  # uniformo - e ~ (stesso significato ma espresso con caratteri diversi)
  torque <- gsub("~", "-", torque)
  
  # tolgo le power band tenendo solo i valori iniziali (stesso motivo della coppia max sopra)
  torque <- gsub("-....", "", torque)
  
  # sostituisco soltanto la barra (formato xxxx/xxx)
  torque <- sub('/', '°', torque)
  
  # assegno NA alle righe rimaste stringa vuota ( ="" )
  torque[torque==""]<-NA
  
  # splitto per dividere coppia da giri per le colonne separate
  spl_torque <- strsplit(torque, "°")
  
  # uniformazione dimensioni per creazione colonne: 
  for(i in 1:length(spl_torque)) {
    if(length(spl_torque[[i]]) == 1) { # se è monovalore,
      if(is.na(spl_torque[[i]])) { # e manca la potenza (NB: non c'e' il caso in cui ci sono solo i giri)
        spl_torque[[i]] <- c(NA, NA) # metti 0 in tq e rpm;
      }
      else if(is.na(spl_torque[[i]][2])) { # altrimenti, se mancano i giri
        spl_torque[[i]] <- c(spl_torque[[i]][1], NA)
      }
    }
    spl_torque[[i]]  <- as.numeric(spl_torque[[i]])
  }
  
  # trasformazione in dataframe, warning se matrix(unlist)) non ha il numero giusto di righe, risolto con uniformazione dimensioni sopra
  cleaned <- data.frame(matrix(unlist(spl_torque), nrow=length(spl_torque), byrow=TRUE))
  colnames(cleaned) <- c("torque", "rpm")
  
  # ancora da risolvere la questione dell'UDM: ci sono outliers anche in kgm, dati raccolti male.
  for(i in 1:nrow(carsCSV)) #scorro il DF 
  {
    kIndex <- as.numeric(gregexpr(pattern = "k", carsCSV$torque[i], ignore.case = TRUE)) #-1 se non c'e'
    nmIndex <- as.numeric(gregexpr(pattern = "n", carsCSV$torque[i], ignore.case = TRUE)) #-1 se non c'e'
    
    if(is.na(kIndex) || is.na(nmIndex)) { next }
    else if((kIndex == -1 && nmIndex != -1)) { next } # se non ci sono i "kg" e ci sono i Nm, NEXT
    
    else if((nmIndex == -1 && kIndex != -1)) {
      cleaned$torque[i] <- cleaned$torque[i]*9.81 # se ci sono i "kg" ma non i Nm
      next
    }
    else if((nmIndex<kIndex)) { next }# se i Nm vengono prima dei kg, NEXT
    
    else if((kIndex<nmIndex)) {
      cleaned$torque[i] <- cleaned$torque[i]*9.81 # se i kg vengono prima dei Nm moltiplica
      next
    }
    # l'ultima casistica e': non vi e' unità di misura. Pertanto sono scelti i Nm, vista la maggioranza di valori Nm ed i valori stessi (nulla minore di 150, che in Kgm sarebbero troppo alti)
  }
  return(cleaned)
}

# controllo come si comportano le variabili qualitative
# unique(carsCSV$fuel)
# unique(carsCSV$seller_type)
# unique(carsCSV$transmission)
# unique(carsCSV$owner)
# unique(carsCSV$seats)
# unique(carsCSV$engine)

# controllo il numero di NA per ciascuna colonna. Nel dataframe sono presenti in certi casi stringhe vuote,
# saranno da convertire in NA
colSums(is.na(carsCSV))

# scriviamo il codice per pulire il resto delle colonne
clean_name <- function(name_col){
  splitted <- strsplit(name_col, " ");
  brand_names <- c();
  for( i in 1:length(splitted) ){
    brand_names <- c(brand_names, splitted[[i]][[1]]);
  }
  names_no_space <- gsub(" ", "", brand_names, fixed = TRUE)
  brand_names[names_no_space==""] <- NA # assegno NA alle righe vuote
  
  car_types <- name_col
  for(i in 1:length(brand_names)){
    if (!is.na(brand_names[i])) {
      car_types[i] <- gsub(paste(brand_names[i], " ", sep = ""), "", car_types[i])
    }
  }
  return(list("brand" = brand_names, "car_type" = car_types));
}

# TODO tre FUNZ CHE FANNO LA STESSA COSA? E' STATO CONTROLLATO L'OUTPUT?
clean_mileage <- function(lista){
  lista <- paste(lista, " ");
  splitted <- strsplit(lista, " ");
  valori <- lapply(splitted, '[[', 1);
  values_no_space <- gsub(" ", "", valori, fixed = TRUE)
  valori[values_no_space==""] <- NA # assegno NA alle righe vuote
  valori <- as.numeric(unlist(valori))
  valori[valori==0] <- NA
  return(valori);
}
clean_max_power <- function(lista){
  lista <- paste(lista, " ");
  splitted <- strsplit(lista, " ");
  valori <- lapply(splitted, '[[', 1);
  values_no_space <- gsub(" ", "", valori, fixed = TRUE)
  valori[values_no_space==""] <- NA # assegno NA alle righe vuote
  valori <- as.numeric(unlist(valori))
  valori[valori==0] <- NA
  return(valori);
}
clean_engine <- function(lista){
  lista <- paste(lista, " ");
  splitted <- strsplit(lista, " ");
  valori <- lapply(splitted, '[[', 1);
  values_no_space <- gsub(" ", "", valori, fixed = TRUE)
  valori[values_no_space==""] <- NA # assegno NA alle righe vuote
  valori <- as.numeric(unlist(valori))
  valori <- 100*round(valori/100)
  valori[valori==0] <- NA
  return(valori);
}

cleanedCSV <- clean_torque(carsCSV)
brand_and_types <- clean_name(carsCSV$name)
cleanedCSV <- cbind(
  "brand" = factor(brand_and_types$brand),
  "car_type" = brand_and_types$car_type,
  "year" = carsCSV$year, # TODO FACTOR?
  "selling_price" = carsCSV$selling_price*0.0118,
  "km_driven" = carsCSV$km_driven,
  "fuel" = factor(carsCSV$fuel),
  "seller_type" = factor(carsCSV$seller_type),
  "transmission" = factor(carsCSV$transmission),
  "owner" = factor(carsCSV$owner),
  "mileage" = clean_mileage(carsCSV$mileage),
  "engine" = clean_engine(carsCSV$engine),
  "max_power" = clean_max_power(carsCSV$max_power),
  "seats" = factor(carsCSV$seats),
  cleanedCSV
)
# guardo com'e' il nuovo dataset in confronto al vecchio
#str(cleanedCSV)
#colSums(is.na(carsCSV))
#colSums(is.na(cleanedCSV))

# salvo il dataframe
write.csv(cleanedCSV, "cleanedCSV.csv", row.names = FALSE)
