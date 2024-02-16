# UED - Drought Designations Table
  # Osmotic potential at turgor loss point and P50
    # Updated by - A. Tumino on 15January2024

library(data.table)
library(googlesheets4)
library(ggplot2)
library(dplyr)
library(wrapr)
library(stringr)
library(reshape2)
library(tidyr)
library(plotly)
#library(Rcmdr)
library(WorldFlora)

setwd("G:/Shared drives/Urban Ecological Drought/Drought Tolerance Designations/PSI-TLP Data/Data for R_do not edit")


coords<-read.csv("USA_LatLongs.csv",header=T)
coords<-coords%>%
  mutate(Latitude = round(Latitude,4),
         Longitude = round(Longitude, 4))%>%
  separate(USDA_title_23, into = c("Zone_2023", "TempRange_F"), sep = ": ", remove = FALSE)%>%
  mutate(Zone_2023 = str_trim(Zone_2023),TempRange_F=str_trim(TempRange_F))

names(coords)
# Hirons et al. 2020 data merge with Sjoman Data-------------------------------------------------
jakedat <- read.csv("Jake summary data.csv", header = TRUE)

jakedat <- jakedat %>%
  mutate(Species = if_else(species == "Quercus muhlenbergii", "Quercus muehlenbergii", species), #fix spelling
         Source = "Hirons et al. 2020", # add source information
         #Comments = NA, # dummy column
         Species_short = ifelse(str_detect(Species, "'"), str_split(Species, pattern = "'", simplify = TRUE)[, 1], Species),
         Cultivar = ifelse(str_detect(Species, "'"), str_split(Species, pattern = "'", simplify = TRUE)[, 2], NA_character_),
         sum.tlp = round(sum.tlp, 2),
         sum.opft = round(sum.opft, 2)) %>%
  transmute(Species, garden, Source, Species_short, Cultivar,sum.tlp,sum.opft) # separate out cultivar
jakedat$Species_short <- str_trim(jakedat$Species_short, side = "right") # trim off trailing white space


# Add in lat long and country for the botanic gardens listed
botanicgardens<-data.frame(
  garden = c("Alnarp","Wespelaar","Cornell","Kew","Morton","Ness","Savill"),
  Country = c("Sweden","Belgium","USA","UK","USA","UK","UK"),
  Latitude = c(55.6578147740059, 50.95864872436378, 42.45249241332518, 51.48381159036358, 41.81650499204021, 53.27250615951263, 51.4267556117446),
  Longitude = c(13.082413825025307, 4.634059925481531, -76.45488406152685, -0.2897730340057771, -88.06907647320472, -3.0430448608760723, -0.5976980493519929)
)

jakedat<-jakedat%>% # combine with jake data frame (Hirons 2020 data)
  left_join(botanicgardens)%>%
  mutate(Longitude = round(Longitude, 4),
         Latitude = round(Latitude, 4))

compiled<-read.csv("TRY Data Sjöman-Hirons Leaf Turgor Loss .csv",header=T)

compiled <- compiled %>%
  rename(Reference="Source")%>% # rename column
  mutate(Species = if_else(Species == "Quercus muhlenbergii", "Quercus muehlenbergii", Species), # fix spelling
         Source = "Sjoman et al. 2018", # add source column
         #Latitude = NA, # dummy column
         #Longitude = NA, # dummy column
         #Country = NA, # dummy column
         Species_short = ifelse(str_detect(Species, "'"), str_split(Species, pattern = "'", simplify = TRUE)[, 1], Species),
         Cultivar = ifelse(str_detect(Species, "'"), str_split(Species, pattern = "'", simplify = TRUE)[, 2], NA_character_)) %>%
  transmute(Species,Species_short,Exposition,Maturity,Source,Reference,Cultivar,Osmotic_potential_full_turgor_MPa,
            Leaf_turgor_loss_pt_MPa,Comments) # separate cultivar
compiled$Species_short<- str_trim(compiled$Species_short, side = "right") # trim of trailing space

#compiled<- melt(compiled, id = c("Species", "Location","Country","Latitude", # wide to long
#                                "Longitude","Species_new", "Cultivar","Comments",
#                                "Maturity","Source","Reference")) %>%
# rename(Dat.Type = "variable", Psi = "value") %>% # rename
#  mutate(Dat.Type = if_else(Dat.Type == "Leaf_turgor_loss_pt_MPa", "Ptlp", "Pft")) # change names in data type

hirons<-jakedat%>%
  left_join(compiled,by=c("Species","Species_short","Cultivar"))%>%
  rename(Source_Hirons = Source.x,P0_Hirons = sum.tlp, P100_Hirons = sum.opft,
         Source_Sjoman = Source.y, P100_Sjoman = Osmotic_potential_full_turgor_MPa,
         P0_Sjoman = Leaf_turgor_loss_pt_MPa, Refernce_Sjoman = Reference)%>%
  left_join(coords,by=c("Latitude","Longitude","Country","garden"))
#150 obs


# Google Sheets -----------------------------------------------------------
desig<-data.frame(read_sheet(ss="https://docs.google.com/spreadsheets/d/1Ap2zxfzQ2tA7Vw2YFKWG5WlASvh_DakGJGc-i07YDOo/edit#gid=2107989582"))
2
desig_list<-desig%>%
  select(Species)%>%
  mutate(Species = if_else(Species == "Platanus x acerifolia (syn hispanica)", "Platanus x acerifolia", Species),
         Species_new=Species)
#desig_list<- data.frame(desig_list[1:159, ])
desig_list<-data.frame(desig_list[-c(158,160:162,176:179)])




# TRY Data ----------------------------------------------------------------


trydat <- fread("25193.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T,
                encoding = "UTF-8")

strings_to_match <- c("P50 (MPa)", "P12 (MPa)", "P88 (MPa)","P50","P88","lat","lon",
                      "LAT","LONG","Location","Lat","Lon","Country","Reference",
                      "Latitude","Longitude","LATnew","LONnew")

# Pull out just ObservationID rows that are associated with P50 data
try_obs<-trydat %>%
  filter(OriglName=="P50 (MPa)"|OriglName=="P12 (MPa)"|OriglName=="P88 (MPa)")%>%
  select(ObservationID)%>% distinct(ObservationID)%>%
  inner_join(.,trydat, by = "ObservationID")%>% 
  filter(grepl(paste(strings_to_match, collapse = "|"), OriglName))%>%
  select(ObservationID, OriglName,OrigValueStr,SpeciesName)%>%
  pivot_wider(names_from=OriglName,values_from=OrigValueStr)%>%
  rename(P88="P88 (MPa)", P50_method="P50 method",P50="P50 (MPa)",
         Prov_Long="Provenance Longitude", Prov_Lat="Provenance Latitude")

try_obs$Country<-as.character(try_obs$Country)
try_obs$P50<-as.numeric(as.character(try_obs$P50))
try_obs$P88<-as.numeric(as.character(try_obs$P88))
try_obs$Location<-as.character(try_obs$Location)
try_obs$Reference<-as.character(try_obs$Reference)
try_obs$Latitude<-as.numeric(as.character(try_obs$Latitude))
try_obs$Longitude<-as.numeric(as.character(try_obs$Longitude))
try_obs$P50_method<-as.character(try_obs$P50_method)
try_obs$Prov_Lat<-as.numeric(as.character(try_obs$Prov_Lat))
try_obs$Prov_Long<-as.numeric(as.character(try_obs$Prov_Long))
try_obs$LONnew<-as.numeric(as.character(try_obs$LONnew))
try_obs$LATnew<-as.numeric(as.character(try_obs$LATnew))
try_obs$SpeciesName<-as.character(try_obs$SpeciesName)
str(try_obs)
summary(try_obs)

try_obs<-try_obs%>%
  mutate(LATnew = if_else(is.na(LATnew), Latitude, LATnew),
         LONnew = if_else(is.na(LONnew), Longitude, LONnew),
         Country = if_else(Country == "NULL", NA, Country),
         Reference = if_else(Reference == "NULL", NA, Reference),
         Location = if_else(Location == "NULL", NA, Location),
         P50_method = if_else(P50_method == "NULL", NA, P50_method),
         P50 = round(P50, 2),
         P88 = round(P88, 2),
         LATnew = round(LATnew, 4),
         LONnew = round (LONnew, 4))%>%
  rename(Species = SpeciesName)%>%
  filter(!duplicated(select(., -ObservationID)))%>%
  mutate(Last = word(Reference, 1, sep = " "))%>%
  select(-Latitude,-Longitude)%>%
  rename(Latitude = LATnew, Longitude = LONnew)#%>%
  #left_join(coords, by=c("Latitude","Longitude","Country"))
#1269 rows

# Choat et al. 2012 data --------------------------------------------------


choat<-read.csv("Choat et al. 2012, Supplementary data tabla_ACT.csv",header=T)
choat<-choat%>%
  select(Species,p50,p88,Location,Country,Reference,Latitude,Longitude,Notes)%>%
  mutate(Source="Choat et al. 2012",
         p50=round(p50,2),
         Latitude = round(Latitude, 4),
         Longitude = round(Longitude, 4))%>% # add source column
  rename(Comments="Notes",
         P88 = p88,
         P50 = p50)%>%
  mutate(Last = word(Reference, 1, sep = " "))# change to comments to match other df
#480 obs


# Choat - Try merge -------------------------------------------------------
# list of family names from the data set 
family<-read.csv("Genus_Choat.csv",header=T)
family$Family<-trimws(family$Family,"both")


choat_try<-try_obs%>%
  rename(Location_try = Location,
         Reference_try = Reference)%>%
  full_join(choat, by = c("Species", "Country","Last","Latitude","Longitude","P50","P88"))%>%
  distinct(P50, P88, Species, .keep_all = TRUE)
#1539
#with distinct line it slims to 1474




# 2024 Data Combination - HIRONS - Ptlp and Pft---------------------------------------------------

# Going to try again to combine all of the data and compare it to the list
# in the drought designations table to see what we are missing

# calculating water potential at turgor loss point from water potential at full turgor
# ΨP0= −0.2554 + 1.1243x Ψπ100
# Hirons et al 2020 (Jake summary data - jakedat) uses Ptlp aka P0
# Hirons & Sjoman (compiled) uses Ptlp aka P0

# data combined for Hirons et al 2020 and Sjoman Hirons 2015.

# Instead of merging the data frames together, pull information from one to the other 
# based on the columns that they do or do not have in common. 

hirons<-hirons %>%
  mutate(Species_short = ifelse(Species_short =="Ulmus",Species,Species_short ))#%>%
#  left_join(desig_list, by = "Species")
  
hirons$Table <- ""

# Loop through each row in test
for (i in 1:nrow(hirons)) {
  # Check if the Species names in the test df match any of the 
  # species names in the drought designation table
  if (hirons$Species_short[i] %in% desig_list$Species_new) {
    hirons$Table[i] <- "X"  # Mark 'X' in Table if there's a match
  }
}
#150

# Include all of the elms
hirons<-hirons%>%
  #  mutate(Table=ifelse("Ulmus" %in% Species_new,Table=="X",Table))
  mutate(Table = ifelse(grepl("ulmus", Species_short, ignore.case = TRUE), "X", Table))

hirons<-hirons%>%
  #filter(Table=="X")%>%
  mutate(Genus = str_split(Species, " ", simplify = TRUE)[, 1])%>%
  mutate(Genus = str_trim(Genus))
# 100 rows

hirons_des<-hirons %>% filter (Table == "X")
# 64 rows
hirons_des%>%  
  filter(Country == "USA") %>%
  group_by(Zone_2023)%>%
  summarise(Count_USDA = length(unique(Species)))
length(unique(hirons_des$Species[hirons_des$Country=="USA"]))
# 58 unique species (including variety/cultivar)
# 49 in the USA

hirons_des%>%  
  filter(Country == "USA") %>%
  group_by(Zone_2023)%>%
  summarise(Count_USDA = length(unique(Species_short)))



length(unique(hirons_des$Species_short))
# 52 unique species total (not counting variety)
length(unique(hirons_des$Species))
# 58 with cultivar
length(unique(hirons_des$Genus))
# 7 unique genera
length(unique(hirons$Species_short))
# 124 unique species total (not counting variety)
length(unique(hirons$Species))
# 136 species with cultivar
length(unique(hirons$Genus))
# 8 unique genera


hirons_des%>%
  filter(Species_short %like% "Ulmus")%>%
  filter(Country=="USA")%>%
  summarise(Count = length(unique(Species_short)))

# 19 different elm species in the entire data set
# all were located in the USA as well.

# Graphing the Ptlp data - 43 species that intersect with our current list

tlp_des<-hirons_des %>%
  group_by(Species_short)%>%
  summarise(Count = length(P0_Hirons),
            Mean_TLP=mean(P0_Hirons),
            SD_TLP=sd(P0_Hirons),
            SE_TLP= sd(P0_Hirons) / sqrt(n())) %>%
  mutate(Mean_TLP = round(Mean_TLP, 2),SD_TLP = round(SD_TLP,2),
         SE_TLP=round(SE_TLP,2))%>%
  left_join(hirons_des)#%>%
#  filter(Count>1)
ggplotly(ggplot(tlp_des,aes(x=reorder(Species_short, P0_Hirons), y=P0_Hirons))+
           labs(x="Species",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
           geom_boxplot() + ggtitle("Drought Designations Species - Hirons")+
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                 axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip()+
           geom_point(aes(text = paste("<br>Cultivar:",Cultivar,"<br>Country:",Country),
                      col=garden)))


ggplotly(ggplot(tlp_des,aes(x=reorder(Genus, P0_Hirons), y=P0_Hirons))+
           labs(x="Genera",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
           geom_boxplot() + ggtitle("Drought Designations Genera - Hirons")+
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                 axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip()+
           geom_point(aes(text = paste("<br>Species:",Species_short,"<br>Cultivar:",Cultivar,"<br>Country:",Country),
                          col=garden)))


# Graphing the Ptlp data - all species Andy had data for

tlp<-hirons %>%
  group_by(Species_short)%>%
  summarise(Count = length(P0_Hirons),
            Mean_TLP=mean(P0_Hirons),
            SD_TLP=sd(P0_Hirons),
            SE_TLP= sd(P0_Hirons) / sqrt(n())) %>%
  mutate(Mean_TLP = round(Mean_TLP, 2),SD_TLP = round(SD_TLP,2),
         SE_TLP=round(SE_TLP,2))%>%
  left_join(hirons)#%>%
#  filter(Count>1)
ggplotly(ggplot(tlp,aes(x=reorder(Species_short, P0_Hirons), y=P0_Hirons))+
           labs(x="Species",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
           geom_boxplot() + ggtitle("Full Species List - Hirons")+
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                 axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip()+
           geom_point(aes(text = paste("<br>Cultivar:",Cultivar,"<br>Country:",Country),
                          col=garden)))


ggplotly(ggplot(tlp,aes(x=reorder(Genus, P0_Hirons), y=P0_Hirons))+
           labs(x="Genera",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
           geom_boxplot() + ggtitle("All Genera - Hirons")+
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                 axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip()+
           geom_point(aes(text = paste("<br>Species:",Species_short,"<br>Cultivar:",Cultivar,
                                       "<br>Country:",Country),
                          col=garden)))

# Summary of P50 and P88 ----------------------------------

#choat_des<-choat_try%>%
#  left_join(desig_list, by = "Species")

choat_try$Table <- ""

# Loop through each row in test
for (i in 1:nrow(choat_try)) {
  # Check if the Species names in the test df match any of the 
  # species names in the drought designation table
  if (choat_try$Species[i] %in% desig_list$Species_new) {
    choat_try$Table[i] <- "X"  # Mark 'X' in Table if there's a match
  }
}
#1424

# Include all of the elms
choat_try<-choat_try%>%
  #  mutate(Table=ifelse("Ulmus" %in% Species_new,Table=="X",Table))
  mutate(Table = ifelse(grepl("ulmus", Species, ignore.case = TRUE), "X", Table))%>%
  mutate(Genus = str_split(Species, " ", simplify = TRUE)[, 1]) %>%
  mutate(Genus = str_trim(Genus))%>%
  left_join(family,by="Genus")
  

choat_des<- choat_try %>%
  filter(Table=="X") %>%
  left_join(coords,by=c("Latitude","Longitude","Country"))


choat_try %>%
  filter(!is.na(P50))%>%
  summarise(NumSpecies=length(unique(Species)),
            NumGenera = length(unique(Genus)),
            NumFamily = length(unique(Family)))
# 562 unique species 
# 289 unique genera
# 100 unique Family

choat_try %>%
  filter(!is.na(P88))%>%
  summarise(NumSpecies=length(unique(Species)),
            NumGenera = length(unique(Genus)),
            NumFamily = length(unique(Family)))
# 468 unique species with P88 data
# 252 unique genera with P88 data
# 93 families

choat_des %>%
  filter(!is.na(P50))%>%
  summarise(NumSpecies=length(unique(Species)),
            NumGenera = length(unique(Genus)),
            NumFamily = length(unique(Family)))
# 43 unique species from designation list with P50 data
# 31 unique genera from designation list with P50 data
# 18 unique families from designation list with P50 data

choat_des %>%
  filter(!is.na(P88))%>%
  summarise(NumSpecies=length(unique(Species)),
            NumGenera = length(unique(Genus)),
            NumFamily = length(unique(Family)))
# 37 unique species from designation list with P88 data
# 26 unique genera from designation list with P88 data
# 14 unique families from designation list with P88 data


# Graphing the Ptlp data

names(choat_des)
head(choat_des)

p_50<-choat_des %>%
  select(Species,Country,P50)%>%
  distinct(.)%>%
  group_by(Species)%>%
  summarise(Count = length(P50),
            Mean_TLP=mean(P50),
            SD_TLP=sd(P50),
            SE_TLP= sd(P50) / sqrt(n())) %>%
  mutate(Mean_TLP = round(Mean_TLP, 2),SD_TLP = round(SD_TLP,2),
         SE_TLP=round(SE_TLP,2))%>%
  left_join(choat_des)#%>%
  #filter(Count>1)

#choat_try %>%
 # select(Genus)%>%
  #filter(!duplicated(Genus))#%>%
 # write.csv("Genus_Choat.csv",row.names=F)




length(unique(p_50$Genus))
ggplotly(ggplot(choat_des,aes(x=reorder(Species, P50), y=P50))+
           labs(x="Species",y="P50 (MPa)") +
           geom_boxplot() + ggtitle("Drought Designations - P50 Choat") +
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                 axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip()+
           geom_point(aes(#text = paste("<br>Reference:", Reference_try),
                          col=Country)))


ggplotly(ggplot(choat_try,aes(x=reorder(Family, P50), y=P50))+
           labs(x="Family",y="P50 (MPa)") +
           geom_boxplot() + ggtitle("Full list - P50 Choat") +
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                 axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip()+#geom_point())
#           geom_point(aes(col=Country)))
           geom_point(aes(text=paste("<br>Species:", Species,"<br>P88:", P88))))
#             col=Country)))


ggplotly(ggplot(choat_des,aes(x=Species, y=P88))+
           labs(x="Species",y="P88 (MPa)") +
           geom_boxplot() + ggtitle("Drought Designations - P88 Choat") +
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                 axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip()+
           geom_point(aes(text=paste("<br>Species:", Species,"<br>P50:", P50))))

ggplotly(ggplot(choat_try,aes(x=reorder(Family, P88), y=P88))+
           labs(x="Family",y="P88 (MPa)") +
           geom_boxplot() + ggtitle("Full List - P88 Choat") +
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                 axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip()+ #geom_point())
          geom_point(aes(text=paste("<br>Species:", Species,"<br>P50:", P50))))

# How many Choat species have both P50 and P88?

seeall<-choat_try %>%
  select(Species,P50,P88,Reference,Source) %>%
  filter(!is.na(P50)&!is.na(P88))#%>%
 #summarize(Count = length(Species))
# 948 observations with both P88 and P50

ggplotly(ggplot(seeall,aes(x=P50, y= P88))+geom_point())

see<-choat_des %>%
  select(Species,P50,P88,Reference,Source) %>%
  filter(!is.na(P50)&!is.na(P88))##%>%
 # summarize(Count = length(Species))
# 131 species that have both P50 and P88  

# Coordinates for Choat_designations --------------------------------------


coord_choat<-choat_des%>%
  select("Latitude","Longitude","Country")%>%
  filter(!is.na(Latitude) & !is.na(Longitude)& Country=="USA")%>% 
  distinct(Latitude, Longitude, .keep_all = TRUE)#%>% 
  mutate(USDA_title_23=c("5b: -15 to -10",
                    "6a: -10 to -5",
                    "7a: 0 to 5",
                    "8a: 10 to 15",
                    "5a: -20 to -15",
                    "6b: -5 to 0",
                    "3b: -35 to -30",
                    "8a: 10 to 15",
                    "8b: 15 to 20",
                    "7a: 0 to 5",
                    "6a: -10 to -5",
                    "8a: 10 to 15",
                    "10b: 35 to 40",
                    "4b: -25 to -20",
                    "7b: 5 to 10"))%>%
  separate(USDA_title_23, into = c("Zone_2023", "Temp_C"), sep = ": ", remove = FALSE)%>%
 mutate(Zone_2023 = str_trim(Zone_2023),Temp_C=str_trim(Temp_C))
# 15 distnct lat long coordinates for the entire data set.

  

# Old code 2 --------------------------------------------------------------

  
alldat_des<-alldat_des%>%
  #left_join(coord)%>%
  mutate(Genus = str_split(Species, " ", simplify = TRUE)[, 1])%>%
  mutate(Genus = str_trim(Genus))

#alldat <- all_try %>%
# arrange(desc(across(everything(), ~!is.na(.)))) %>%
# distinct(Psi, .keep_all = TRUE)


alldat_des%>%
  #  filter(Dat.Type=="P0")%>%
  group_by(Source,Dat.Type)%>%
  summarise(Count = length(unique(Species_new)))
# TRY - Choat: P50 (43) and P88 (37)
# Choat et al: P50 (41) and P88 (16)
# Hirons 2020: Pft (52) and Ptlp (52) 
# Sjoman 2015: Pft (38) and Plp (38)

length(unique(alldat_des$Species_new))
# 68 species represented when cultivars lumped with parent in the USA
# 102 total species
length(unique(alldat_des$Species))
# 113 species when cultivars as separate species

length(unique(alldat_des$Genus[alldat_des$Country=="USA"]))
# 27 genus in USA
length(unique(alldat_des$Genus))
# 46 genera in entire set

alldat_des%>%
  filter(Species_new %like% "Ulmus")%>%
  filter(Country=="USA")%>%
  summarise(Count = length(unique(Species_new)))
# 19 different elm species in the entire data set
# all were located in the USA as well.


alldat_des%>%
  #  filter(Dat.Type=="P0")%>%
  group_by(Dat.Type)%>%
  summarise(Count = length(unique(Species_new)))
# P50 - 43
# P88 - 37
# PFT - 73
# P tlp - 73

alldat_des%>%
  filter(Country=="USA")%>%
  group_by(Dat.Type)%>%
  summarise(Count = length(unique(Species_new)))
# Ptlp 45
# P50 29
# P88 22
# Pft 45

# Graphing the Ptlp data



TLP_dat<- alldat_des%>%
  filter(Dat.Type=="Ptlp")%>%
  group_by(Species_new)%>%
  summarise(Count_TLP=length(Psi),
            Mean_TLP=mean(Psi),
            SD_TLP=sd(Psi),
            SE_TLP= sd(Psi) / sqrt(n()))%>%
  mutate(Mean_TLP = round(Mean_TLP, 2),SD_TLP = round(SD_TLP,2),
         SE_TLP=round(SE_TLP,2))

FT_dat<- alldat_des%>%
  filter(Dat.Type=="Pft")%>%
  group_by(Species_new)%>%
  summarise(Count_FT=length(Psi),
            Mean_FT=mean(Psi),
            SD_FT=sd(Psi),
            SE_FT= sd(Psi) / sqrt(n()))%>%
  mutate(Mean_FT = round(Mean_FT, 2),SD_FT = round(SD_FT,2),
         SE_FT=round(SE_FT,2))#%>% filter(!is.na(Latitude)

P88_dat<- alldat_des%>%
  filter(Dat.Type=="P88")%>%
  group_by(Species_new)%>%
  summarise(Count_P88=length(Psi),
            Mean_P88=mean(Psi))%>%
  mutate(Mean_P88 = round(Mean_P88, 2))#%>% filter(!is.na(Latitude)

P50_dat<- alldat_des%>%
  filter(Dat.Type=="P50")%>%
  group_by(Species_new)%>%
  summarise(Count_P50=length(Psi),
            Mean_P50=mean(Psi))%>%
  mutate(Mean_P50 = round(Mean_P50, 2))

slack<-alldat_des%>%
  left_join(TLP_dat)%>%
  left_join(FT_dat) %>%
  filter(!is.na(Species_new))%>%
  #filter(!if_all(Count_TLP:SE_FT, ~ !is.na(.)))
  filter(!is.na(Count_TLP))%>%
  select(-Count_FT)%>%
  rename(N="Count_TLP")

plot<-slack %>%
  filter(N>1&SE_TLP>0.00)%>%
  filter(Dat.Type=="Ptlp")
library(plotly)  
ggplotly(ggplot(plot,aes(x=Species_new, y=Psi))+
    labs(x="Species",y="Leaf Water Potential at Turgor Loss Point (MPa)",
         color="Data Type") +
    geom_boxplot() + 
    theme_classic()+theme(axis.text.y=element_text(color="black",
                                                   size=15,vjust=0.5,
                                                   hjust=1,face="italic"),
                          axis.text.x=element_blank(),
                          axis.ticks=element_line(color="black"),
                          axis.title=element_text(size=15),
                          legend.position="none")+ coord_flip()+
    geom_point(aes(col=Location)))
  
plot50<-alldat_des %>%
  filter(Dat.Type=="P50")

ggplotly(ggplot(plot50,aes(x=Species_new, y=Psi))+
           labs(x="Species",y="P50 (MPa)",
                color="Data Type") +
           geom_boxplot() + 
           theme_classic()+
           theme(axis.text.y=element_text(color="black",
                                          size=10,vjust=0.5,
                                          hjust=1,face="italic"),
                                 axis.text.x=element_blank(),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip()+
           geom_point(aes(col=Location)))

  # Old Code -----------------------------------------------------------------


jakedat<-jakedat %>% select(where(not_all_na)) %>%
  rename("Leaf_turgor_loss_pt_MPa"="sum.tlp","Exposition"="garden",
         "Species"="species") %>%
  select(.,c("Species","Exposition","Leaf_turgor_loss_pt_MPa"))


sentinel<-compiled %>% 
  select(.,c("Species","Exposition","Leaf_turgor_loss_pt_MPa")) %>%
  bind_rows(.,jakedat) %>%
  rename("Species_cultivar"="Species") %>%
  mutate("Species" = trimws((gsub("\\'.*", "", Species_cultivar)), which = "right", whitespace = "[ ]")) %>%
  merge(.,lukedat,by="Species",all.y=T) %>%
  merge(.,desig[,c(1,3,4)],by="Species") %>%
  mutate(NITS=factor(NITS,levels=c("NA","Intolerant","Moderate","Tolerant"))) %>%
  mutate(Hirons.Sjoman=factor(Hirons.Sjoman,levels=c("NA","Mod Sensitive","Mod Tolerant","Tolerant"))) %>%
  mutate(Species= with(., reorder(Species,Leaf_turgor_loss_pt_MPa))) %>%
  merge(.,TRYdata_sub,by="Species",all=T) %>%
  rename("P50_MPa"="OrigValueStr")%>%
  mutate(Species= with(., reorder(Species,Leaf_turgor_loss_pt_MPa)))







# NITS designations and Osm Pot at TLP ------------------------------------
N<-ggplot(sentinel,aes(x=Species, y=Leaf_turgor_loss_pt_MPa,color=NITS))+
  labs(x="Species",y="",title="",tag="A",
       color="Drought Designations \n from NITS") +
  geom_boxplot() + 
  theme_classic()+theme(axis.text.y=element_text(color="black",size=15,vjust=0.5,hjust=1,face="italic"),
                        axis.text.x=element_blank(),
                        axis.ticks=element_line(color="black"),
                        axis.title=element_text(size=15),
                        legend.position="none")+ coord_flip()+
  geom_point()

# Hirons and Sjmonan Designations and Osm Pot at TLP ----------------------
HS<-ggplot(sentinel,aes(x=Species, y=Leaf_turgor_loss_pt_MPa,color=Hirons.Sjoman))+
  labs(x="Species",y=expression(paste(psi[tlp](MPa))),tag="C",color="Drought Designations \n from Hiron and Sjöman") +
  geom_boxplot() + 
  theme_classic()+theme(axis.text.y=element_text(color="black",size=15,vjust=0.5,hjust=1,face="italic"),
                        axis.text.x=element_text(color="black",size=15),
                        axis.ticks=element_line(color="black"),
                        axis.title=element_text(size=15),
                        legend.position="none")+ coord_flip()+
  geom_point()


# NITS designations and P50 -----------------------------------------------
NP50<-sentinel %>%
  mutate(P50_MPa= as.numeric(as.character(P50_MPa)))%>%
  mutate(Species= with(., reorder(Species,P50_MPa))) %>%
  ggplot(.,aes(x=Species, y=P50_MPa,color=NITS))+
  labs(x="Species",y="",title="",tag="B",
       color="Drought Designations \n from NITS") +
  geom_boxplot() + 
  theme_classic()+theme(axis.text.y=element_text(color="black",size=15,vjust=0.5,hjust=1,face="italic"),
                        axis.text.x=element_blank(),
                        axis.ticks=element_line(color="black"),
                        axis.title=element_text(size=15))+ coord_flip()+
  geom_point()

# Hirons and Sjmonan Designations and Osm Pot at TLP ----------------------
HS50<-sentinel %>%
  mutate(P50_MPa= as.numeric(as.character(P50_MPa)))%>%
  mutate(Species= with(., reorder(Species,P50_MPa))) %>%
  ggplot(.,aes(x=Species, y=P50_MPa,color=Hirons.Sjoman))+
  labs(x="Species",y="P50",tag="D",color="Drought Designations \n from Hiron and Sjöman") +
  geom_boxplot() + 
  theme_classic()+theme(axis.text.y=element_text(color="black",size=15,vjust=0.5,hjust=1,face="italic"),
                        axis.text.x=element_text(color="black",size=15),
                        axis.ticks=element_line(color="black"),
                        axis.title=element_text(size=15))+ coord_flip()+
  geom_point()

(N+NP50)/(HS+HS50)
# save landscape, 15x10




