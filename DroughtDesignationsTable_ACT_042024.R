# UED - Drought Designations Table
    # Updated by - A. Tumino on 24June2024

library(data.table)
library(googlesheets4)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(wrapr)
library(stringr)
library(reshape2)
library(tidyr)
library(plotly)
library(rtry)

#test
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
         #Source_file = "jakedat",
         #Comments = NA, # dummy column
         Species_short = ifelse(str_detect(Species, "'"), str_split(Species, pattern = "'", simplify = TRUE)[, 1], Species),
         Cultivar = ifelse(str_detect(Species, "'"), str_split(Species, pattern = "'", simplify = TRUE)[, 2], NA_character_),
         sum.tlp = round(sum.tlp, 2),
         sum.opft = round(sum.opft, 2)) %>%
  rename(psi.tlp = sum.tlp, psi.ft = sum.opft)%>%
  transmute(Species, garden, Source, Species_short, Cultivar,psi.tlp,psi.ft) # separate out cultivar
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

jakedat_lon<- jakedat %>%
  pivot_longer(cols = -c(Species, garden, Source, Species_short,
                         Cultivar, Country, Latitude, Longitude), names_to = "dat.type", values_to = "value") %>%
  rename(Exposition = "garden") %>%
  mutate(Reference = "Hirons", Maturity = NA, Comments = NA, Source_file = "jakedat")

compiled<-read.csv("TRY Data Sjöman-Hirons Leaf Turgor Loss .csv",header=T)

compiled <- compiled %>%
  rename(Reference="Source")%>% # rename column
  mutate(Species = if_else(Species == "Quercus muhlenbergii", "Quercus muehlenbergii", Species), # fix spelling
         Source = "Sjoman et al. 2018", # add source column
         #Source_file = "compiled",
         #Latitude = NA, # dummy column
         #Longitude = NA, # dummy column
         #Country = NA, # dummy column
         Species_short = ifelse(str_detect(Species, "'"), str_split(Species, pattern = "'", simplify = TRUE)[, 1], Species),
         Cultivar = ifelse(str_detect(Species, "'"), str_split(Species, pattern = "'", simplify = TRUE)[, 2], NA_character_)) %>%
  transmute(Species,Species_short,Exposition,Maturity,Source,Reference,Cultivar,Osmotic_potential_full_turgor_MPa,
            Leaf_turgor_loss_pt_MPa,Comments) %>% # separate cultivar 
  rename(psi.ft = Osmotic_potential_full_turgor_MPa, psi.tlp = Leaf_turgor_loss_pt_MPa)
compiled$Species_short<- str_trim(compiled$Species_short, side = "right") # trim of trailing space

compiled_lon <- compiled %>%
  pivot_longer(cols = -c(Species, Exposition, Source, Species_short,
                         Cultivar, Reference, Comments, Maturity), names_to = "dat.type", values_to = "value") %>%
  mutate(Country = NA, Latitude = NA, Longitude = NA, Source_file = "compiled")
#compiled<- melt(compiled, id = c("Species", "Location","Country","Latitude", # wide to long
#                                "Longitude","Species_new", "Cultivar","Comments",
#                                "Maturity","Source","Reference")) %>%
# rename(Dat.Type = "variable", Psi = "value") %>% # rename
#  mutate(Dat.Type = if_else(Dat.Type == "Leaf_turgor_loss_pt_MPa", "Ptlp", "Pft")) # change names in data type

hirons<-jakedat%>%
  full_join(compiled,by=c("Species","Species_short","Cultivar"))%>%
  rename(Source_Hirons = Source.x,Psi_tlp_Hirons = psi.tlp.x, Psi_ft_Hirons = psi.ft.x,
         Source_Sjoman = Source.y, Psi_ft_Sjoman = psi.ft.y,
         Psi_tlp_Sjoman = psi.tlp.y, Refernce_Sjoman = Reference)%>%
  full_join(coords,by=c("Latitude","Longitude","Country","garden"))
#217 obs

# make into one long DF
# Because all of this data is from Andy Hirons, there are duplicated rows across the two
# datasets, where the jakedat_lon has more complete information.
# The below code should combine columns where they match and then remove extra columns. 
# As much origin information was retained by combining the Source and Reference columns to reflect
# where the infomration came from.

hirons_lon <- full_join(jakedat_lon, compiled_lon, by = c("Species", "dat.type","value")) %>%
  mutate(
    Exposition = coalesce(Exposition.x, Exposition.y),
    Country = coalesce(Country.x, Country.y),
    Latitude = coalesce(Latitude.x, Latitude.y),
    Longitude = coalesce(Longitude.x, Longitude.y),
    Maturity = coalesce(Maturity.x,Maturity.y),
    Species_short = coalesce(Species_short.x, Species_short.y),
    Comments = coalesce(Comments.x, Comments.y),
    Cultivar = coalesce(Cultivar.x, Cultivar.y),
    Source_file = coalesce(Source_file.x, Source_file.y),
    Source = paste(Source.x, Source.y, sep = ", "),
    Reference = paste(Reference.x, Reference.y, sep = ", ")
  ) %>%
  select(-Exposition.x, -Country.x, -Latitude.x, -Longitude.x, -Exposition.y, -Country.y, -Latitude.y, -Longitude.y,
         -Maturity.x, -Maturity.y, -Species_short.x, -Species_short.y, -Comments.x, -Comments.y, -Cultivar.x, -Cultivar.y, -Reference.x,
         -Reference.y, -Source.x, -Source.y, -Source_file.x, -Source_file.y)

# Google Sheets -----------------------------------------------------------
desig<-data.frame(read_sheet(ss="https://docs.google.com/spreadsheets/d/1Ap2zxfzQ2tA7Vw2YFKWG5WlASvh_DakGJGc-i07YDOo/edit#gid=2107989582"))
2
desig_list<-desig%>%
  select(Species)%>%
  mutate(Species = if_else(Species == "Platanus x acerifolia (syn hispanica)", "Platanus x acerifolia", Species),
         Species_new=Species)
#desig_list<- data.frame(desig_list[1:159, ])
desig_list<-data.frame(desig_list[-c(158,160:162,176:179),])



# Bartlett data -----------------------------------------------------------
bartlett <-read.csv("Bartlett 2012 - supplemental data.xlsx - Data.csv", header = T)
head(bartlett)
str(bartlett)

bartlett <- bartlett %>%
  filter(Growth.form == "W") %>%
  select(Species,Biomes,psi.ft, psi.tlp, Reference) %>%
  rename(Exposition = Biomes, Species_short = Species)

# TRY Data ----------------------------------------------------------------

#Adding in TRY Data pull 31506 from Jan 2024 request.

trydat_2024 <- fread("31506.txt", header = T, sep = "\t", dec = ".",
                     quote = "", data.table = T,
                     encoding = "UTF-8")
names(trydat_2024)
unique(trydat_2024$DataName)
str(trydat_2024)
# Import try data using rtry package

TRYdata1 <- rtry_import("31506.txt", separator = "\t",
                        encoding = "Latin-1",
                        quote = "",
                        showOverview = T)

# Explore data usuing TraitID and Trait Name
TRYdata1_explore_trait <- rtry_explore(TRYdata1, TraitID, TraitName)
# Trait ID: 188 (FT), 189 (TL), 3468 (TLP)
# Ancillary data 16,752 (Trait ID = NA)
TRYdata1_explore_species <- rtry_explore(TRYdata1, 
                                         AccSpeciesID, 
                                         AccSpeciesName, 
                                         TraitID, TraitName)
# Summary of ancillary data
TRYdata1_explore_anc <- rtry_explore(TRYdata1, DataID, 
                                     DataName, TraitID, 
                                     TraitName, sortBy = TraitID)

# Read in try data.
TRYdata2 <- rtry_import("25193.txt", separator = "\t",
                        encoding = "Latin-1",
                        quote = "",
                        showOverview = T)

# Explore data usuing TraitID and Trait Name
TRYdata2_explore_trait <- rtry_explore(TRYdata2, TraitID, TraitName)
# Trait ID: 719 (P20, P50, P80), 3479 (P20, P50, P80)
# Ancillary data 135,531 (Trait ID = NA)

TRYdata2_explore_species <- rtry_explore(TRYdata2, 
                                         AccSpeciesID, 
                                         AccSpeciesName, 
                                         TraitID, TraitName)
# Summary of ancillary data
TRYdata2_explore_anc <- rtry_explore(TRYdata2, DataID, 
                                     DataName, TraitID, 
                                     TraitName, sortBy = TraitID)

# Combine both TRY datasets into one file.
TRYdata <- rtry_bind_row(TRYdata1, TRYdata2)

# Explore data
TRYdata_explore_trait <- rtry_explore(TRYdata, TraitID, TraitName)


# Group the input data based on AccSpeciesID, AccSpeciesName, TraitID and TraitName
# Note: For TraitID == "NA", meaning that entry is an ancillary data
TRYdata_explore_species <- rtry_explore(TRYdata, AccSpeciesID, AccSpeciesName, TraitID, TraitName)

# Group the input data based on DataID, DataName, TraitID and TraitName
# Then sort the output by TraitID using the sortBy argument
TRYdata_explore_anc <- rtry_explore(TRYdata, DataID, DataName, TraitID, TraitName, sortBy = TraitID)

# remove empty column 
workdata <- rtry_remove_col(TRYdata, V29)
workdata <- rtry_select_col(workdata, ObsDataID, ObservationID, 
                            AccSpeciesID, AccSpeciesName, ValueKindName, 
                            TraitID, TraitName, DataID, DataName, OriglName, 
                            OrigValueStr, OrigUnitStr, StdValue, UnitName, 
                            OrigObsDataID, ErrorRisk, Comment)

# Retrieve DataIDS with trait records and DataIDs with ancillary information
# 59 Latitude
# 60 Longitude
# 61 Altitude
# 6601 Sampling date
# 327 Exposition
# 413 Plant developmental status / plant age / maturity / plant life stage
# 961 Health status of plants (vitality)
# 113 Reference / source
# 1861 Plant Organ Measured

workdata <- rtry_select_row(workdata, TraitID > 0 | 
                              DataID %in% c(59, 60, 61, 6601, 
                                            327, 413, 1961, 113, 1861,1412))
# Check the data is selected
workdata_explore_anc <- rtry_explore(workdata, DataID, DataName, 
                                     TraitID, TraitName, sortBy = TraitID)


# Remove data that is sapling, seedling or s, or T (T/F for if plant was a seedling)
workdata <- rtry_exclude(workdata, (DataID %in% 413) & 
                           (OrigValueStr %in% c("Sapling", "seedling", "S", "T")), 
                         baseOn = ObservationID)

# Group the input data based on DataID, DataName, TraitID and TraitName
# Then sort the output by TraitID using the sortBy argument
tmp_unfiltered <- rtry_explore(workdata, DataID, DataName, TraitID, TraitName, sortBy = TraitID)

# Criteria
# 1. DataID equals to 2103, 2104, 6912, 6913, 2543, 2542, 2102, 2101, 6914, 2538,
# 8617, 2550, 2199, 7803, 7802
workdata <- rtry_exclude(workdata, 
                         DataID %in% c(2103, 2104, 6912, 6913, 2543, 2542, 2102, 2101, 6914, 2538,
                                                  8617, 2550, 2199, 7803, 7802), baseOn = ObsDataID)

# Group the input data based on DataID, DataName, TraitID and TraitName
# Then sort the output by TraitID using the sortBy argument
tmp_filtered <- rtry_explore(workdata, DataID, DataName, TraitID, TraitName, sortBy = TraitID)

# filter data out with error risk >= 3 (>= 3 sd from the mean for the species, genus, family)
workdata <- rtry_exclude(workdata, ErrorRisk >= 3, baseOn = ObsDataID)

# check
tmp_filtered <- rtry_explore(workdata, DataID, DataName, TraitID, TraitName, ErrorRisk, sortBy = ErrorRisk)

# Remove duplicates
workdata <- rtry_remove_dup(workdata)
# 327 duplicates removed.

# Exclude
# 1. All entries with "" in TraitID
# 2. Potential categorical traits that don't have a StdValue
# 3. Traits that have not yet been standardized in TRY
# Then select the relevant columns for transformation
# Note: The complete.cases() is used to ensure the cases are complete,
#       i.e. have no missing values

num_traits <- rtry_select_row(workdata, complete.cases(TraitID) & complete.cases(OrigValueStr))
# dim:   5804 17 
num_traits <- rtry_select_col(num_traits, ObservationID, AccSpeciesID, AccSpeciesName, TraitID, TraitName, StdValue, UnitName,
                              OriglName, OrigValueStr, OrigObsDataID)
# dim:   5804 8 
# col:   ObservationID AccSpeciesID AccSpeciesName TraitID TraitName StdValue UnitName 


# Extract the unique value of latitude (DataID 59) and longitude (DataID 60) together with the corresponding ObservationID
workdata_georef <- rtry_select_anc(workdata, 59, 60)
# dim:   3716 3 
# col:   ObservationID Latitude Longitude

workdata_organ <- rtry_select_anc(workdata, 1861)
workdata_ref <- rtry_select_anc(workdata, 113)
workdata_mat <- rtry_select_anc(workdata, 413)
workdata_country <- rtry_select_anc(workdata, 1412)

# To merge the extracted ancillary data with the numerical traits
# Merge the relevant data frames based on the ObservationID using rtry_join_left (left join)
# num_traits_georef <- rtry_join_left(num_traits, workdata_georef, baseOn = ObservationID)
num_traits_organ <- rtry_join_left(num_traits, workdata_organ, baseOn = ObservationID)
workdata_1 <- rtry_join_left(num_traits_organ, workdata_ref, baseOn = ObservationID)
workdata_2 <- rtry_join_left(workdata_1, workdata_mat, baseOn = ObservationID)
workdata_2 <- rtry_join_left(workdata_2, workdata_country, baseOn = ObservationID)
workdata_2 <- rtry_remove_dup(workdata_2)

workdata_2 <- workdata_2 %>%
  rename(Plant_organ = "Plant organ measured")

workdata_2 <- workdata_2 %>%
  mutate(Plant_organ = ifelse(OriglName %like% "Stem", "S",Plant_organ),
         Plant_organ = ifelse(OriglName %like% "Leaf", "L", Plant_organ),
         Plant_organ = ifelse(TraitName %like% "Leaf", "L", Plant_organ),
         Plant_organ = ifelse(Plant_organ == "P", "L", Plant_organ)) #,
         #Plant_organ = ifelse(Reference %like% "Choat", "S", Plant_organ))

workdata_2 <- workdata_2 %>%
  mutate(dat.type = ifelse(OriglName %like% "full turgor", "psi.ft",
                           ifelse(OriglName %like% "turgor loss", "psi.tlp",
                                  ifelse(OriglName %like% "P50", "P50",
                                          ifelse(OriglName %like% "P88", "P88", 
                                                 ifelse(TraitName %like% "full turgor", "psi.ft",
                                                        ifelse(TraitName %like% "turgor loss", "psi.tlp", 
                                                               ifelse(OriglName %like% "50", "P50",
                                                                      ifelse(OriglName %like% "12", "P12", 
                                                                             ifelse(OriglName %like% "P80", "P80",
                                                                                    ifelse(OriglName %like% "P20", "P20", NA)))))))))))
workdata_2 <- workdata_2 %>%
  mutate(OrigValueStr = as.numeric(OrigValueStr))

workdata_2 <- workdata_2 %>%
  mutate(OrigValueStr = ifelse(UnitName %like% "-Mpa", (-1*OrigValueStr), OrigValueStr),
         StdValue = ifelse(UnitName %like% "-Mpa", (-1*StdValue), StdValue),
         Source_file = "TRY data",
         Species_short = AccSpeciesName) %>%
  rename(Country = "Location Country",
         value = "OrigValueStr",
         Source = "Reference / source",
         Maturity = "Plant developmental status / plant age / maturity / plant life stage")
# 5804 obs

workdata_2 <- rtry_remove_dup(workdata_2)

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
  mutate(Last = word(Reference, 1, sep = " "),
         Plant_organ = "P")# change to comments to match other df
#480 obs


# Choat - Try merge -------------------------------------------------------
# list of family names from the data set 
family<-read.csv("Genus_Choat.csv",header=T)
family$Family<-trimws(family$Family,"both")

choat <- choat %>%
  mutate(Species_short = Species) %>%
  pivot_longer(cols = -c(Species, Location, Reference, Latitude,
                          Longitude, Country, Source, Species_short,
                          Reference, Comments, Plant_organ, Last),
                names_to = "dat.type", values_to = "value") %>%
  filter(!(dat.type == "P88" & is.na(value)))
# 662


# Create an empty dataframe to store the results
result_df <- data.frame(Species_short = character(), 
                        Listed_with_hirons_lon = character(),
                        Listed_with_choat = character(),
                        Listed_with_workdata_2 = character(),
                        Listed_with_bartlett = character(),
                        stringsAsFactors = FALSE)

# Loop through each species
for (Species_short in unique(c(hirons_lon$Species_short, choat$Species_short, workdata_2$Species_short, bartlett$Species_short))) {
  # Check if species is listed with certain data in each dataframe
  listed_with_hirons_lon <- ifelse(Species_short %in% hirons_lon$Species_short, "X", "")
  listed_with_choat <- ifelse(Species_short %in% choat$Species_short, "X", "")
  listed_with_workdata_2 <- ifelse(Species_short %in% workdata_2$Species_short, "X", "")
  listed_with_bartlett <- ifelse(Species_short %in% bartlett$Species_short, "X", "")
  
  # Append the result to the result dataframe
  result_df <- rbind(result_df, data.frame(Species_short = Species_short,
                                           Listed_with_hirons_lon = listed_with_hirons_lon, 
                                           Listed_with_choat = listed_with_choat, 
                                           Listed_with_workdata_2 = listed_with_workdata_2, 
                                           Listed_with_bartlett = listed_with_bartlett,
                                           stringsAsFactors = FALSE))
}
# 1212 obs

# Remove the first row (empty row) from result_df
result_df <- result_df[-1, ]

sum(rowSums(result_df[, c("Listed_with_hirons_lon", "Listed_with_choat", "Listed_with_workdata_2", "Listed_with_bartlett")] == "X") == 4)
# 5 species present in all 4 datasets.


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

hirons_lon<-hirons_lon %>%
  mutate(Species_short = ifelse(Species_short =="Ulmus",Species,Species_short ))#%>%
#  left_join(desig_list, by = "Species")
  
hirons_lon$Table <- ""

# Loop through each row in test
for (i in 1:nrow(hirons_lon)) {
  # Check if the Species names in the test df match any of the 
  # species names in the drought designation table
  if (hirons_lon$Species_short[i] %in% desig_list$Species_new) {
    hirons_lon$Table[i] <- "X"  # Mark 'X' in Table if there's a match
  }
}
# 90

# Include all of the elms
hirons<-hirons_lon%>%
  #  mutate(Table=ifelse("Ulmus" %in% Species_new,Table=="X",Table))
  mutate(Table = ifelse(grepl("ulmus", Species_short, ignore.case = TRUE), "X", Table))
# 383

hirons<-hirons%>%
  #filter(Table=="X")%>%
  mutate(Genus = str_split(Species, " ", simplify = TRUE)[, 1])%>%
  mutate(Genus = str_trim(Genus))
# 383 rows

hirons_des<-hirons %>% filter (Table == "X")
# 186
hirons_des%>%  
  filter(Country == "USA") %>%
  #group_by(Zone_2023)%>%
  summarise(Count_USDA = length(unique(Species)))
length(unique(hirons_des$Species[hirons_des$Country=="USA"]))
# 50 in the USA

hirons_des%>%  
  filter(Country == "USA") %>%
#  group_by(Zone_2023)%>%
  summarise(Count_USDA = length(unique(Species_short)))
# 45


length(unique(hirons_des$Species_short))
# 73 unique species total (not counting variety)
length(unique(hirons_des$Species))
# 82 with cultivar
length(unique(hirons_des$Genus))
# 26 unique genera
length(unique(hirons$Species_short))
# 160 unique species total (not counting variety)
length(unique(hirons$Species))
# 171 species with cultivar
length(unique(hirons$Genus))
# 36 unique genera


hirons_des%>%
  filter(Species_short %like% "Ulmus")%>%
  filter(Country=="USA")%>%
  summarise(Count = length(unique(Species_short)))

# 19 different elm species in the entire data set
# all were located in the USA as well.

# Graphing the Ptlp data - 43 species that intersect with our current list

tlp_des<-hirons_des %>%
  filter(dat.type == "psi.tlp") %>%
  group_by(Species_short)%>%
  summarise(Count = length(value),
            Mean_TLP=mean(value),
            SD_TLP=sd(value),
            SE_TLP= sd(value) / sqrt(n())) %>%
  mutate(Mean_TLP = round(Mean_TLP, 2),SD_TLP = round(SD_TLP,2),
         SE_TLP=round(SE_TLP,2))%>%
  left_join(hirons_des)#%>%
#  filter(Count>1)
## 73 unique species
ggplotly(ggplot(tlp_des,aes(x=reorder(Species_short, value), y=value))+
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
                      col=Exposition)))


ggplotly(ggplot(tlp_des,aes(x=reorder(Genus, value), y=value))+
           labs(x="Genera",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
           geom_boxplot(outlier.color=NA) + ggtitle("Drought Designations Genera - Hirons")+
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                 axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip()+#
           geom_point(aes(text = paste("<br>Species:",Species_short,"<br>Cultivar:",Cultivar,"<br>Country:",Country),
                          col=Exposition)))


# Graphing the Ptlp data - all species Andy had data for

tlp<-hirons %>%
  filter(dat.type == "psi.tlp") %>%
  group_by(Species_short)%>%
  summarise(Count = length(value),
            Mean_TLP=mean(value),
            SD_TLP=sd(value),
            SE_TLP= sd(value) / sqrt(n())) %>%
  mutate(Mean_TLP = round(Mean_TLP, 2),SD_TLP = round(SD_TLP,2),
         SE_TLP=round(SE_TLP,2))%>%
  left_join(hirons)#%>%
#  filter(Count>1)
ggplotly(ggplot(tlp,aes(x=reorder(Species_short, value), y= value))+
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
                          col=Exposition)))


ggplotly(ggplot(tlp,aes(x=reorder(Genus, value), y= value))+
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
                          col=Exposition)))

# Combining Data Frames-------------------------------------

# Data with TLP and FT - hirons_lon (long) and subset_l (long)
# Data with P50 and P88 - choat_try (wide) OR choat_lon (long)


# make choat_try long
#choat_lon <-choat %>%
# mutate(value = as.numeric(value)) %>%
# pivot_longer(cols = -c(1:11),
#              names_to = "dat.type", values_to = "value") %>%
# rename(Exposition = Location)%>%
#  filter(!is.na(value))

# We want to see what data we have for what species.
# Make an ugly, long dataframe that can be summarized.

### 25/3/2024: Start here tomorrow. Looks like the bind_rows did not properly
# work. Column called dat.type and DataName, not all psi.ft or tlp data
# is included in the graphs.



hirons_lon <- hirons_lon %>%
  mutate(Plant_organ = "P")

workdata_2 <- workdata_2 %>%
  rename(Species = AccSpeciesName,
         Reference = Source) %>%
  mutate(Source = "TRY data")
# combine rows
combined_long <- bind_rows(hirons_lon, workdata_2)
# 6187

names(combined_long)
bartlett <- bartlett %>%
  mutate(Source = "Bartlett 2012") %>%
  pivot_longer(cols = -c(1,2,5,6), 
               names_to = "dat.type",
               values_to = "value") %>%
  mutate(Plant_organ = "P")

combined_long <- bind_rows(choat, bartlett, combined_long) # 7373  
# include on plot: R, P, N. 

combined_long <- combined_long %>% mutate(value = round(value, 2),
                                          Last = word(Reference, 1, sep = " "))
# 7373
summary <- combined_long %>%
  group_by(Species_short, dat.type) %>%
  summarize(Count = n()) %>%
  pivot_wider(names_from = dat.type, values_from = Count)
# 791 with bartlett
# 790 without bartlett
summary <- summary %>%
  mutate(Table = if_else(Species_short %in% desig$Species, "X", ""))

# remove duplicates

combined_long<- combined_long [!duplicated(combined_long[c("Species",
                                                             "Last",
                                                             "dat.type",
                                                             "value")]),]
# 5863

# Next steps. Calculate TLP from the FT values where needed. Intersect the species
# list with the drought designations table and populate columns where data exists. 



# Calculating PSI TLP from PSI FT -----------------------------------------

# Using the equation Andy Hirons used, we are calculating PSI TLP from PSI FT
# only for the plants with values recorded for either the petiole or leaf
sus <- combined_long %>%
  filter(value > -20 & value <= 0) %>%
  filter(Plant_organ == "P" | Plant_organ == "L")
#           Reference != "Ogaya, R. and J. Penuelas. 2003. Comparative field study of Quercus ilex and Phillyrea latifolia: photosynthetic response to experimental drought conditions. Environmental and Experimental Botany 50:137-148.")
# 1795

turgorpt <- sus %>%
  filter(dat.type == "psi.tlp" | dat.type == "psi.ft") %>% 
  select(Species_short, dat.type, value, Source, Reference,Plant_organ)# %>%
# 1110
#pivot_wider(names_from = dat.type, values_from = value)

# figure out what species I have psi.ft values for and not tlp values.
df_calc <- turgorpt %>%
  group_by(Species_short, dat.type) %>%
  summarise(num_values = n()) %>%
  pivot_wider(names_from = dat.type, values_from = num_values) %>%
  filter(is.na(psi.tlp)) %>%
  select(Species_short) %>% # 41 species with ft and not tlp
  inner_join(., combined_long, by = "Species_short") %>%
  filter(dat.type == "psi.ft") # 41

tlp_dat <- df_calc %>%
  rename(psi.ft = value) %>%
  select(-dat.type) %>%
  mutate(psi.tlp = -0.2554 + (1.1243 * psi.ft)) #%>% # Calculation from Hirons et al 2020
tlp_dat<- tlp_dat %>%
  pivot_longer(cols = -c(1:11,13:25), 
               names_to = "dat.type",
               values_to = "value") 
tlp_dat <- tlp_dat %>%
  filter(dat.type == "psi.tlp") # 41
# long data frame with TLP and FT values and corresponding metadata.
# need to combine this back with the "sus" data frame
# i think it worked
 
# repeat for full turgor data

df_calc_ft <- turgorpt %>%
  group_by(Species_short, dat.type) %>%
  summarise(num_values = n()) %>%
  pivot_wider(names_from = dat.type, values_from = num_values) %>%
  filter(is.na(psi.ft)) %>%
  select(Species_short) %>% # 231 species with tlp and not ft
  inner_join(., combined_long, by = "Species_short") %>%
  filter(dat.type == "psi.tlp") #  231

ft_dat <- df_calc_ft %>%
  rename(psi.tlp = value) %>%
  select(-dat.type) %>%
  mutate(psi.ft = (psi.tlp+0.2554)/1.1243) #%>% # Calculation from Hirons et al 2020
# 231
ft_dat<- ft_dat %>%
  pivot_longer(cols = -c(1:11,13:25), 
               names_to = "dat.type",
               values_to = "value") 
# 462
ft_dat <- ft_dat %>%
  filter(dat.type == "psi.ft") # 231




turgor_dat <- bind_rows(tlp_dat, combined_long,ft_dat) # bind rows to add back in new calculated data
# 7645 rows
turgor_dat <- turgor_dat %>%
  distinct() #
# 7644

# double check what is duplicated.
duplicated_rows <- duplicated(turgor_dat)
# Select duplicated rows from 'sus'
duplicated_sus <- turgor_dat[duplicated_rows, ]

elms <- combined_long %>%
  filter(Species_short %like% "Ulmus")

desig_table <- desig_list %>%
  rename(Species_short = Species) %>%
  left_join(.,turgor_dat) %>% #%>% # 1561 observations
#  filter(dat.type == "psi.tlp" | dat.type == "psi.ft") # 1344 obs
  mutate(value = ifelse(value > 0, (value*-1), value))
# get elm data back in
desig_table <- bind_rows(desig_table, elms)


### END HERE ON 6/24. need to double check the left_join is doing what we are
# hoping for. 


#get rid of duplicates
desig_table <- desig_table %>% filter(!duplicated(desig_table))
# 1344
# calculate mean and SD for each species based on data type
sum_desig_table<- desig_table %>%
  filter(Plant_organ == "P" | Plant_organ == "L" | is.na(Plant_organ)) %>%
  select(Species_short,dat.type, value) %>%
  group_by(Species_short, dat.type) %>%
  mutate(Mean_Value = round(mean(value),2),
         SD_Value = round(sd(value),2),
         Count = n()  # Calculate the count of points in each group
  ) %>%
  select(-value) %>%
  distinct(.) %>% #733
  pivot_wider(.,names_from = dat.type, values_from = c(Mean_Value,SD_Value, Count)) %>%
  select(-Mean_Value_NA, -SD_Value_NA, -Count_NA) 
#190

# TDAG associations for Psi turgor loss point
# Sensitive: > -2.5 MPa
# Moderately Sensitive -2.5 MPa to -3 MPa
# Moderately Tolerant -3 MPa to -3.5 MPa
# Tolerant < -3.5 MPa

# if else statements to create column of assigned drought designations using the above parameters
sum_desig_table$TDAG_UED <- ifelse(sum_desig_table$Mean_Value_psi.tlp > -2.5, "Sensitive",
                                   ifelse((sum_desig_table$Mean_Value_psi.tlp >= -3 & sum_desig_table$Mean_Value_psi.tlp <= -2.5), "Moderately Sensitive",
                                          ifelse((sum_desig_table$Mean_Value_psi.tlp >= -3.5 & sum_desig_table$Mean_Value_psi.tlp < -3), "Moderately Tolerant",
                                                 ifelse(sum_desig_table$Mean_Value_psi.tlp < -3.5, "Tolerant", NA))))
# combine data into one cell for the table 
# this gives weird NA ( NA ) as character strings
sum_desig_table <- sum_desig_table %>%  
  mutate(P50 = paste(Mean_Value_P50, "(", SD_Value_P50, ")"),
         P88 = paste(Mean_Value_P88, "(", SD_Value_P88, ")"),
         psi.tlp = paste(Mean_Value_psi.tlp, "(", SD_Value_psi.tlp, ")"),
         psi.ft = paste(Mean_Value_psi.ft, "(", SD_Value_psi.ft, ")")) %>%
  select(-2:-11)

# get rid of the weird  NA ( NA ) 
sum_desig_table$P50 <- ifelse(sum_desig_table$P50 == "NA ( NA )", NA, sum_desig_table$P50)
sum_desig_table$P88 <- ifelse(sum_desig_table$P88 == "NA ( NA )", NA, sum_desig_table$P88)
sum_desig_table$psi.tlp <- ifelse(sum_desig_table$psi.tlp == "NA ( NA )", NA, sum_desig_table$psi.tlp)
sum_desig_table$psi.ft <- ifelse(sum_desig_table$psi.ft == "NA ( NA )", NA, sum_desig_table$psi.ft)

# pull out data for elms so rejoin later
elm <- sum_desig_table %>%
  filter(Species_short %like% "Ulmus") %>%
  rename(Species = Species_short)

# join summary of PSI TLP and PSI FT data to the original
# desig table created by Brendon, Jake, and UED team
desig_table_com <- sum_desig_table %>%
  rename(Species = Species_short) %>%
  right_join(desig)

# bring back elm data
desig_table_com <- desig_table_com %>%
  bind_rows(., elm)

desig_table_com <- desig_table_com %>%
  mutate(psi.tlp = ifelse(psi.tlp > 0, (psi.tlp(-1)), psi.tlp))
# 199

# write out csv file for the team
# write.csv(desig_table_com,"droughtdesignations_table_2024_03May.csv", row.names = F)




# Plots for Luke ----------------------------------------------------------
desig_table_com

turgor_dat <- turgor_dat %>%
  mutate(o_type = ifelse(Plant_organ == "S" & dat.type == "P50", "S_P50",
                         ifelse(Plant_organ == "P" & dat.type == "P50","P_P50",
                                ifelse(Plant_organ == "S" & dat.type == "P88", "S_P88",
                                ifelse(Plant_organ == "P" & dat.type == "P88", "P_P88",dat.type)))))

# Filter Out Outliers

# filter out all values that seem suspicious < -20
sus <- turgor_dat %>%
  filter(value > -20 & value < 0 &
           Reference != "Ogaya, R. and J. Penuelas. 2003. Comparative field study of Quercus ilex and Phillyrea latifolia: photosynthetic response to experimental drought conditions. Environmental and Experimental Botany 50:137-148.")

# Plot for TLP and P50, IF there are multiple values per species, calculated
# mean values and plotted those. 
a_df<-sus %>%
  filter(o_type == "S_P50" | o_type == "psi.tlp") %>%
  select(Species_short,o_type, value) %>%
  group_by(Species_short, o_type) %>%
  mutate(Mean_Value = mean(value),
         SD_Value = sd(value),
         Count = n()
  ) %>%
  select(-value) %>%
  distinct(.) %>%
  pivot_wider(.,names_from = o_type, values_from = c(Mean_Value,SD_Value, Count)) %>%
  filter(!is.na(Mean_Value_S_P50),!is.na(Mean_Value_psi.tlp))
# 45

correlation <- cor(a_df$Mean_Value_S_P50, a_df$Mean_Value_psi.tlp)
# r = 0.29
p_value <- cor.test(a_df$Mean_Value_S_P50, a_df$Mean_Value_psi.tlp)$p.value
# p = 0.05
shapiro.test((a_df$Mean_Value_S_P50))
# normal
shapiro.test(a_df$Mean_Value_psi.tlp)
# normal, p = 0.5615

# try for spearman correlation
#correlation <- cor.test(a_df$Mean_Value_P50, a_df$Mean_Value_psi.tlp, type='pearson')$estimate
# r = 0.21
#p_value <- cor.test(a_df$Mean_Value_P50, a_df$Mean_Value_psi.tlp, type='spearman')$p.value
# p = 0.20
# value is IDENTICAL for spearman and pearson



a <- ggplot(a_df, aes(x = Mean_Value_S_P50, y = Mean_Value_psi.tlp)) + geom_point()+
  geom_smooth(method = "lm", color = "blue") +
  #geom_point(aes(text = paste("<br>Species:",Species_short,
  #                            "<br>Country:",Country)))+
  theme_classic()+theme(axis.text.y=element_text(color="black",
                                                 size=10,vjust=0.5,
                                                 hjust=1),
                        axis.text.x=element_text(color="black",size=10),
                        axis.ticks=element_line(color="black"),
                        axis.title=element_text(size=10),
                        legend.position="none") + 
  xlab("") + ylab("Water Potential at TLP (MPa)") +
  annotate("text", x = min(a_df$Mean_Value_S_P50), y = max(a_df$Mean_Value_psi.tlp), 
           label = paste("r(s):", round(correlation, 2), "\n", 
                         "p:", round(p_value, 3), "\n",
                         "n:", 45),
           hjust = 1, vjust = 1) +
  geom_errorbar(
    aes(ymin = a_df$Mean_Value_psi.tlp - a_df$SD_Value_psi.tlp,
        ymax = a_df$Mean_Value_psi.tlp + a_df$SD_Value_psi.tlp),
    width = 0.1,
    color = "black"
  ) +
  geom_errorbarh(
    aes(xmin = a_df$Mean_Value_S_P50 - a_df$SD_Value_S_P50,
        xmax = a_df$Mean_Value_S_P50 + a_df$SD_Value_S_P50),
    height = 0.1,
    color = "black"
  ) 
# 45 points

# Plot for TLP and P88, IF there are multiple values per species, calculated
# mean values and plotted those. 
b_df<- sus %>%
  filter(o_type == "S_P88" | o_type == "psi.tlp") %>%
  select(Species_short,o_type, value) %>%
  group_by(Species_short, o_type) %>%
  mutate(Mean_Value = mean(value),
         SD_Value = sd(value),
         Count = n()
  ) %>%
  select(-value) %>%
  distinct(.) %>%
  pivot_wider(.,names_from = o_type, values_from = c(Mean_Value,SD_Value,Count)) %>%
  filter(!is.na(Mean_Value_S_P88),!is.na(Mean_Value_psi.tlp))
# 51

correlation <- cor(b_df$Mean_Value_S_P88, b_df$Mean_Value_psi.tlp)
# r = 0.21
p_value <- cor.test(b_df$Mean_Value_S_P88, b_df$Mean_Value_psi.tlp)$p.value
# p = 0.23
shapiro.test((b_df$Mean_Value_S_P88))
# not normal
shapiro.test(b_df$Mean_Value_psi.tlp)
# normal

# try for spearman correlation
correlation <- cor.test(b_df$Mean_Value_S_P88, b_df$Mean_Value_psi.tlp, type='pearson')$estimate
# r = 0.21
p_value <- cor.test(b_df$Mean_Value_S_P88, b_df$Mean_Value_psi.tlp, type='spearman')$p.value
# p = 0.23
# value is IDENTICAL for spearman and pearson


b <- ggplot(b_df, aes(x = Mean_Value_S_P88, y = Mean_Value_psi.tlp)) + geom_point()+
  geom_smooth(method = "lm", color = "blue") +
  theme_classic()+theme(axis.text.y=element_text(color="black",
                                                 size=10,vjust=0.5,
                                                 hjust=1),
                        axis.text.x=element_text(color="black",size=10),
                        axis.ticks=element_line(color="black"),
                        axis.title=element_text(size=10),
                        legend.position="none") + xlab("") + ylab("") + 
  annotate("text", x = min(b_df$Mean_Value_S_P88+1), y = max(b_df$Mean_Value_psi.tlp), 
           label = paste("r(s):", round(correlation, 2), "\n", 
                         "p:", round(p_value, 3), "\n",
                         "n:", 51),
           hjust = 1, vjust = 1) +
  geom_errorbar(
    aes(ymin = b_df$Mean_Value_psi.tlp - b_df$SD_Value_psi.tlp,
        ymax = b_df$Mean_Value_psi.tlp + b_df$SD_Value_psi.tlp),
    width = 0.1,
    color = "black"
  ) +
  geom_errorbarh(
    aes(xmin = b_df$Mean_Value_S_P88 - b_df$SD_Value_S_P88,
        xmax = b_df$Mean_Value_S_P88 + b_df$SD_Value_S_P88),
    height = 0.1,
    color = "black"
  ) 

# no apparent pattern
# positive trend, doesn't seem strong.

# Plot for FT and P50, IF there are multiple values per species, calculated
# mean values and plotted those. 
c_df<- sus %>%
  filter(o_type == "S_P50" | o_type == "psi.ft") %>%
  select(Species_short,o_type, value) %>%
  group_by(Species_short, o_type) %>%
  mutate(Mean_Value = mean(value),
         SD_Value = sd(value),
         Count = n(), # Calculate the count of points in each group
  )%>%
  select(-value) %>%
  distinct(.) %>% #733
  pivot_wider(.,names_from = o_type, values_from = c(Mean_Value,SD_Value, Count)) %>%
  filter(!is.na(Mean_Value_S_P50),!is.na(Mean_Value_psi.ft))



correlation <- cor(c_df$Mean_Value_S_P50, c_df$Mean_Value_psi.ft)
# r = 0.24
p_value <- cor.test(c_df$Mean_Value_S_P50, c_df$Mean_Value_psi.ft)$p.value
# p = 0.15
shapiro.test((c_df$Mean_Value_S_P50))
# normal
shapiro.test(c_df$Mean_Value_psi.ft)
# normal

# try for spearman correlation
#correlation <- cor.test(c_df$Mean_Value_P50, c_df$Mean_Value_psi.ft, type='pearson')$estimate
# r = 0.50
#p_value <- cor.test(c_df$Mean_Value_P50, c_df$Mean_Value_psi.ft, type='spearman')$p.value
# p = 0.002
# value is IDENTICAL for spearman and pearson


c <- ggplot(c_df, aes(x = Mean_Value_S_P50, y = Mean_Value_psi.ft)) + geom_point()+
  geom_smooth(method = "lm", color = "blue") +
  theme_classic()+theme(axis.text.y=element_text(color="black",
                                                 size=10,vjust=0.5,
                                                 hjust=1),
                        axis.text.x=element_text(color="black",size=10),
                        axis.ticks=element_line(color="black"),
                        axis.title=element_text(size=10),
                        legend.position="none") + 
  xlab("Stem P50 (MPa)") + ylab("Water Potential at Full Turgor (MPa)") +
  annotate("text", x = min(c_df$Mean_Value_S_P50), y = max(c_df$Mean_Value_psi.ft), 
           label = paste("r(s):", round(correlation, 2), "\n", 
                         "p:", round(p_value, 3), "\n",
                         "n:", 43),
           hjust = 1, vjust = 1) +
  geom_errorbar(
    aes(ymin = c_df$Mean_Value_psi.ft - c_df$SD_Value_psi.ft,
        ymax = c_df$Mean_Value_psi.ft + c_df$SD_Value_psi.ft),
    width = 0.1,
    color = "black"
  ) +
  geom_errorbarh(
    aes(xmin = c_df$Mean_Value_S_P50 - c_df$SD_Value_S_P50,
        xmax = c_df$Mean_Value_S_P50 + c_df$SD_Value_S_P50),
    height = 0.1,
    color = "black"
  ) 


# Plot for FT and P88, IF there are multiple values per species, calculated
# mean values and plotted those. 
d_df<- sus %>%
  filter(o_type == "S_P88" | o_type == "psi.ft") %>%
  select(Species_short,o_type, value) %>%
  group_by(Species_short, o_type) %>%
  mutate(Mean_Value = mean(value),
         SD_Value = sd(value),
         Count = n()  # Calculate the count of points in each group
  ) %>%
  select(-value) %>%
  distinct(.) %>%
  pivot_wider(.,names_from = o_type, values_from = c(Mean_Value,SD_Value, Count)) %>%
  filter(!is.na(Mean_Value_S_P88),!is.na(Mean_Value_psi.ft))


correlation <- cor(d_df$Mean_Value_S_P88, d_df$Mean_Value_psi.ft)
# r = 0.28
p_value <- cor.test(d_df$Mean_Value_S_P88, d_df$Mean_Value_psi.ft)$p.value
# p = 0.10
shapiro.test((d_df$Mean_Value_S_P88))
# not normal
shapiro.test(d_df$Mean_Value_psi.ft)
# normal


# try for spearman correlation
correlation <- cor.test(d_df$Mean_Value_S_P88, d_df$Mean_Value_psi.ft, type='pearson')$estimate
# r = 0.28
p_value <- cor.test(d_df$Mean_Value_S_P88, d_df$Mean_Value_psi.ft, type='spearman')$p.value
# p = 0.10
# value is IDENTICAL for spearman and pearson

d<-ggplot(d_df, aes(x = Mean_Value_S_P88, y = Mean_Value_psi.ft)) + geom_point()+
  geom_smooth(method = "lm", color = "blue") +
  theme_classic()+theme(axis.text.y=element_text(color="black",
                                                 size=10,vjust=0.5,
                                                 hjust=1),
                        axis.text.x=element_text(color="black",size=10),
                        axis.ticks=element_line(color="black"),
                        axis.title=element_text(size=10),
                        legend.position="none") + 
  xlab("Stem P88 (MPa)") + ylab("") +
  annotate("text", x = min(d_df$Mean_Value_S_P88+1), y = max(d_df$Mean_Value_psi.ft), 
           label = paste("r(s):", round(correlation, 2), "\n", 
                         "p:", round(p_value, 3), "\n",
                         "n:", 48),
           hjust = 1, vjust = 1) +
  geom_errorbar(
    aes(ymin = d_df$Mean_Value_psi.ft - d_df$SD_Value_psi.ft,
        ymax = d_df$Mean_Value_psi.ft + d_df$SD_Value_psi.ft),
    width = 0.1,
    color = "black"
  ) +
  geom_errorbarh(
    aes(xmin = d_df$Mean_Value_S_P88 - d_df$SD_Value_S_P88,
        xmax = d_df$Mean_Value_S_P88 + d_df$SD_Value_S_P88),
    height = 0.1,
    color = "black"
  ) 

ggplotly(c)
ggarrange(a, b, c, d, ncol = 2, nrow = 2)



# ERA5 Data ---------------------------------------------------------------

# on 19 March 2024, I downloaded the last 30 years of temperature and 
# precipitation data from ERA5 per Trent Ford's recommendation (see below for 
# communication)

# "I *think* I recommended a global temperature and precipitation data set 
# that you could easily download/query to compare Chicago's climate to the 
# climates of areas where you have data. My favorite global data set is ERA5, 
# and you can download monthly data - including temperature, precipitation, etc.
# - either globally or for smaller regions here:
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=form 
# There are also Python and R scripts already made to help batch download if 
# you need to. My recommendation is to just use the last 30 years or so of 
# data to generalize climate of areas where you have data." ~Trent 20 Feb 2024

# Test downloaded the IL area and the file is stored in the 
# Data for R folder on the drive.


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
#202 

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




