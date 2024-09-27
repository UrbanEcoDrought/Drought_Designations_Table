# UED - Drought Designations Table
    # Updated by - A. Tumino on 24Sept2024

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
library(readr)
library(readxl)

setwd("G:/Shared drives/Urban Ecological Drought/Drought Tolerance Designations/PSI-TLP Data/Data for R_do not edit")

coords<-read.csv("USA_LatLongs.csv",header=T)
coords<-coords%>%
 mutate(Latitude = round(Latitude,4),
        Longitude = round(Longitude, 4))%>%
 separate(USDA_title_23, into = c("Zone_2023", "TempRange_F"), sep = ": ", remove = FALSE)%>%
 mutate(Zone_2023 = str_trim(Zone_2023),TempRange_F=str_trim(TempRange_F))

# Hirons et al. 2020 data merge with Sjoman Data-------------------------------------------------
jakedat <- read_csv("Jake summary data.csv") %>%
  mutate(
    Species = str_trim(if_else(species == "Quercus muhlenbergii", "Quercus muehlenbergii", species), side = "right"), # Fix spelling
    Reference = "Hirons et al. 2020", # Add source information
    Species_short = str_trim(if_else(str_detect(Species, "'"), str_split(Species, pattern = "'", simplify = TRUE)[, 1], Species), side = "right"),
    Cultivar = if_else(str_detect(Species, "'"), str_split(Species, pattern = "'", simplify = TRUE)[, 2], NA_character_),
    psi.tlp = round(sum.tlp, 2),
    psi.ft = round(sum.opft, 2)
  ) %>%
  transmute(Species, garden, Reference, Species_short, Cultivar, psi.tlp, psi.ft)


# Add in lat long and country for the botanic gardens listed
botanicgardens<-data.frame(
  garden = c("Alnarp","Wespelaar","Cornell","Kew","Morton","Ness","Savill"),
  Country = c("Sweden","Belgium","USA","UK","USA","UK","UK"),
  Latitude = c(55.6578147740059, 50.95864872436378, 42.45249241332518, 51.48381159036358, 41.81650499204021, 53.27250615951263, 51.4267556117446),
  Longitude = c(13.082413825025307, 4.634059925481531, -76.45488406152685, -0.2897730340057771, -88.06907647320472, -3.0430448608760723, -0.5976980493519929)
)

jakedat <- jakedat %>%
  left_join(botanicgardens) %>% # Combine with botanicgardens
  mutate(
    Longitude = round(Longitude, 4),
    Latitude = round(Latitude, 4)
  )

# make long df
jakedat_lon <- jakedat %>%
  pivot_longer(
    cols = -c(Species, garden, Reference, Species_short, Cultivar, Country, Latitude, Longitude),
    names_to = "dat.type",
    values_to = "value"
  ) %>%
  rename(Exposition = garden) %>%
  mutate(
    Maturity = NA,
    Comments = NA,
    Reference_datfile = "jakedat"
  )

# 300 obs

compiled <- read_csv("TRY Data Sjöman-Hirons Leaf Turgor Loss .csv") %>%
  rename(Reference = "Source") %>% # Rename column
  mutate(
    Species = str_trim(if_else(Species == "Quercus muhlenbergii", "Quercus muehlenbergii", Species), side = "right"), # Fix spelling
    Reference_datfile = "compiled", # Add source column
    Species_short = str_trim(if_else(str_detect(Species, "'"), str_split(Species, pattern = "'", simplify = TRUE)[, 1], Species), side = "right"),
    Cultivar = if_else(str_detect(Species, "'"), str_split(Species, pattern = "'", simplify = TRUE)[, 2], NA_character_)
  ) %>%
  rename(
    psi.ft = Osmotic_potential_full_turgor_MPa,
    psi.tlp = Leaf_turgor_loss_pt_MPa
  ) %>%
  select(Species, Species_short, Exposition, Maturity, Reference, Reference_datfile, Cultivar, psi.ft, psi.tlp, Comments) # Select relevant columns

compiled_lon <- compiled %>%
  pivot_longer(
    cols = -c(Species, Exposition, Reference, Reference_datfile,Species_short, Cultivar, Reference, Comments, Maturity),
    names_to = "dat.type",
    values_to = "value"
  ) %>%
  mutate(
    Country = NA,
    Latitude = NA,
    Longitude = NA,
    Reference_datfile = "compiled"
  )
# 142 obs

# make into one long DF
# Because all of this data is from Andy Hirons, there are duplicated rows across the two
# datasets, where the jakedat_lon has more complete information.
# The below code should combine columns where they match and then remove extra columns. 
# As much origin information was retained by combining the Source and Reference columns to reflect
# where the information came from.


hirons_lon <- full_join(jakedat_lon, compiled_lon, by = c("Species", "dat.type")) 

# if there are two rows with the exact same Species and dat.type values, 
## then 1) average the value
##      2) coalesce the rows.


hirons_lon <- hirons_lon %>%
  mutate(
    value = round(rowMeans(select(., starts_with("value")), na.rm = TRUE),2),
    Exposition = coalesce(Exposition.x, Exposition.y),
    Country = coalesce(Country.x, Country.y),
    Latitude = coalesce(Latitude.x, Latitude.y),
    Longitude = coalesce(Longitude.x, Longitude.y),
    Maturity = coalesce(Maturity.x,Maturity.y),
    Species_short = coalesce(Species_short.x, Species_short.y),
    Comments = coalesce(Comments.x, Comments.y),
    Cultivar = coalesce(Cultivar.x, Cultivar.y),
    Reference_datfile = coalesce(Reference_datfile.x, Reference_datfile.y),
    #Source = paste(Source.x, Source.y, sep = ", "),
    Reference = paste(Reference.x, Reference.y, sep = ", ")
  ) %>%
  select(-matches("\\.x$"), -matches("\\.y$")) %>%
  mutate_at("Reference", str_replace, "NA, ", "") %>%
  mutate_at("Reference", str_replace, ", NA", "") %>%
  mutate(Source = "Hirons & Sjoman (2015, 2018, 2020)")
# 364 
  
# Google Sheets -----------------------------------------------------------
desig<-data.frame(read_sheet(ss="https://docs.google.com/spreadsheets/d/1Ap2zxfzQ2tA7Vw2YFKWG5WlASvh_DakGJGc-i07YDOo/edit#gid=2107989582"))
2
desig_list <- desig %>%
  select(Species) %>%
  mutate(
    Species = if_else(Species == "Platanus x acerifolia (syn hispanica)", "Platanus x acerifolia", Species),
    Species_new = Species
  ) %>%
  filter(!row_number() %in% c(158, 160:162, 176:179)) %>%
  as.data.frame()




# Bartlett data -----------------------------------------------------------
bartlett <-read.csv("Bartlett 2012 - supplemental data.xlsx - Data.csv", header = T)
head(bartlett)
str(bartlett)

bartlett <- bartlett %>%
  filter(Growth.form == "W") %>%
  select(Species,Biomes,psi.ft, psi.tlp, Reference) %>%
  rename(Exposition = Biomes, Species_short = Species, Source = Reference) %>%
  mutate(Reference_datfile = "bartlett", Reference = "Bartlett 2012")

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
workdata <- rtry_select_col(workdata,Dataset, LastName, ObsDataID, ObservationID, 
                            AccSpeciesID, AccSpeciesName, ValueKindName, 
                            TraitID, TraitName, DataID, DataName, OriglName, 
                            OrigValueStr, OrigUnitStr, StdValue, UnitName, 
                            OrigObsDataID, ErrorRisk, Comment, Reference)

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
                              OriglName, OrigValueStr, OrigObsDataID, Dataset, LastName, Reference)
# dim:   5804 10
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
         Plant_organ = ifelse(TraitName %like% "Leaf", "L", Plant_organ))#,
        # Plant_organ = ifelse(Plant_organ == "P", "L", Plant_organ)) #,
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
         Reference_datfile = "TRY data",
         Species_short = AccSpeciesName) %>%
  rename(Country = "Location Country",
         value = "OrigValueStr",
         Source = "Reference / source",
         Maturity = "Plant developmental status / plant age / maturity / plant life stage")
# 5804 obs

workdata_2 <- rtry_remove_dup(workdata_2)

workdata_2 <- workdata_2 %>%
  mutate(Reference = ifelse(is.na(Reference), LastName, Reference)) %>%
  mutate(Reference = ifelse(Reference == "unpub.", LastName, Reference)) %>%
  mutate(Source = ifelse(is.na(Source), Reference, Source))

# 5804 obs

#checksource <- TRYdata %>%
#  select(DatasetID, Reference) %>%
#  distinct() %>%
#  write.csv(., "TRY Data Source List.csv", row.names=F)

# Choat et al. 2012 data --------------------------------------------------


choat<-read.csv("Choat et al. 2012, Supplementary data tabla_ACT.csv",header=T)
choat$Plant_organ <- ifelse(choat$pmin == "Leaf water potential", "L", "Xylem")
choat<-choat%>%
  select(Species,p50,p88,Location,Country,Reference,Latitude,Longitude,Notes, Plant_organ)%>%
  rename(Source = Reference) %>%
  mutate(Reference = "Choat et al. 2012",
         p50=round(p50,2),
         Latitude = round(Latitude, 4),
         Longitude = round(Longitude, 4))%>% # add source column
  rename(Comments="Notes",
         P88 = p88,
         P50 = p50)%>%
  mutate(Last = word(Reference, 1, sep = " "),
         Reference_datfile = "choat")
#480 obs


# Choat - Try merge -------------------------------------------------------
# list of family names from the data set 
family<-read.csv("Genus_Choat.csv",header=T)
family$Family<-trimws(family$Family,"both")

choat <- choat %>%
  mutate(Species_short = Species) %>%
  pivot_longer(cols = -c(Species, Location, Reference, Latitude,
                          Longitude, Country, Source, Species_short,Reference,
                          Reference, Comments, Plant_organ, Last, Reference_datfile),
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

# Root Lab Summer 2024 Data -----------------------------------------------
#root <- read_excel(path = "G:/Shared drives/Urban Ecological Drought/Sentinel Species/Duong REU 2024/Data/2024 REU Ultimate VAPRO Data Sheet.xlsx", 
#                sheet = "Samples Species VAPRO")
root<-read.csv("2024_VAPRO_RootLab.csv",header=T)
names(root)

root <- root %>%
  select(2,4:7,32:34) %>%
  group_by(Accession) %>%
  mutate(psi.tlp = mean(`PsiP0..MPa.`),
         psi.ft = mean(`PsiPi100..MPa.`)) %>%
  ungroup() %>%
  select(!c(`PsiPi100..MPa.`,`PsiP0..MPa.`)) %>%
  mutate(Location = "Morton",
         Country = "USA",
         Source = "Root Lab VAPRO",
         Species_short = SpeciesName,
         Plant_organ = "L",
         Reference_datfile = "root",
         Reference = "Root Lab") %>%
  rename(Comments = Note,
         Species = SpeciesName) %>%
  pivot_longer(.,cols = c("psi.tlp","psi.ft"), names_to = "dat.type", values_to = "value") %>%
  distinct()

# How many species are in all 5 datasets? ---------------------------------

# Loop through each species
for (Species_short in unique(c(hirons_lon$Species_short, choat$Species_short, workdata_2$Species_short, bartlett$Species_short,
                               root$Species_short))) 
  {
  # Check if species is listed with certain data in each dataframe
  listed_with_hirons_lon <- ifelse(Species_short %in% hirons_lon$Species_short, "X", "")
  listed_with_choat <- ifelse(Species_short %in% choat$Species_short, "X", "")
  listed_with_workdata_2 <- ifelse(Species_short %in% workdata_2$Species_short, "X", "")
  listed_with_bartlett <- ifelse(Species_short %in% bartlett$Species_short, "X", "")
  listed_with_root <- ifelse(Species_short %in% root$Species_short, "X", "")  
  # Append the result to the result dataframe
  result_df <- rbind(result_df, data.frame(Species_short = Species_short,
                                           Listed_with_hirons_lon = listed_with_hirons_lon, 
                                           Listed_with_choat = listed_with_choat, 
                                           Listed_with_workdata_2 = listed_with_workdata_2, 
                                           Listed_with_bartlett = listed_with_bartlett,
                                           Listed_with_root = listed_with_root,
                                           stringsAsFactors = FALSE))
}
# 1219 obs


# Sum of rows where all specified columns have "X"
sum(rowSums(result_df[, c("Listed_with_hirons_lon", 
                          "Listed_with_choat", 
                          "Listed_with_workdata_2", 
                          "Listed_with_bartlett", 
                          "Listed_with_root")] == "X") == 2)
# No species present in all 5
# 10 species listed in at least 4
# 69 species listed in at least 3
# 396 species listed in at least 2


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
# 364

hirons<-hirons%>%
  #filter(Table=="X")%>%
  mutate(Genus = str_split(Species, " ", simplify = TRUE)[, 1])%>%
  mutate(Genus = str_trim(Genus))
# 364 rows

hirons_des<-hirons %>% filter (Table == "X")
# 172
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
# 79 with cultivar
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
# Old plots of dat in Hirons data
#ggplotly(ggplot(tlp_des,aes(x=reorder(Species_short, value), y=value))+
#           labs(x="Species",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
#           geom_boxplot() + ggtitle("Drought Designations Species - Hirons")+
#           theme_classic()+theme(axis.text.y=element_text(color="black",
#                                                          size=10,vjust=0.5,
#                                                          hjust=1,face="italic"),
#                                axis.text.x=element_text(color="black",size=10),
#                                 axis.ticks=element_line(color="black"),
#                                axis.title=element_text(size=10),
#                                 legend.position="none")+ coord_flip()+
#           geom_point(aes(text = paste("<br>Cultivar:",Cultivar,"<br>Country:",Country),
#                      col=Exposition)))


#ggplotly(ggplot(tlp_des,aes(x=reorder(Genus, value), y=value))+
#           labs(x="Genera",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
#           geom_boxplot(outlier.color=NA) + ggtitle("Drought Designations Genera - Hirons")+
#          theme_classic()+theme(axis.text.y=element_text(color="black",
#                                                         size=10,vjust=0.5,
#                                                         hjust=1,face="italic"),
#                                axis.text.x=element_text(color="black",size=10),
#                                 axis.ticks=element_line(color="black"),
#                                 axis.title=element_text(size=10),
#                                legend.position="none")+ coord_flip()+#
#           geom_point(aes(text = paste("<br>Species:",Species_short,"<br>Cultivar:",Cultivar,"<br>Country:",Country),
#                          col=Exposition)))


# Graphing the Ptlp data - all species Andy had data for

#tlp<-hirons %>%
#  filter(dat.type == "psi.tlp") %>%
#  group_by(Species_short)%>%
#  summarise(Count = length(value),
#            Mean_TLP=mean(value),
#            SD_TLP=sd(value),
#            SE_TLP= sd(value) / sqrt(n())) %>%
#  mutate(Mean_TLP = round(Mean_TLP, 2),SD_TLP = round(SD_TLP,2),
#         SE_TLP=round(SE_TLP,2))%>%
#  left_join(hirons)#%>%
#  filter(Count>1)
#ggplotly(ggplot(tlp,aes(x=reorder(Species_short, value), y= value))+
#           labs(x="Species",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
#           geom_boxplot() + ggtitle("Full Species List - Hirons")+
#           theme_classic()+theme(axis.text.y=element_text(color="black",
#                                                          size=10,vjust=0.5,
#                                                          hjust=1,face="italic"),
#                                 axis.text.x=element_text(color="black",size=10),
#                                 axis.ticks=element_line(color="black"),
#                                axis.title=element_text(size=10),
#                                legend.position="none")+ coord_flip()+
#           geom_point(aes(text = paste("<br>Cultivar:",Cultivar,"<br>Country:",Country),
#                          col=Exposition)))


#ggplotly(ggplot(tlp,aes(x=reorder(Genus, value), y= value))+
#           labs(x="Genera",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
#           geom_boxplot() + ggtitle("All Genera - Hirons")+
#           theme_classic()+theme(axis.text.y=element_text(color="black",
#                                                          size=10,vjust=0.5,
#                                                          hjust=1,face="italic"),
#                                 axis.text.x=element_text(color="black",size=10),
#                                 axis.ticks=element_line(color="black"),
#                                 axis.title=element_text(size=10),
#                                 legend.position="none")+ coord_flip()+
#           geom_point(aes(text = paste("<br>Species:",Species_short,"<br>Cultivar:",Cultivar,
#                                       "<br>Country:",Country),
#                          col=Exposition)))


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
  mutate(Plant_organ = "L")

workdata_2 <- workdata_2 %>%
  rename(Species = AccSpeciesName) 
# combine rows
combined_long <- bind_rows(hirons_lon, workdata_2)
# 6168

names(combined_long)
bartlett <- bartlett %>%
  mutate(Reference_datfile = "Bartlett 2012") %>%
  pivot_longer(cols = -c(1,2,5,6,7), 
               names_to = "dat.type",
               values_to = "value") %>%
  mutate(Plant_organ = "L")

combined_long <- bind_rows(choat, bartlett, combined_long, root) # 7550
# include on plot: R, P, N. 

combined_long <- combined_long %>% mutate(value = round(value, 2),
                                          Last = word(Source, 1, sep = " ")) %>%
  mutate(value = ifelse(value>0, value * -1,value))
# 7550
summary <- combined_long %>%
  group_by(Species_short, dat.type) %>%
  summarize(Count = n()) %>%
  pivot_wider(names_from = dat.type, values_from = Count)
#1225
summary <- summary %>%
  mutate(Table = if_else(Species_short %in% desig$Species, "X", ""))

# remove duplicates

combined_long<- combined_long [!duplicated(combined_long[c("Species",
                                                             "Last",
                                                             "dat.type",
                                                             "value")]),]
# 5966


# Standardize TLP using Hirons equation -----------------------------------

all_dat_hirons_tlp <- combined_long %>%
  filter(dat.type == "psi.ft") %>% 
  rename(psi.ft = value) %>%
  select(-dat.type) %>%
  mutate(hirons_psi.tlp = -0.2554 + (1.1243 * psi.ft)) %>% # Equation from Hirons et al. 2020
  pivot_longer(.,cols = -c(1:12,14:31), 
             names_to = "dat.type",
             values_to = "value") %>%
  filter(.,dat.type == "hirons_psi.tlp")

# 949 psi.ft values

all_dat <- bind_rows(combined_long, all_dat_hirons_tlp)

all_dat_bartlett_tlp <- combined_long %>%
  filter(dat.type == "psi.ft") %>% 
  rename(psi.ft = value) %>%
  select(-dat.type) %>%
  mutate(bartlett_psi.tlp = -0.2554 + (1.1243 * psi.ft)) %>% # Equation from Hirons et al. 2020
  pivot_longer(.,cols = -c(1:12,14:31), 
               names_to = "dat.type",
               values_to = "value") %>%
  filter(.,dat.type == "bartlett_psi.tlp")

# 949 psi.ft values

# Next steps. Calculate TLP from the FT values where needed. Intersect the species
# list with the drought designations table and populate columns where data exists. 

# Calculating PSI TLP from PSI FT -----------------------------------------

# Using the equation Andy Hirons used, we are calculating PSI TLP from PSI FT
# only for the plants with values recorded for either the petiole or leaf
sus <- combined_long %>%
  filter(value > -20 & value <= 0) %>%
  filter(Plant_organ == "P" | Plant_organ == "L")
# 1749

turgorpt <- sus %>%
  filter(dat.type == "psi.tlp" | dat.type == "psi.ft") %>% 
  select(Species_short, dat.type, value, Source, Reference, Reference_datfile,Plant_organ)# %>%
# 1597

# figure out what species I have psi.ft values for and not tlp values.
df_calc <- turgorpt %>%
  group_by(Species_short, dat.type, Reference, Source, Reference_datfile) %>%
  summarise(num_values = n()) %>%
  pivot_wider(names_from = dat.type, values_from = num_values) %>%
  filter(is.na(psi.tlp)) %>%
  select(Species_short, Reference, Source, Reference_datfile) %>% # 58 entries with ft and not tlp
  inner_join(., combined_long, by = c("Species_short", "Reference", "Source", "Reference_datfile")) %>%
  filter(dat.type == "psi.ft") # 304
### stop here....SEARCH for me
tlp_dat <- df_calc %>%
  rename(psi.ft = value) %>%
  select(-dat.type) %>%
  mutate(psi.tlp = -0.2554 + (1.1243 * psi.ft)) %>% # Calculation from Hirons et al 2020
  pivot_longer(.,cols = -c(1:12,14:31), 
               names_to = "dat.type",
               values_to = "value") %>%
  filter(.,dat.type == "psi.tlp")

# long data frame with TLP and FT values and corresponding metadata.
# need to combine this back with the "sus" data frame
# i think it worked
 
# repeat for full turgor data

#df_calc_ft <- turgorpt %>%
#  group_by(Species_short, dat.type) %>%
#  summarise(num_values = n()) %>%
#  pivot_wider(names_from = dat.type, values_from = num_values) %>%
#  filter(is.na(psi.ft)) %>%
# select(Species_short) %>% # 231 species with tlp and not ft
#  inner_join(., combined_long, by = "Species_short") %>%
#  filter(dat.type == "psi.tlp") #  239

#ft_dat <- df_calc_ft %>%
#  rename(psi.tlp = value) %>%
#  select(-dat.type) %>%
#  mutate(psi.ft = (psi.tlp+0.2554)/1.1243) #%>% # Calculation from Hirons et al 2020
# 239
#ft_dat<- ft_dat %>%
#  pivot_longer(cols = -c(1:11,13:25), 
#               names_to = "dat.type",
#               values_to = "value") 
# 478
#ft_dat <- ft_dat %>%
#  filter(dat.type == "psi.ft") # 239




turgor_dat <- bind_rows(tlp_dat, combined_long) # bind rows to add back in new calculated data
# 6292 rows
turgor_dat <- turgor_dat %>%
  distinct() #
# 6292

# Read out a file for Deb of psi tlp and psi ft data

#deb <- turgor_dat %>%
#  filter(dat.type == "psi.tlp",Plant_organ == "P" | Plant_organ == "L" | is.na(Plant_organ)) %>%
#  write.csv("droughtdesignations_data_DebDuong_20240726.csv", row.names = F)


# double check what is duplicated.
duplicated_rows <- duplicated(turgor_dat)
# Select duplicated rows from 'sus'
duplicated_sus <- turgor_dat[duplicated_rows, ]



# Full DataSet Designations -----------------------------------------------

turgor_dat <- turgor_dat %>%
  mutate(value = ifelse(value > 0, (value*-1), value),
         Genus = str_split(Species_short, " ", simplify = TRUE)[, 1])%>%
           mutate(Genus = str_trim(Genus)) %>%
  filter(value > -20 & value < 0)
# 6199
#
turgor_dat <- turgor_dat %>%
  mutate(Reference = ifelse(OriglName == "Choat et al. 2012 reported ?50 (MPa)", "Choat et al. 2012", Reference),
         Reference = ifelse(Dataset == "Torres-Ruiz", "Torres-Ruiz", Reference),
         Cultivar = ifelse(Species_short == 'Maclura pomifera "white shield"', "White Shield", Cultivar),
         Genus = ifelse(!is.na(Cultivar) & Genus == "Ulmus", "Ulmus Cultivar", Genus),
         Dataset = ifelse(is.na(Dataset), Source, Dataset))
turgor_dat$Species_short <- str_replace(turgor_dat$Species_short, 'Maclura pomifera "white shield"', "Maclura pomifera")


# change Maclura pomifera "White Shield" to just Maclura pomifera
# change all elm cultivars to "Ulmus cultivars"

# calculate mean and SD for each species based on data type
sum_turgor<- turgor_dat%>%
  filter(Plant_organ == "L") %>%
  group_by(Species_short, dat.type) %>%
  select(Species_short,dat.type, value) %>%
  mutate(Mean_Value = round(mean(value),2),
         SD_Value = round(sd(value),2),
         Count = n()  # Calculate the count of points in each group
  )%>%
  select(-value) %>%
  distinct(.) %>% 
  pivot_wider(.,names_from = dat.type, values_from = c(Mean_Value,SD_Value, Count)) 
# 1186
# 631 if I take out the NAs.

# TDAG associations for Psi turgor loss point
# Sensitive: > -2.5 MPa
# Moderately Sensitive -2.5 MPa to -3 MPa
# Moderately Tolerant -3 MPa to -3.5 MPa
# Tolerant < -3.5 MPa

# if else statements to create column of assigned drought designations using the above parameters
sum_turgor$Drought_tol <- ifelse(sum_turgor$Mean_Value_psi.tlp > -2.5, "Sensitive",
                                  ifelse((sum_turgor$Mean_Value_psi.tlp >= -3 & sum_turgor$Mean_Value_psi.tlp <= -2.5), "Moderately Sensitive",                                         ifelse((sum_turgor$Mean_Value_psi.tlp >= -3.5 & sum_turgor$Mean_Value_psi.tlp < -3), "Moderately Tolerant",
                                                ifelse(sum_turgor$Mean_Value_psi.tlp < -3.5, "Tolerant", NA))))


# Plot P12/P50/P88 against PSI TLP to see if there is any relationship 
ggplotly(ggplot(sum_turgor,aes(x=Mean_Value_psi.tlp, y= Mean_Value_P12))+
           labs(x="Mean PSI TLP",y="Mean P12") +
           geom_point(aes(
             text = paste("<br>Species:", Species_short,
                          "<br>Mean P12:", Mean_Value_P12,
                          "<br>SD P12:",SD_Value_P12,
                          "<br>Mean PSI TLP:", Mean_Value_psi.tlp,
                          "<br>SD PSI TLP:",SD_Value_psi.tlp))) +
             ggtitle("P12 ~ PSI TLP")+ geom_smooth(method = "lm",se = F)+
  lims(x=c(-3,-1), y=c(-4,0)) +
  theme_classic()+theme(axis.text.y=element_text(color="black",
                                                 size=10,vjust=0.5,
                                                 hjust=1,face="italic"),
                        axis.text.x=element_text(color="black",size=10),
                        axis.ticks=element_line(color="black"),
                        axis.title=element_text(size=10),
                        legend.position="none"), tooltip = "text") 


cor.test(sum_turgor$Mean_Value_psi.tlp, sum_turgor$Mean_Value_P12, method ="spearman") # r = 0.5
cor.test(sum_turgor$Mean_Value_psi.tlp, sum_turgor$Mean_Value_P50, method ="spearman") # r = 0.23, p = 0.005
cor.test(sum_turgor$Mean_Value_psi.tlp, sum_turgor$Mean_Value_P88, method ="spearman") # r = 0.24, p = 0.1391

shapiro.test(sum_turgor$Mean_Value_psi.tlp) # not normal
shapiro.test(sum_turgor$Mean_Value_P12) # not normal
shapiro.test(sum_turgor$Mean_Value_P50)# not normal
shapiro.test(sum_turgor$Mean_Value_P88) #

ggplotly(ggplot(sum_turgor,aes(x=Mean_Value_psi.tlp, y= Mean_Value_P50))+
           labs(x="Mean PSI TLP",y="Mean P50") +
           geom_point(aes(
             text = paste("<br>Species:", Species_short,
                          "<br>Mean P50:", Mean_Value_P50,
                          "<br>SD P50:",SD_Value_P50,
                          "<br>Mean PSI TLP:", Mean_Value_psi.tlp,
                          "<br>SD PSI TLP:",SD_Value_psi.tlp)))+
           geom_smooth(method = "lm",se = F)+
           lims(x=c(-5,0), y=c(-10,0)) +
           ggtitle("P50 ~ PSI TLP")+ geom_text(x=-4.5, y= -9, label = "r = 0.23", parse =T)+
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                 axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none"), tooltip = "text") 

ggplotly(ggplot(sum_turgor,aes(x=Mean_Value_psi.tlp, y= Mean_Value_P88))+
           labs(x="Mean PSI TLP",y="Mean P88") +
           geom_point(aes(
             text = paste("<br>Species:", Species_short,
                          "<br>Mean P88:", Mean_Value_P88,
                          "<br>SD P88:",SD_Value_P88,
                          "<br>Mean PSI TLP:", Mean_Value_psi.tlp,
                          "<br>SD PSI TLP:",SD_Value_psi.tlp))) + 
           geom_smooth(method = "lm",se = F)+
           lims(x=c(-5,0), y=c(-15,0)) +
           ggtitle("P88 ~ PSI TLP") + geom_text(x=-4.5, y= -14, label = "r = 0.24", parse =T) +
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                 axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none"), tooltip = "text") 

# create plots by species - bar plot

tlp_dat <- sum_turgor %>% 
  select(Species_short,Drought_tol) %>%
  full_join(turgor_dat) %>%
  filter(dat.type == "psi.tlp" & Plant_organ == "L") %>%
  group_by(Dataset, Species_short) %>%
  mutate(psi.tlp_avg = round(mean(value),2),
         Count = n()) %>%
  select(Species_short, Drought_tol, Source, Source_file, Genus, psi.tlp_avg, Count) %>%
  distinct()

## edited this to average across a study. 26 Sept 2024
## Left off here. I THINK this is working the way we want? At least in terms of 
# all species & all points (with each point an average across a study)

ggplotly(ggplot(tlp_dat,aes(x=reorder(Species_short, psi.tlp_avg), y=psi.tlp_avg))+
           labs(x="Species",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
           geom_boxplot() + ggtitle("Full Data PSI TLP")+
           geom_point(aes(
             text = paste(
               "<br>Species:",Species_short,
              # "<br>Cultivar:", Cultivar,
               "<br>Drought Tolerance:",Drought_tol,
               "<br>psi TLP:", psi.tlp_avg,
               "<br>Count:", Count,
               "<br>Source Data File:",Source),
                          col=Drought_tol)) +
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip(), tooltip = "text")

ggplotly(ggplot(tlp_dat,aes(x=reorder(Genus, psi.tlp_avg), y= psi.tlp_avg))+
           labs(x="Genus",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
           geom_boxplot() + ggtitle("Full Data PSI TLP")+
           geom_point(aes(
             text = paste(
               "<br>Species:",Species_short,
               #"<br>Cultivar:", Cultivar,
               "<br>Drought Tolerance:",Drought_tol,
               "<br>psi TLP:", psi.tlp_avg,
               "<br>Count:", Count,
               "<br>Source Data File:",Source),
                          col=Drought_tol)) +
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                 axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip(), tooltip = "text")


# Full Data Set Question: How many references do into this data set?
# DF = turgor_dat
length(unique(turgor_dat$Reference))
# 523 Unique References
length(unique(turgor_dat$Reference[turgor_dat$dat.type == "psi.tlp" | turgor_dat$dat.type == "psi.ft"]))
# 79 unique references for psi tlp data (checked if I add psi.ft and it is still 79 ref)

ref <- turgor_dat %>%
  filter(dat.type == "psi.tlp")
# 1034 rows of data with just psi.tlp
length(unique(ref$Plant_organ))

# Intersect and Update Designation Table ----------------------------------

turgor_dat <- turgor_dat %>%
  mutate(value = ifelse(value > 0, (value*-1), value),
         Genus = str_split(Species_short, " ", simplify = TRUE)[, 1])%>%
  mutate(Genus = str_trim(Genus)) %>%
  filter(value > -20 & value < 0)
# 6199



# calculate mean and SD for each species based on data type
sum_turgor<- turgor_dat%>%
  filter(Plant_organ == "P" | Plant_organ == "L" | is.na(Plant_organ)) %>%
  group_by(Species_short, dat.type) %>%
  select(Species_short,dat.type, value) %>%
  mutate(Mean_Value = round(mean(value),2),
         SD_Value = round(sd(value),2),
         Count = n()  # Calculate the count of points in each group
  )%>%
  select(-value) %>%
  distinct(.) %>% 
  pivot_wider(.,names_from = dat.type, values_from = c(Mean_Value,SD_Value, Count)) 
# 1186

# TDAG associations for Psi turgor loss point
# Sensitive: > -2.5 MPa
# Moderately Sensitive -2.5 MPa to -3 MPa
# Moderately Tolerant -3 MPa to -3.5 MPa
# Tolerant < -3.5 MPa


## need to use pivot_wider above to get this output to work
# I wanted to create a faceted graph to look at the relationship between psi.tlp and 
# P50 and P88, so I have left it out for now.

# if else statements to create column of assigned drought designations using the above parameters
sum_turgor$Drought_tol <- ifelse(sum_turgor$Mean_Value_psi.tlp > -2.5, "Sensitive",
                                 ifelse((sum_turgor$Mean_Value_psi.tlp >= -3 & sum_turgor$Mean_Value_psi.tlp <= -2.5), "Moderately Sensitive",  
                                        ifelse((sum_turgor$Mean_Value_psi.tlp >= -3.5 & sum_turgor$Mean_Value_psi.tlp < -3), "Moderately Tolerant",
                                               ifelse(sum_turgor$Mean_Value_psi.tlp < -3.5, "Tolerant", NA))))


# create plots by species - bar plot

tlp_dat <- sum_turgor %>% 
  select(Species_short,Drought_tol) %>%
  full_join(turgor_dat) %>%
   filter(dat.type == "psi.tlp")

elms <- tlp_dat %>%
  filter(Species_short %like% "Ulmus") %>%
  filter(dat.type == "psi.tlp")
# 19


tlp_dat$ExistsInDesig <- tlp_dat$Species_short %in% desig_list$Species

desig_tlp_dat <- tlp_dat %>%
  filter(ExistsInDesig == TRUE & dat.type == "psi.tlp")


desig_tlp_dat <- bind_rows(desig_tlp_dat, elms)
# 366
length(unique(desig_tlp_dat$Species_short))
# 90, only psi.tlp data so less species than 114 below.There are 24 species with no psi.tlp data.

sum_turgor$ExistsInDesig <- sum_turgor$Species_short %in% desig_list$Species

desig_sum_turgor <- sum_turgor %>%
  filter(ExistsInDesig == TRUE)

elms_sum <- sum_turgor %>%
  filter(Species_short %like% "Ulmus")
# 19 elms


desig_sum_turgor <- bind_rows(desig_sum_turgor, elms_sum)
# 114

ggplotly(ggplot(desig_tlp_dat,aes(x=reorder(Species_short, value), y=value))+
             labs(x="Species",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
             geom_boxplot() + ggtitle("Designation List Data PSI TLP")+
             geom_point(aes(
               text = paste(
                 "<br> PSI TLP:", value,
                 "<br>Cultivar:",Cultivar,
                 "<br>Drought Tolerance:",Drought_tol,
                 "<br>Source Data File:",Source),
               col=Drought_tol)) +
             theme_classic()+theme(axis.text.y=element_text(color="black",
                                                            size=10,vjust=0.5,
                                                            hjust=1,face="italic"),
                                   axis.text.x=element_text(color="black",size=10),
                                   axis.ticks=element_line(color="black"),
                                   axis.title=element_text(size=10),
                                   legend.position="none")+ coord_flip(), tooltip = "text")

ggplotly(ggplot(desig_tlp_dat,aes(x=reorder(Genus, value), y=value))+
           labs(x="Genus",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
           geom_boxplot() + ggtitle("Designation List Data PSI TLP")+
           geom_point(aes(
             text = paste(
               "<br> PSI TLP:", value,
               "<br>Species:",Species_short,
               "<br>Drought Tolerance:",Drought_tol,
               "<br>Source Data File:",Source),
             col=Drought_tol)) +
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                 axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip(), tooltip = "text")


#combine data into one cell for the table 
# this gives weird NA ( NA ) as character strings
desig_sum_turgor <- desig_sum_turgor %>%  
  mutate(P50 = paste(Mean_Value_P50, "(", SD_Value_P50, ")"),
         P88 = paste(Mean_Value_P88, "(", SD_Value_P88, ")"),
         psi.tlp = paste(Mean_Value_psi.tlp, "(", SD_Value_psi.tlp, ")"),
         psi.ft = paste(Mean_Value_psi.ft, "(", SD_Value_psi.ft, ")")) %>%
  select(-2:-11) %>%
  rename(Species = Species_short)
# get rid of the weird  NA ( NA ) 
desig_sum_turgor$P50 <- ifelse(desig_sum_turgor$P50 == "NA ( NA )", NA, desig_sum_turgor$P50)
desig_sum_turgor$P88 <- ifelse(desig_sum_turgor$P88 == "NA ( NA )", NA, desig_sum_turgor$P88)
desig_sum_turgor$psi.tlp <- ifelse(desig_sum_turgor$psi.tlp == "NA ( NA )", NA, desig_sum_turgor$psi.tlp)
desig_sum_turgor$psi.ft <- ifelse(desig_sum_turgor$psi.ft == "NA ( NA )", NA, desig_sum_turgor$psi.ft)


desig_sum_turgor <- desig_sum_turgor %>%
  rename(Species = Species_short)
desig_sum_turgor <- desig_sum_turgor %>%
left_join(.,desig)

desig_list$HadData <- desig_list$Species %in% desig_sum_turgor$Species

desig_nodat<- desig_list %>%
  filter(HadData == FALSE) %>%
  left_join(.,desig)

desig_sum_turgor <- bind_rows(desig_sum_turgor, desig_nodat)
# write out csv file for the team
#write.csv(desig_sum_turgor,"droughtdesignations_table_2024_25Sept.csv", row.names = F)


# How does our list compare to NITS? --------------------------------------

# this is an absolute mess. leaving off for now. I want to intersect the
# desig table list with NITS and ALSO intersect the full tlp_dat list with NITS

# desig_list
# combined_long
# Species includes cultivar
# Species_short does not include cultivar

NITS <- read.csv("NITS_plantlist.csv",header=T)

names(NITS)
length(unique(NITS$Species)) # 197 unique species including cultivar
length(unique(NITS$Species_short)) # 187 unique species not including cultivar

# Cross reference this list with the designation table 
names(desig_list)

desig_list <- elms %>% select(Species) %>% bind_rows(.,desig_list) %>% select(-Species_short)

NITS$ExistsInDesig <- NITS$Species_short %in% desig_list$Species

int <- NITS %>%
  filter(ExistsInDesig == F)
# 39 species in NITS that are not on our table

NITS$ExistsInAllDat <- NITS$Species_short %in% combined_long$Species_short
int <- NITS %>%
  filter(ExistsInAllDat == F)

# 82 species in NITS that we don't have any data for in our large DF

# How does our list compare to the website?

TMA_web <- read.csv("TMA-TreeList.csv", header=T)


all_lists <- TMA_web %>%
  merge(., NITS, desig_list)



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
