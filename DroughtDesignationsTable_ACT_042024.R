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

# Google Sheets -----------------------------------------------------------
desig<-data.frame(read_sheet(ss="https://docs.google.com/spreadsheets/d/1Ap2zxfzQ2tA7Vw2YFKWG5WlASvh_DakGJGc-i07YDOo/edit#gid=2107989582"))
2
desig_list <- desig %>%
  select(Species) %>%
  mutate(
    Species = if_else(Species == "Platanus x acerifolia (syn hispanica)", "Platanus x acerifolia", Species),
    Species = ifelse(Species == "Acer saccharum ssp nigrum", "Acer nigrum", Species),
    Species = ifelse(Species == "Larix larcina", "Larix laricina", Species),
    Species = str_replace(Species,"Alnus incana ssp.rugosa", "Alnus incana ssp rugosa"),
    Species = str_replace(Species, "Ulmus parviflora", "Ulmus parvifolia")
  ) %>%
  filter(!row_number() %in% c(158, 160:162, 176:179)) %>%
  as.data.frame() %>%
  rename(Botanical.Name = Species)


# Cross Check Designation List with Morton Web List -----------------------------------------------------
Morton_Web <- data.frame(read_sheet(ss="https://docs.google.com/spreadsheets/d/1LXnWGUHcJwK6vsTBP_VLRDaWmEwbSebb2ma7gDl84e0/edit?gid=221521861#gid=221521861"))
2


desig_list$OnMortonWeb <- ifelse(desig_list$Botanical.Name %in% Morton_Web$Botanical.Name, "x", "desiglist")

Morton_Web <- desig_list %>%
  filter(OnMortonWeb == "desiglist") %>%
  bind_rows(Morton_Web)

Morton_Web <- Morton_Web %>%
  select(!Cultivar, !Variety.Ssp) %>%
  rename(Species = Botanical.Name) %>%
  mutate(Species = str_replace(Species,  "Quercus muhlenbergii", "Quercus muehlenbergii"),
         Species = str_replace(Species, "Cladrastris kentukea", "Cladrastis kentukea"),
         Species = str_replace(Species, "viburnum lentago","Viburnum lentago")) %>%
  mutate(CultivarName = ifelse(str_detect(Species, "'"), str_trim(str_split(Species, pattern = "'", simplify = TRUE)[, 2]), NA_character_),
         CultivarName = ifelse(str_detect(Species, "\""), str_trim(str_split(Species, pattern = "\"", simplify = TRUE)[,2]), CultivarName),
         SubSpecies = ifelse(str_detect(Species, "ssp\\.?|subsp\\.?"), 
                             str_trim(str_extract(Species, "(?<=ssp\\.?\\s|subsp\\.?\\s)\\w+")), 
                             NA_character_),
         VarietyName = ifelse(str_detect(Species, "var\\.?\\s"), 
                              str_trim(str_extract(Species, "(?<=var\\.?\\s)\\w+")), 
                              NA_character_)) %>%
  # Create a new column with the cleaned species name
  mutate(SpeciesClean = Species %>%
           # Step 1: Remove the cultivar (anything in quotes)
           str_remove("\"[^\"]*\"") %>%
           # Remove anything in single quotes
           str_remove("'[^']*'") %>%
           # Step 2: Remove subspecies (ssp or subsp)
           str_remove("(ssp\\.?|subsp\\.?)\\s*\\w+") %>%
           # Step 3: Remove variety (var. or var with a space)
           str_remove("var\\.?\\s*\\w+") %>%
           # Step 4: Trim extra spaces left after removal
           str_trim()) %>%
  filter(!str_detect(Species, "\\b\\w+\\s+sp\\b") & # drop anything that is just genus sp (ex. diospyros sp.)
           !str_detect(Species, "(?i)ferns") & # drop all the ferns
           !str_detect(Species, "^[A-Za-z]+$")) # drop anything that just lists the genus (ex. Eucalyptus)

#desig_list$OnDesList <- "x"

# all of the drought designation species are ALSO on the website already.

# Hirons et al. 2020 data merge with Sjoman Data-------------------------------------------------
jakedat <- read_csv("Jake summary data.csv") %>%
  mutate(
    Species = str_trim(if_else(species == "Quercus muhlenbergii", "Quercus muehlenbergii", species), side = "right"), # Fix spelling
    Reference = "Hirons et al. 2020", # Add source information
    Species_short = str_trim(if_else(str_detect(Species, "'"), str_split(Species, pattern = "'", simplify = TRUE)[, 1], Species), side = "right"),
    Cultivar = if_else(str_detect(Species, "'"), str_split(Species, pattern = "'", simplify = TRUE)[, 2], NA_character_),
    psi.tlp = round(sum.tlp, 2),
    psi.ft = round(sum.opft, 2),
    Species = str_replace(Species, "Magnolia x loebneri 'Leonard Messel'", "Magnolia x loebneri 'Messel'")
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
  


hirons_lon <- hirons_lon

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


# Combining Data Frames-------------------------------------


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

# remove duplicates
combined_long <- combined_long %>%
  mutate(Species = ifelse(is.na(Species), Species_short, Species))

combined_long<- combined_long [!duplicated(combined_long[c("Species",
                                                             "Last",
                                                             "dat.type",
                                                             "value")]),]

# 5995



# Export all References and Sources associated with TLP and FT data for Luke --------

ref_list <- combined_long %>%
  filter(dat.type == "psi.ft" | dat.type == "psi.tlp") %>%
  select(Reference) %>%
  distinct()# %>%
#  write.csv(.,"Turgor_Reference List.csv", row.names=F)

source_list <- combined_long %>%
  filter(dat.type == "psi.ft" | dat.type == "psi.tlp") %>%
  select(Source) %>% 
  distinct() #%>%
# write.csv(.,"Turgor_Source List.csv", row.names = F)



# Export Species List -----------------------------------------------------

species_list <- combined_long %>%
  filter(dat.type %like% "psi") %>%
  select(Species, Species_short) %>% distinct()

Morton_Web <- data.frame(read_sheet(ss="https://docs.google.com/spreadsheets/d/1LXnWGUHcJwK6vsTBP_VLRDaWmEwbSebb2ma7gDl84e0/edit?gid=221521861#gid=221521861"))

Morton_species <- Morton_Web %>%
  select(Botanical.Name) %>%
  rename(Species = Botanical.Name)


NITS <- read.csv("NITS_plantlist.csv",header=T)

species_list <- bind_rows(species_list, Morton_species, NITS)

species_list$Species<-trimws(species_list$Species,"both")

species_list <- species_list %>%
  select(Species) %>% distinct() #%>% 
 # write.csv(.,"all_species.csv", row.names=F) 
# 777 obs  


  # Standardize TLP using Hirons equation -----------------------------------


all_dat_hirons_tlp <- combined_long %>%
  filter(dat.type == "psi.ft") %>% 
  rename(psi.ft = value) %>%
  select(-dat.type) %>%
  mutate(hirons_psi.tlp = -0.2554 + (1.1243 * psi.ft)) %>% # Equation from Hirons et al. 2020
  pivot_longer(.,cols = -c(1:12,14:30), 
             names_to = "dat.type",
             values_to = "value") %>%
  filter(.,dat.type == "hirons_psi.tlp")

# 964 psi.ft values

all_dat <- bind_rows(combined_long, all_dat_hirons_tlp)
# 6959


# Full DataSet Designations -----------------------------------------------



turgor_dat <- all_dat %>%
  mutate(value = ifelse(value > 0, (value*-1), value),
         Genus = str_split(Species_short, " ", simplify = TRUE)[, 1])%>%
           mutate(Genus = str_trim(Genus)) %>%
  filter(value > -20 & value < 0)
# 6734
#
turgor_dat <- turgor_dat %>%
  mutate(Reference = ifelse(OriglName == "Choat et al. 2012 reported ?50 (MPa)", "Choat et al. 2012", Reference),
         Reference = ifelse(Dataset == "Torres-Ruiz", "Torres-Ruiz", Reference),
         Cultivar = ifelse(Species_short == 'Maclura pomifera "white shield"', "White Shield", Cultivar),
         Genus = ifelse(!is.na(Cultivar) & Genus == "Ulmus", "Ulmus Cultivar", Genus),
         Dataset = ifelse(is.na(Dataset), Source, Dataset))

turgor_dat$Source <-str_replace(turgor_dat$Source, "Choat et al. 2007","Choat B, Sack L, Holbrook NM., 2007, New Phytologist, 175, 686-698")
turgor_dat$Source <- str_replace(turgor_dat$Source, "Hao et al. 2010", "Hao G-Y, Sack L, Wang A-Y, Cao K-F, Goldstein G, 2010, Functional Ecology, 24, 731-740")
turgor_dat$Source <- str_replace(turgor_dat$Source,"Sack L, Cowan PD, Jaikumar NJ, Holbrook NM. 2003., 2003, Plant, Cell & Environment, 26, 1343-1356", "Sack L, Cowan PD, Jaikumar NJ, Holbrook NM., 2003, Plant, Cell & Environment, 26, 1343-1356")
turgor_dat$Source <- str_replace(turgor_dat$Source, "Scoffoni et al. 2008", "Scoffoni C, Pou A, Aasamaa K, Sack L., 2008, Plant, Cell & Environment, 13, 1803-1812")

turgor_dat$Species_short <- str_replace(turgor_dat$Species_short, 'Maclura pomifera "white shield"', "Maclura pomifera")
turgor_dat$Species <- str_replace(turgor_dat$Species, 'Pseudotsuga menziesii', "Psuedotsuga menziesii")
turgor_dat$Species <- str_replace(turgor_dat$Species, 'Magnolia loebneri_x', 'Magnolia loebneri')


turgor_dat <- turgor_dat %>%
  mutate(CultivarName = ifelse(str_detect(Species, "'"), str_trim(str_split(Species, pattern = "'", simplify = TRUE)[, 2]), NA_character_),
         CultivarName = ifelse(str_detect(Species, "\""), str_trim(str_split(Species, pattern = "\"", simplify = TRUE)[,2]), CultivarName),
         SubSpecies = ifelse(str_detect(Species, "ssp\\.?|subsp\\.?"), 
                            str_trim(str_extract(Species, "(?<=ssp\\.?\\s|subsp\\.?\\s)\\w+")), 
                            NA_character_),
         VarietyName = ifelse(str_detect(Species, "var\\.?\\s"), 
                              str_trim(str_extract(Species, "(?<=var\\.?\\s)\\w+")), 
                              NA_character_)) %>%
  # Create a new column with the cleaned species name
  mutate(SpeciesClean = Species %>%
           # Step 1: Remove the cultivar (anything in quotes)
          str_remove("\"[^\"]*\"") %>%
           # Remove anything in single quotes
          str_remove("'[^']*'") %>%
          # Step 2: Remove subspecies (ssp or subsp)
          str_remove("(ssp\\.?|subsp\\.?)\\s*\\w+") %>%
          # Step 3: Remove variety (var. or var with a space)
          str_remove("var\\.?\\s*\\w+") %>%
          # Step 4: Trim extra spaces left after removal
          str_trim()) %>%
  filter(!str_detect(Species, "\\b\\w+\\s+sp\\b") & # drop anything that is just genus sp (ex. diospyros sp.)
           !str_detect(Species, "(?i)ferns") & # drop all the ferns
           !str_detect(Species, "^[A-Za-z]+$")) %>% # drop anything that just lists the genus (ex. Eucalyptus)
  select(!Species_short) %>%
  mutate(OnMortonWeb = ifelse(SpeciesClean %in% Morton_Web$SpeciesClean, "x", NA_character_),
         SpeciesClean = ifelse(grepl("^\\S+$", SpeciesClean), Species, SpeciesClean),
         SpeciesClean = ifelse(SpeciesClean == "Magnolia x kewensis s Memory'", "Magnolia x kewensis", SpeciesClean),
         CultivarName = ifelse(CultivarName == "Wada'", "Wada's Memory", CultivarName),
         OnMortonWeb = ifelse(Genus == "Ulmus cultivar", "x", OnMortonWeb))


# 6643

# Standardize Plant Names - SKIP FOR NOW -------------------------------------------------

#WFO_names <- read.csv("WFO_names.csv",header=T)

#WFO_names <- WFO_names %>%
#  select(Submitted_Name, Accepted_SPNAME) %>% distinct() %>%
#  select(Species_short)
## 1179 names.


# take the mean of each source, then take the mean of that to get to species,
# then take the mean of that to get to genus. 
# calculate mean and SD for each species based on data type
all_sum_turgor <- turgor_dat %>%
  filter(Plant_organ == "L" & dat.type == "hirons_psi.tlp") %>%
  select(SpeciesClean, Source, value, dat.type) %>%
  group_by(SpeciesClean, Source) %>%
  mutate(
    Mean_TLP_Value_Source = round(mean(value), 2),  # Calculate mean for each group
    SE_TLP_Value_Source = round((sd(value) / sqrt(length(value))), 2)) %>%   # Calculate standard error
  ungroup()  %>%  # Ungroup if you're planning further joins or grouping steps
  right_join(.,turgor_dat) 

all_sum_turgor <- all_sum_turgor %>%
  filter(Plant_organ == "L" & dat.type == "hirons_psi.tlp") %>%
  select(SpeciesClean, Mean_TLP_Value_Source, dat.type, SE_TLP_Value_Source) %>%
  distinct() %>%
  group_by(SpeciesClean) %>%
  mutate(Mean_TLP_Value_Species = round(mean(Mean_TLP_Value_Source), 2),     # Calculate mean for each group
    SE_TLP_Value_Species = round((sd(Mean_TLP_Value_Source) / sqrt(length(Mean_TLP_Value_Source))), 2))%>%   # Calculate standard error
  ungroup() %>%
  right_join(.,all_sum_turgor) 

all_sum_turgor <- all_sum_turgor %>%
  filter(Plant_organ == "L" & dat.type == "hirons_psi.tlp") %>%
  select(Genus, Mean_TLP_Value_Species, dat.type) %>%
  distinct() %>%
  group_by(Genus) %>%
  mutate(Mean_TLP_Value_Genus = round(mean(Mean_TLP_Value_Species), 2),                    # Calculate mean for each group
         SE_TLP_Value_Genus = round((sd(Mean_TLP_Value_Species) / sqrt(length(Mean_TLP_Value_Species))), 2)   # Calculate standard error
  ) %>%
  ungroup() %>%
  right_join(all_sum_turgor)


all_sum_turgor$Drought_tol_source <- ifelse(all_sum_turgor$Mean_TLP_Value_Source > -2.5, "Sensitive",
                                          ifelse((all_sum_turgor$Mean_TLP_Value_Source >= -3 & all_sum_turgor$Mean_TLP_Value_Source <= -2.5), "Moderately Sensitive",
                                                 ifelse((all_sum_turgor$Mean_TLP_Value_Source >= -3.5 & all_sum_turgor$Mean_TLP_Value_Source < -3), "Moderately Tolerant",
                                                        ifelse(all_sum_turgor$Mean_TLP_Value_Source < -3.5, "Tolerant", NA))))



# if else statements to create column of assigned drought designations using the above parameters
all_sum_turgor$Drought_tol_spec <- ifelse(all_sum_turgor$Mean_TLP_Value_Species > -2.5, "Sensitive",
                                          ifelse((all_sum_turgor$Mean_TLP_Value_Species >= -3 & all_sum_turgor$Mean_TLP_Value_Species <= -2.5), "Moderately Sensitive",
                                                 ifelse((all_sum_turgor$Mean_TLP_Value_Species >= -3.5 & all_sum_turgor$Mean_TLP_Value_Species < -3), "Moderately Tolerant",
                                                        ifelse(all_sum_turgor$Mean_TLP_Value_Species < -3.5, "Tolerant", NA))))

# sum turgor no longer has p12, p50 or p88 data summary. To make the following code work,
# need to redo sum_turgor and change grouping variable or filter.

all_sum_turgor$Drought_tol_Genus <- ifelse(all_sum_turgor$Mean_TLP_Value_Genus > -2.5, "Sensitive",
                                 ifelse((all_sum_turgor$Mean_TLP_Value_Genus >= -3 & all_sum_turgor$Mean_TLP_Value_Genus <= -2.5), "Moderately Sensitive",
                                        ifelse((all_sum_turgor$Mean_TLP_Value_Genus >= -3.5 & all_sum_turgor$Mean_TLP_Value_Genus < -3), "Moderately Tolerant",
                                               ifelse(all_sum_turgor$Mean_TLP_Value_Genus < -3.5, "Tolerant", NA))))


# TDAG associations for Psi turgor loss point
# Sensitive: > -2.5 MPa
# Moderately Sensitive -2.5 MPa to -3 MPa
# Moderately Tolerant -3 MPa to -3.5 MPa
# Tolerant < -3.5 MPa




# Looking at P12, P50, P88 - do not run - is broken -----------------------


# Plot P12/P50/P88 against PSI TLP to see if there is any relationship 
#ggplotly(ggplot(sum_turgor,aes(x=Mean_Value_psi.tlp, y= Mean_Value_P12))+
#           labs(x="Mean PSI TLP",y="Mean P12") +
#           geom_point(aes(
#             text = paste("<br>Species:", Species_short,
#                          "<br>Mean P12:", Mean_Value_P12,
#                          "<br>SD P12:",SD_Value_P12,
#                         "<br>Mean PSI TLP:", Mean_Value_psi.tlp,
#                          "<br>SD PSI TLP:",SD_Value_psi.tlp))) +
#             ggtitle("P12 ~ PSI TLP")+ geom_smooth(method = "lm",se = F)+
#  lims(x=c(-3,-1), y=c(-4,0)) +
#  theme_classic()+theme(axis.text.y=element_text(color="black",
#                                                size=10,vjust=0.5,
##                                                 hjust=1,face="italic"),
#                        axis.text.x=element_text(color="black",size=10),
#                        axis.ticks=element_line(color="black"),
#                        axis.title=element_text(size=10),
#                        legend.position="none"), tooltip = "text") ##


#cor.test(sum_turgor$Mean_Value_psi.tlp, sum_turgor$Mean_Value_P12, method ="spearman") # r = 0.5
#cor.test(sum_turgor$Mean_Value_psi.tlp, sum_turgor$Mean_Value_P50, method ="spearman") # r = 0.23, p = 0.005
#cor.test(sum_turgor$Mean_Value_psi.tlp, sum_turgor$Mean_Value_P88, method ="spearman") # r = 0.24, p = 0.1391#

#shapiro.test(sum_turgor$Mean_Value_psi.tlp) # not normal
#shapiro.test(sum_turgor$Mean_Value_P12) # not normal
#shapiro.test(sum_turgor$Mean_Value_P50)# not normal
#shapiro.test(sum_turgor$Mean_Value_P88) #

#ggplotly(ggplot(sum_turgor,aes(x=Mean_Value_psi.tlp, y= Mean_Value_P50))+
#           labs(x="Mean PSI TLP",y="Mean P50") +
#           geom_point(aes(
#             text = paste("<br>Species:", Species_short,
#                          "<br>Mean P50:", Mean_Value_P50,
#                          "<br>SD P50:",SD_Value_P50,
#                          "<br>Mean PSI TLP:", Mean_Value_psi.tlp,
#                          "<br>SD PSI TLP:",SD_Value_psi.tlp)))+
#           geom_smooth(method = "lm",se = F)+
#           lims(x=c(-5,0), y=c(-10,0)) +
#           ggtitle("P50 ~ PSI TLP")+ geom_text(x=-4.5, y= -9, label = "r = 0.23", parse =T)+
#           theme_classic()+theme(axis.text.y=element_text(color="black",
#                                                          size=10,vjust=0.5,
#                                                          hjust=1,face="italic"),
#                                 axis.text.x=element_text(color="black",size=10),
#                                 axis.ticks=element_line(color="black"),
#                                 axis.title=element_text(size=10),
#                                 legend.position="none"), tooltip = "text") #

#ggplotly(ggplot(sum_turgor,aes(x=Mean_Value_psi.tlp, y= Mean_Value_P88))+
#           labs(x="Mean PSI TLP",y="Mean P88") +
#           geom_point(aes(
#            text = paste("<br>Species:", Species_short,
#                          "<br>Mean P88:", Mean_Value_P88,
#                          "<br>SD P88:",SD_Value_P88,
#                          "<br>Mean PSI TLP:", Mean_Value_psi.tlp,
#                         "<br>SD PSI TLP:",SD_Value_psi.tlp))) + 
#           geom_smooth(method = "lm",se = F)+
#           lims(x=c(-5,0), y=c(-15,0)) +
#          ggtitle("P88 ~ PSI TLP") + geom_text(x=-4.5, y= -14, label = "r = 0.24", parse =T) +
#           theme_classic()+theme(axis.text.y=element_text(color="black",
#                                                          size=10,vjust=0.5,
#                                                          hjust=1,face="italic"),
#                                 axis.text.x=element_text(color="black",size=10),
#                                 axis.ticks=element_line(color="black"),
#                                 axis.title=element_text(size=10),
#                                 legend.position="none"), tooltip = "text") 



# Barplots by Species -----------------------------------------------------

## edited this to average across a study. 26 Sept 2024
## Left off here. I THINK this is working the way we want? At least in terms of 
# all species & all points (with each point an average across a study)

# boxplot

spec_box <- all_sum_turgor %>% filter(dat.type == "hirons_psi.tlp") %>%
  select(SpeciesClean, Mean_TLP_Value_Source, 
         Source, Drought_tol_spec, 
         Drought_tol_source) %>% distinct()

ggplotly(ggplot(spec_box,aes(x=reorder(SpeciesClean, Mean_TLP_Value_Source), y=Mean_TLP_Value_Source))+
           labs(x="Species",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
           geom_boxplot() + ggtitle("All Data")+
           geom_point(aes(
             text = paste(
               "<br>Species:",SpeciesClean,
               #"<br>Cultivar:", CultivarName,
               "<br> Avg Species Level Drought Tolerance:",Drought_tol_spec,
               "<br> Source Level Drought Tolerance:", Drought_tol_source,
               "<br>psi TLP:", Mean_TLP_Value_Source,
               #"<br>Count:", Count,
               "<br>Source Data File:",Source),
                          col=Drought_tol_source)) +
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip(), tooltip = "text")


spec_bar <- all_sum_turgor %>% filter(dat.type == "hirons_psi.tlp") %>%
  select(SpeciesClean, Mean_TLP_Value_Species, Drought_tol_spec, SE_TLP_Value_Species) %>% distinct()

# species barplot
ggplotly(ggplot(spec_bar, aes(x=reorder(SpeciesClean, Mean_TLP_Value_Species), 
                       y = Mean_TLP_Value_Species), color = Drought_tol_spec) +
           labs(x="Species",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
           geom_bar(stat = "identity", 
                    aes(
                      text = paste(
                        "<br>Species:",SpeciesClean,
                        # "<br>Cultivar:", Cultivar,
                        "<br>Species Drought Tolerance:",Drought_tol_spec,
                        "<br>Mean psi TLP:",  Mean_TLP_Value_Species,
                        "<br>SE psi TLP:", SE_TLP_Value_Species),
                      col=Drought_tol_spec)) + ggtitle("All Data") +
           geom_pointrange(aes(x = SpeciesClean, ymin = Mean_TLP_Value_Species -SE_TLP_Value_Species, 
                              ymax =  Mean_TLP_Value_Species + SE_TLP_Value_Species),
                          width=0.4, colour = "black", alpha=0.75, size = 0.5) + 
           coord_flip() +
           theme_classic() + 
           theme(axis.text.y=element_text(color = "black", size = 10, vjust = 0.5, hjust = 1, face = "italic"),
                                 axis.text.x = element_text(color = "black", size = 10),
                                 axis.ticks = element_line(color = "black"),
                                 axis.title = element_text(size = 10),
                 legend.position = "none")+ coord_flip(), tooltip = "text")


gen_box <- all_sum_turgor %>% filter(dat.type == "hirons_psi.tlp") %>%
  select(Genus, SpeciesClean, Mean_TLP_Value_Species, SE_TLP_Value_Species,Drought_tol_spec,Drought_tol_Genus) %>% distinct()

# genus boxplot
ggplotly(ggplot(gen_box,aes(x=reorder(Genus, Mean_TLP_Value_Species),
                            y= Mean_TLP_Value_Species))+
           labs(x="Genera",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
           geom_boxplot() + ggtitle("All Data - Genera")+
           geom_point(aes(
             text = paste(
               "<br>Species:",SpeciesClean,
               "<br>Species Level Drought Tolerance:",Drought_tol_spec,
               "<br>Genus Level Drought Tolerance:", Drought_tol_Genus,
               "<br>psi TLP:", Mean_TLP_Value_Species,
               "<br>SE psi TLP:", SE_TLP_Value_Species),
                          col=Drought_tol_spec)) +
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                 axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip(), tooltip = "text")

gen_bar <- all_sum_turgor %>% filter(dat.type == "hirons_psi.tlp") %>%
  select(Genus, Mean_TLP_Value_Genus, Drought_tol_Genus, SE_TLP_Value_Genus) %>% distinct()

# genus barplot
ggplotly(ggplot(gen_bar, aes(x=reorder(Genus, Mean_TLP_Value_Genus),
                                    y= Mean_TLP_Value_Genus), color = Drought_tol_Genus) +
           labs(x="Genera",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
           geom_bar(stat = "identity", 
                    aes(
                      text = paste(
                        "<br>Genus:",Genus,
                        "<br>Genera Level Drought Tolerance:",Drought_tol_Genus,
                        "<br>Mean psi TLP:", Mean_TLP_Value_Genus,
                        "<br>SE psi TLP:", SE_TLP_Value_Genus),
                      col=Drought_tol_Genus)) + ggtitle("All Data - Genera") +
           geom_pointrange(aes(x = Genus, ymin = Mean_TLP_Value_Genus - SE_TLP_Value_Genus, 
                               ymax = Mean_TLP_Value_Genus + SE_TLP_Value_Genus),
                           width=0.4, colour = "black", alpha=0.75, size = 0.5) + 
           coord_flip() +
           theme_classic() + 
           theme(axis.text.y=element_text(color = "black", size = 10, vjust = 0.5, hjust = 1, face = "italic"),
                 axis.text.x = element_text(color = "black", size = 10),
                 axis.ticks = element_line(color = "black"),
                 axis.title = element_text(size = 10),
                 legend.position = "none")+ coord_flip(), tooltip = "text")


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




# Just TMA Website Trees --------------------------------------------------
# For Just trees on Web
# calculate mean and SD for each species based on data type

# TDAG associations for Psi turgor loss point
# Sensitive: > -2.5 MPa
# Moderately Sensitive -2.5 MPa to -3 MPa
# Moderately Tolerant -3 MPa to -3.5 MPa
# Tolerant < -3.5 MPa


Morton_sum_turgor<- turgor_dat%>%
  filter(Plant_organ == "L" & dat.type == "hirons_psi.tlp" & OnMortonWeb == "x") %>%
 # bind_rows(., ElmCultivar) %>%
  group_by(SpeciesClean, dat.type, Source) %>%
  select(SpeciesClean, dat.type, value) %>%
  mutate(Mean_Value = round(mean(value),2)) %>%
  ungroup() %>%
  select(-Source, -value, -dat.type) %>%
  distinct() %>%
  group_by(SpeciesClean) %>%
  mutate(Mean_Value_hirons_psi.tlp = round(mean(Mean_Value),2),
         SE_Value_hirons_psi.tlp = round((sd(Mean_Value)/n()),2),
         Count_hirons_psi.tlp = n()) %>%
  distinct() %>%
 select(-Mean_Value)


# TDAG associations for Psi turgor loss point
# Sensitive: > -2.5 MPa
# Moderately Sensitive -2.5 MPa to -3 MPa
# Moderately Tolerant -3 MPa to -3.5 MPa
# Tolerant < -3.5 MPa


# Barplots by Species -----------------------------------------------------


# ALL 
# create plots by species - bar plot
# grouped by Source data

## edited this to average across a study. 26 Sept 2024
## Left off here. I THINK this is working the way we want? At least in terms of 
# all species & all points (with each point an average across a study)

# boxplot
spec_box_mort <- all_sum_turgor %>% filter(dat.type == "hirons_psi.tlp" & OnMortonWeb == "x") %>%
  select(SpeciesClean, Mean_TLP_Value_Source, 
         Source, Drought_tol_spec, 
         Drought_tol_source) %>% distinct()


ggplotly(ggplot(spec_box_mort,aes(x=reorder(SpeciesClean, Mean_TLP_Value_Source), y=Mean_TLP_Value_Source))+
           labs(x="Species",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
           geom_boxplot() + ggtitle("All Data")+
           geom_point(aes(
             text = paste(
               "<br>Species:",SpeciesClean,
               #"<br>Cultivar:", CultivarName,
               "<br> Avg Species Level Drought Tolerance:",Drought_tol_spec,
               "<br> Source Level Drought Tolerance:", Drought_tol_source,
               "<br>psi TLP:", Mean_TLP_Value_Source,
               #"<br>Count:", Count,
               "<br>Source Data File:",Source),
             col=Drought_tol_source)) +
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                 axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip(), tooltip = "text")


spec_bar_mort <- all_sum_turgor %>% filter(dat.type == "hirons_psi.tlp" & OnMortonWeb == "x") %>%
  select(SpeciesClean, Mean_TLP_Value_Species, Drought_tol_spec, SE_TLP_Value_Species) %>% distinct()

# species barplot
ggplotly(ggplot(spec_bar_mort, aes(x=reorder(SpeciesClean, Mean_TLP_Value_Species), 
                              y = Mean_TLP_Value_Species), color = Drought_tol_spec) +
           labs(x="Species",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
           geom_bar(stat = "identity", 
                    aes(
                      text = paste(
                        "<br>Species:",SpeciesClean,
                        # "<br>Cultivar:", Cultivar,
                        "<br>Species Drought Tolerance:",Drought_tol_spec,
                        "<br>Mean psi TLP:",  Mean_TLP_Value_Species,
                        "<br>SE psi TLP:", SE_TLP_Value_Species),
                      col=Drought_tol_spec)) + ggtitle("All Data") +
           geom_pointrange(aes(x = SpeciesClean, ymin = Mean_TLP_Value_Species -SE_TLP_Value_Species, 
                               ymax =  Mean_TLP_Value_Species + SE_TLP_Value_Species),
                           width=0.4, colour = "black", alpha=0.75, size = 0.5) + 
           coord_flip() +
           theme_classic() + 
           theme(axis.text.y=element_text(color = "black", size = 10, vjust = 0.5, hjust = 1, face = "italic"),
                 axis.text.x = element_text(color = "black", size = 10),
                 axis.ticks = element_line(color = "black"),
                 axis.title = element_text(size = 10),
                 legend.position = "none")+ coord_flip(), tooltip = "text")


gen_box_mort <- all_sum_turgor %>% filter(dat.type == "hirons_psi.tlp" & OnMortonWeb == "x") %>%
  select(Genus, SpeciesClean, Mean_TLP_Value_Species, SE_TLP_Value_Species,Drought_tol_spec,Drought_tol_Genus) %>% distinct()

# genus boxplot
ggplotly(ggplot(gen_box_mort,aes(x=reorder(Genus, Mean_TLP_Value_Species),
                            y= Mean_TLP_Value_Species))+
           labs(x="Genera",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
           geom_boxplot() + ggtitle("All Data - Genera")+
           geom_point(aes(
             text = paste(
               "<br>Species:",SpeciesClean,
               "<br>Species Level Drought Tolerance:",Drought_tol_spec,
               "<br>Genus Level Drought Tolerance:", Drought_tol_Genus,
               "<br>psi TLP:", Mean_TLP_Value_Species,
               "<br>SE psi TLP:", SE_TLP_Value_Species),
             col=Drought_tol_spec)) +
           theme_classic()+theme(axis.text.y=element_text(color="black",
                                                          size=10,vjust=0.5,
                                                          hjust=1,face="italic"),
                                 axis.text.x=element_text(color="black",size=10),
                                 axis.ticks=element_line(color="black"),
                                 axis.title=element_text(size=10),
                                 legend.position="none")+ coord_flip(), tooltip = "text")

gen_bar_mort <- all_sum_turgor %>% filter(dat.type == "hirons_psi.tlp" & OnMortonWeb == "x") %>%
  select(Genus, Mean_TLP_Value_Genus, Drought_tol_Genus, SE_TLP_Value_Genus) %>% distinct()

# genus barplot
ggplotly(ggplot(gen_bar_mort, aes(x=reorder(Genus, Mean_TLP_Value_Genus),
                             y= Mean_TLP_Value_Genus), color = Drought_tol_Genus) +
           labs(x="Genera",y="Leaf Water Potential at Turgor Loss Point (MPa)") +
           geom_bar(stat = "identity", 
                    aes(
                      text = paste(
                        "<br>Genus:",Genus,
                        "<br>Genera Level Drought Tolerance:",Drought_tol_Genus,
                        "<br>Mean psi TLP:", Mean_TLP_Value_Genus,
                        "<br>SE psi TLP:", SE_TLP_Value_Genus),
                      col=Drought_tol_Genus)) + ggtitle("All Data - Genera") +
           geom_pointrange(aes(x = Genus, ymin = Mean_TLP_Value_Genus - SE_TLP_Value_Genus, 
                               ymax = Mean_TLP_Value_Genus + SE_TLP_Value_Genus),
                           width=0.4, colour = "black", alpha=0.75, size = 0.5) + 
           coord_flip() +
           theme_classic() + 
           theme(axis.text.y=element_text(color = "black", size = 10, vjust = 0.5, hjust = 1, face = "italic"),
                 axis.text.x = element_text(color = "black", size = 10),
                 axis.ticks = element_line(color = "black"),
                 axis.title = element_text(size = 10),
                 legend.position = "none")+ coord_flip(), tooltip = "text")

desig_sum_turgor <- Morton_Web %>%
  left_join(spec_bar_mort) %>%
  left_join(desig) %>%
  distinct()# %>%
  #write.csv("2024_10_22_DroughtDesignationTable.csv",row.names=F)



# A meeting with Luke on 10/25/2024
# need to read out a new table that has the values for cultivar include separately. 


desig_table_turgor <- turgor_dat %>%
  filter(Plant_organ == "L" & dat.type == "hirons_psi.tlp" & OnMortonWeb == "x") %>%
  select(Species, Source, value, dat.type) %>%
  group_by(Species, Source) %>%
  mutate(
    Mean_TLP_Value_Source = round(mean(value), 2),  # Calculate mean for each group
    SE_TLP_Value_Source = round((sd(value) / sqrt(length(value))), 2)) %>%   # Calculate standard error
  ungroup()  %>%  # Ungroup if you're planning further joins or grouping steps
  right_join(.,turgor_dat) %>%
  filter(dat.type == "hirons_psi.tlp" & OnMortonWeb == "x")

desig_table_turgor <- desig_table_turgor %>%
  select(Species, Mean_TLP_Value_Source, SE_TLP_Value_Source) %>%
  distinct() %>%
  group_by(Species) %>%
  mutate(Mean_TLP_Value_Species = round(mean(Mean_TLP_Value_Source), 2),     # Calculate mean for each group
         SE_TLP_Value_Species = round((sd(Mean_TLP_Value_Source) / sqrt(length(Mean_TLP_Value_Source))), 2))%>%   # Calculate standard error
  ungroup() %>%
  right_join(.,desig_table_turgor) 

desig_table_turgor <- desig_table_turgor %>%
  select(Genus, Mean_TLP_Value_Species, dat.type) %>%
  distinct() %>%
  group_by(Genus) %>%
  mutate(Mean_TLP_Value_Genus = round(mean(Mean_TLP_Value_Species), 2),                    # Calculate mean for each group
         SE_TLP_Value_Genus = round((sd(Mean_TLP_Value_Species) / sqrt(length(Mean_TLP_Value_Species))), 2)   # Calculate standard error
  ) %>%
  ungroup() %>%
  right_join(desig_table_turgor)

desig_table_turgor$Drought_tol_source <- ifelse(desig_table_turgor$Mean_TLP_Value_Source > -2.5, "Sensitive",
                                            ifelse((desig_table_turgor$Mean_TLP_Value_Source >= -3 & desig_table_turgor$Mean_TLP_Value_Source <= -2.5), "Moderately Sensitive",
                                                   ifelse((desig_table_turgor$Mean_TLP_Value_Source >= -3.5 & desig_table_turgor$Mean_TLP_Value_Source < -3), "Moderately Tolerant",
                                                          ifelse(desig_table_turgor$Mean_TLP_Value_Source < -3.5, "Tolerant", NA))))



# if else statements to create column of assigned drought designations using the above parameters
desig_table_turgor$Drought_tol_spec <- ifelse(desig_table_turgor$Mean_TLP_Value_Species > -2.5, "Sensitive",
                                          ifelse((desig_table_turgor$Mean_TLP_Value_Species >= -3 & desig_table_turgor$Mean_TLP_Value_Species <= -2.5), "Moderately Sensitive",
                                                 ifelse((desig_table_turgor$Mean_TLP_Value_Species >= -3.5 & desig_table_turgor$Mean_TLP_Value_Species < -3), "Moderately Tolerant",
                                                        ifelse(desig_table_turgor$Mean_TLP_Value_Species < -3.5, "Tolerant", NA))))

# sum turgor no longer has p12, p50 or p88 data summary. To make the following code work,
# need to redo sum_turgor and change grouping variable or filter.

desig_table_turgor$Drought_tol_Genus <- ifelse(desig_table_turgor$Mean_TLP_Value_Genus > -2.5, "Sensitive",
                                           ifelse((desig_table_turgor$Mean_TLP_Value_Genus >= -3 & desig_table_turgor$Mean_TLP_Value_Genus <= -2.5), "Moderately Sensitive",
                                                  ifelse((desig_table_turgor$Mean_TLP_Value_Genus >= -3.5 & desig_table_turgor$Mean_TLP_Value_Genus < -3), "Moderately Tolerant",
                                                         ifelse(desig_table_turgor$Mean_TLP_Value_Genus < -3.5, "Tolerant", NA))))


Morton_Web1 <- Morton_Web %>%
  mutate(InDatabase = SpeciesClean %in% desig_table_turgor$SpeciesClean) %>% filter(InDatabase == "FALSE")  %>%
  select(Species, SpeciesClean)


desig_sum_turgor <- desig_table_turgor %>%
  select(6,2,9,42,44) %>%
  bind_rows(Morton_Web1) %>%
  left_join(desig) %>%
  distinct() %>%
  select(-4,-6,-7,-10,-15) %>%
  write.csv("2024_10_25_DroughtDesignationTable.csv",row.names=F)
