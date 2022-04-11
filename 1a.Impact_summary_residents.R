

# Read in CA data from the "tigris" library. This data set is used to merge with 
# other data sets used throughout to create the maps

cali <- counties("California", cb = TRUE)

# change the name of the 6th column to "County" for easy merging.
colnames(cali)[6] <- "County"

# Read in CMF data and filter for awardee data
all_cycles <- read.csv("All Cycles.csv", stringsAsFactors = FALSE)
awardees <- subset(all_cycles, Awarded.Yes.No == "Yes")
rm(all_cycles)

# Group by county and discipline for summary data
County_summary <- awardees %>% 
  group_by(County, Discipline) %>% 
  summarize(n = sum(Positions.Awarded))
            
#####################################################################################################

# For the following, we breakdown by county and create the summary text for each county. The total will have to be updated by hand.
#Alameda
Alameda <- County_summary %>% filter(County == "Alameda") 

Alameda_mytext <- paste0("County: ", Alameda$County, "<br>",
                         "IM resident positions funded: ", Alameda$n[2],"<br>",
                         "OBGYN resident positions funded: ", Alameda$n[3], "<br>",
                         "Peds resident positions funded: ", Alameda$n[4], "<br>",
                         "EM resident positions funded: ", Alameda$n[1], "<br>", sep = " ")
Alameda_mytext <- unique(Alameda_mytext)

rm(Alameda)

#Amador
Amador_cost <- County_summary %>% filter(County == "Amador")
Amador_text <-  paste0("County: ", Amador_cost$County, 
                       "FM resident positions funded: ", Amador_cost$n, sep = " ")
rm(Amador_cost)

###Contra Costa####
Contra_cost <- County_summary %>% filter(County == "Contra Costa")
Conta_text <-  paste0("County: ", Contra_cost$County, "<br>",
                      "IM resident positions funded: ", Contra_cost$n, sep = " ")
rm(Contra_cost)

###Fresno
Fresno <- County_summary %>% filter(County == "Fresno") 
Fresno_mytext <- paste0("County: ", Fresno$County, "<br>",
                        "FM resident positions funded: ", Fresno$n[2], "<br>",
                        "IM resident positions funded: ", Fresno$n[3], "<br>",
                        "OBGYN resident positions funded: ", Fresno$n[4], "<br>",
                        "Peds resident positions funded: ", Fresno$n[5], "<br>",
                        "EM resident positions funded: ", Fresno$n[1], sep =" ")
Fresno_mytext <- unique(Fresno_mytext)
rm(Fresno)
######Humboldt

Humboldt_cost <- County_summary %>% filter(County == "Humboldt")
Humboldt_text <-  paste0("County: ", Humboldt_cost$County, "<br>", 
                         "FM resident positions funded: ", Humboldt_cost$n, sep = " ")
rm(Humboldt_cost)

##Imperial

imperial <- County_summary %>% filter(County == "Imperial")
imperial_mytext <- paste0("County: ", imperial$County, "<br>",
                          "EM resident positions funded: ", imperial$n[1], sep =" ")

imperial_mytext <- unique(imperial_mytext)

rm(imperial)

#####Kern
Kern <- County_summary %>% filter(County == "Kern") 
Kern_mytext <- paste0("County: ", Kern$County, "<br>",
                      "FM resident positions funded: ", Kern$n[2], "<br>",
                      "IM resident positions funded: ", Kern$n[3], "<br>",
                      "OBGYN resident positions funded: ", Kern$n[4], "<br>",
                      "EM resident positions funded: ", Kern$n[1], sep =" ")
Kern_mytext <- unique(Kern_mytext)
rm(Kern)

#####Kings
Kings_cost <- County_summary %>% filter(County == "Kings")
Kings_text <-  paste0("County: ", Kings_cost$County, "<br>", 
                      "FM resident positions funded: ", Kings_cost$n, sep = " ")
rm(Kings_cost)

########Los Angeles
Los_Angeles <- County_summary %>% filter(County == "Los Angeles") 

Los_Angeles_mytext <- paste0("County: ", Los_Angeles$County, "<br>",
                             "FM resident positions funded: ", Los_Angeles$n[2], "<br>",
                             "IM resident positions funded: ", Los_Angeles$n[3], "<br>",
                             "OBGYN resident positions funded: ", Los_Angeles$n[4], "<br>",
                             "Peds resident positions funded: ", Los_Angeles$n[5], "<br>",
                             "EM resident positions funded: ", Los_Angeles$n[1], sep =" ")

Los_Angeles_mytext <- unique(Los_Angeles_mytext)
rm(Los_Angeles)
#####Madera

Madera_cost <- County_summary %>% filter(County == "Madera")
Madera_text <-  paste0("County: ", Madera_cost$County, "<br>",
                       "Peds resident positions funded: ", Madera_cost$n, sep = " ")
rm(Madera_cost)

#####Mendocino
Mendocino_cost <- County_summary %>% filter(County == "Mendocino")
Mendocino_text <-  paste0("County: ", Mendocino_cost$County, "<br>", 
                          "FM resident positions funded: ", Mendocino_cost$n, sep = " ")
rm(Mendocino_cost)

######Merced
Merced_cost <- County_summary %>% filter(County == "Merced")
Merced_text <-  paste0("County: ", Merced_cost$County, "<br>", 
                       "FM resident positions funded: ", Merced_cost$n, sep = " ")
rm(Merced_cost)
###Monterey

Monterey_cost <- County_summary %>% filter(County == "Monterey")
Monterey_text <-  paste0("County: ", Monterey_cost$County, "<br>", 
                         "FM resident positions funded: ", Monterey_cost$n, sep = " ")
rm(Monterey_cost)

#######Orange
Orange <- County_summary %>% filter(County == "Orange") 
Orange_mytext <- paste0("County: ", Orange$County, "<br>", 
                        "FM resident positions funded: ", Orange$n[1], "<br>",
                        "IM resident positions funded: ", Orange$n[2], "<br>",
                        "Peds resident positions funded: ", Orange$n[3], sep = " ")
Orange_mytext <- unique(Orange_mytext)
rm(Orange)
#######Riverside

Riverside <- County_summary %>% filter(County == "Riverside") 

Riverside_mytext <- paste0("County: ", Riverside$County, "<br>",
                           "FM resident positions funded: ", Riverside$n[2], "<br>",
                           "IM resident positions funded: ", Riverside$n[3], "<br>",
                           "EM resident positions funded: ", Riverside$n[1], sep = " ")

Riverside_mytext <- unique(Riverside_mytext)
rm(Riverside)
##############Sacramento

Sacramento <- County_summary %>% filter(County == "Sacramento") 
Sacramento_mytext <- paste0("County: ", Sacramento$County, "<br>", 
                            "FM resident positions funded: ", Sacramento$n[1], "<br>",
                            "IM resident positions funded: ", Sacramento$n[2], "<br>",
                            "OBGYN resident positions funded: ", Sacramento$n[3], "<br>",
                            "Peds resident positions funded: ", Sacramento$n[4], sep = " ")
Sacramento_mytext <- unique(Sacramento_mytext)
rm(Sacramento)
###################San Bernardino

San_Berna <- County_summary %>% filter(County == "San Bernardino") 

San_Berna_mytext <- paste0("County: ", San_Berna$County, "<br>", 
                           "FM resident positions funded: ", San_Berna$n[2], "<br>",
                           "IM resident positions funded: ", San_Berna$n[3], "<br>",
                           "OBGYN resident positions funded: ", San_Berna$n[4], "<br>",
                           "Peds resident positions funded: ", San_Berna$n[5], "<br>",
                           "EM resident positions funded: ", San_Berna$n[1], sep = " ")

San_Berna_mytext <- unique(San_Berna_mytext)
rm(San_Berna)
######San Diego

San_Di <- County_summary %>% filter(County == "San Diego") 

San_Di_mytext <- paste0("County: ", San_Di$County, "<br>", 
                        "FM resident positions funded: ", San_Di$n[2], "<br>",
                        "IM resident positions funded: ", San_Di$n[3], "<br>",
                        "OBGYN resident positions funded: ", San_Di$n[4], "<br>",
                        "Peds resident positions funded: ", San_Di$n[5],"<br>",
                        "EM resident positions funded: ", San_Di$n[1], sep = " ")

San_Di_mytext <- unique(San_Di_mytext)
rm(San_Di)
######San Fran

San_Fran <- County_summary %>% filter(County == "San Francisco")

San_Fran_mytext <- paste0("County: ", San_Fran$County, "<br>", 
                          "OBGYN resident positions funded: ", San_Fran$n[2], "<br>",
                          "Peds resident positions funded: ", San_Fran$n[3], "<br>",
                          "EM resident positions funded: ", San_Fran$n[1], sep = "")

San_Fran_mytext <- unique(San_Fran_mytext)
rm(San_Fran)
####San Joaquin

San_Joaq <- County_summary %>% filter(County == "San Joaquin") 

San_Joaq_mytext <- paste0("County: ", San_Joaq$County, "<br>", 
                          "FM resident positions funded: ", San_Joaq$n[2], "<br>",
                          "IM resident positions funded: ", San_Joaq$n[3], "<br>",
                          "EM resident positions funded: ", San_Joaq$n[1], sep = "")

San_Joaq_mytext <- unique(San_Joaq_mytext)
rm(San_Joaq)
###San Mateo

San_Mat_cost <- County_summary %>% filter(County == "San Mateo")
San_Mat_text <-  paste0("County: ", San_Mat_cost$County, "<br>", 
                        "Peds resident positions funded: ", San_Mat_cost$n, sep = "")
rm(San_Mat_cost)
#####Santa Barbara

Santa_Barbs_cost <- County_summary %>% filter(County == "Santa Barbara")

Santa_Barbs_text <-  paste0("County: ", Santa_Barbs_cost$County, "<br>", 
                            "FM resident positions funded: ", Santa_Barbs_cost$n[1], "<br>",
                            "OBGYN resident positions funded: ", Santa_Barbs_cost$n[2], "<br>",
                            "Peds resident positions funded: ", Santa_Barbs_cost$n[3], sep = "")

Santa_Barbs_text <- unique(Santa_Barbs_text)
rm(Santa_Barbs_cost)
#######Santa Clara

Santa_Clara <- County_summary %>% filter(County == "Santa Clara") 
Santa_Clara_mytext <- paste0("County: ", Santa_Clara$County, "<br>", 
                             "FM resident positions funded: ", Santa_Clara$n[1], "<br>",
                             "IM resident positions funded: ", Santa_Clara$n[2], "<br>",
                             "OBGYN resident positions funded: ", Santa_Clara$n[3], "<br>",
                             "Peds resident positions funded: ", Santa_Clara$n[4], sep = "")
Santa_Clara_mytext <- unique(Santa_Clara_mytext)
rm(Santa_Clara)
#####Shasta

Shasta_cost <- County_summary %>% filter(County == "Shasta")

Shasta_text <-  paste0("County: ", Shasta_cost$County, "<br>", 
                       "FM resident positions funded: ", Shasta_cost$n[1], "<br>",
                       "IM resident positions funded: ", Shasta_cost$n[2], sep = "")

Shasta_text <- unique(Shasta_text)
rm(Shasta_cost)
#####Sonoma

Sonoma_cost <- County_summary %>% filter(County == "Sonoma")
Sonoma_text <-  paste0("County: ", Sonoma_cost$County, "<br>", 
                       "FM resident positions funded: ", Sonoma_cost$n, sep = "" )
rm(Sonoma_cost)
##############Stanislaus

Stanislaus <- County_summary %>% filter(County == "Stanislaus") 
Stanislaus_mytext <- paste0("County: ", Stanislaus$County, "<br>", 
                            "FM resident positions funded: ", Stanislaus$n[2], "<br>",
                            "EM resident positions funded: ", Stanislaus$n[1], sep = "")
Stanislaus_mytext <- unique(Stanislaus_mytext)
rm(Stanislaus)
######Tulare

Tulare <- County_summary %>% filter(County == "Tulare") 
Tulare_mytext <- paste0("County: ", Tulare$County, "<br>", 
                        "FM resident positions funded: ", Tulare$n[2], "<br>",
                        "EM resident positions funded: ", Tulare$n[1], sep = "")
Tulare_mytext <- unique(Tulare_mytext)
rm(Tulare)
####Ventura

Ventura <- County_summary %>% filter(County == "Ventura") 
Ventura_mytext <- paste0("County: ", Ventura$County, "<br>", 
                         "FM resident positions funded: ", Ventura$n[2], "<br>",
                         "EM resident positions funded: ", Ventura$n[1], sep = "")
Ventura_mytext <- unique(Ventura_mytext)
rm(Ventura)

# the following counties do not have awardees. If county gets awardees for the format from the top.
# Del Norte
delNorte <- paste0("County: Del Norte", "<br>",
                   "No resident positions funded in this county", sep = "")
#Siskiyou
siskiyo <- paste0("County: Siskiyo", "<br>",
                  "No resident positions funded in this county", sep = "")
#modoc
modoc <- paste0("County: Modoc", "<br>",
                "No resident positions funded in this county", sep = "")
#trinity
Trinity <- paste0("County: Trinity", "<br>",
                  "No resident positions funded in this county", sep = "")
#lassen
lassen <- paste0("County: Lassen", "<br>",
                 "No resident positions funded in this county", sep = "")
#tehama
tehama <- paste0("County: Tehama", "<br>",
                 "No resident positions funded in this county", sep = "")
#plumas
Plumas <- paste0("County: Plumas", "<br>",
                 "No resident positions funded in this county", sep = "")
#butte
Butte <- paste0("County: Butte", "<br>",
                "No resident positions funded in this county", sep = "")
#glenn
Glenn <- paste0("County: Glenn", "<br>",
                "No resident positions funded in this county", sep = "")
#lake
Lake <- paste0("County: Lake", "<br>",
               "No resident positions funded in this county", sep = "")
#colusa
colusa <- paste0("County: Colusa", "<br>",
                 "No resident positions funded in this county", sep = "")
#sutter
Sutter <- paste0("County: Sutter", "<br>",
                 "No resident positions funded in this county", sep = "")
#yuba
Yuba <- paste0("County: Yuba", "<br>",
               "No resident positions funded in this county", sep = "")
#nevada
nevada <- paste0("County: Nevada", "<br>",
                 "No resident positions funded in this county", sep = "")
#placer
Placer <- paste0("County: Placer", "<br>",
                 "No resident positions funded in this county", sep = "")
#el dorado
elDorado <- paste0("County: El Dorado", "<br>",
                   "No resident positions funded in this county", sep = "")
#yolo
Yolo <- paste0("County: Yolo", "<br>",
               "No resident positions funded in this county", sep = "")
#NAPA
napa <- paste0("County: Napa", "<br>",
               "No resident positions funded in this county", sep = "")
#solano
solano <- paste0("County: Solano", "<br>",
                 "No resident positions funded in this county", sep = "")
#marin
marin <- paste0("County: Marin", "<br>",
                "No resident positions funded in this county", sep = "")
#calaveras
calaveras <- paste0("County: Calaveras", "<br>",
                    "No resident positions funded in this county", sep = "")
#alpine
Alpine <- paste0("County: Alpine", "<br>",
                 "No resident positions funded in this county", sep = "")
#tuolumne
tuolumne <- paste0("County: Tuolumne", "<br>",
                   "No resident positions funded in this county", sep = "")
#mariposa
mariposa <- paste0("County: Mariposa", "<br>",
                   "No resident positions funded in this county", sep = "")
#mono
mono <- paste0("County: Mono", "<br>",
               "No resident positions funded in this county", sep = "")
#inyo
Inyo <- paste0("County: Inyo", "<br>",
               "No resident positions funded in this county", sep = "")
#santa cruz
santo_Cruz <- paste0("County: Santa Cruz", "<br>",
                     "No resident positions funded in this county", sep = "")
#san benito
sanBenito <- paste0("County: San Benito", "<br>",
                    "No resident positions funded in this county", sep = "")
#slo
slo <- paste0("County: San Luis Obispo", "<br>",
              "No resident positions funded in this county", sep = "")
#sierra
Sierra <- paste0("County: Sierra", "<br>",
                 "No resident positions funded in this county", sep = "")

# we then paste all the text into a vecto
x <- c(Alameda_mytext, Alpine, Amador_text, Butte, calaveras, colusa, Conta_text, delNorte, elDorado, Fresno_mytext,
       Glenn, Humboldt_text, imperial_mytext, Inyo, Kern_mytext, Kings_text, Lake, lassen, Los_Angeles_mytext,
       Madera_text, marin, mariposa, Mendocino_text, Merced_text, modoc, mono, Monterey_text, napa, nevada,
       Orange_mytext, Placer, Plumas, Riverside_mytext, Sacramento_mytext, sanBenito, San_Berna_mytext,
       San_Di_mytext, San_Fran_mytext, San_Joaq_mytext, slo, San_Mat_text, Santa_Barbs_text,
       Santa_Clara_mytext, santo_Cruz, Shasta_text, Sierra, siskiyo, solano, Sonoma_text, Stanislaus_mytext, Sutter, 
       tehama, Trinity, Tulare_mytext, tuolumne, Ventura_mytext, Yolo, Yuba)

# remove the text we created to clean up some space
rm(list = c("Alameda_mytext", "Amador_text", "Conta_text", "Fresno_mytext", "imperial_mytext",
            "Humboldt_text", "Kern_mytext", "Kings_text", "Los_Angeles_mytext",
            "Madera_text", "Mendocino_text", "Merced_text", "Monterey_text",
            "Orange_mytext", "Riverside_mytext", "Sacramento_mytext", "San_Berna_mytext",
            "San_Di_mytext", "San_Fran_mytext", "San_Joaq_mytext", "San_Mat_text", "Santa_Barbs_text",
            "Santa_Clara_mytext", "Shasta_text", "Sonoma_text", "Stanislaus_mytext", "Tulare_mytext", "Ventura_mytext",
            "Alpine", "Butte", "calaveras", "colusa", "delNorte", "Glenn", "Inyo",
            "Lake", "lassen", "marin", "mariposa", "modoc", "mono", "napa", "nevada","Placer", "sanBenito", "santo_Cruz", "Sierra", "siskiyo", "slo", 
            "Sutter", "tehama", "Trinity", "tuolumne", "Yolo", "Yuba", "elDorado", "Plumas", "solano"))

# turn the vector into a data frame to merge
x <- as.data.frame(x)
#remove the county summary data set
rm(County_summary)

#Get group by county and tier level
county_summaries <- awardees %>% 
  group_by(County) %>% 
  summarize(positions = sum(Positions.Awarded))

#merge county to Cali dataset by "county"
resident_joined <- left_join(cali, county_summaries, by = "County")

#arrange by alphabetical order
resident_joined <- resident_joined %>% arrange(County)

#join dataset with the text data frame
resident_joined <- cbind(resident_joined, x)

labeltext <- resident_joined %>% filter(!is.na(positions)) %>% 
  mutate(labeltext = paste0("County: ", County, "<br>",
                            "Total resident positions funded: ", positions, sep = ""))

labeltext1 <- resident_joined %>% filter(is.na(positions)) %>% 
  mutate(labeltext = paste0("County: ", County, "<br>",
                            "No resident positions funded in this county", sep = ""))

residents <- rbind(labeltext, labeltext1) %>% arrange(County) %>% 
  select("County", "positions", "x", "labeltext")


rm(list = c("county_summaries", "x", "labeltext", "labeltext1", "resident_joined"))

gc()
