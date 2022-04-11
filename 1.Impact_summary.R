

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
  group_by(County, Discipline, Tier.Level) %>% 
  summarize(n = n(),
            Total.Awarded.Amount = sum(Award.Amount))

County_summary$Total.Awarded.Amount <- dollar(County_summary$Total.Awarded.Amount)
#####################################################################################################

# For the following, we breakdown by county and create the summary text for each county. The total will have to be updated by hand.
#Alameda
Alameda <- County_summary %>% filter(County == "Alameda") 

Alameda_mytext <- paste0("County: ", Alameda$County, "<br>",
                         "Tier Level: ", Alameda$Tier.Level, "<br>",
                         "IM Programs Awarded: ", Alameda$n[2], " & ", "Total Amount Awarded: ", Alameda$Total.Awarded.Amount[2], "<br>",
                         "OBGYN Programs Awarded: ", Alameda$n[3], " & ", "otal Amount Awarded: ", Alameda$Total.Awarded.Amount[3], "<br>",
                         "Peds Programs Awarded: ", Alameda$n[4], " & ", "Total Amount Awarded: ", Alameda$Total.Awarded.Amount[4], "<br>",
                         "EM Programs Awarded: ", Alameda$n[1], " & ", "Total Amount Awarded: ", Alameda$Total.Awarded.Amount[1], "<br>",
                         "<b>Total Programs Awarded: 13 | Total Funds Awarded: $4,940,000</b>", sep = " ")
Alameda_mytext <- unique(Alameda_mytext)

rm(Alameda)

#Amador
Amador_cost <- County_summary %>% filter(County == "Amador")
Amador_text <-  paste0("County: ", Amador_cost$County, 
                       "Tier Level: ", Amador_cost$Tier.Level, "<br>",
                       "FM Programs Awarded: ", Amador_cost$n, " & ", "Total Amount Awarded: ", Amador_cost$Total.Awarded.Amount, "<br>",
                       "<b>Total Programs Awarded: 1 | Total Funds Awarded: $225,000</b>", sep = " ")
rm(Amador_cost)

###Contra Costa####
Contra_cost <- County_summary %>% filter(County == "Contra Costa")
Conta_text <-  paste0("County: ", Contra_cost$County, "<br>",
                      "Tier Level: ", Contra_cost$Tier.Level, "<br>",
                      "IM Programs Awarded: ", Contra_cost$n, " & ", "Total Amount Awarded: ", Contra_cost$Total.Awarded.Amount, "<br>",
                      "<b>Total Programs Awarded: 9 | Total Funds Awarded: $2,625,000</b>", sep = " ")
rm(Contra_cost)

###Fresno
Fresno <- County_summary %>% filter(County == "Fresno") 
Fresno_mytext <- paste0("County: ", Fresno$County, "<br>",
                        "Tier Level: ", Fresno$Tier.Level, "<br>",
                        "FM Programs Awarded: ", Fresno$n[2], " & ", "Total Amount Awarded: ", Fresno$Total.Awarded.Amount[2], "<br>",
                        "IM Programs Awarded: ", Fresno$n[3], " & ", "Total Amount Awarded: ", Fresno$Total.Awarded.Amount[3], "<br>",
                        "OBGYN Programs Awarded: ", Fresno$n[4], " & ", "Total Amount Awarded: ", Fresno$Total.Awarded.Amount[4], "<br>",
                        "Peds Programs Awarded: ", Fresno$n[5], " & ", "Total Amount Awarded: ", Fresno$Total.Awarded.Amount[5], "<br>",
                        "EM Programs Awarded: ", Fresno$n[1], " & ", "Total Amount Awarded: ", Fresno$Total.Awarded.Amount[1], "<br>",
                        "<b>Total Programs Awarded: 32 | Total Funds Awarded: $18,955,000</b>", sep =" ")
Fresno_mytext <- unique(Fresno_mytext)
rm(Fresno)
######Humboldt

Humboldt_cost <- County_summary %>% filter(County == "Humboldt")
Humboldt_text <-  paste0("County: ", Humboldt_cost$County, "<br>", 
                         "Tier Level: ", Humboldt_cost$Tier.Level, "<br>",
                         "FM Programs Awarded: ", Humboldt_cost$n, " & ", "Total Amount Awarded: ", Humboldt_cost$Total.Awarded.Amount, "<br>",
                         "<b>Total Programs Awarded: 4 | Total Funds Awarded: $1,350,000</b>", sep = " ")
rm(Humboldt_cost)

##Imperial

imperial <- County_summary %>% filter(County == "Imperial")
imperial_mytext <- paste0("County: ", imperial$County, "<br>",
                          "Tier Level: ", imperial$Tier.Level, "<br>",
                          "EM Programs Awarded: ", imperial$n[1], " & ", "Total Amount Awarded: ", imperial$Total.Awarded.Amount[1], "<br>",
                          "<b>Total Programs Awarded: 1 | Total Funds Awarded: $200,000</b>", sep =" ")
imperial_mytext <- unique(imperial_mytext)
rm(imperial)

#####Kern
Kern <- County_summary %>% filter(County == "Kern") 
Kern_mytext <- paste0("County: ", Kern$County, "<br>",
                      "Tier Level: ", Kern$Tier.Level, "<br>",
                      "FM Programs Awarded: ", Kern$n[2], " & ", "Total Amount Awarded: ", Kern$Total.Awarded.Amount[2], "<br>",
                      "IM Programs Awarded: ", Kern$n[3], " & ", "Total Amount Awarded: ", Kern$Total.Awarded.Amount[3], "<br>",
                      "OBGYN Programs Awarded: ", Kern$n[4], " & ", "Total Amount Awarded: ", Kern$Total.Awarded.Amount[4], "<br>",
                      "EM Programs Awarded: ", Kern$n[1], " & ", "Total Amount Awarded: ", Kern$Total.Awarded.Amount[1], "<br>",
                      "<b>Total Programs Awarded: 16 | Total Funds Awarded: $6,485,000</b>", sep =" ")
Kern_mytext <- unique(Kern_mytext)
rm(Kern)

#####Kings
Kings_cost <- County_summary %>% filter(County == "Kings")
Kings_text <-  paste0("County: ", Kings_cost$County, "<br>", 
                      "Tier Level: ", Kings_cost$Tier.Level, "<br>",
                      "FM Programs Awarded: ", Kings_cost$n, " & ", "Total Amount Awarded: ", Kings_cost$Total.Awarded.Amount, "<br>",
                      "<b>Total Programs Awarded: 4 | Total Funds Awarded: $885,000</b>", sep = " ")
rm(Kings_cost)

########Los Angeles
Los_Angeles <- County_summary %>% filter(County == "Los Angeles") 
Los_Angeles_mytext <- paste0("County: ", Los_Angeles$County, "<br>",
                             "Tier Level: ", Los_Angeles$Tier.Level, "<br>",
                             "FM Programs Awarded: ", Los_Angeles$n[2], " &", "Total Amount Awarded: ", Los_Angeles$Total.Awarded.Amount[2], "<br>",
                             "IM Programs Awarded: ", Los_Angeles$n[3], " & ", "Total Amount Awarded: ", Los_Angeles$Total.Awarded.Amount[3], "<br>",
                             "OBGYN Programs Awarded: ", Los_Angeles$n[4], " & ", "Total Amount Awarded: ", Los_Angeles$Total.Awarded.Amount[4], "<br>",
                             "Peds Programs Awarded: ", Los_Angeles$n[5], " & ", "Total Amount Awarded: ", Los_Angeles$Total.Awarded.Amount[5], "<br>",
                             "EM Programs Awarded: ", Los_Angeles$n[1], " & ", "Total Amount Awarded: ", Los_Angeles$Total.Awarded.Amount[1], "<br>",
                             "<b>Total Programs Awarded: 67 | Total Funds Awarded: $24,395,000</b>", sep =" ")
Los_Angeles_mytext <- unique(Los_Angeles_mytext)
rm(Los_Angeles)
#####Madera

Madera_cost <- County_summary %>% filter(County == "Madera")
Madera_text <-  paste0("County: ", Madera_cost$County, "<br>",
                       "Tier Level: ", Madera_cost$Tier.Level, "<br>",
                       "Peds Programs Awarded: ", Madera_cost$n, " & ", "Total Amount Awarded: ", Madera_cost$Total.Awarded.Amount, "<br>",
                       "<b>Total Programs Awarded: 4 | Total Funds Awarded: $3,470,000</b>", sep = " ")
rm(Madera_cost)

#####Mendocino
Mendocino_cost <- County_summary %>% filter(County == "Mendocino")
Mendocino_text <-  paste0("County: ", Mendocino_cost$County, "<br>", 
                          "Tier Level: ", Mendocino_cost$Tier.Level, "<br>",
                          "FM Programs Awarded: ", Mendocino_cost$n, " & ", "Total Amount Awarded: ", Mendocino_cost$Total.Awarded.Amount, "<br>",
                          "<b>Total Programs Awarded: 3 | Total Funds Awarded: $675,000</b>", sep = " ")
rm(Mendocino_cost)

######Merced
Merced_cost <- County_summary %>% filter(County == "Merced")
Merced_text <-  paste0("County: ", Merced_cost$County, "<br>", 
                       "Tier Level: ", Merced_cost$Tier.Level, "<br>",
                       "FM Programs Awarded: ", Merced_cost$n, " & ", "Total Amount Awarded: ", Merced_cost$Total.Awarded.Amount, "<br>",
                       "<b>Total Programs Awarded: 4 | Total Funds Awarded: $975,000</b>", sep = " ")
rm(Merced_cost)
###Monterey

Monterey_cost <- County_summary %>% filter(County == "Monterey")
Monterey_text <-  paste0("County: ", Monterey_cost$County, "<br>", 
                         "Tier Level: ", Monterey_cost$Tier.Level, "<br>",
                         "FM Programs Awarded: ", Monterey_cost$n, " & ", "Total Amount Awarded: ", Monterey_cost$Total.Awarded.Amount, "<br>",
                         "<b>Total Programs Awarded: 3 | Total Funds Awarded: $450,000</b>", sep = " ")
rm(Monterey_cost)

#######Orange
Orange <- County_summary %>% filter(County == "Orange") 
Orange_mytext <- paste0("County: ", Orange$County, "<br>", 
                        "Tier Level: ", Orange$Tier.Level, "<br>",
                        "FM Programs Awarded: ", Orange$n[1], " & ", "Total Amount Awarded: ", Orange$Total.Awarded.Amount[1], "<br>",
                        "IM Programs Awarded: ", Orange$n[2], " & ", "Total Amount Awarded: ", Orange$Total.Awarded.Amount[2], "<br>",
                        "Peds Programs Awarded: ", Orange$n[3], " & ", "Total Amount Awarded: ", Orange$Total.Awarded.Amount[3], "<br>",
                        "<b>Total Programs Awarded: 8 | Total Funds Awarded: $2,700,000</b>", sep = " ")
Orange_mytext <- unique(Orange_mytext)
rm(Orange)
#######Riverside

Riverside <- County_summary %>% filter(County == "Riverside") 
Riverside_mytext <- paste0("County: ", Riverside$County, "<br>",
                           "Tier Level: ", Riverside$Tier.Level, "<br>",
                           "FM Programs Awarded: ", Riverside$n[2], " & ", "Total Amount Awarded: ", Riverside$Total.Awarded.Amount[2], "<br>",
                           "IM Programs Awarded: ", Riverside$n[3], " & ", "Total Amount Awarded: ", Riverside$Total.Awarded.Amount[3], "<br>",
                           "EM Programs Awarded: ", Riverside$n[1], " & ", "Total Amount Awarded: ", Riverside$Total.Awarded.Amount[1], "<br>",
                           "<b>Total Programs Awarded: 56 | Total Funds Awarded: $23,895,000</b>", sep = " ")
Riverside_mytext <- unique(Riverside_mytext)
rm(Riverside)
##############Sacramento

Sacramento <- County_summary %>% filter(County == "Sacramento") 
Sacramento_mytext <- paste0("County: ", Sacramento$County, "<br>", 
                            "Tier Level: ", Sacramento$Tier.Level, "<br>",
                            "FM Programs Awarded: ", Sacramento$n[1], " & ", "Total Amount Awarded: ", Sacramento$Total.Awarded.Amount[1], "<br>",
                            "IM Programs Awarded: ", Sacramento$n[2], " & ", "Total Amount Awarded: ", Sacramento$Total.Awarded.Amount[2], "<br>",
                            "OBGYN Programs Awarded: ", Sacramento$n[3], " & ", "Total Amount Awarded: ", Sacramento$Total.Awarded.Amount[3], "<br>",
                            "Peds Programs Awarded: ", Sacramento$n[4], " & ", "Total Amount Awarded: ", Sacramento$Total.Awarded.Amount[4], "<br>",
                            "<b>Total Programs Awarded: 11 | Total Funds Awarded: $2,520,000</b>", sep = " ")
Sacramento_mytext <- unique(Sacramento_mytext)
rm(Sacramento)
###################San Bernardino

San_Berna <- County_summary %>% filter(County == "San Bernardino") 
San_Berna_mytext <- paste0("County: ", San_Berna$County, "<br>", 
                           "Tier Level: ", San_Berna$Tier.Level, "<br>",
                           "FM Programs Awarded: ", San_Berna$n[2], " & ", "Total Amount Awarded: ", San_Berna$Total.Awarded.Amount[2], "<br>",
                           "IM Programs Awarded: ", San_Berna$n[3], " & ", "Total Amount Awarded: ", San_Berna$Total.Awarded.Amount[3], "<br>",
                           "OBGYN Programs Awarded: ", San_Berna$n[4], " & ", "Total Amount Awarded: ", San_Berna$Total.Awarded.Amount[4], "<br>",
                           "Peds Programs Awarded: ", San_Berna$n[5], " & ", "Total Amount Awarded: ", San_Berna$Total.Awarded.Amount[5], "<br>",
                           "EM Programs Awarded: ", San_Berna$n[1], " & ", "Total Amount Awarded: ", San_Berna$Total.Awarded.Amount[1], "<br>",
                           "<b>Total Programs Awarded: 38 | Total Funds Awarded: $21,380,000</b>", sep = " ")
San_Berna_mytext <- unique(San_Berna_mytext)
rm(San_Berna)
######San Diego

San_Di <- County_summary %>% filter(County == "San Diego") 
San_Di_mytext <- paste0("County: ", San_Di$County, "<br>", 
                        "Tier Level: ", San_Di$Tier.Level, "<br>",
                        "FM Programs Awarded: ", San_Di$n[2], " & ", "Amount Awarded: ", San_Di$Total.Awarded.Amount[2], "<br>",
                        "IM Programs Awarded: ", San_Di$n[3], " & ", "Amount Awarded: ", San_Di$Total.Awarded.Amount[3], "<br>",
                        "OBGYN Programs Awarded: ", San_Di$n[4], " & ", "Amount Awarded: ", San_Di$Total.Awarded.Amount[4], "<br>",
                        "Peds Programs Awarded: ", San_Di$n[5], " & ", "Amount Awarded: ", San_Di$Total.Awarded.Amount[5], "<br>",
                        "EM Programs Awarded: ", San_Di$n[1], " & ", "Amount Awarded: ", San_Di$Total.Awarded.Amount[1], "<br>",
                        "<b>Total Programs Awarded: 18 | Total Funds Awarded: $5,990,000</b>", sep = " ")
San_Di_mytext <- unique(San_Di_mytext)
rm(San_Di)
######San Fran

San_Fran <- County_summary %>% filter(County == "San Francisco") 
San_Fran_mytext <- paste0("County: ", San_Fran$County, "<br>", 
                          "Tier Level: ", San_Fran$Tier.Level, "<br>",
                          "OBGYN Programs Awarded: ", San_Fran$n[2], " & ", "Total Amount Awarded: ", San_Fran$Total.Awarded.Amount[2], "<br>",
                          "Peds Programs Awarded: ", San_Fran$n[3], " & ", "Total Amount Awarded: ", San_Fran$Total.Awarded.Amount[3], "<br>",
                          "EM Programs Awarded: ", San_Fran$n[1], " & ", "Total Amount Awarded: ", San_Fran$Total.Awarded.Amount[1], "<br>",
                          "<b>Total Programs Awarded: 12 | Total Funds Awarded: $4,470,000</b>", sep = " ")
San_Fran_mytext <- unique(San_Fran_mytext)
rm(San_Fran)
####San Joaquin

San_Joaq <- County_summary %>% filter(County == "San Joaquin") 
San_Joaq_mytext <- paste0("County: ", San_Joaq$County, "<br>", 
                          "Tier Level: ", San_Joaq$Tier.Level, "<br>",
                          "FM Programs Awarded: ", San_Joaq$n[2], " & ", "Total Amount Awarded: ", San_Joaq$Total.Awarded.Amount[2], "<br>",
                          "IM Programs Awarded: ", San_Joaq$n[3], " & ", "Total Amount Awarded: ", San_Joaq$Total.Awarded.Amount[3], "<br>",
                          "EM Programs Awarded: ", San_Joaq$n[1], " & ", "Total Amount Awarded: ", San_Joaq$Total.Awarded.Amount[1], "<br>",
                          "<b>Total Programs Awarded: 19 | Total Funds Awarded: $9,675,000</b>", sep = " ")
San_Joaq_mytext <- unique(San_Joaq_mytext)
rm(San_Joaq)
###San Mateo

San_Mat_cost <- County_summary %>% filter(County == "San Mateo")
San_Mat_text <-  paste0("County: ", San_Mat_cost$County, "<br>", 
                        "Tier Level: ", San_Mat_cost$Tier.Level, "<br>",
                        "Peds Programs Awarded: ", San_Mat_cost$n, " & ", "Total Amount Awarded: ", San_Mat_cost$Total.Awarded.Amount, "<br>",
                        "<b>Total Programs Awarded: 3 | Total Funds Awarded: $1,660,000</b>", sep = " ")
rm(San_Mat_cost)
#####Santa Barbara

Santa_Barbs_cost <- County_summary %>% filter(County == "Santa Barbara")
Santa_Barbs_text <-  paste0("County: ", Santa_Barbs_cost$County, "<br>", 
                            "Tier Level: ", Santa_Barbs_cost$Tier.Level, "<br>",
                            "FM Programs Awarded: ", Santa_Barbs_cost$n[1], "<br>", "Total Amount Awarded: ", Santa_Barbs_cost$Total.Awarded.Amount[1], "<br>",
                            "OBGYN Programs Awarded: ", Santa_Barbs_cost$n[2], " & ", "Total Amount Awarded: ", Santa_Barbs_cost$Total.Awarded.Amount[2], "<br>",
                            "Peds Programs Awarded: ", Santa_Barbs_cost$n[3], " & ", "Total Amount Awarded: ", Santa_Barbs_cost$Total.Awarded.Amount[3], "<br>",
                            "<b>Total Programs Awarded: 7 | Total Funds Awarded: $3,675,000</b>", sep = " ")
Santa_Barbs_text <- unique(Santa_Barbs_text)
rm(Santa_Barbs_cost)
#######Santa Clara

Santa_Clara <- County_summary %>% filter(County == "Santa Clara") 
Santa_Clara_mytext <- paste0("County: ", Santa_Clara$County, "<br>", 
                             "Tier Level: ", Santa_Clara$Tier.Level, "<br>",
                             "FM Programs Awarded: ", Santa_Clara$n[1], " & ", "Total Amount Awarded: ", Santa_Clara$Total.Awarded.Amount[1], "<br>",
                             "IM Programs Awarded: ", Santa_Clara$n[2], " & ", "Total Amount Awarded: ", Santa_Clara$Total.Awarded.Amount[2], "<br>",
                             "OBGYN Programs Awarded: ", Santa_Clara$n[3], " & ", "Total Amount Awarded: ", Santa_Clara$Total.Awarded.Amount[3], "<br>",
                             "Peds Programs Awarded: ", Santa_Clara$n[4], " & ", "Total Amount Awarded: ", Santa_Clara$Total.Awarded.Amount[4], "<br>",
                             "<b>Total Programs Awarded: 8 | Total Funds Awarded: $2,125,000</b>", sep = " ")
Santa_Clara_mytext <- unique(Santa_Clara_mytext)
rm(Santa_Clara)
#####Shasta

Shasta_cost <- County_summary %>% filter(County == "Shasta")
Shasta_text <-  paste0("County: ", Shasta_cost$County, "<br>", 
                       "Tier Level: ", Shasta_cost$Tier.Level, "<br>",
                       "FM Programs Awarded: ", Shasta_cost$n[1], " & ", "Total Amount Awarded: ", Shasta_cost$Total.Awarded.Amount[1], "<br>",
                       "IM Programs Awarded: ", Shasta_cost$n[2], " & ", "Total Amount Awarded: ", Shasta_cost$Total.Awarded.Amount[2], "<br>",
                       "<b>Total Programs Awarded: 5 | Total Funds Awarded: $1,080,000</b>", sep = " ")
Shasta_text <- unique(Shasta_text)
rm(Shasta_cost)
#####Sonoma

Sonoma_cost <- County_summary %>% filter(County == "Sonoma")
Sonoma_text <-  paste0("County: ", Sonoma_cost$County, "<br>", 
                       "Tier Level: ", Sonoma_cost$Tier.Level, "<br>",
                       "FM Programs Awarded: ", Sonoma_cost$n, " & ", "Total Amount Awarded: ", Sonoma_cost$Total.Awarded.Amount, "<br>",
                       "<b>Total Programs Awarded: 6 | Total Funds Awarded: $1,125,000</b>", sep = " " )
rm(Sonoma_cost)
##############Stanislaus

Stanislaus <- County_summary %>% filter(County == "Stanislaus") 
Stanislaus_mytext <- paste0("County: ", Stanislaus$County, "<br>", 
                            "Tier Level: ", Stanislaus$Tier.Level, "<br>",
                            "FM Programs Awarded: ", Stanislaus$n[2], " & ", "Total Amount Awarded: ", Stanislaus$Total.Awarded.Amount[2], "<br>",
                            "EM Programs Awarded: ", Stanislaus$n[1], " & ", "Total Amount Awarded: ", Stanislaus$Total.Awarded.Amount[1], "<br>", 
                            "<b>Total Programs Awarded: 5 | Total Funds Awarded: $900,000</b>", sep = " ")
Stanislaus_mytext <- unique(Stanislaus_mytext)
rm(Stanislaus)
######Tulare

Tulare <- County_summary %>% filter(County == "Tulare") 
Tulare_mytext <- paste0("County: ", Tulare$County, "<br>", 
                        "Tier Level: ", Tulare$Tier.Level, "<br>",
                        "FM Programs Awarded: ", Tulare$n[2], " & ", "Total Amount Awarded: ", Tulare$Total.Awarded.Amount[2], "<br>",
                        "EM Programs Awarded: ", Tulare$n[1], " & ", "Total Amount Awarded: ", Tulare$Total.Awarded.Amount[1], "<br>",
                        "<b>Total Programs Awarded: 8 | Total Funds Awarded: $1,830,000</b>", sep = " ")
Tulare_mytext <- unique(Tulare_mytext)
rm(Tulare)
####Ventura

Ventura <- County_summary %>% filter(County == "Ventura") 
Ventura_mytext <- paste0("County: ", Ventura$County, "<br>", 
                         "Tier Level: ", Ventura$Tier.Level, "<br>",
                         "FM Programs Awarded: ", Ventura$n[2], " & ", "Total Amount Awarded: ", Ventura$Total.Awarded.Amount[2], "<br>",
                         "EM Programs Awarded: ", Ventura$n[1], " & ", "Total Amount Awarded: ", Ventura$Total.Awarded.Amount[1], "<br>", 
                         "<b>Total Programs Awarded: 10 | Total Funds Awarded: $2,010,000</b>", sep = " ")
Ventura_mytext <- unique(Ventura_mytext)
rm(Ventura)

# the following counties do not have awardees. If county gets awardees for the format from the top.
# Del Norte
delNorte <- paste0("County: Del Norte", "<br>",
                   "Tier Level: Tier 1", "<br>",
                   "No Awardees in this County", sep = "")
#Siskiyou
siskiyo <- paste0("County: Siskiyo", "<br>",
                  "Tier Level: Tier 1", "<br>",
                  "No Awardees in this County", sep = "")
#modoc
modoc <- paste0("County: Modoc", "<br>",
                "Tier Level: Tier 1", "<br>",
                "No Awardees in this County", sep = "")
#trinity
Trinity <- paste0("County: Trinity", "<br>",
                  "Tier Level: Tier 1", "<br>",
                  "No Awardees in this County", sep = "")
#lassen
lassen <- paste0("County: Lassen", "<br>",
                 "Tier Level: Tier 2", "<br>",
                 "No Awardees in this County", sep = "")
#tehama
tehama <- paste0("County: Tehama", "<br>",
                 "Tier Level: Tier 1", "<br>",
                 "No Awardees in this County", sep = "")
#plumas
Plumas <- paste0("County: Plumas", "<br>",
                 "Tier Level: Tier 3", "<br>",
                 "No Awardees in this County", sep = "")
#butte
Butte <- paste0("County: Butte", "<br>",
                "Tier Level: Tier 2", "<br>",
                "No Awardees in this County", sep = "")
#glenn
Glenn <- paste0("County: Glenn", "<br>",
                "Tier Level: Tier 1", "<br>",
                "No Awardees in this County", sep = "")
#lake
Lake <- paste0("County: Lake", "<br>",
               "Tier Level: Tier 1", "<br>",
               "No Awardees in this County", sep = "")
#colusa
colusa <- paste0("County: Colusa", "<br>",
                 "Tier Level: Tier 1", "<br>",
                 "No Awardees in this County", sep = "")
#sutter
Sutter <- paste0("County: Sutter", "<br>",
                 "Tier Level: Tier 2", "<br>",
                 "No Awardees in this County", sep = "")
#yuba
Yuba <- paste0("County: Yuba", "<br>",
               "Tier Level: Tier 1", "<br>",
               "No Awardees in this County", sep = "")
#nevada
nevada <- paste0("County: Nevada", "<br>",
                 "Tier Level: Tier 2", "<br>",
                 "No Awardees in this County", sep = "")
#placer
Placer <- paste0("County: Placer", "<br>",
                 "Tier Level: Tier 3", "<br>",
                 "No Awardees in this County", sep = "")
#el dorado
elDorado <- paste0("County: El Dorado", "<br>",
                   "Tier Level: Tier 1", "<br>",
                   "No Awardees in this County", sep = "")
#yolo
Yolo <- paste0("County: Yolo", "<br>",
               "Tier Level: Tier 2", "<br>",
               "No Awardees in this County", sep = "")
#NAPA
napa <- paste0("County: Napa", "<br>",
               "Tier Level: Tier 3", "<br>",
               "No Awardees in this County", sep = "")
#solano
solano <- paste0("County: Solano", "<br>",
                 "Tier Level: Tier 1", "<br>",
                 "No Awardees in this County", sep = "")
#marin
marin <- paste0("County: Marin", "<br>",
                "Tier Level: Tier 3", "<br>",
                "No Awardees in this County", sep = "")
#calaveras
calaveras <- paste0("County: Calaveras", "<br>",
                    "Tier Level: Tier 1", "<br>",
                    "No Awardees in this County", sep = "")
#alpine
Alpine <- paste0("County: Alpine", "<br>",
                 "Tier Level: Tier 1", "<br>",
                 "No Awardees in this County", sep = "")
#tuolumne
tuolumne <- paste0("County: Tuolumne", "<br>",
                   "Tier Level: Tier 3", "<br>",
                   "No Awardees in this County", sep = "")
#mariposa
mariposa <- paste0("County: Mariposa", "<br>",
                   "Tier Level: Tier 2", "<br>",
                   "No Awardees in this County", sep = "")
#mono
mono <- paste0("County: Mono", "<br>",
               "Tier Level: Tier 3", "<br>",
               "No Awardees in this County", sep = "")
#inyo
Inyo <- paste0("County: Inyo", "<br>",
               "Tier Level: Tier 3", "<br>",
               "No Awardees in this County", sep = "")
#santa cruz
santo_Cruz <- paste0("County: Santa Cruz", "<br>",
                     "Tier Level: Tier 2", "<br>",
                     "No Awardees in this County", sep = "")
#san benito
sanBenito <- paste0("County: San Benito", "<br>",
                    "Tier Level: Tier 1", "<br>",
                    "No Awardees in this County", sep = "")
#slo
slo <- paste0("County: San Luis Obispo", "<br>",
              "Tier Level: Tier 2", "<br>",
              "No Awardees in this County", sep = "")
#sierra
Sierra <- paste0("County: Sierra", "<br>",
                 "Tier Level: Tier 1", "<br>",
                 "No Awardees in this County", sep = "")

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
  group_by(County, Tier.Level) %>% 
  summarize(n = n(),
            positions = sum(Positions.Awarded),
            funds = sum(Award.Amount))

#merge county to Cali dataset by "county"
cali_joined <- left_join(cali, county_summaries, by = "County")

#arrange by alphabetical order
cali_joined <- cali_joined %>% arrange(County)

#join dataset with the text data frame
cali_joined <- cbind(cali_joined, x)

rm(list = c("county_summaries", "x"))

label1 <- cali_joined %>% filter(!is.na(cali_joined$n)) %>% 
  mutate(label1 = paste0("County: ", County, "<br>",
                         "Total programs awarded: ", n, sep = ""))

label2 <- cali_joined %>% filter(is.na(cali_joined$n)) %>% 
  mutate(label1 = paste0("County: ", County, "<br>",
                         "No GME programs awarded in this county", sep = ""))

impact_summary <- rbind(label1, label2) %>% 
  select("County", "Tier.Level", "n", "positions", "funds", "x", "label1")

rm(list = c("cali_joined", "label1", "label2"))


gc()


source("1a.Impact_summary_residents.R")