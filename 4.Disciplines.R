#Read in data from the file sent from Janet Coffman of the CalMedForce Specialty workforce

physicians <- read.csv("physician_data_Coffman.csv", stringsAsFactors = FALSE)

#split data into the 5 disciplines to get workforce ratio for each discipline

#Family medicine

fm <- physicians %>% select(County, Family.Medicine, Population) %>% 
  mutate(ratio = (Family.Medicine * 100000) / Population) %>% 
  mutate(mytext = paste0("County: ", County, "<br>",
                         "Number of FM Physicians: ", Family.Medicine, "<br>",
                         "County Population: ", Population, "<br>",
                         "Ratio per 100,000: ", round(ratio, 2), sep = "")) %>% 
  mutate(text1 = paste0("County: ", County, "<br>", 
                        "Ratio: ", round(ratio, 2), sep = ""))

fm_phy <- left_join(cali, fm, by = "County") %>% 
  select(c("County", "Family.Medicine", "Population", "ratio", "mytext", "text1"))

#Internal Medicine

im <- physicians %>% select(County, Internal.Medicine, Population) %>% 
  mutate(ratio = (Internal.Medicine * 100000) / Population) %>% 
  mutate(mytext = paste0("County: ", County, "<br>",
                         "Number of IM Physicians: ", Internal.Medicine, "<br>",
                         "County Population: ", Population, "<br>",
                         "Ratio per 100,000: ", round(ratio, 2), sep = "")) %>% 
  mutate(text1 = paste0("County: ", County, "<br>", 
                        "Ratio: ", round(ratio, 2), sep = ""))

im_phy <- left_join(cali, im, by = "County") %>% 
  select("County", "Internal.Medicine", "Population", "ratio", "mytext", "text1")

# Pediatrics

peds <- physicians %>% select(County, Pediatrics, Population) %>% 
  mutate(ratio = (Pediatrics * 100000) / Population) %>% 
  mutate(mytext = paste0("County: ", County, "<br>",
                         "Number of Pediatric Physicians: ", Pediatrics, "<br>",
                         "County Population: ", Population, "<br>",
                         "Ratio per 100,000: ", round(ratio, 2), sep = "")) %>% 
  mutate(text1 = paste0("County: ", County, "<br>", 
                        "Ratio: ", round(ratio, 2), sep = ""))

peds_phy <- left_join(cali, peds, by = "County") %>% 
  select("County", "Pediatrics", "Population", "ratio", "mytext", "text1")

#OBGYN

ob <- physicians %>% select(County, Obstetrics.Gynecology, Population) %>% 
  mutate(ratio = (Obstetrics.Gynecology * 100000) / Population) %>% 
  mutate(mytext = paste0("County: ", County, "<br>",
                         "Number of OBGYN Physicians: ", Obstetrics.Gynecology, "<br>",
                         "County Population: ", Population, "<br>",
                         "Ratio per 100,000: ", round(ratio, 2), sep = "")) %>% 
  mutate(text1 = paste0("County: ", County, "<br>", 
                        "Ratio: ", round(ratio, 2), sep = ""))

ob_phy <- left_join(cali, ob, by = "County") %>% 
  select("County", "Obstetrics.Gynecology", "Population", "ratio", "mytext", "text1")

#EM

em <- physicians %>% select(County, Emergency.Medicine, Population) %>% 
  mutate(ratio = (Emergency.Medicine * 100000) / Population) %>% 
  mutate(mytext = paste0("County: ", County, "<br>",
                         "Number of EM Physicians: ", Emergency.Medicine, "<br>",
                         "County Population: ", Population, "<br>",
                         "Ratio per 100,000: ", round(ratio, 2), sep = "")) %>% 
  mutate(text1 = paste0("County: ", County, "<br>", 
                        "Ratio: ", round(ratio, 2), sep = ""))

em_phy <- left_join(cali, em, by = "County") %>%
  select("County", "Emergency.Medicine", "Population", "ratio", "mytext", "text1")

rm(list = c("fm", "im", "peds", "ob", "em", "physicians"))

#discipline level data

all_cycles <- read.csv("All Cycles.csv", stringsAsFactors = FALSE)
awardees <- subset(all_cycles, Awarded.Yes.No == "Yes")
rm(all_cycles)

#Family Medicine

fm_data <- awardees %>% 
  filter(Discipline == "FM")

fm_county <- fm_data %>% 
  group_by(Discipline, County) %>% 
  summarize(n = sum(Positions.Awarded),
            programs = n()) %>% 
  mutate(mytext = paste0("County: ", County, "<br>",
                         "Programs Awarded: ", programs, "<br>", 
                         "Number of Residents: ", n, sep = " "))

fm_joined <- left_join(cali, fm_county, by = "County")
rm(fm_county)

# Internal Medicine
im_data <- awardees %>% 
  filter(Discipline == "IM")

im_county <- im_data %>% 
  group_by(Discipline, County) %>% 
  summarize(n = sum(Positions.Awarded),
            programs = n()) %>% 
  mutate(mytext = paste0("County: ", County, "<br>",
                         "Programs Awarded: ", programs, "<br>",
                         "Number of Residents: ", n, sep = ""))

im_joined <- left_join(cali, im_county, by = "County")
rm(im_county)

#Peds
Peds_data <- awardees %>% 
  filter(Discipline == "Peds")

peds_county <- Peds_data %>% 
  group_by(Discipline, County) %>% 
  summarize(n = sum(Positions.Awarded),
            programs = n()) %>% 
  mutate(mytext = paste0("County: ", County, "<br>",
                         "Programs Awarded: ", programs, "<br>",
                         "Number of Residents: ", n, sep = ""))

peds_joined <- left_join(cali, peds_county, by = "County")
rm(peds_county)

#OB
OB_data <- awardees %>% 
  filter(Discipline == "OBGYN")

ob_county <- OB_data %>% 
  group_by(Discipline, County) %>% 
  summarize(n = sum(Positions.Awarded),
            programs = n()) %>% 
  mutate(mytext = paste0("County: ", County, "<br>",
                         "Programs Awarded: ",programs, "<br>",
                         "Number of Residents: ", n, sep = ""))

ob_joined <- left_join(cali, ob_county, by = "County")
rm(ob_county)

# EM
em_data <- awardees %>% 
  filter(Discipline == "EM")

em_county <- em_data %>% 
  group_by(Discipline, County) %>% 
  summarize(n = sum(Positions.Awarded),
            programs = n()) %>% 
  mutate(mytext = paste0("County: ", County, "<br>",
                         "Programs Awarded: ", programs, "<br>",
                         "Number of Residents: ", n, sep = ""))

em_joined <- left_join(cali, em_county, by = "County")
rm("em_county")


fm_joined$mytext[is.na(fm_joined$mytext)] <- "County has no awardees"
im_joined$mytext[is.na(im_joined$mytext)] <- "County has no awardees"
peds_joined$mytext[is.na(peds_joined$mytext)] <- "County has no awardees"
ob_joined$mytext[is.na(ob_joined$mytext)] <- "County has no awardees"
em_joined$mytext[is.na(em_joined$mytext)] <- "County has no awardees"

