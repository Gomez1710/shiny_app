
#read in data on grads. this data is from CMF application. Update this to accommodate graduate data
grads <- read.csv("Grad_data.csv", stringsAsFactors = FALSE)

#subset for grads that practice more that 50%
Grad_cali <- filter(grads, State == "California" & County != "Out of State" & Speciality == "Yes")
rm(grads)

#Group by county to get overall county data
grads_county <- Grad_cali %>% 
  group_by(County) %>% 
  summarize(n = n()) %>% 
  mutate(mytext = paste0("County: ", County, "<br>",
                         "Total Number of Grads: ", n))

#group grads by county and discipline to get discipline level data
grads_grouped <- Grad_cali %>% 
  count(County, Specialty) %>% 
  group_by(County, Specialty) %>% 
  mutate(my_text = paste0("County: ", County, "<br>",
                          "Discipline: ", Specialty, "<br>",
                          "Total Number of Graduates: ", n, sep = " "))

#rm(Grad_cali)

#join the county map with grad discipline data
grads_discipline <- left_join(cali, grads_grouped, by = "County")
grads_discipline$Specialty[is.na(grads_discipline$Specialty)] <- "County has no graduates here"
grads_discipline$n[is.na(grads_discipline$n)] <- 0

#join cali map with grads by county
county_grads <- left_join(cali, grads_county, by = "County") %>% 
  select(c("County", "n", "mytext"))

county_grads$mytext[is.na(county_grads$mytext)] <- "County has no graduates"


#### create group summary for bar graph
grad_summary <- Grad_cali %>% 
  group_by(Specialty) %>% 
  summarize(n = n())