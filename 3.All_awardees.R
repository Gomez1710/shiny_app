#Prepare data for all cycles and all awardees
all_cycles <- read.csv("All Cycles.csv", stringsAsFactors = FALSE)

all_awardees <- all_cycles %>% filter(Awarded.Yes.No == "Yes")
rm(all_cycles)

#read in data for all cycles
#gather data for breakdown of total awardees and residents by discipline and program type
all_cycles <- read.csv("All Cycles.csv", stringsAsFactors = FALSE)
awardees <- subset(all_cycles, Awarded.Yes.No == "Yes")
rm(all_cycles)

#group by discipline to get overall discipline level data
grouped_all <- awardees %>% 
  group_by(Discipline) %>% 
  summarize(positions_awarded = sum(New.Positions.Awarded, na.rm = TRUE) +
              sum(Expanded.Positions.Awarded, na.rm = TRUE) +
              sum(Existing.Positions.Awarded, na.rm = TRUE))

#create column for "all cycles"
grouped_all$Cycle <- "All Cycles"

#rearrange columns
grouped_all <- select(grouped_all, Discipline, Cycle, positions_awarded)

#group by disicpline and cycle year
grouped_disicipline <- awardees %>% 
  group_by(Discipline, Cycle) %>% 
  summarize(positions_awarded = sum(New.Positions.Awarded, na.rm = TRUE) +
              sum(Expanded.Positions.Awarded, na.rm = TRUE) +
              sum(Existing.Positions.Awarded, na.rm = TRUE))

#combine rows with overall discipline level data
all_disciplines <- rbind(grouped_disicipline, grouped_all)

rm(grouped_disicipline)
rm(grouped_all)

#create new variables for total resident type
total.new <- sum(awardees$New.Positions.Awarded, na.rm = TRUE)
total.exp <- sum(awardees$Expanded.Positions.Awarded, na.rm = TRUE)
total.exist <- sum(awardees$Existing.Positions.Awarded, na.rm = TRUE)

#create vector of total resident type
new.p <- c("All Disciplines", "New", total.new)
exp.p <- c("All Disciplines", "Expanding", total.exp)
exist.p <- c("All Disciplines", "Existing", total.exist)

#create date frame for total program type and combined rows with overall program types
total.cycles <- data.frame("Discipline", "Program.Type", "total.positions")
all_cycles_t <- rbind(total.cycles, new.p, exp.p, exist.p)
rm(total.cycles)

#remove the first row
all_cyclesT <- all_cycles_t[-1, ]
rm(all_cycles_t)

#rename column names
colnames(all_cyclesT) <- c("Discipline", "Program.Type", "total.positions")

#convert positions into an integer
all_cyclesT$total.positions <- as.integer(all_cyclesT$total.positions)

#group by discipline and program type to get resident numbers 
grouped_typ_D <- awardees %>% 
  group_by(Discipline, Program.Type) %>% 
  summarize(total.positions = sum(New.Positions.Awarded, na.rm = TRUE) +
              sum(Expanded.Positions.Awarded, na.rm = TRUE) +
              sum(Existing.Positions.Awarded, na.rm = TRUE))

all_types <- rbind(grouped_typ_D, all_cyclesT)
rm(list = c("grouped_typ_D", "exist.p", "exp.p", "new.p", "total.exist",
            "total.exp", "total.new", "all_cyclesT"))
