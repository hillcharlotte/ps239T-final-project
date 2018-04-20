############################################### 1. SET UP DATA ###############################################

# Clear workspace
rm(list = ls())

# Load packages
library(pacman)
p_load(dplyr, lubridate, taRifx, ggplot2, gmodels, readr)

# Set working directory
setwd("/Users/charlottehill/Documents/PhD/Spring 2018/Seattle voucher research")

# Read in 2017 contributor data
seattle2017 <- read_csv("Contributions/2017_contributors.csv")

# Read in 2015 contributor data
seattle2015 <- read_csv("Contributions/2015_contributors.csv")

# Add rows of datasets together
seattle <- rbind(seattle2017, seattle2015)

# Rename columns
names(seattle) <- c("Cycle", "Contest", "CampaignName", "OrderName", "CandidateID", "TransactionType", "ReportingForm",
                    "LinkID", "TransactorName", "City", "State", "Zip", "EmployerName", "EmployerCity", "EmployerState", 
                    "Occupation", "Amount", "TotalDonated", "Misc1", "Misc2", "Description", "TransactionDate",
                    "DepositDate", "ContributionEffect", "ExpenditureEffect", "CodedDistrict", "VoucherID")

# Remove contests that did not take vouchers
seattle <- subset(seattle, seattle$Contest=="City Council Position 8" | seattle$Contest=="City Council Position 9")

# Convert dates
seattle$TransactionDate <- mdy(seattle$TransactionDate)
seattle$DepositDate <- mdy(seattle$DepositDate)

# Create voucher dummy
seattle$Voucher <- 0

for (i in 1:length(seattle$VoucherID)) {
  if (seattle$VoucherID[i] != 0) {
    seattle$Voucher[i] <- 1
  }
}

############################################### 2. CODE OCCUPATIONAL CATEGORIES ###############################################

# Create Occupation Type vector
seattle$OccupationType <- NA

# Set default value to "Other"
for (i in 1:length(seattle$Occupation)) {
  if (is.na(seattle$Occupation[i])==FALSE) {
    seattle$OccupationType[i] <- "Other Private-Sector"
  }
}

# Create lists of keywords for each occupation category
key_bus <- c("business", "chairman", "CEO", "chief", "COO", "board", "retailer", "client", "manager", "hr", "human resources", 
             "sales", "vice president", "president", "director", "vp", "cfo", "executive", "management", "GM", "admin", "corporat", 
             "advisor", "marketing", "brand", "public relation", "press", "media rel", "communic", "public aff", "content strat", 
             "buyer", "strategic advisor", "owner", "entrepren", "founder", "supercargo", "mariner", "tester", "hospitality", 
             "treasurer", "geograph", "farm", "agri", 
             "artist", "theat", "music", "costume", "video", "producer", "danc", "actor", "actress", "art", "design", "film", "creativ", 
             "painting cons", "journalis", "reporter", "news", "blogg", "editor", "writer", "author", "transport", "pilot", "driver", 
             "taxi", "train", "transit", "costco", "retail", "warehouse", "shopkeeper", "merchandise", "chef", "meat", "restaur", "food", 
             "brewer", "baker", "Sommelier", "chocolat", "gafco", "farmbox", "self", "coach", "comedian", "astrologer", "pianist",
             "analyst", "bookkeeper", "assistant", "customer", "photographer", "customer serv", "receptionist", "tour operator", "shopkeeper")
key_health <- c("lmp", "physician", "clinical", "dentist", "doctor", "trist", "surgeon", "veterin", "disease", "patient", 
                "health", "medic", "primary care", "nurs", "trician", "psych", "doula", "massage", "caregiver", "logist", "therapist", 
                "PHYISICIAN", "ortho", "acupunct", "Physicisn", "clinic", "behavior an", "dental", "therapy")
key_lawyers <- c("lawy", "legal", "attorn", "lobby", "partner", "arbit", "mediat", "counsel", "laywer", "atty", "attroney",
                 "judicial")
key_techies <- c("engineer", "recruiter", "software", "hardware", "web", "online", "database", "techn", "software develop", 
                 "web develop", "browser develop", "game develop", "data", "social media", "computer", "programmer", "system",
                 "ux", "graphic des", "user exp", "game des", "quality", "QA", "IT special", "tech supp", "Eng Supvsr",
                 "cyber sec", "customer succ", "help desk")
key_financers <- c("financ", "tax", "bank", "broker", "stock", "capital", "account", "fin'l", "invest", "wealth", "loan", "lender",
                   "insurance", "actuary", "cfa", "CPA", "shareholder", "securities", "manging member")
key_realtors <- c("realt", "land", "build", "real est", "archit", "contractor", "agent", "developer", "development",
                  "construct", "estimator", "leasing", "planning", "market leader")
key_gov <- c("politician", "represent", "council", "mayor", "commission", "legislat", "judge", "notary", "city", "senat",
             "candidate", "colonel", "diplomat", "congressional", "parks", "librar", "transit planner", "urban planner", 
             "transportation planner", "city planner", "consult", "government relations", "leadership cens")
key_serviceprofs <- c("case manager", "case work", "social work", "outreach worker", "advocate", "youth work", "adoption", "community staff",
                      "health couns", "alcohol couns", "teach", "educat", "school", "colleg", "professor", "instructor", "faculty", 
                      "lectur", "tutor", "principal", "dean", "superintendent", "executive dir", "profit", "policy", "organizing", "political", 
                      "activist", "program", "chapter dir", "outreach", "volunteer", "exec dir", "fundrais", "grant", "organiz", 
                      "youth development dir", "philan", "staffer", "community organizer", "development director", "fund raising", "pastor", 
                      "rabbi", "cantor", "chaplain", "clergy", "homemaker", "mother", "mom", "caretaker", "nanny", "child care", "day care")
key_workers <- c("cadet", "manufactur", "carpent", "steel", "iron", "welder", "cabinet maker", "countertop", "machine", "lumber", 
                 "labor", "union", "SEIU", "AFL", "UAW", "cleaner", "server", "service", "grocer", "stylist", "cashier", "barista",
                 "maintenance", "seamstress", "mechanic", "flight attendant", "clerk", "team member", "plumber", "watier",
                 "barber", "dog", "bridge", "meat", "security guard", "server", "dj", "fire", "letter carrier", "aesthet", "waiter")
key_retir <- c("retired")
key_unemp <- c("unemployed", "not emp", "seeking info", "none", "disabled", "student")


# Create function to name the occupation type of a given donor
name_occtype <- function (Keywords, Occupation_Name) {
  for (i in 1:length(Keywords)) {
    seattle$OccupationType[(grepl(Keywords[i], seattle$Occupation, ignore.case = TRUE)==T) & 
                             is.na(seattle$Occupation)==F] <- Occupation_Name
  }
  return(seattle$OccupationType)
}

# Name occupation types of donors using keywords
seattle$OccupationType <- name_occtype(key_bus, "Other Private-Sector")
seattle$OccupationType <- name_occtype(key_health, "Health")
seattle$OccupationType <- name_occtype(key_lawyers, "Lawyers")
seattle$OccupationType <- name_occtype(key_techies, "Technology")
seattle$OccupationType <- name_occtype(key_financers, "Finance & Insurance")
seattle$OccupationType <- name_occtype(key_realtors, "Real Estate")
seattle$OccupationType <- name_occtype(key_gov, "Government")
seattle$OccupationType <- name_occtype(key_serviceprofs, "Service & Nonprofit")
seattle$OccupationType <- name_occtype(key_workers, "Blue-Collar & Labor")
seattle$OccupationType <- name_occtype(key_retir, "Retired")
seattle$OccupationType <- name_occtype(key_unemp, "Unemployed")

# View(seattle[grep("Private-Sector Professionals", seattle$OccupationType, ignore.case=TRUE),])


# Develop employer name keywords
empl_bus <- c("business association", "nicky", "united parcel", "sound testing",
              "public rel", "public affairs", "communication", "self", "biology", "hospital", "providence", "seattle children", 
              "swedish", "group health", "medical center", "healing center", "hutch", "kaiser", "eye clinic", "pharmaceut", 
              "UWMC", "polyclinic", "james squire", "nanostring", "sibcr", "boeing", "vulcan", "oecotext", "Nordstrom", "Costco", 
              "have a heart", "DCMM", "starbucks", "pizza", "cheesemongers", "candies", "consult")
empl_health <- c("hospital", "providence", "seattle children", "swedish", "group health", "medical center", "healing center",
                 "hutch", "kaiser", "eye clinic", "pharmaceut", "UWMC", "polyclinic", "james squire", "nanostring", "sibcr")
empl_lawyers <- c("law", "pacifica", "perkins coie", "quinn", "schroeter", "foster pepper", "hillis clark", "K&L", "lane powell",
                  "blue wave", "CBE", "ceis bayne", "davis wright", "PLLC", "wsaj", "wsba")
empl_techies <- c("amazon", "microsoft", "airbnb", "tamarac", "zillow", "parlay", "at&t", "technolog", "google", "tableau")
empl_financers <- c("coldwell", "banker", "Seattle CFO", "capital one", "wealth management", "insurance", "brighton jones", "capital")
empl_realtors <- c("pine street", "architects", "washington holdings", "jll", "AECOM", "seneca")
empl_gov <- c("city of", "gao", "government", "county", "housing authority", "state of", "house of rep", "wa gov", "washington gov",
              "port of seattle", "historic south", "center for infectious", "fisheries science", "USGS", "geological survey", "seattle department",
              "police")
empl_serviceprofs <- c("foundation", "nonprofit", "association", "organization", "community services", "alliance",
                       "transportation choices", "housing conso", "ymca", "ywca", "fuse washington", "fuse wa", "systems biology",
                       "action institute", "brookings inst", "university", "univ", "college", "public school", "school district", "school")
empl_workers <- c("boatman", "Boatmen", "Machinists union", "Sailors’ union", "Service employees", "Riders union",
                  "SEIU", "UAW", "workers", "local", "labor council", "UA plumbers", "UFCW", "MLKCLC", "nurses association", "wsna",
                  "washington education association", "seattle education association", "teamster", "wfse",
                  "washington federation of state employees", "aerospace machinists", "aerospace workers", "speea", "smwia")
empl_retir <- c("retired")
empl_unemp <- c("not available", "unemployed", "none", "N/A")


# Create function to update occupation types using employer names
name_occup_emp <- function (Employer_Name, Occupation_Name) {
  for (i in 1:length(Employer_Name)) {
    seattle$OccupationType[(grepl(Employer_Name[i], seattle$EmployerName, ignore.case = TRUE)==T) & 
                             is.na(seattle$EmployerName)==F] <- Occupation_Name
  }
  return(seattle$OccupationType)
}

# Name occupation types using employer name keywords
seattle$OccupationType <- name_occup_emp(empl_bus, "Other Private-Sector")
seattle$OccupationType <- name_occup_emp(empl_health, "Health")
seattle$OccupationType <- name_occup_emp(empl_lawyers, "Lawyers")
seattle$OccupationType <- name_occup_emp(empl_techies, "Technology")
seattle$OccupationType <- name_occup_emp(empl_financers, "Finance & Insurance")
seattle$OccupationType <- name_occup_emp(empl_realtors, "Real Estate")
seattle$OccupationType <- name_occup_emp(empl_gov, "Government")
seattle$OccupationType <- name_occup_emp(empl_serviceprofs, "Service & Nonprofit")
seattle$OccupationType <- name_occup_emp(empl_workers, "Blue-Collar & Labor")
seattle$OccupationType <- name_occup_emp(empl_retir, "Retired")
seattle$OccupationType <- name_occup_emp(empl_unemp, "Unemployed")

# Subset data to only include contributions with identified occupation types:
seattle_occ <- subset(seattle, !is.na(OccupationType))

# Save data into new file for analysis
write.csv(seattle_occ, "final_data.csv")
