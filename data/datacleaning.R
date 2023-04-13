pfas <- read_xlsx("../data/Twin Cities PFAS Data Ticket WO00000021224685.xlsx", sheet=2)

pfas7 <- pfas %>%
  filter(CHEMICAL_NAME %in% c("Perfluorobutanoic acid", "Perfluorooctanoic acid", "Perfluoropentanoic acid", "Perfluorohexanoic acid", "Perfluorooctane sulfonate", "Perfluorohexane sulfonate", "Perfluorobutane sulfonate")) %>%
  mutate(commonName = case_when(CHEMICAL_NAME == "Perfluorobutanoic acid" ~ "PFBA", 
                                CHEMICAL_NAME == "Perfluorooctanoic acid" ~ "PFOA", 
                                CHEMICAL_NAME == "Perfluoropentanoic acid" ~ "PFPeA",
                                CHEMICAL_NAME == "Perfluorohexanoic acid" ~ "PFHxA", 
                                CHEMICAL_NAME == "Perfluorooctane sulfonate" ~ "PFOS", 
                                CHEMICAL_NAME == "Perfluorohexane sulfonate" ~ "PFHxS", 
                                CHEMICAL_NAME == "Perfluorobutane sulfonate" ~ "PFBS")) %>%
  mutate(LOC_TYPE_2 = case_when(LOC_TYPE_2 == "Well-DOmestic" ~ "Well-Domestic", 
                                TRUE ~ LOC_TYPE_2))

write.csv(pfas7, "pfas7.csv")

threeM <- read_csv("../data/superfund_site_data/3m_chemolite.csv") %>%
  mutate(site = "3M Chemolite", county = "Washington", SYS_LOC_CODE = as.numeric(SYS_LOC_CODE))
oakdale <- read_csv("../data/superfund_site_data/oakdale.csv") %>%
  mutate(site = "3M Oakland", county = "Washington")
ashland <- read_csv("../data/superfund_site_data/ashland.csv") %>%
  mutate(site = "Ashland Oil - Park Penta", county = "Washington")
bayport <- read_csv("../data/superfund_site_data/bayport.csv") %>%
  mutate(site = "Baytown Township", county = "Washington")
lakeland <- read_csv("../data/superfund_site_data/lakeland.csv") %>%
  mutate(site = "Lakeland", county = "Washington")

lyndale55 <- read_csv("../data/superfund_site_data/66th.csv") %>%
  mutate(site = "55th St & Lyndale Ave S", county = "Hennepin")
vincent66 <- read_csv("../data/superfund_site_data/lynndale55.csv") %>%
  mutate(site = "66th St. & Vincent Ave", county = "Hennepin")
cedarServices <- read_csv("../data/superfund_site_data/cedarservices.csv") %>%
  mutate(site = "Cedar Services (MDA)", county = "Hennepin")
chemMarketing <- read_csv("../data/superfund_site_data/chemMarketing.csv") %>%
  mutate(site = "Chemical Marketing Corp of America", county = "Hennepin")
cmcHeartland <- read_csv("../data/superfund_site_data/cmcHeartland.csv") %>%
  mutate(site = "CMC Heartland Lite Yard (MDA)", county = "Hennepin")
generalMills <- read_csv("../data/superfund_site_data/generalMills.csv") %>%
  mutate(site = "General Mills/Henkel Corp. site", county = "Hennepin")
goldEagleH <- read_csv("../data/superfund_site_data/goldenEagleH.csv") %>%
  mutate(site = "Gold Eagle Cleaners", county = "Hennepin")
hmong <- read_csv("../data/superfund_site_data/hmong.csv") %>%
  mutate(site = "Hmong Shopping Center/Pilgrim Cleaners", county = "Hennepin")
minnegasco <-  read_csv("../data/superfund_site_data/minnegasco.csv") %>%
  mutate(site = "Minnegasco", county = "Hennepin")
pilgrim <- read_csv("../data/superfund_site_data/pilgrim.csv") %>%
  mutate(site = "Pilgrim Cleaner's site", county = "Hennepin")
precision <- read_csv("../data/superfund_site_data/precision.csv") %>%
  mutate(site = "Precision Plating Inc.", county = "Hennepin")
reilly <- read_csv("../data/superfund_site_data/reilly.csv") %>%
  mutate(site = "Reilly Tar", county = "Hennepin")
schloff <- read_csv("../data/superfund_site_data/schloff.csv") %>%
  mutate(site = "Schloff Chemical", county = "Hennepin")
sehennepin <- read_csv("../data/superfund_site_data/seHennepin.csv") %>%
  mutate(site = "Southeast Hennepin Area groundwater and vapor site", county = "Hennepin")
springpark <- read_csv("../data/superfund_site_data/springpark.csv") %>%
  mutate(site = "Spring Park Municipal Wells", county = "Hennepin")
saintlouis <- read_csv("../data/superfund_site_data/saintlouis.csv") %>%
  mutate(site = "St. Louis Park solvent plume", county = "Hennepin", SYS_LOC_CODE = as.numeric(SYS_LOC_CODE))
superior <- read_csv("../data/superfund_site_data/superior.csv") %>%
  mutate(site = "Superior Plating Inc.", county = "Hennepin")
tonkaMain <- read_csv("../data/superfund_site_data/tonkaMain.csv") %>%
  mutate(site = "Tonka Main Plant", county = "Hennepin")
universalPlating <- read_csv("../data/superfund_site_data/universal.csv") %>%
  mutate(site = "Universal Plating", county = "Hennepin")
whiteway <- read_csv("../data/superfund_site_data/whiteway.csv") %>%
  mutate(site = "White Way Cleaners", county = "Hennepin")

arcadeHawthrone <- read_csv("../data/superfund_site_data/arcade.csv") %>%
  mutate(site = "Arcade & Hawthrone Ave E", county = "Ramsey")
bellLumber <- read_csv("../data/superfund_site_data/bell_lumber.csv") %>%
  mutate(site = "Bell Lumber & Pole Company", county = "Ramsey")
centerville <- read_csv("../data/superfund_site_data/centerville.csv") %>%
  mutate(site = "Centerville Road Dump", county = "Ramsey")
fishHatchery <- read_csv("../data/superfund_site_data/fish_hatchery.csv") %>%
  mutate(site = "Fish Hatchery Dump", county = "Ramsey")
highway96 <- read_csv("../data/superfund_site_data/highway96.csv") %>%
  mutate(site = "Highway 96 Dump", county = "Ramsey")
macGillis <- read_csv("../data/superfund_site_data/macGillis.csv") %>%
  mutate(site = "MacGillis and Gibbs waste site", county = "Ramsey")
pigseye <- read_csv("../data/superfund_site_data/pigseye.csv") %>%
  mutate(site = "Pig's Eye Landfill", county = "Ramsey")
tcaap <- read_csv("../data/superfund_site_data/tcaap.csv") %>%
  mutate(site = "Twin Cities Army Ammunition Plant (TCAAP)", county = "Ramsey")
universityPascal <- read_csv("../data/superfund_site_data/universityPascal.csv") %>%
  mutate(site = "University Ave & Pascal St.", county = "Ramsey")

superfund <- bind_rows(arcadeHawthrone, ashland, bayport, bellLumber, cedarServices, centerville, chemMarketing, cmcHeartland, fishHatchery, generalMills, goldEagleH, highway96, hmong, lakeland, lyndale55, macGillis, minnegasco, oakdale, pigseye, pilgrim, precision, reilly, saintlouis, schloff, sehennepin, springpark, superior, tcaap, threeM, tonkaMain, universalPlating, universityPascal, vincent66, whiteway) %>%
  mutate(commonName = case_when(ANALYTE_NAME == "Perfluorobutanoic acid (PFBA)" ~ "PFBA", 
                                ANALYTE_NAME == "Perfluorooctanoic acid (PFOA)" ~ "PFOA", 
                                ANALYTE_NAME == "Perfluoropentanoic acid (PFPeA)" ~ "PFPeA",
                                ANALYTE_NAME == "Perfluorohexanoic acid (PFHxA)" ~ "PFHxA", 
                                ANALYTE_NAME == "Perfluorooctanesulfonate (PFOS)" ~ "PFOS", 
                                ANALYTE_NAME == "Perfluorohexanesulfonate (PFHxS)" ~ "PFHxS", 
                                ANALYTE_NAME == "Perfluorobutanesulfonate (PFBS)" ~ "PFBS"), 
         RESULT_NUMERIC = case_when(RESULT_UNIT == "ng/L" ~ RESULT_NUMERIC/ 1000, 
                                    TRUE ~ RESULT_NUMERIC))

save(superfund, file = "../data/superfund_site_data/superfund.rds")


