rm(list = ls())

#load packages
x = c("RODBC", "dplyr", "tidyr", "stringr", "ggplot2", "ggthemes")

lapply(x, library, character.only = T)

#connect to database
db <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//EDU-FileServer/HomeDirs/dduffy/My Documents/Data/RDD Exiting Effects/Database/Exit Effects.accdb")

#set home directory

homedir <- "//EDU-FileServer/HomeDirs/dduffy/My Documents/Data/RDD Exiting Effects"

#read in ACCESS data
ACCESS_2012 <- sqlQuery(db, paste("SELECT * FROM ACCESS_2012"), as.is = T)

PROFICIENT_2012 <- sqlQuery(db, paste("SELECT MARSSSTATEREPORTINGNUMBER, GRADE, ACCESS_PROFICIENT FROM COMBINED_2012"), as.is = T)
PROFICIENT_2013 <- sqlQuery(db, paste("SELECT MARSSSTATEREPORTINGNUMBER, GRADE, ACCESS_PROFICIENT FROM COMBINED_2013"), as.is = T)
PROFICIENT_2014 <- sqlQuery(db, paste("SELECT MARSSSTATEREPORTINGNUMBER, GRADE, ACCESS_PROFICIENT FROM COMBINED_2014"), as.is = T)
PROFICIENT_2015 <- sqlQuery(db, paste("SELECT MARSSSTATEREPORTINGNUMBER, GRADE, ACCESS_PROFICIENT FROM COMBINED_2015"), as.is = T)
PROFICIENT_2016 <- sqlQuery(db, paste("SELECT MARSSSTATEREPORTINGNUMBER, GRADE, ACCESS_PROFICIENT FROM COMBINED_2016"), as.is = T)

#Rename LEP variable from each data fram
PROFICIENT_2012 <- rename(PROFICIENT_2012, ACCESS_PROFICIENT_2012 = ACCESS_PROFICIENT, 
                          GRADE_2012 = GRADE)

PROFICIENT_2013 <- rename(PROFICIENT_2013, ACCESS_PROFICIENT_2013 = ACCESS_PROFICIENT, 
                          GRADE_2013 = GRADE)

PROFICIENT_2014 <- rename(PROFICIENT_2014, ACCESS_PROFICIENT_2014 = ACCESS_PROFICIENT, 
                          GRADE_2014 = GRADE)

PROFICIENT_2015 <- rename(PROFICIENT_2015, ACCESS_PROFICIENT_2015 = ACCESS_PROFICIENT, 
                          GRADE_2015 = GRADE)

PROFICIENT_2016 <- rename(PROFICIENT_2016, ACCESS_PROFICIENT_2016 = ACCESS_PROFICIENT, 
                          GRADE_2016 = GRADE)

#combine with ACCESS data from 2012

COMBINED <- ACCESS_2012 %>%
  left_join(PROFICIENT_2012, by = "MARSSSTATEREPORTINGNUMBER") %>%
  left_join(PROFICIENT_2013, by = "MARSSSTATEREPORTINGNUMBER") %>%
  left_join(PROFICIENT_2014, by = "MARSSSTATEREPORTINGNUMBER") %>%
  left_join(PROFICIENT_2015, by = "MARSSSTATEREPORTINGNUMBER") %>%
  left_join(PROFICIENT_2016, by = "MARSSSTATEREPORTINGNUMBER")
  
grades <- c("KG", "01", "02", "03", "04", "05")

GRADE = character()
RESULT.2013 <- numeric()
RESULT.2014 <- numeric()
RESULT.2015 <- numeric()
RESULT.2016 <- numeric()

GRADE_PROBABILITIES <- data.frame(GRADE, RESULT.2013, RESULT.2014, RESULT.2015, RESULT.2016)

for(GRADE in grades){

  DATA.USE <- COMBINED %>%
    mutate(GRADE_2012 = as.character(GRADE_2012)) %>%
    filter(GRADE_2012 == GRADE, 
           COMPOSITE_LEVEL >= 4.5 & COMPOSITE_LEVEL <= 4.9) %>%
    mutate(EVER_PROFICIENT_2013 = ifelse(ACCESS_PROFICIENT_2012 + ACCESS_PROFICIENT_2013 > 0, 1, 0), 
           EVER_PROFICIENT_2014 = ifelse(is.na(ACCESS_PROFICIENT_2012 + ACCESS_PROFICIENT_2013 + ACCESS_PROFICIENT_2014), EVER_PROFICIENT_2013, ifelse(ACCESS_PROFICIENT_2012 + ACCESS_PROFICIENT_2013 + ACCESS_PROFICIENT_2014 > 0, 1, 0)),
           EVER_PROFICIENT_2015 = ifelse(is.na(ACCESS_PROFICIENT_2012 + ACCESS_PROFICIENT_2013 + ACCESS_PROFICIENT_2014 + ACCESS_PROFICIENT_2015), EVER_PROFICIENT_2014, ifelse(ACCESS_PROFICIENT_2012 + ACCESS_PROFICIENT_2013 + ACCESS_PROFICIENT_2014 + ACCESS_PROFICIENT_2015 > 0, 1, 0)),
           EVER_PROFICIENT_2016 = ifelse(is.na(ACCESS_PROFICIENT_2012 + ACCESS_PROFICIENT_2013 + ACCESS_PROFICIENT_2014 + ACCESS_PROFICIENT_2015 + + ACCESS_PROFICIENT_2016), EVER_PROFICIENT_2015, ifelse(ACCESS_PROFICIENT_2012 + ACCESS_PROFICIENT_2013 + ACCESS_PROFICIENT_2014 + ACCESS_PROFICIENT_2015 + ACCESS_PROFICIENT_2016 > 0, 1, 0))) 
  
  RESULT.2013 <- DATA.USE %>%
    summarise(mean(EVER_PROFICIENT_2013, na.rm = T))
  
  RESULT.2014 <-DATA.USE %>%
    summarise(mean(EVER_PROFICIENT_2014, na.rm = T))
  
  RESULT.2015 <- DATA.USE %>%
    summarise(mean(EVER_PROFICIENT_2015, na.rm = T))
  
  RESULT.2016 <- DATA.USE %>%
    summarise(mean(EVER_PROFICIENT_2016, na.rm = T))
  
  RESULT.2013 <- RESULT.2013[1,1]
  RESULT.2014 <- RESULT.2014[1,1]
  RESULT.2015 <- RESULT.2015[1,1]
  RESULT.2016 <- RESULT.2016[1,1]
  
  RESULTS <- data.frame(GRADE, RESULT.2013, RESULT.2014, RESULT.2015, RESULT.2016)
  
  GRADE_PROBABILITIES <- rbind(GRADE_PROBABILITIES, RESULTS)
  
}

GRADE_PROBABILITIES <- GRADE_PROBABILITIES %>%
  gather("YEAR", "RESULTS", RESULT.2013, RESULT.2014, RESULT.2015, RESULT.2016) %>%
  mutate(YEAR = as.factor(substr(YEAR, 8, 11)))

p1 <- ggplot(GRADE_PROBABILITIES, aes(x = YEAR, y = RESULTS)) + geom_bar(stat = "identity") + facet_grid(. ~ GRADE)

#reset working director
setwd(paste(homedir, "/GRAPHS", sep = ""))

#save plot
ggsave(p1, file = "Likelihood of proficiency in subsequent grades by starting grade.png", height = 5, width = 9)


#Look at probability of reclassification of 5th graders for 6th, 7th, and 8th grades

FLAG_2013 <- sqlQuery(db, paste("SELECT MARSSSTATEREPORTINGNUMBER, LEP FROM COMBINED_2013"), as.is = T)
FLAG_2014 <- sqlQuery(db, paste("SELECT MARSSSTATEREPORTINGNUMBER, LEP FROM COMBINED_2014"), as.is = T)
FLAG_2015 <- sqlQuery(db, paste("SELECT MARSSSTATEREPORTINGNUMBER, LEP FROM COMBINED_2015"), as.is = T)
FLAG_2016 <- sqlQuery(db, paste("SELECT MARSSSTATEREPORTINGNUMBER, LEP FROM COMBINED_2016"), as.is = T)

FLAG_2013 <- rename(FLAG_2013, LEP_2013 = LEP)
FLAG_2014 <- rename(FLAG_2014, LEP_2014 = LEP)
FLAG_2015 <- rename(FLAG_2015, LEP_2015 = LEP)
FLAG_2016 <- rename(FLAG_2016, LEP_2016 = LEP)


ACCESS_2013 <- sqlQuery(db, paste("SELECT * FROM ACCESS_2013"), as.is = T)

COMBINED_DATA <- ACCESS_2013 %>%
  filter(GRADE == "05") %>%
  left_join(FLAG_2014, by = "MARSSSTATEREPORTINGNUMBER") %>%
  left_join(FLAG_2015, by = "MARSSSTATEREPORTINGNUMBER") %>%
  left_join(FLAG_2016, by = "MARSSSTATEREPORTINGNUMBER") %>%
  mutate(RECLASSIFIED_2014 = ifelse(LEP_2014 == 0, 1, 0), 
         RECLASSIFIED_2015 = ifelse(LEP_2014 == 1 & LEP_2015 == 0, 1, 0), 
         RECLASSIFIED_2016 = ifelse(LEP_2015 == 1 & LEP_2016 == 0, 1, 0), 
         BY_2015 = ifelse(RECLASSIFIED_2014 == 1 | RECLASSIFIED_2015 == 1, 1, 0), 
         BY_2016 = ifelse(RECLASSIFIED_2014 == 1 | RECLASSIFIED_2015 == 1 | RECLASSIFIED_2016 == 1, 1, 0))

M1 <- glm(RECLASSIFIED_2014 ~ factor(COMPOSITE_LEVEL), data = COMBINED_DATA, family = "binomial")

PROB_2014 <- COMBINED_DATA %>%
  select(COMPOSITE_LEVEL, RECLASSIFIED_2014) %>%
  filter(complete.cases(.))

PROB_2014$PROBABILITIES <- M1$fitted.values

p2 <- ggplot(PROB_2014, aes(x = COMPOSITE_LEVEL, y = PROBABILITIES)) + geom_point()

ggsave(p2, file = "Likelihood of Reclassification after 1 year.png", height = 5, width = 9)


M2 <- glm(BY_2015 ~ factor(COMPOSITE_LEVEL), data = COMBINED_DATA, family = "binomial")

PROB_2015 <- COMBINED_DATA %>%
  select(COMPOSITE_LEVEL, BY_2015) %>%
  filter(complete.cases(.))

PROB_2015$PROBABILITIES <- M2$fitted.values

p3 <- ggplot(PROB_2015, aes(x = COMPOSITE_LEVEL, y = PROBABILITIES)) + geom_point()

ggsave(p3, file = "Likelihood of Reclassification after 2 years.png", height = 5, width = 9)


M3 <- glm(BY_2016 ~ factor(COMPOSITE_LEVEL), data = COMBINED_DATA, family = "binomial")

PROB_2016 <- COMBINED_DATA %>%
  select(COMPOSITE_LEVEL, BY_2016) %>%
  filter(complete.cases(.))

PROB_2016$PROBABILITIES <- M3$fitted.values

p4 <- ggplot(PROB_2016, aes(x = COMPOSITE_LEVEL, y = PROBABILITIES)) + geom_point()

ggsave(p4, file = "Likelihood of Reclassification after 3 years.png", height = 5, width = 9)

#Save combined data

sqlSave(db, COMBINED_DATA, "COMBINED_FIFTH", rownames = F)

setwd(homedir)
saveRDS(COMBINED_DATA, file = "COMBINED_FIFTH")



odbcClose(db)



































