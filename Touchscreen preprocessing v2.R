#set environment
rm(list = ls())
setwd("/Users/Intercalation/")
library(dplyr)
options(max.print = 10000)


#read touchscreen df
ts1 <- read.delim("TableDownloads/Touchscreen_1_v2_participant.tsv", sep = '\t')
ts2 <- read.delim("TableDownloads/Touchscreen_2_v2_participant.tsv", sep = "\t")
ts <- merge(ts1,ts2)
colnames(ts)

crs <- read.delim(file = "TableDownloads/Clinical_Predictors_v3.tsv", sep = "\t")
colnames(crs)
sex <- crs[,c('Participant.ID','Sex')]
#colnames(sex)[colnames(sex) == "Participant.ID"] <- "eid"
colnames(sex)

ts <- merge(ts,sex)
colnames(ts)


#exclude non-original questions
ts <- ts %>%
  select(-Current.employment.status...corrected...Instance.0, #this is based on verbal interviews
         -At.or.above.moderate.vigorous.recommendation...Instance.0, #the following fields are fed-back and calculated based on touchscreen data                                                
         -IPAQ.activity.group...Instance.0,                                                                         
         -MET.minutes.per.week.for.moderate.activity...Instance.0,                                                   
         -MET.minutes.per.week.for.vigorous.activity...Instance.0,                                                   
         -MET.minutes.per.week.for.walking...Instance.0,                                                             
         -Summed.MET.minutes.per.week.for.all.activity...Instance.0,                                                 
         -Summed.days.activity...Instance.0,                                                                        
         -Summed.minutes.activity...Instance.0,
         -Ever.smoked...Instance.0,
         -Pack.years.adult.smoking.as.proportion.of.life.span.exposed.to.smoking...Instance.0,
         -Pack.years.of.smoking...Instance.0,
         -Bipolar.and.major.depression.status...Instance.0,
         -Bipolar.disorder.status...Instance.0,
         -Neuroticism.score...Instance.0,
         -Probable.recurrent.major.depression..moderate....Instance.0,
         -Probable.recurrent.major.depression..severe....Instance.0,
         -Single.episode.of.probable.major.depression...Instance.0)


ts_backup <- ts
#ts <- ts_backup


#make NAs consistent
ts <- as.data.frame(lapply(ts, function(x) ifelse(x == "", NA, x)))
ts <- as.data.frame(lapply(ts, function(x) ifelse(x %in% c("-3","-1"), NA, x)))
ts <- as.data.frame(lapply(ts, function(x) ifelse(x %in% c("-10"), 0.5, x)))


#small number of data usually due to 'Question was introduced part way through fieldwork in April 2009.'


#reformat category-wise

#define functions to facilitate this process
splitLevelsIntoColumns <- function(dataFrame, columnName) {
  levels_char <- ifelse(is.na(dataFrame[[columnName]]), NA, as.character(dataFrame[[columnName]]))
  split_levels_list <- strsplit(levels_char, "|", fixed = TRUE)
  unique_levels <- unique(unlist(split_levels_list))
  dataFrame[[columnName]] <- NULL
  na_or_empty <- is.na(levels_char)
  for(level in unique_levels) {
    if (!is.na(level)) {  
      colName <- paste(columnName, gsub("-", "neg", gsub(" ", "_", level), fixed = TRUE), sep = "_")
      dataFrame[[colName]] <- sapply(split_levels_list, function(x) {
        if (is.na(x[[1]])) {
          return(NA)
        } else {
          return(!is.na(match(level, x)))
        }
      })
      dataFrame[na_or_empty, colName] <- NA
    }
  }
  return(dataFrame)
}


#define functions to split misc value to a new column
splitMiscLevels <- function(dataFrame, columnName, misc) {
  targetColumn <- as.character(dataFrame[[columnName]])
  initialNAIndices <- which(is.na(targetColumn))
  for (miscValue in misc) {
    newColumnName <- paste(columnName, gsub("-", "neg", gsub(" ", "_", miscValue), fixed = TRUE), sep = "_")
    dataFrame[[newColumnName]] <- ifelse(is.na(targetColumn), NA, targetColumn == miscValue)
  }
  dataFrame[[columnName]] <- ifelse(targetColumn %in% misc, NA, dataFrame[[columnName]])
  
  dataFrame[[columnName]] <- as.numeric(as.character(dataFrame[[columnName]]))
  thresholds <- quantile(dataFrame[[columnName]], probs = c(1/3, 2/3), na.rm = TRUE)
  classifiedValues <- ifelse(dataFrame[[columnName]] < thresholds[1], "Lower.third",
                             ifelse(dataFrame[[columnName]] > thresholds[2], "Upper.third", "Middle.third"))
  dataFrame[[paste0(columnName,"_Lower.third")]] <- (classifiedValues == "Lower.third")
  dataFrame[[paste0(columnName,"_Lower.third")]] <- ifelse(is.na(dataFrame[[paste0(columnName,"_Lower.third")]]), 
                                                           FALSE, dataFrame[[paste0(columnName,"_Lower.third")]])
  dataFrame[[paste0(columnName,"_Upper.third")]] <- (classifiedValues == "Upper.third")
  dataFrame[[paste0(columnName,"_Upper.third")]] <- ifelse(is.na(dataFrame[[paste0(columnName,"_Upper.third")]]), 
                                                           FALSE, dataFrame[[paste0(columnName,"_Upper.third")]])
  dataFrame[[paste0(columnName,"_Middle.third")]] <- (classifiedValues == "Middle.third")
  dataFrame[[paste0(columnName,"_Middle.third")]] <- ifelse(is.na(dataFrame[[paste0(columnName,"_Middle.third")]]), 
                                                            FALSE, dataFrame[[paste0(columnName,"_Middle.third")]])
  newColumns <- grep(paste0("^", columnName, "_"), names(dataFrame), value = TRUE)
  for (n in newColumns) {
    dataFrame[initialNAIndices, n] <- NA
  }
  dataFrame[[columnName]] <- NULL
  return(dataFrame)
}



#sociodemographics
colnames(ts)
#household
levels(as.factor(ts$Type.of.accommodation.lived.in...Instance.0))

table(ts$Type.of.accommodation.lived.in...Instance.0)
table(is.na(ts$Type.of.accommodation.lived.in...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Type.of.accommodation.lived.in...Instance.0")
colnames(ts)


table(ts$Own.or.rent.accommodation.lived.in...Instance.0)
table(is.na(ts$Own.or.rent.accommodation.lived.in...Instance.0))
ts$Own.or.rent.accommodation.lived.in...Instance.0 <- ifelse(ts$Type.of.accommodation.lived.in...Instance.0_4 %in% "TRUE",
                                                             "Sheltered.accommodation",ts$Own.or.rent.accommodation.lived.in...Instance.0)
ts$Own.or.rent.accommodation.lived.in...Instance.0 <- ifelse(ts$Type.of.accommodation.lived.in...Instance.0_5 %in% "TRUE",
                                                             "Care.home",ts$Own.or.rent.accommodation.lived.in...Instance.0)
table(ts$Own.or.rent.accommodation.lived.in...Instance.0)
table(is.na(ts$Own.or.rent.accommodation.lived.in...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Own.or.rent.accommodation.lived.in...Instance.0")
colnames(ts)
ts <- select(ts, -c("Own.or.rent.accommodation.lived.in...Instance.0_Sheltered.accommodation", "Own.or.rent.accommodation.lived.in...Instance.0_Care.home"))
colnames(ts)



table(ts$Gas.or.solid.fuel.cooking.heating...Instance.0)
table(is.na(ts$Gas.or.solid.fuel.cooking.heating...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Gas.or.solid.fuel.cooking.heating...Instance.0")
colnames(ts)


table(ts$Heating.type.s..in.home...Instance.0)
table(is.na(ts$Heating.type.s..in.home...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Heating.type.s..in.home...Instance.0")
colnames(ts)


table(ts$Length.of.time.at.current.address...Instance.0)
table(is.na(ts$Length.of.time.at.current.address...Instance.0))


table(ts$Number.in.household...Instance.0)
table(is.na(ts$Number.in.household...Instance.0))
ts$Number.in.household...Instance.0 <- ifelse(ts$Type.of.accommodation.lived.in...Instance.0_4 %in% "TRUE",
                                              "Sheltered.accommodation",ts$Number.in.household...Instance.0)
ts$Number.in.household...Instance.0 <- ifelse(ts$Type.of.accommodation.lived.in...Instance.0_5 %in% "TRUE",
                                              "Care.home",ts$Number.in.household...Instance.0)
table(ts$Number.in.household...Instance.0)
table(is.na(ts$Number.in.household...Instance.0))
ts <- splitMiscLevels(ts,"Number.in.household...Instance.0",c("Care.home","Sheltered.accommodation"))
colnames(ts)
ts <- select(ts, -c("Number.in.household...Instance.0_Care.home", "Number.in.household...Instance.0_Sheltered.accommodation"))
colnames(ts)


table(ts$How.are.people.in.household.related.to.participant...Instance.0)
table(is.na(ts$How.are.people.in.household.related.to.participant...Instance.0))
ts$How.are.people.in.household.related.to.participant...Instance.0 <- ifelse(ts$Type.of.accommodation.lived.in...Instance.0_4 %in% "TRUE",
                                                             "Sheltered.accommodation",ts$How.are.people.in.household.related.to.participant...Instance.0)
ts$How.are.people.in.household.related.to.participant...Instance.0 <- ifelse(ts$Type.of.accommodation.lived.in...Instance.0_5 %in% "TRUE",
                                                             "Care.home",ts$How.are.people.in.household.related.to.participant...Instance.0)
table(ts$How.are.people.in.household.related.to.participant...Instance.0)
table(is.na(ts$How.are.people.in.household.related.to.participant...Instance.0))
ts$How.are.people.in.household.related.to.participant...Instance.0 <- ifelse(ts_backup$Number.in.household...Instance.0 %in% c("1"),
                                                                             "Living.alone",ts$How.are.people.in.household.related.to.participant...Instance.0)
table(ts$How.are.people.in.household.related.to.participant...Instance.0)
table(is.na(ts$How.are.people.in.household.related.to.participant...Instance.0))
ts <- splitLevelsIntoColumns(ts,"How.are.people.in.household.related.to.participant...Instance.0")
colnames(ts)
ts <- select(ts, -c("How.are.people.in.household.related.to.participant...Instance.0_Living.alone", 
                    "How.are.people.in.household.related.to.participant...Instance.0_Sheltered.accommodation", 
                    "How.are.people.in.household.related.to.participant...Instance.0_Care.home"))
colnames(ts)


table(ts$Number.of.vehicles.in.household...Instance.0)
table(is.na(ts$Number.of.vehicles.in.household...Instance.0))
ts$Number.of.vehicles.in.household...Instance.0 <- ifelse(ts$Type.of.accommodation.lived.in...Instance.0_4 %in% "TRUE",
                                                             "Sheltered.accommodation",ts$Number.of.vehicles.in.household...Instance.0)
ts$Number.of.vehicles.in.household...Instance.0 <- ifelse(ts$Type.of.accommodation.lived.in...Instance.0_5 %in% "TRUE",
                                                             "Care.home",ts$Number.of.vehicles.in.household...Instance.0)
table(ts$Number.of.vehicles.in.household...Instance.0)
table(is.na(ts$Number.of.vehicles.in.household...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Number.of.vehicles.in.household...Instance.0")
colnames(ts)
ts <- select(ts, -c("Number.of.vehicles.in.household...Instance.0_Sheltered.accommodation", 
                    "Number.of.vehicles.in.household...Instance.0_Care.home"))
colnames(ts)


table(ts$Average.total.household.income.before.tax...Instance.0)
table(is.na(ts$Average.total.household.income.before.tax...Instance.0))
ts$Average.total.household.income.before.tax...Instance.0 <- ifelse(ts$Type.of.accommodation.lived.in...Instance.0_4 %in% "TRUE",
                                                             "Sheltered.accommodation",ts$Average.total.household.income.before.tax...Instance.0)
ts$Average.total.household.income.before.tax...Instance.0 <- ifelse(ts$Type.of.accommodation.lived.in...Instance.0_5 %in% "TRUE",
                                                             "Care.home",ts$Average.total.household.income.before.tax...Instance.0)
table(ts$Average.total.household.income.before.tax...Instance.0)
table(is.na(ts$Average.total.household.income.before.tax...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Average.total.household.income.before.tax...Instance.0")
colnames(ts)
ts <- select(ts, -c("Average.total.household.income.before.tax...Instance.0_Sheltered.accommodation", 
                    "Average.total.household.income.before.tax...Instance.0_Care.home"))
colnames(ts)


#employment
table(ts$Current.employment.status...Instance.0)
table(is.na(ts$Current.employment.status...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Current.employment.status...Instance.0")
colnames(ts)


table(ts$Frequency.of.travelling.from.home.to.job.workplace...Instance.0)
table(is.na(ts$Frequency.of.travelling.from.home.to.job.workplace...Instance.0))
ts$Frequency.of.travelling.from.home.to.job.workplace...Instance.0 <- ifelse(ts$Current.employment.status...Instance.0_1 %in% c("FALSE"),
                                                                                   "Other.employment",ts$Frequency.of.travelling.from.home.to.job.workplace...Instance.0)
table(ts$Frequency.of.travelling.from.home.to.job.workplace...Instance.0)
table(is.na(ts$Frequency.of.travelling.from.home.to.job.workplace...Instance.0))
ts <- splitMiscLevels(ts,"Frequency.of.travelling.from.home.to.job.workplace...Instance.0", "Other.employment")
colnames(ts)
ts <- select(ts, -c("Frequency.of.travelling.from.home.to.job.workplace...Instance.0_Other.employment"))
colnames(ts)


table(ts$Distance.between.home.and.job.workplace...Instance.0)
table(is.na(ts$Distance.between.home.and.job.workplace...Instance.0))
ts$Distance.between.home.and.job.workplace...Instance.0 <- ifelse(ts$Current.employment.status...Instance.0_1 %in% c("FALSE"),
                                                                  "Other.employment",ts$Distance.between.home.and.job.workplace...Instance.0)
ts$Distance.between.home.and.job.workplace...Instance.0 <- ifelse(ts_backup$Frequency.of.travelling.from.home.to.job.workplace...Instance.0 %in% c("0"),
                                                                  "Work.from.home",ts$Distance.between.home.and.job.workplace...Instance.0)
table(ts$Distance.between.home.and.job.workplace...Instance.0)
table(is.na(ts$Distance.between.home.and.job.workplace...Instance.0))
ts <- splitMiscLevels(ts,"Distance.between.home.and.job.workplace...Instance.0", c("Other.employment", "Work.from.home"))
colnames(ts)
ts <- select(ts, -c("Distance.between.home.and.job.workplace...Instance.0_Other.employment",
                    "Distance.between.home.and.job.workplace...Instance.0_Work.from.home"))
colnames(ts)


table(ts$Time.employed.in.main.current.job...Instance.0)
table(is.na(ts$Time.employed.in.main.current.job...Instance.0))
ts$Time.employed.in.main.current.job...Instance.0 <- ifelse(ts$Current.employment.status...Instance.0_1 %in% c("FALSE"),
                                                            "Other.employment",ts$Time.employed.in.main.current.job...Instance.0)
table(ts$Time.employed.in.main.current.job...Instance.0)
table(is.na(ts$Time.employed.in.main.current.job...Instance.0))
ts <- splitMiscLevels(ts,"Time.employed.in.main.current.job...Instance.0", "Other.employment")
colnames(ts)
ts <- select(ts, -c("Time.employed.in.main.current.job...Instance.0_Other.employment"))
colnames(ts)


table(ts$Length.of.working.week.for.main.job...Instance.0)
table(is.na(ts$Length.of.working.week.for.main.job...Instance.0))
ts$Length.of.working.week.for.main.job...Instance.0 <- ifelse(ts$Current.employment.status...Instance.0_1 %in% c("FALSE"),
                                                              "Other.employment",ts$Length.of.working.week.for.main.job...Instance.0)
table(ts$Length.of.working.week.for.main.job...Instance.0)
table(is.na(ts$Length.of.working.week.for.main.job...Instance.0))
ts <- splitMiscLevels(ts,"Length.of.working.week.for.main.job...Instance.0", "Other.employment")
colnames(ts)
ts <- select(ts, -c("Length.of.working.week.for.main.job...Instance.0_Other.employment"))
colnames(ts)


table(ts$Transport.type.for.commuting.to.job.workplace...Instance.0)
table(is.na(ts$Transport.type.for.commuting.to.job.workplace...Instance.0))
ts$Transport.type.for.commuting.to.job.workplace...Instance.0 <- ifelse(ts$Current.employment.status...Instance.0_1 %in% c("FALSE"),
                                                                        "Other.employment",ts$Transport.type.for.commuting.to.job.workplace...Instance.0)
table((ts$Transport.type.for.commuting.to.job.workplace...Instance.0))
ts$Transport.type.for.commuting.to.job.workplace...Instance.0 <- ifelse(ts_backup$Frequency.of.travelling.from.home.to.job.workplace...Instance.0 %in% c("0"),
                                                                        "Work.from.home",ts$Transport.type.for.commuting.to.job.workplace...Instance.0)
table(ts$Transport.type.for.commuting.to.job.workplace...Instance.0)
ts <- splitLevelsIntoColumns(ts,"Transport.type.for.commuting.to.job.workplace...Instance.0")
colnames(ts)
ts <- select(ts, -c("Transport.type.for.commuting.to.job.workplace...Instance.0_Other.employment", 
                    "Transport.type.for.commuting.to.job.workplace...Instance.0_Work.from.home"))
colnames(ts)


table(ts$Job.involves.mainly.walking.or.standing...Instance.0)
table(is.na(ts$Job.involves.mainly.walking.or.standing...Instance.0))
ts$Job.involves.mainly.walking.or.standing...Instance.0 <- ifelse(ts$Current.employment.status...Instance.0_1 %in% c("FALSE"),
                                                                  "Other.employment",ts$Job.involves.mainly.walking.or.standing...Instance.0)
table(ts$Job.involves.mainly.walking.or.standing...Instance.0)
table(is.na(ts$Job.involves.mainly.walking.or.standing...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Job.involves.mainly.walking.or.standing...Instance.0")
colnames(ts)
ts <- select(ts, -c("Job.involves.mainly.walking.or.standing...Instance.0_Other.employment"))
colnames(ts)


table(ts$Job.involves.heavy.manual.or.physical.work...Instance.0)
table(is.na(ts$Job.involves.heavy.manual.or.physical.work...Instance.0))
ts$Job.involves.heavy.manual.or.physical.work...Instance.0 <- ifelse(ts$Current.employment.status...Instance.0_1 %in% c("FALSE"),
                                                                     "Other.employment",ts$Job.involves.heavy.manual.or.physical.work...Instance.0)
table(ts$Job.involves.heavy.manual.or.physical.work...Instance.0)
table(is.na(ts$Job.involves.heavy.manual.or.physical.work...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Job.involves.heavy.manual.or.physical.work...Instance.0")
colnames(ts)
ts <- select(ts, -c("Job.involves.heavy.manual.or.physical.work...Instance.0_Other.employment"))
colnames(ts)


table(ts$Job.involves.shift.work...Instance.0)
table(is.na(ts$Job.involves.shift.work...Instance.0))
ts$Job.involves.shift.work...Instance.0 <- ifelse(ts$Current.employment.status...Instance.0_1 %in% c("FALSE"),
                                                  "Other.employment",ts$Job.involves.shift.work...Instance.0)
table(ts$Job.involves.shift.work...Instance.0)
table(is.na(ts$Job.involves.shift.work...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Job.involves.shift.work...Instance.0")
colnames(ts)
ts <- select(ts, -c("Job.involves.shift.work...Instance.0_Other.employment"))
colnames(ts)


table(ts$Job.involves.night.shift.work...Instance.0)
table(is.na(ts$Job.involves.night.shift.work...Instance.0))
ts$Job.involves.night.shift.work...Instance.0 <- ifelse(ts$Current.employment.status...Instance.0_1 %in% c("FALSE"),
                                                        "Other.employment",ts$Job.involves.night.shift.work...Instance.0)
table(ts$Job.involves.night.shift.work...Instance.0)
ts$Job.involves.night.shift.work...Instance.0 <- ifelse(ts$Job.involves.shift.work...Instance.0_1 %in% c("TRUE"),
                                                        "Never/rarely.a.shift",ts$Job.involves.night.shift.work...Instance.0)
table(ts$Job.involves.night.shift.work...Instance.0)
table(is.na(ts$Job.involves.night.shift.work...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Job.involves.night.shift.work...Instance.0")
colnames(ts)
ts <- select(ts, -c("Job.involves.night.shift.work...Instance.0_Other.employment",
                    "Job.involves.night.shift.work...Instance.0_Never/rarely.a.shift" ))
colnames(ts)


#education
table(ts$Qualifications...Instance.0)
table(is.na(ts$Qualifications...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Qualifications...Instance.0")
colnames(ts)


table(ts$Age.completed.full.time.education...Instance.0)
table(is.na(ts$Age.completed.full.time.education...Instance.0))
ts$Age.completed.full.time.education...Instance.0 <- ifelse(ts$Qualifications...Instance.0_1 %in% c("TRUE"),
                                                            "College.or.university",ts$Age.completed.full.time.education...Instance.0)
table(ts$Age.completed.full.time.education...Instance.0)
table(is.na(ts$Age.completed.full.time.education...Instance.0))
ts <- splitMiscLevels(ts,"Age.completed.full.time.education...Instance.0",c("-2","College.or.university"))
colnames(ts)
ts <- select(ts, -c("Age.completed.full.time.education...Instance.0_College.or.university"))
colnames(ts)


#early life factors
table(ts$Country.of.birth..UK.elsewhere....Instance.0)
table(is.na(ts$Country.of.birth..UK.elsewhere....Instance.0))
ts <- splitLevelsIntoColumns(ts,"Country.of.birth..UK.elsewhere....Instance.0")
colnames(ts)


#ethnicity
table(ts$Year.immigrated.to.UK..United.Kingdom....Instance.0)
table(is.na(ts$Year.immigrated.to.UK..United.Kingdom....Instance.0))
ts$Year.immigrated.to.UK..United.Kingdom....Instance.0 <- ifelse(ts$Country.of.birth..UK.elsewhere....Instance.0_6 %in% ("FALSE"),
                                                                 "Born.in.the.UK",ts$Year.immigrated.to.UK..United.Kingdom....Instance.0)
table(ts$Year.immigrated.to.UK..United.Kingdom....Instance.0)
table(is.na(ts$Year.immigrated.to.UK..United.Kingdom....Instance.0))
ts <- splitMiscLevels(ts,"Year.immigrated.to.UK..United.Kingdom....Instance.0", "Born.in.the.UK")
colnames(ts)
ts <- select(ts, -c("Year.immigrated.to.UK..United.Kingdom....Instance.0_Born.in.the.UK"))
colnames(ts)


table(ts$Ethnic.background...Instance.0)
table(is.na(ts$Ethnic.background...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Ethnic.background...Instance.0")
colnames(ts)


#other sociodemographic factors
table(ts$Attendance.disability.mobility.allowance...Instance.0)
table(is.na(ts$Attendance.disability.mobility.allowance...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Attendance.disability.mobility.allowance...Instance.0")
colnames(ts)


table(ts$Private.healthcare...Instance.0)
table(is.na(ts$Private.healthcare...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Private.healthcare...Instance.0")
colnames(ts)

#lifestyle and environment
#physical activity
table(ts$Drive.faster.than.motorway.speed.limit...Instance.0)
table(is.na(ts$Drive.faster.than.motorway.speed.limit...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Drive.faster.than.motorway.speed.limit...Instance.0")
colnames(ts)


table(ts$Types.of.physical.activity.in.last.4.weeks...Instance.0)
table(is.na(ts$Types.of.physical.activity.in.last.4.weeks...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Types.of.physical.activity.in.last.4.weeks...Instance.0")
colnames(ts)


table(ts$Types.of.physical.activity.in.last.4.weeks...Instance.0_4 %in% c("FALSE"))

table(ts$Duration.of.heavy.DIY...Instance.0)
table(is.na(ts$Duration.of.heavy.DIY...Instance.0))
ts$Duration.of.heavy.DIY...Instance.0 <- ifelse(ts$Types.of.physical.activity.in.last.4.weeks...Instance.0_5 %in% c("FALSE"),
                                                "No.heavy.DIY",ts$Duration.of.heavy.DIY...Instance.0)
table(ts$Duration.of.heavy.DIY...Instance.0)
table(is.na(ts$Duration.of.heavy.DIY...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Duration.of.heavy.DIY...Instance.0")
colnames(ts)
ts <- select(ts, -c("Duration.of.heavy.DIY...Instance.0_No.heavy.DIY"))
colnames(ts)


table(ts$Duration.of.light.DIY...Instance.0)
table(is.na(ts$Duration.of.light.DIY...Instance.0))
ts$Duration.of.light.DIY...Instance.0 <- ifelse(ts$Types.of.physical.activity.in.last.4.weeks...Instance.0_4 %in% c("FALSE"),
                                                "No.light.DIY",ts$Duration.of.light.DIY...Instance.0)
table(ts$Duration.of.light.DIY...Instance.0)
table(is.na(ts$Duration.of.light.DIY...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Duration.of.light.DIY...Instance.0")
colnames(ts)
ts <- select(ts, -c("Duration.of.light.DIY...Instance.0_No.light.DIY"))
colnames(ts)


table(ts$Number.of.days.week.of.moderate.physical.activity.10..minutes...Instance.0)
table(is.na(ts$Number.of.days.week.of.moderate.physical.activity.10..minutes...Instance.0))


table(ts$Duration.of.moderate.activity...Instance.0)
table(is.na(ts$Duration.of.moderate.activity...Instance.0))
ts$Duration.of.moderate.activity...Instance.0 <- ifelse(ts$Number.of.days.week.of.moderate.physical.activity.10..minutes...Instance.0 %in% c("0"),
                                                        "No.moderate.activity",ts$Duration.of.moderate.activity...Instance.0)
table(ts$Duration.of.moderate.activity...Instance.0)
table(is.na(ts$Duration.of.moderate.activity...Instance.0))
ts <- splitMiscLevels(ts,"Duration.of.moderate.activity...Instance.0", "No.moderate.activity")
colnames(ts)
ts <- select(ts, -c("Duration.of.moderate.activity...Instance.0_No.moderate.activity" ))
colnames(ts)


table(ts$Types.of.physical.activity.in.last.4.weeks...Instance.0_3 %in% c("FALSE"))

table(ts$Duration.of.other.exercises...Instance.0)
table(is.na(ts$Duration.of.other.exercises...Instance.0))
ts$Duration.of.other.exercises...Instance.0 <- ifelse(ts$Types.of.physical.activity.in.last.4.weeks...Instance.0_2 %in% c("FALSE"),
                                                      "No.other.exercises",ts$Duration.of.other.exercises...Instance.0)
table(ts$Duration.of.other.exercises...Instance.0)
table(is.na(ts$Duration.of.other.exercises...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Duration.of.other.exercises...Instance.0")
colnames(ts)
ts <- select(ts, -c("Duration.of.other.exercises...Instance.0_No.other.exercises"))
colnames(ts)


table(ts$Duration.of.strenuous.sports...Instance.0)
table(is.na(ts$Duration.of.strenuous.sports...Instance.0))
ts$Duration.of.strenuous.sports...Instance.0 <- ifelse(ts$Types.of.physical.activity.in.last.4.weeks...Instance.0_3 %in% c("FALSE"),
                                                       "No.strenuous.sports",ts$Duration.of.strenuous.sports...Instance.0)
table(ts$Duration.of.strenuous.sports...Instance.0)
table(is.na(ts$Duration.of.strenuous.sports...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Duration.of.strenuous.sports...Instance.0")
colnames(ts)
ts <- select(ts, -c("Duration.of.strenuous.sports...Instance.0_No.strenuous.sports"))
colnames(ts)


table(ts$Number.of.days.week.of.vigorous.physical.activity.10..minutes...Instance.0)
table(is.na(ts$Number.of.days.week.of.vigorous.physical.activity.10..minutes...Instance.0))


table(ts$Duration.of.vigorous.activity...Instance.0)
table(is.na(ts$Duration.of.vigorous.activity...Instance.0))
ts$Duration.of.vigorous.activity...Instance.0 <- ifelse(ts$Number.of.days.week.of.vigorous.physical.activity.10..minutes...Instance.0 %in% c("0"),
                                                        "No.vigorous.activity",ts$Duration.of.vigorous.activity...Instance.0)
table(ts$Duration.of.vigorous.activity...Instance.0)
table(is.na(ts$Duration.of.vigorous.activity...Instance.0))
ts <- splitMiscLevels(ts,"Duration.of.vigorous.activity...Instance.0", "No.vigorous.activity")
colnames(ts)
ts <- select(ts, -c("Duration.of.vigorous.activity...Instance.0_No.vigorous.activity"))
colnames(ts)


table(ts$Number.of.days.week.walked.10..minutes...Instance.0)
table(is.na(ts$Number.of.days.week.walked.10..minutes...Instance.0))
ts <- splitMiscLevels(ts,"Number.of.days.week.walked.10..minutes...Instance.0","-2")


table(ts$Duration.of.walks...Instance.0)
table(is.na(ts$Duration.of.walks...Instance.0))
ts$Duration.of.walks...Instance.0 <- ifelse(ts_backup$Number.of.days.week.walked.10..minutes...Instance.0 %in% c("-2"),
                                            "Unable.to.walk",ts$Duration.of.walks...Instance.0)
ts$Duration.of.walks...Instance.0 <- ifelse(ts_backup$Number.of.days.week.walked.10..minutes...Instance.0 %in% c("0"),
                                            "No.walks",ts$Duration.of.walks...Instance.0)
table(ts$Duration.of.walks...Instance.0)
table(is.na(ts$Duration.of.walks...Instance.0))
ts <- splitMiscLevels(ts,"Duration.of.walks...Instance.0", c("No.walks", "Unable.to.walk"))
colnames(ts)
ts <- select(ts, -c("Duration.of.walks...Instance.0_No.walks", 
                    "Duration.of.walks...Instance.0_Unable.to.walk"))
colnames(ts)


table(ts$Types.of.physical.activity.in.last.4.weeks...Instance.0_1 %in% c("FALSE"))

table(ts$Duration.walking.for.pleasure...Instance.0)
table(is.na(ts$Duration.walking.for.pleasure...Instance.0))
ts$Duration.walking.for.pleasure...Instance.0 <- ifelse(ts$Types.of.physical.activity.in.last.4.weeks...Instance.0_1 %in% c("FALSE"),
                                                        "No.walking.for.pleasure",ts$Duration.walking.for.pleasure...Instance.0)
table(ts$Duration.walking.for.pleasure...Instance.0)
table(is.na(ts$Duration.walking.for.pleasure...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Duration.walking.for.pleasure...Instance.0")
colnames(ts)
ts <- select(ts, -c("Duration.walking.for.pleasure...Instance.0_No.walking.for.pleasure"))
colnames(ts)


table(ts$Types.of.physical.activity.in.last.4.weeks...Instance.0_1 %in% c("FALSE"))

table(ts$Frequency.of.heavy.DIY.in.last.4.weeks...Instance.0)
table(is.na(ts$Frequency.of.heavy.DIY.in.last.4.weeks...Instance.0))
ts$Frequency.of.heavy.DIY.in.last.4.weeks...Instance.0 <- ifelse(ts$Types.of.physical.activity.in.last.4.weeks...Instance.0_5 %in% c("FALSE"),
                                                                 "No.heavy.DIY",ts$Frequency.of.heavy.DIY.in.last.4.weeks...Instance.0)
table(ts$Frequency.of.heavy.DIY.in.last.4.weeks...Instance.0)
table(is.na(ts$Frequency.of.heavy.DIY.in.last.4.weeks...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Frequency.of.heavy.DIY.in.last.4.weeks...Instance.0")
colnames(ts)
ts <- select(ts, -c("Frequency.of.heavy.DIY.in.last.4.weeks...Instance.0_No.heavy.DIY"))
colnames(ts)


table(ts$Frequency.of.light.DIY.in.last.4.weeks...Instance.0)
table(is.na(ts$Frequency.of.light.DIY.in.last.4.weeks...Instance.0))
ts$Frequency.of.light.DIY.in.last.4.weeks...Instance.0 <- ifelse(ts$Types.of.physical.activity.in.last.4.weeks...Instance.0_4 %in% c("FALSE"),
                                                                 "No.light.DIY",ts$Frequency.of.light.DIY.in.last.4.weeks...Instance.0)
table(ts$Frequency.of.light.DIY.in.last.4.weeks...Instance.0)
table(is.na(ts$Frequency.of.light.DIY.in.last.4.weeks...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Frequency.of.light.DIY.in.last.4.weeks...Instance.0")
colnames(ts)
ts <- select(ts, -c("Frequency.of.light.DIY.in.last.4.weeks...Instance.0_No.light.DIY"))
colnames(ts)


table(ts$Frequency.of.other.exercises.in.last.4.weeks...Instance.0)
table(is.na(ts$Frequency.of.other.exercises.in.last.4.weeks...Instance.0))
ts$Frequency.of.other.exercises.in.last.4.weeks...Instance.0 <- ifelse(ts$Types.of.physical.activity.in.last.4.weeks...Instance.0_2 %in% c("FALSE"),
                                                                       "No.other.exercises",ts$Frequency.of.other.exercises.in.last.4.weeks...Instance.0)
table(ts$Frequency.of.other.exercises.in.last.4.weeks...Instance.0)
table(is.na(ts$Frequency.of.other.exercises.in.last.4.weeks...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Frequency.of.other.exercises.in.last.4.weeks...Instance.0")
colnames(ts)
ts <- select(ts, -c("Frequency.of.other.exercises.in.last.4.weeks...Instance.0_No.other.exercises"))
colnames(ts)


table(ts$Frequency.of.stair.climbing.in.last.4.weeks...Instance.0)
table(is.na(ts$Frequency.of.stair.climbing.in.last.4.weeks...Instance.0))
ts$Frequency.of.stair.climbing.in.last.4.weeks...Instance.0 <- ifelse(ts_backup$Number.of.days.week.walked.10..minutes...Instance.0 %in% c("-2"),
                                                                      "Unable.to.walk",ts$Frequency.of.stair.climbing.in.last.4.weeks...Instance.0)
table(ts$Frequency.of.stair.climbing.in.last.4.weeks...Instance.0)
table(is.na(ts$Frequency.of.stair.climbing.in.last.4.weeks...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Frequency.of.stair.climbing.in.last.4.weeks...Instance.0")
colnames(ts)
ts <- select(ts, -c("Frequency.of.stair.climbing.in.last.4.weeks...Instance.0_Unable.to.walk"))
colnames(ts)


table(ts$Frequency.of.strenuous.sports.in.last.4.weeks...Instance.0)
table(is.na(ts$Frequency.of.strenuous.sports.in.last.4.weeks...Instance.0))
ts$Frequency.of.strenuous.sports.in.last.4.weeks...Instance.0 <- ifelse(ts$Types.of.physical.activity.in.last.4.weeks...Instance.0_3 %in% c("FALSE"),
                                                                        "No.strenuous.sports",ts$Frequency.of.strenuous.sports.in.last.4.weeks...Instance.0)
table(ts$Frequency.of.strenuous.sports.in.last.4.weeks...Instance.0)
table(is.na(ts$Frequency.of.strenuous.sports.in.last.4.weeks...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Frequency.of.strenuous.sports.in.last.4.weeks...Instance.0")
colnames(ts)
ts <- select(ts, -c("Frequency.of.strenuous.sports.in.last.4.weeks...Instance.0_No.strenuous.sports"))
colnames(ts)



table(ts$Frequency.of.walking.for.pleasure.in.last.4.weeks...Instance.0)
table(is.na(ts$Frequency.of.walking.for.pleasure.in.last.4.weeks...Instance.0))
ts$Frequency.of.walking.for.pleasure.in.last.4.weeks...Instance.0 <- ifelse(ts$Types.of.physical.activity.in.last.4.weeks...Instance.0_1 %in% c("FALSE"),
                                                                            "No.walking.for.pleasure",ts$Frequency.of.walking.for.pleasure.in.last.4.weeks...Instance.0)
table(ts$Frequency.of.walking.for.pleasure.in.last.4.weeks...Instance.0)
table(is.na(ts$Frequency.of.walking.for.pleasure.in.last.4.weeks...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Frequency.of.walking.for.pleasure.in.last.4.weeks...Instance.0")
colnames(ts)
ts <- select(ts, -c("Frequency.of.walking.for.pleasure.in.last.4.weeks...Instance.0_No.walking.for.pleasure"))
colnames(ts)


table(ts$Time.spent.driving...Instance.0)
table(is.na(ts$Time.spent.driving...Instance.0))


table(ts$Time.spent.using.computer...Instance.0)
table(is.na(ts$Time.spent.using.computer...Instance.0))


table(ts$Time.spent.watching.television..TV....Instance.0)
table(is.na(ts$Time.spent.watching.television..TV....Instance.0))


table(ts$Types.of.transport.used..excluding.work....Instance.0)
table(is.na(ts$Types.of.transport.used..excluding.work....Instance.0))
ts$Types.of.transport.used..excluding.work....Instance.0 <- ifelse(ts_backup$Number.of.days.week.walked.10..minutes...Instance.0 %in% c("-2"),
                                                                   "Unable.to.walk",ts$Types.of.transport.used..excluding.work....Instance.0)
table(ts$Types.of.transport.used..excluding.work....Instance.0)
table(is.na(ts$Types.of.transport.used..excluding.work....Instance.0))
ts <- splitLevelsIntoColumns(ts,"Types.of.transport.used..excluding.work....Instance.0")
colnames(ts)
ts <- select(ts, -c("Types.of.transport.used..excluding.work....Instance.0_Unable.to.walk"))
colnames(ts)


table(ts$Usual.walking.pace...Instance.0)
table(is.na(ts$Usual.walking.pace...Instance.0))
ts$Usual.walking.pace...Instance.0 <- ifelse(ts_backup$Number.of.days.week.walked.10..minutes...Instance.0 %in% c("-2"),
                                             "Unable.to.walk",ts$Usual.walking.pace...Instance.0)
table(ts$Usual.walking.pace...Instance.0)
table(is.na(ts$Usual.walking.pace...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Usual.walking.pace...Instance.0")
colnames(ts)
ts <- select(ts, -c("Usual.walking.pace...Instance.0_Unable.to.walk"))
colnames(ts)


#met scores not considered


#electronic device use
table(ts_backup$Length.of.mobile.phone.use...Instance.0 %in% c("-3","0"))

table(ts$Weekly.usage.of.mobile.phone.in.last.3.months...Instance.0)
table(is.na(ts$Weekly.usage.of.mobile.phone.in.last.3.months...Instance.0))
ts$Weekly.usage.of.mobile.phone.in.last.3.months...Instance.0 <- ifelse(ts_backup$Length.of.mobile.phone.use...Instance.0 %in% c("-3","0"),
                                                                        "Never.used.mobile.phone",ts$Weekly.usage.of.mobile.phone.in.last.3.months...Instance.0)
table(ts$Weekly.usage.of.mobile.phone.in.last.3.months...Instance.0)
table(is.na(ts$Weekly.usage.of.mobile.phone.in.last.3.months...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Weekly.usage.of.mobile.phone.in.last.3.months...Instance.0")
colnames(ts)
ts <- select(ts, -c("Weekly.usage.of.mobile.phone.in.last.3.months...Instance.0_Never.used.mobile.phone"))
colnames(ts)


table(ts$Hands.free.device.speakerphone.use.with.mobile.phone.in.last.3.month...Instance.0)
table(is.na(ts$Hands.free.device.speakerphone.use.with.mobile.phone.in.last.3.month...Instance.0))
ts$Hands.free.device.speakerphone.use.with.mobile.phone.in.last.3.month...Instance.0 <- ifelse(ts_backup$Length.of.mobile.phone.use...Instance.0 %in% c("-3","0"),
                                                                                               "Never.used.mobile.phone",ts$Hands.free.device.speakerphone.use.with.mobile.phone.in.last.3.month...Instance.0)
table(ts$Hands.free.device.speakerphone.use.with.mobile.phone.in.last.3.month...Instance.0)
table(is.na(ts$Hands.free.device.speakerphone.use.with.mobile.phone.in.last.3.month...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Hands.free.device.speakerphone.use.with.mobile.phone.in.last.3.month...Instance.0")
colnames(ts)
ts <- select(ts, -c("Hands.free.device.speakerphone.use.with.mobile.phone.in.last.3.month...Instance.0_Never.used.mobile.phone"))
colnames(ts)


table(ts$Difference.in.mobile.phone.use.compared.to.two.years.previously...Instance.0)
table(is.na(ts$Difference.in.mobile.phone.use.compared.to.two.years.previously...Instance.0))
ts$Difference.in.mobile.phone.use.compared.to.two.years.previously...Instance.0 <- ifelse(ts_backup$Length.of.mobile.phone.use...Instance.0 %in% c("-3","0"),
                                                                                          "Never.used.mobile.phone",ts$Difference.in.mobile.phone.use.compared.to.two.years.previously...Instance.0)
table(ts$Difference.in.mobile.phone.use.compared.to.two.years.previously...Instance.0)
table(is.na(ts$Difference.in.mobile.phone.use.compared.to.two.years.previously...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Difference.in.mobile.phone.use.compared.to.two.years.previously...Instance.0")
colnames(ts)
ts <- select(ts, -c("Difference.in.mobile.phone.use.compared.to.two.years.previously...Instance.0_Never.used.mobile.phone"))
colnames(ts)


table(ts$Usual.side.of.head.for.mobile.phone.use...Instance.0)
table(is.na(ts$Usual.side.of.head.for.mobile.phone.use...Instance.0))
ts$Usual.side.of.head.for.mobile.phone.use...Instance.0 <- ifelse(ts_backup$Length.of.mobile.phone.use...Instance.0 %in% c("-3","0"),
                                                                  "Never.used.mobile.phone",ts$Usual.side.of.head.for.mobile.phone.use...Instance.0)
table(ts$Usual.side.of.head.for.mobile.phone.use...Instance.0)
table(is.na(ts$Usual.side.of.head.for.mobile.phone.use...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Usual.side.of.head.for.mobile.phone.use...Instance.0")
colnames(ts)
ts <- select(ts, -c("Usual.side.of.head.for.mobile.phone.use...Instance.0_Never.used.mobile.phone"))
colnames(ts)


table(ts$Length.of.mobile.phone.use...Instance.0)
table(is.na(ts$Length.of.mobile.phone.use...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Length.of.mobile.phone.use...Instance.0")
colnames(ts)


table(ts$Plays.computer.games...Instance.0)
table(is.na(ts$Plays.computer.games...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Plays.computer.games...Instance.0")
colnames(ts)


#sleep
table(ts$Sleep.duration...Instance.0)
table(is.na(ts$Sleep.duration...Instance.0))


table(ts$Getting.up.in.morning...Instance.0)
table(is.na(ts$Getting.up.in.morning...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Getting.up.in.morning...Instance.0")
colnames(ts)


table(ts$Morning.evening.person..chronotype....Instance.0)
table(is.na(ts$Morning.evening.person..chronotype....Instance.0))
ts <- splitLevelsIntoColumns(ts, "Morning.evening.person..chronotype....Instance.0")
colnames(ts)


table(ts$Nap.during.day...Instance.0)
table(is.na(ts$Nap.during.day...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Nap.during.day...Instance.0")
colnames(ts)


table(ts$Sleeplessness...insomnia...Instance.0)
table(is.na(ts$Sleeplessness...insomnia...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Sleeplessness...insomnia...Instance.0")
colnames(ts)


table(ts$Snoring...Instance.0)
table(is.na(ts$Snoring...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Snoring...Instance.0")
colnames(ts)


table(ts$Daytime.dozing...sleeping...Instance.0)
table(is.na(ts$Daytime.dozing...sleeping...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Daytime.dozing...sleeping...Instance.0")
colnames(ts)

#smoking
table(ts$Smoking.status...Instance.0)
table(is.na(ts$Smoking.status...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Smoking.status...Instance.0")
colnames(ts)


table(ts$Current.tobacco.smoking...Instance.0)
table(is.na(ts$Current.tobacco.smoking...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Current.tobacco.smoking...Instance.0")
colnames(ts)


table(ts$Past.tobacco.smoking...Instance.0)
table(is.na(ts$Past.tobacco.smoking...Instance.0))
ts$Past.tobacco.smoking...Instance.0 <- ifelse(ts_backup$Current.tobacco.smoking...Instance.0 %in% c("1"),
                                               "Current.smoker",ts$Past.tobacco.smoking...Instance.0)
table(ts$Past.tobacco.smoking...Instance.0)
table(is.na(ts$Past.tobacco.smoking...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Past.tobacco.smoking...Instance.0")
colnames(ts)
ts <- select(ts, -c("Past.tobacco.smoking...Instance.0_Current.smoker"))
colnames(ts)


table(ts$Light.smokers..at.least.100.smokes.in.lifetime...Instance.0)
table(is.na(ts$Light.smokers..at.least.100.smokes.in.lifetime...Instance.0))
ts$Light.smokers..at.least.100.smokes.in.lifetime...Instance.0 <- ifelse(ts_backup$Past.tobacco.smoking...Instance.0 %in% c("1","4"),
                                                                         "Previous.heavy.smoker.or.never.smoked",ts$Light.smokers..at.least.100.smokes.in.lifetime...Instance.0)
table(ts$Light.smokers..at.least.100.smokes.in.lifetime...Instance.0)
table(is.na(ts$Light.smokers..at.least.100.smokes.in.lifetime...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Light.smokers..at.least.100.smokes.in.lifetime...Instance.0")
colnames(ts)
ts <- select(ts, -c("Light.smokers..at.least.100.smokes.in.lifetime...Instance.0_Previous.heavy.smoker.or.never.smoked"))
colnames(ts)


table(ts$Age.started.smoking.in.current.smokers...Instance.0)
table(is.na(ts$Age.started.smoking.in.current.smokers...Instance.0))
ts$Age.started.smoking.in.current.smokers...Instance.0 <- ifelse(ts_backup$Current.tobacco.smoking...Instance.0 %in% c("0","2"),
                                                                 "Light.smoker.or.never.smoked",ts$Age.started.smoking.in.current.smokers...Instance.0)
table(ts$Age.started.smoking.in.current.smokers...Instance.0)
table(is.na(ts$Age.started.smoking.in.current.smokers...Instance.0))
ts <- splitMiscLevels(ts, "Age.started.smoking.in.current.smokers...Instance.0", c("Light.smoker.or.never.smoked"))
colnames(ts)
ts <- select(ts, -c("Age.started.smoking.in.current.smokers...Instance.0_Light.smoker.or.never.smoked"))
colnames(ts)


table(ts$Type.of.tobacco.currently.smoked...Instance.0)
table(is.na(ts$Type.of.tobacco.currently.smoked...Instance.0))
ts$Type.of.tobacco.currently.smoked...Instance.0 <- ifelse(ts_backup$Current.tobacco.smoking...Instance.0 %in% c("0","2"),
                                                           "Light.smoker.or.never.smoked",ts$Type.of.tobacco.currently.smoked...Instance.0)
table(ts$Type.of.tobacco.currently.smoked...Instance.0)
table(is.na(ts$Type.of.tobacco.currently.smoked...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Type.of.tobacco.currently.smoked...Instance.0")
colnames(ts)
ts <- select(ts, -c("Type.of.tobacco.currently.smoked...Instance.0_Light.smoker.or.never.smoked"))
colnames(ts)


table(ts$Previously.smoked.cigarettes.on.most.all.days...Instance.0)
table(is.na(ts$Previously.smoked.cigarettes.on.most.all.days...Instance.0))
ts$Previously.smoked.cigarettes.on.most.all.days...Instance.0 <- ifelse(ts_backup$Current.tobacco.smoking...Instance.0 %in% c("0","2"),
                                                                        "Light.smoker.or.never.smoked",ts$Previously.smoked.cigarettes.on.most.all.days...Instance.0)
table((ts$Previously.smoked.cigarettes.on.most.all.days...Instance.0))
ts$Previously.smoked.cigarettes.on.most.all.days...Instance.0 <- ifelse(ts_backup$Type.of.tobacco.currently.smoked...Instance.0 %in% c("-7","1","2"),
                                                                        "Current.cigarette.or.other",ts$Previously.smoked.cigarettes.on.most.all.days...Instance.0)
table(ts$Previously.smoked.cigarettes.on.most.all.days...Instance.0)
table(is.na(ts$Previously.smoked.cigarettes.on.most.all.days...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Previously.smoked.cigarettes.on.most.all.days...Instance.0")
colnames(ts)
ts <- select(ts, -c("Previously.smoked.cigarettes.on.most.all.days...Instance.0_Light.smoker.or.never.smoked",                   
                    "Previously.smoked.cigarettes.on.most.all.days...Instance.0_Current.cigarette.or.other"))
colnames(ts)


table(ts$Number.of.cigarettes.currently.smoked.daily..current.cigarette.smokers....Instance.0)
table(is.na(ts$Number.of.cigarettes.currently.smoked.daily..current.cigarette.smokers....Instance.0))
ts$Number.of.cigarettes.currently.smoked.daily..current.cigarette.smokers....Instance.0 <- ifelse(ts_backup$Current.tobacco.smoking...Instance.0 %in% c("0","2"),
                                                                                                  "Light.smoker.or.never.smoked",ts$Number.of.cigarettes.currently.smoked.daily..current.cigarette.smokers....Instance.0)
table(is.na(ts$Number.of.cigarettes.currently.smoked.daily..current.cigarette.smokers....Instance.0))
ts$Number.of.cigarettes.currently.smoked.daily..current.cigarette.smokers....Instance.0 <- ifelse(ts_backup$Type.of.tobacco.currently.smoked...Instance.0 %in% c("-7","3"),
                                                                                                  "No.current.cigarette",ts$Number.of.cigarettes.currently.smoked.daily..current.cigarette.smokers....Instance.0)
table(ts$Number.of.cigarettes.currently.smoked.daily..current.cigarette.smokers....Instance.0)
table(is.na(ts$Number.of.cigarettes.currently.smoked.daily..current.cigarette.smokers....Instance.0))
ts <- splitMiscLevels(ts, "Number.of.cigarettes.currently.smoked.daily..current.cigarette.smokers....Instance.0", 
                      c("Light.smoker.or.never.smoked", "No.current.cigarette"))
colnames(ts)
ts <- select(ts, -c("Number.of.cigarettes.currently.smoked.daily..current.cigarette.smokers....Instance.0_Light.smoker.or.never.smoked",
                    "Number.of.cigarettes.currently.smoked.daily..current.cigarette.smokers....Instance.0_No.current.cigarette"))
colnames(ts)


table(ts$Age.stopped.smoking.cigarettes..current.cigar.pipe.or.previous.cigarette.smoker....Instance.0)
table(is.na(ts$Age.stopped.smoking.cigarettes..current.cigar.pipe.or.previous.cigarette.smoker....Instance.0))
ts$Age.stopped.smoking.cigarettes..current.cigar.pipe.or.previous.cigarette.smoker....Instance.0 <- ifelse(ts_backup$Current.tobacco.smoking...Instance.0 %in% c("0","2"),
                                                                                                           "Light.smoker.or.never.smoked",ts$Age.stopped.smoking.cigarettes..current.cigar.pipe.or.previous.cigarette.smoker....Instance.0)
table(is.na(ts$Age.stopped.smoking.cigarettes..current.cigar.pipe.or.previous.cigarette.smoker....Instance.0))
ts$Age.stopped.smoking.cigarettes..current.cigar.pipe.or.previous.cigarette.smoker....Instance.0 <- ifelse(ts_backup$Type.of.tobacco.currently.smoked...Instance.0 %in% c("-7","1","2"),
                                                                                                           "Current.cigarette.or.other",ts$Age.stopped.smoking.cigarettes..current.cigar.pipe.or.previous.cigarette.smoker....Instance.0)
table(is.na(ts$Age.stopped.smoking.cigarettes..current.cigar.pipe.or.previous.cigarette.smoker....Instance.0))
ts$Age.stopped.smoking.cigarettes..current.cigar.pipe.or.previous.cigarette.smoker....Instance.0 <- ifelse(ts_backup$Previously.smoked.cigarettes.on.most.all.days...Instance.0 %in% c("0"),
                                                                                                           "Not.previous.cigarette.smoker",ts$Age.stopped.smoking.cigarettes..current.cigar.pipe.or.previous.cigarette.smoker....Instance.0)
table(ts$Age.stopped.smoking.cigarettes..current.cigar.pipe.or.previous.cigarette.smoker....Instance.0)
table(is.na(ts$Age.stopped.smoking.cigarettes..current.cigar.pipe.or.previous.cigarette.smoker....Instance.0))
ts <- splitMiscLevels(ts, "Age.stopped.smoking.cigarettes..current.cigar.pipe.or.previous.cigarette.smoker....Instance.0", 
                      c("Light.smoker.or.never.smoked", "Current.cigarette.or.other", "Not.previous.cigarette.smoker"))
colnames(ts)
ts <- select(ts, -c("Age.stopped.smoking.cigarettes..current.cigar.pipe.or.previous.cigarette.smoker....Instance.0_Light.smoker.or.never.smoked", 
                    "Age.stopped.smoking.cigarettes..current.cigar.pipe.or.previous.cigarette.smoker....Instance.0_Current.cigarette.or.other",  
                    "Age.stopped.smoking.cigarettes..current.cigar.pipe.or.previous.cigarette.smoker....Instance.0_Not.previous.cigarette.smoker"))
colnames(ts)


table(ts$Number.of.cigarettes.previously.smoked.daily..current.cigar.pipe.smokers....Instance.0)
table(is.na(ts$Number.of.cigarettes.previously.smoked.daily..current.cigar.pipe.smokers....Instance.0))
ts$Number.of.cigarettes.previously.smoked.daily..current.cigar.pipe.smokers....Instance.0 <- ifelse(ts_backup$Current.tobacco.smoking...Instance.0 %in% c("0","2"),
                                                                                                    "Light.smoker.or.never.smoked",ts$Number.of.cigarettes.previously.smoked.daily..current.cigar.pipe.smokers....Instance.0)
table(is.na(ts$Number.of.cigarettes.previously.smoked.daily..current.cigar.pipe.smokers....Instance.0))
ts$Number.of.cigarettes.previously.smoked.daily..current.cigar.pipe.smokers....Instance.0 <- ifelse(ts_backup$Type.of.tobacco.currently.smoked...Instance.0 %in% c("-7","1","2"),
                                                                                                    "Current.cigarette.or.other",ts$Number.of.cigarettes.previously.smoked.daily..current.cigar.pipe.smokers....Instance.0)
table(is.na(ts$Number.of.cigarettes.previously.smoked.daily..current.cigar.pipe.smokers....Instance.0))
ts$Number.of.cigarettes.previously.smoked.daily..current.cigar.pipe.smokers....Instance.0 <- ifelse(ts_backup$Previously.smoked.cigarettes.on.most.all.days...Instance.0 %in% c("0"),
                                                                                                    "Not.previous.cigarette.smoker",ts$Number.of.cigarettes.previously.smoked.daily..current.cigar.pipe.smokers....Instance.0)
table(ts$Number.of.cigarettes.previously.smoked.daily..current.cigar.pipe.smokers....Instance.0)
table(is.na(ts$Number.of.cigarettes.previously.smoked.daily..current.cigar.pipe.smokers....Instance.0))
ts <- splitMiscLevels(ts, "Number.of.cigarettes.previously.smoked.daily..current.cigar.pipe.smokers....Instance.0", 
                      c("Light.smoker.or.never.smoked", "Current.cigarette.or.other", "Not.previous.cigarette.smoker"))
colnames(ts)
ts <- select(ts, -c("Number.of.cigarettes.previously.smoked.daily..current.cigar.pipe.smokers....Instance.0_Light.smoker.or.never.smoked", 
                    "Number.of.cigarettes.previously.smoked.daily..current.cigar.pipe.smokers....Instance.0_Current.cigarette.or.other",   
                    "Number.of.cigarettes.previously.smoked.daily..current.cigar.pipe.smokers....Instance.0_Not.previous.cigarette.smoker"))
colnames(ts)


table(ts$Time.from.waking.to.first.cigarette...Instance.0)
table(is.na(ts$Time.from.waking.to.first.cigarette...Instance.0))
ts$Time.from.waking.to.first.cigarette...Instance.0 <- ifelse(ts_backup$Current.tobacco.smoking...Instance.0 %in% c("0","2"),
                                                              "Light.smoker.or.never.smoked",ts$Time.from.waking.to.first.cigarette...Instance.0)
table(is.na(ts$Time.from.waking.to.first.cigarette...Instance.0))
ts$Time.from.waking.to.first.cigarette...Instance.0 <- ifelse(ts_backup$Type.of.tobacco.currently.smoked...Instance.0 %in% c("-7","3"),
                                                              "No.current.cigarette",ts$Time.from.waking.to.first.cigarette...Instance.0)
table(ts$Time.from.waking.to.first.cigarette...Instance.0)
table(is.na(ts$Time.from.waking.to.first.cigarette...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Time.from.waking.to.first.cigarette...Instance.0")
colnames(ts)
ts <- select(ts, -c("Time.from.waking.to.first.cigarette...Instance.0_Light.smoker.or.never.smoked",
                    "Time.from.waking.to.first.cigarette...Instance.0_No.current.cigarette"))
colnames(ts)



table(ts$Difficulty.not.smoking.for.1.day...Instance.0)
table(is.na(ts$Difficulty.not.smoking.for.1.day...Instance.0))
ts$Difficulty.not.smoking.for.1.day...Instance.0 <- ifelse(ts_backup$Current.tobacco.smoking...Instance.0 %in% c("0","2"),
                                                           "Light.smoker.or.never.smoked",ts$Difficulty.not.smoking.for.1.day...Instance.0)
table(is.na(ts$Difficulty.not.smoking.for.1.day...Instance.0))
ts$Difficulty.not.smoking.for.1.day...Instance.0 <- ifelse(ts_backup$Type.of.tobacco.currently.smoked...Instance.0 %in% c("-7","3"),
                                                           "No.current.cigarette",ts$Difficulty.not.smoking.for.1.day...Instance.0)
table(ts$Difficulty.not.smoking.for.1.day...Instance.0)
table(is.na(ts$Difficulty.not.smoking.for.1.day...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Difficulty.not.smoking.for.1.day...Instance.0")
colnames(ts)
ts <- select(ts, -c("Difficulty.not.smoking.for.1.day...Instance.0_Light.smoker.or.never.smoked",
                    "Difficulty.not.smoking.for.1.day...Instance.0_No.current.cigarette"))
colnames(ts)


table(ts$Ever.tried.to.stop.smoking...Instance.0)
table(is.na(ts$Ever.tried.to.stop.smoking...Instance.0))
ts$Ever.tried.to.stop.smoking...Instance.0 <- ifelse(ts_backup$Current.tobacco.smoking...Instance.0 %in% c("0","2"),
                                                     "Light.smoker.or.never.smoked",ts$Ever.tried.to.stop.smoking...Instance.0)
table(ts$Ever.tried.to.stop.smoking...Instance.0)
table(is.na(ts$Ever.tried.to.stop.smoking...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Ever.tried.to.stop.smoking...Instance.0")
colnames(ts)
ts <- select(ts, -c("Ever.tried.to.stop.smoking...Instance.0_Light.smoker.or.never.smoked"))
colnames(ts)


table(ts$Wants.to.stop.smoking...Instance.0)
table(is.na(ts$Wants.to.stop.smoking...Instance.0))
ts$Wants.to.stop.smoking...Instance.0 <- ifelse(ts_backup$Current.tobacco.smoking...Instance.0 %in% c("0","2"),
                                                "Light.smoker.or.never.smoked",ts$Wants.to.stop.smoking...Instance.0)
table(ts$Wants.to.stop.smoking...Instance.0)
table(is.na(ts$Wants.to.stop.smoking...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Wants.to.stop.smoking...Instance.0")
colnames(ts)
ts <- select(ts, -c("Wants.to.stop.smoking...Instance.0_Light.smoker.or.never.smoked"))
colnames(ts)


table(ts$Smoking.compared.to.10.years.previous...Instance.0)
table(is.na(ts$Smoking.compared.to.10.years.previous...Instance.0))
ts$Smoking.compared.to.10.years.previous...Instance.0 <- ifelse(ts_backup$Current.tobacco.smoking...Instance.0 %in% c("0","2"),
                                                                "Light.smoker.or.never.smoked",ts$Smoking.compared.to.10.years.previous...Instance.0)
table(ts$Smoking.compared.to.10.years.previous...Instance.0)
table(is.na(ts$Smoking.compared.to.10.years.previous...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Smoking.compared.to.10.years.previous...Instance.0")
colnames(ts)
ts <- select(ts, -c("Smoking.compared.to.10.years.previous...Instance.0_Light.smoker.or.never.smoked"))
colnames(ts)


table(ts$Why.reduced.smoking...Instance.0)
table(is.na(ts$Why.reduced.smoking...Instance.0))
ts$Why.reduced.smoking...Instance.0 <- ifelse(ts_backup$Current.tobacco.smoking...Instance.0 %in% c("0","2"),
                                              "Light.smoker.or.never.smoked",ts$Why.reduced.smoking...Instance.0)
table(is.na(ts$Why.reduced.smoking...Instance.0))
ts$Why.reduced.smoking...Instance.0 <- ifelse(ts_backup$Smoking.compared.to.10.years.previous...Instance.0 %in% c("1","2"),
                                              "Not.reduced.10.years",ts$Why.reduced.smoking...Instance.0)
table(ts$Why.reduced.smoking...Instance.0)
table(is.na(ts$Why.reduced.smoking...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Why.reduced.smoking...Instance.0")
colnames(ts)
ts <- select(ts, -c("Why.reduced.smoking...Instance.0_Light.smoker.or.never.smoked",                                             
                    "Why.reduced.smoking...Instance.0_Not.reduced.10.years"))
colnames(ts)


table(ts$Age.started.smoking.in.former.smokers...Instance.0)
table(is.na(ts$Age.started.smoking.in.former.smokers...Instance.0))
ts$Age.started.smoking.in.former.smokers...Instance.0 <- ifelse(ts_backup$Past.tobacco.smoking...Instance.0 %in% c("2","3","4"),
                                                                "Previous.light.smoker.or.never.smoked",ts$Age.started.smoking.in.former.smokers...Instance.0)
table(ts$Age.started.smoking.in.former.smokers...Instance.0)
table(is.na(ts$Age.started.smoking.in.former.smokers...Instance.0))
ts <- splitMiscLevels(ts, "Age.started.smoking.in.former.smokers...Instance.0", c("Previous.light.smoker.or.never.smoked"))
colnames(ts)
ts <- select(ts, -c("Age.started.smoking.in.former.smokers...Instance.0_Previous.light.smoker.or.never.smoked"))
colnames(ts)


table(ts$Type.of.tobacco.previously.smoked...Instance.0)
table(is.na(ts$Type.of.tobacco.previously.smoked...Instance.0))
ts$Type.of.tobacco.previously.smoked...Instance.0 <- ifelse(ts_backup$Past.tobacco.smoking...Instance.0 %in% c("2","3","4"),
                                                            "Previous.light.smoker.or.never.smoked",ts$Type.of.tobacco.previously.smoked...Instance.0)
table(ts$Type.of.tobacco.previously.smoked...Instance.0)
table(is.na(ts$Type.of.tobacco.previously.smoked...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Type.of.tobacco.previously.smoked...Instance.0")
colnames(ts)
ts <- select(ts, -c("Type.of.tobacco.previously.smoked...Instance.0_Previous.light.smoker.or.never.smoked"))
colnames(ts)


table(ts$Number.of.cigarettes.previously.smoked.daily...Instance.0)
table(is.na(ts$Number.of.cigarettes.previously.smoked.daily...Instance.0))
ts$Number.of.cigarettes.previously.smoked.daily...Instance.0 <- ifelse(ts_backup$Past.tobacco.smoking...Instance.0 %in% c("2","3","4"),
                                                                       "Previous.light.smoker.or.never.smoked",ts$Number.of.cigarettes.previously.smoked.daily...Instance.0)
table(is.na(ts$Number.of.cigarettes.previously.smoked.daily...Instance.0))
ts$Number.of.cigarettes.previously.smoked.daily...Instance.0 <- ifelse(ts_backup$Type.of.tobacco.previously.smoked...Instance.0 %in% c("-7","3"),
                                                                       "No.previous.cigarette",ts$Number.of.cigarettes.previously.smoked.daily...Instance.0)
table(ts$Number.of.cigarettes.previously.smoked.daily...Instance.0)
table(is.na(ts$Number.of.cigarettes.previously.smoked.daily...Instance.0))
ts <- splitMiscLevels(ts, "Number.of.cigarettes.previously.smoked.daily...Instance.0", 
                      c("Previous.light.smoker.or.never.smoked", "No.previous.cigarette"))
colnames(ts)
ts <- select(ts, -c("Number.of.cigarettes.previously.smoked.daily...Instance.0_Previous.light.smoker.or.never.smoked",           
                    "Number.of.cigarettes.previously.smoked.daily...Instance.0_No.previous.cigarette"))
colnames(ts)


table(ts$Age.stopped.smoking...Instance.0)
table(is.na(ts$Age.stopped.smoking...Instance.0))
ts$Age.stopped.smoking...Instance.0 <- ifelse(ts_backup$Past.tobacco.smoking...Instance.0 %in% c("2","3","4"),
                                              "Previous.light.smoker.or.never.smoked",ts$Age.stopped.smoking...Instance.0)
table(ts$Age.stopped.smoking...Instance.0)
table(is.na(ts$Age.stopped.smoking...Instance.0))
ts <- splitMiscLevels(ts, "Age.stopped.smoking...Instance.0", 
                      c("Previous.light.smoker.or.never.smoked"))
colnames(ts)
ts <- select(ts, -c("Age.stopped.smoking...Instance.0_Previous.light.smoker.or.never.smoked"))
colnames(ts)


table(ts$Ever.stopped.smoking.for.6..months...Instance.0)
table(is.na(ts$Ever.stopped.smoking.for.6..months...Instance.0))
ts$Ever.stopped.smoking.for.6..months...Instance.0 <- ifelse(ts_backup$Past.tobacco.smoking...Instance.0 %in% c("2","3","4"),
                                                             "Previous.light.smoker.or.never.smoked",ts$Ever.stopped.smoking.for.6..months...Instance.0)
table(ts$Ever.stopped.smoking.for.6..months...Instance.0)
table(is.na(ts$Ever.stopped.smoking.for.6..months...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Ever.stopped.smoking.for.6..months...Instance.0")
colnames(ts)
ts <- select(ts, -c("Ever.stopped.smoking.for.6..months...Instance.0_Previous.light.smoker.or.never.smoked"))
colnames(ts)


table(ts$Why.stopped.smoking...Instance.0)
table(is.na(ts$Why.stopped.smoking...Instance.0))
ts$Why.stopped.smoking...Instance.0 <- ifelse(ts_backup$Past.tobacco.smoking...Instance.0 %in% c("2","3","4"),
                                              "Previous.light.smoker.or.never.smoked",ts$Why.stopped.smoking...Instance.0)
table(is.na(ts$Why.stopped.smoking...Instance.0))
ts$Why.stopped.smoking...Instance.0 <- ifelse(ts_backup$Ever.stopped.smoking.for.6..months...Instance.0 %in% c("0"),
                                              "Not.stopped.6+.months",ts$Why.stopped.smoking...Instance.0)
table(ts$Why.stopped.smoking...Instance.0)
table(is.na(ts$Why.stopped.smoking...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Why.stopped.smoking...Instance.0")
colnames(ts)
ts <- select(ts, -c("Why.stopped.smoking...Instance.0_Previous.light.smoker.or.never.smoked",                                    
                    "Why.stopped.smoking...Instance.0_Not.stopped.6+.months"))
colnames(ts)


table(ts$Number.of.unsuccessful.stop.smoking.attempts...Instance.0)
table(is.na(ts$Number.of.unsuccessful.stop.smoking.attempts...Instance.0))
ts$Number.of.unsuccessful.stop.smoking.attempts...Instance.0 <- ifelse(ts_backup$Past.tobacco.smoking...Instance.0 %in% c("2","3","4"),
                                                                       "Previous.light.smoker.or.never.smoked",ts$Number.of.unsuccessful.stop.smoking.attempts...Instance.0)
table(is.na(ts$Number.of.unsuccessful.stop.smoking.attempts...Instance.0))
ts$Number.of.unsuccessful.stop.smoking.attempts...Instance.0 <- ifelse(ts_backup$Ever.stopped.smoking.for.6..months...Instance.0 %in% c("0"),
                                                                       "Not.stopped.6+.months",ts$Number.of.unsuccessful.stop.smoking.attempts...Instance.0)
table(ts$Number.of.unsuccessful.stop.smoking.attempts...Instance.0)
table(is.na(ts$Number.of.unsuccessful.stop.smoking.attempts...Instance.0))
ts <- splitMiscLevels(ts, "Number.of.unsuccessful.stop.smoking.attempts...Instance.0", 
                      c("Previous.light.smoker.or.never.smoked", "Not.stopped.6+.months"))
colnames(ts)
ts <- select(ts, -c("Number.of.unsuccessful.stop.smoking.attempts...Instance.0_Previous.light.smoker.or.never.smoked",
                    "Number.of.unsuccessful.stop.smoking.attempts...Instance.0_Not.stopped.6+.months"))
colnames(ts)


table(ts$Likelihood.of.resuming.smoking...Instance.0)
table(is.na(ts$Likelihood.of.resuming.smoking...Instance.0))
ts$Likelihood.of.resuming.smoking...Instance.0 <- ifelse(ts_backup$Past.tobacco.smoking...Instance.0 %in% c("2","3","4"),
                                                         "Previous.light.smoker.or.never.smoked",ts$Likelihood.of.resuming.smoking...Instance.0)
table(is.na(ts$Likelihood.of.resuming.smoking...Instance.0))
ts$Likelihood.of.resuming.smoking...Instance.0 <- ifelse(ts_backup$Ever.stopped.smoking.for.6..months...Instance.0 %in% c("0"),
                                                         "Not.stopped.6+.months",ts$Likelihood.of.resuming.smoking...Instance.0)
table(ts$Likelihood.of.resuming.smoking...Instance.0)
table(is.na(ts$Likelihood.of.resuming.smoking...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Likelihood.of.resuming.smoking...Instance.0")
colnames(ts)
ts <- select(ts, -c("Likelihood.of.resuming.smoking...Instance.0_Previous.light.smoker.or.never.smoked",                         
                    "Likelihood.of.resuming.smoking...Instance.0_Not.stopped.6+.months"))
colnames(ts)


table(ts$Smoking.smokers.in.household...Instance.0)
table(is.na(ts$Smoking.smokers.in.household...Instance.0))
ts$Smoking.smokers.in.household...Instance.0 <- ifelse(ts_backup$Current.tobacco.smoking...Instance.0 %in% c("1"),
                                                       "Heavy.smoker.or.never.smoked",ts$Smoking.smokers.in.household...Instance.0)
table(ts$Smoking.smokers.in.household...Instance.0)
table(is.na(ts$Smoking.smokers.in.household...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Smoking.smokers.in.household...Instance.0")
colnames(ts)
ts <- select(ts, -c("Smoking.smokers.in.household...Instance.0_Heavy.smoker.or.never.smoked"))
colnames(ts)


table(ts$Exposure.to.tobacco.smoke.at.home...Instance.0)
table(is.na(ts$Exposure.to.tobacco.smoke.at.home...Instance.0))
ts$Exposure.to.tobacco.smoke.at.home...Instance.0 <- ifelse(ts_backup$Current.tobacco.smoking...Instance.0 %in% c("1"),
                                                            "Heavy.smoker.or.never.smoked",ts$Exposure.to.tobacco.smoke.at.home...Instance.0)
table(ts$Exposure.to.tobacco.smoke.at.home...Instance.0)
table(is.na(ts$Exposure.to.tobacco.smoke.at.home...Instance.0))
ts <- splitMiscLevels(ts, "Exposure.to.tobacco.smoke.at.home...Instance.0", 
                      c("Heavy.smoker.or.never.smoked"))
colnames(ts)
ts <- select(ts, -c("Exposure.to.tobacco.smoke.at.home...Instance.0_Heavy.smoker.or.never.smoked"))
colnames(ts)


table(ts$Exposure.to.tobacco.smoke.outside.home...Instance.0)
table(is.na(ts$Exposure.to.tobacco.smoke.outside.home...Instance.0))
ts$Exposure.to.tobacco.smoke.outside.home...Instance.0 <- ifelse(ts_backup$Current.tobacco.smoking...Instance.0 %in% c("1"),
                                                                 "Heavy.smoker.or.never.smoked",ts$Exposure.to.tobacco.smoke.outside.home...Instance.0)
table(ts$Exposure.to.tobacco.smoke.outside.home...Instance.0)
table(is.na(ts$Exposure.to.tobacco.smoke.outside.home...Instance.0))
ts <- splitMiscLevels(ts, "Exposure.to.tobacco.smoke.outside.home...Instance.0", 
                      c("Heavy.smoker.or.never.smoked"))
colnames(ts)
ts <- select(ts, -c("Exposure.to.tobacco.smoke.outside.home...Instance.0_Heavy.smoker.or.never.smoked"))
colnames(ts)


#diet
table(ts$Cooked.vegetable.intake...Instance.0)
table(is.na(ts$Cooked.vegetable.intake...Instance.0))


table(ts$Salad...raw.vegetable.intake...Instance.0)
table(is.na(ts$Salad...raw.vegetable.intake...Instance.0))


table(ts$Fresh.fruit.intake...Instance.0)
table(is.na(ts$Fresh.fruit.intake...Instance.0))


table(ts$Dried.fruit.intake...Instance.0)
table(is.na(ts$Dried.fruit.intake...Instance.0))


table(ts$Oily.fish.intake...Instance.0)
table(is.na(ts$Oily.fish.intake...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Oily.fish.intake...Instance.0")
colnames(ts)


table(ts$Non.oily.fish.intake...Instance.0)
table(is.na(ts$Non.oily.fish.intake...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Non.oily.fish.intake...Instance.0")
colnames(ts)


table(ts$Processed.meat.intake...Instance.0)
table(is.na(ts$Processed.meat.intake...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Processed.meat.intake...Instance.0")
colnames(ts)


table(ts$Poultry.intake...Instance.0)
table(is.na(ts$Poultry.intake...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Poultry.intake...Instance.0")
colnames(ts)


table(ts$Beef.intake...Instance.0)
table(is.na(ts$Beef.intake...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Beef.intake...Instance.0")
colnames(ts)


table(ts$Lamb.mutton.intake...Instance.0)
table(is.na(ts$Lamb.mutton.intake...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Lamb.mutton.intake...Instance.0")
colnames(ts)


table(ts$Pork.intake...Instance.0)
table(is.na(ts$Pork.intake...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Pork.intake...Instance.0")
colnames(ts)


table(ts$Age.when.last.ate.meat...Instance.0)
table(is.na(ts$Age.when.last.ate.meat...Instance.0))


table(ts$Never.eat.eggs..dairy..wheat..sugar...Instance.0)
table(is.na(ts$Never.eat.eggs..dairy..wheat..sugar...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Never.eat.eggs..dairy..wheat..sugar...Instance.0")
colnames(ts)


table(ts$Cheese.intake...Instance.0)
table(is.na(ts$Cheese.intake...Instance.0))
ts$Cheese.intake...Instance.0 <- ifelse(ts$Never.eat.eggs..dairy..wheat..sugar...Instance.0_2 %in% c("TRUE"),
                                        "Never.eat.dairy",ts$Cheese.intake...Instance.0)
table(ts$Cheese.intake...Instance.0)
table(is.na(ts$Cheese.intake...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Cheese.intake...Instance.0")
colnames(ts)
ts <- select(ts, -c("Cheese.intake...Instance.0_Never.eat.dairy"))
colnames(ts)


table(ts$Milk.type.used...Instance.0)
table(is.na(ts$Milk.type.used...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Milk.type.used...Instance.0")
colnames(ts)


table(ts$Spread.type...Instance.0)
table(is.na(ts$Spread.type...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Spread.type...Instance.0")
colnames(ts)


table(ts$Non.butter.spread.type.details...Instance.0)
table(is.na(ts$Non.butter.spread.type.details...Instance.0))
ts$Non.butter.spread.type.details...Instance.0 <- ifelse(ts_backup$Spread.type...Instance.0 %in% c("0","1","2"),
                                                         "No.spread.or.spread.specified",ts$Non.butter.spread.type.details...Instance.0)
table(ts$Non.butter.spread.type.details...Instance.0)
table(is.na(ts$Non.butter.spread.type.details...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Non.butter.spread.type.details...Instance.0")
colnames(ts)
ts <- select(ts, -c("Non.butter.spread.type.details...Instance.0_No.spread.or.spread.specified"))
colnames(ts)


table(ts$Bread.intake...Instance.0)
table(is.na(ts$Bread.intake...Instance.0))


table(ts$Bread.type...Instance.0)
table(is.na(ts$Bread.type...Instance.0))
ts$Bread.type...Instance.0 <- ifelse(ts$Bread.intake...Instance.0 %in% c("0","0.5"),
                                     "Does.not.eat.bread",ts$Bread.type...Instance.0)
table(ts$Bread.type...Instance.0)
table(is.na(ts$Bread.type...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Bread.type...Instance.0")
colnames(ts)
ts <- select(ts, -c("Bread.type...Instance.0_Does.not.eat.bread"))
colnames(ts)


table(ts$Cereal.intake...Instance.0)
table(is.na(ts$Cereal.intake...Instance.0))


table(ts$Cereal.type...Instance.0)
table(is.na(ts$Cereal.type...Instance.0))
ts$Cereal.type...Instance.0 <- ifelse(ts$Cereal.intake...Instance.0 %in% c("0","0.5"),
                                      "Does.not.eat.cereal",ts$Cereal.type...Instance.0)
table(ts$Cereal.type...Instance.0)
table(is.na(ts$Cereal.type...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Cereal.type...Instance.0")
colnames(ts)
ts <- select(ts, -c("Cereal.type...Instance.0_Does.not.eat.cereal"))
colnames(ts)


table(ts$Salt.added.to.food...Instance.0)
table(is.na(ts$Salt.added.to.food...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Salt.added.to.food...Instance.0")
colnames(ts)


table(ts$Tea.intake...Instance.0)
table(is.na(ts$Tea.intake...Instance.0))


table(ts$Coffee.intake...Instance.0)
table(is.na(ts$Coffee.intake...Instance.0))


table(ts$Coffee.type...Instance.0)
table(is.na(ts$Coffee.type...Instance.0))
ts$Coffee.type...Instance.0 <- ifelse(ts$Coffee.intake...Instance.0 %in% c("0"),
                                      "Does.not.drink.coffee",ts$Coffee.type...Instance.0)
table(ts$Coffee.type...Instance.0)
table(is.na(ts$Coffee.type...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Coffee.type...Instance.0")
colnames(ts)
ts <- select(ts, -c("Coffee.type...Instance.0_Does.not.drink.coffee"))
colnames(ts)


table(ts$Hot.drink.temperature...Instance.0)
table(is.na(ts$Hot.drink.temperature...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Hot.drink.temperature...Instance.0")
colnames(ts)


table(ts$Water.intake...Instance.0)
table(is.na(ts$Water.intake...Instance.0))


table(ts$Major.dietary.changes.in.the.last.5.years...Instance.0)
table(is.na(ts$Major.dietary.changes.in.the.last.5.years...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Major.dietary.changes.in.the.last.5.years...Instance.0")
colnames(ts)


table(ts$Variation.in.diet...Instance.0)
table(is.na(ts$Variation.in.diet...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Variation.in.diet...Instance.0")
colnames(ts)


#alcohol
table(ts$Alcohol.drinker.status...Instance.0)
table(is.na(ts$Alcohol.drinker.status...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Alcohol.drinker.status...Instance.0")
colnames(ts)


table(ts$Alcohol.intake.frequency....Instance.0)
table(is.na(ts$Alcohol.intake.frequency....Instance.0))
ts <- splitLevelsIntoColumns(ts,"Alcohol.intake.frequency....Instance.0")
colnames(ts)


table(ts$Former.alcohol.drinker...Instance.0)
table(is.na(ts$Former.alcohol.drinker...Instance.0))
ts$Former.alcohol.drinker...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("1","2","3","4","5"),
                                                 "Drinks.alcohol",ts$Former.alcohol.drinker...Instance.0)
table(ts$Former.alcohol.drinker...Instance.0)
table(is.na(ts$Former.alcohol.drinker...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Former.alcohol.drinker...Instance.0")
colnames(ts)
ts <- select(ts, -c("Former.alcohol.drinker...Instance.0_Drinks.alcohol"))
colnames(ts)


table(ts$Average.monthly.red.wine.intake...Instance.0)
table(is.na(ts$Average.monthly.red.wine.intake...Instance.0))
ts$Average.monthly.red.wine.intake...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("1","2","3","6"),
                                                          "Weekly.drinker.or.never.drinks",ts$Average.monthly.red.wine.intake...Instance.0)
table(ts$Average.monthly.red.wine.intake...Instance.0)
table(is.na(ts$Average.monthly.red.wine.intake...Instance.0))
ts <- splitMiscLevels(ts, "Average.monthly.red.wine.intake...Instance.0", 
                      c("Weekly.drinker.or.never.drinks"))
colnames(ts)
ts <- select(ts, -c("Average.monthly.red.wine.intake...Instance.0_Weekly.drinker.or.never.drinks"))
colnames(ts)


table(ts$Average.monthly.champagne.plus.white.wine.intake...Instance.0)
table(is.na(ts$Average.monthly.champagne.plus.white.wine.intake...Instance.0))
ts$Average.monthly.champagne.plus.white.wine.intake...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("1","2","3","6"),
                                                                           "Weekly.drinker.or.never.drinks",ts$Average.monthly.champagne.plus.white.wine.intake...Instance.0)
table(ts$Average.monthly.champagne.plus.white.wine.intake...Instance.0)
table(is.na(ts$Average.monthly.champagne.plus.white.wine.intake...Instance.0))
ts <- splitMiscLevels(ts, "Average.monthly.champagne.plus.white.wine.intake...Instance.0", 
                      c("Weekly.drinker.or.never.drinks"))
colnames(ts)
ts <- select(ts, -c("Average.monthly.champagne.plus.white.wine.intake...Instance.0_Weekly.drinker.or.never.drinks"))
colnames(ts)


table(ts$Average.monthly.beer.plus.cider.intake...Instance.0)
table(is.na(ts$Average.monthly.beer.plus.cider.intake...Instance.0))
ts$Average.monthly.beer.plus.cider.intake...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("1","2","3","6"),
                                                                 "Weekly.drinker.or.never.drinks",ts$Average.monthly.beer.plus.cider.intake...Instance.0)
table(ts$Average.monthly.beer.plus.cider.intake...Instance.0)
table(is.na(ts$Average.monthly.beer.plus.cider.intake...Instance.0))
ts <- splitMiscLevels(ts, "Average.monthly.beer.plus.cider.intake...Instance.0", 
                      c("Weekly.drinker.or.never.drinks"))
colnames(ts)
ts <- select(ts, -c("Average.monthly.beer.plus.cider.intake...Instance.0_Weekly.drinker.or.never.drinks"))
colnames(ts)


table(ts$Average.monthly.spirits.intake...Instance.0)
table(is.na(ts$Average.monthly.spirits.intake...Instance.0))
ts$Average.monthly.spirits.intake...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("1","2","3","6"),
                                                         "Weekly.drinker.or.never.drinks",ts$Average.monthly.spirits.intake...Instance.0)
table(ts$Average.monthly.spirits.intake...Instance.0)
table(is.na(ts$Average.monthly.spirits.intake...Instance.0))
ts <- splitMiscLevels(ts, "Average.monthly.spirits.intake...Instance.0", 
                      c("Weekly.drinker.or.never.drinks"))
colnames(ts)
ts <- select(ts, -c("Average.monthly.spirits.intake...Instance.0_Weekly.drinker.or.never.drinks"))
colnames(ts)


table(ts$Average.monthly.fortified.wine.intake...Instance.0)
table(is.na(ts$Average.monthly.fortified.wine.intake...Instance.0))
ts$Average.monthly.fortified.wine.intake...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("1","2","3","6"),
                                                                "Weekly.drinker.or.never.drinks",ts$Average.monthly.fortified.wine.intake...Instance.0)
table(ts$Average.monthly.fortified.wine.intake...Instance.0)
table(is.na(ts$Average.monthly.fortified.wine.intake...Instance.0))
ts <- splitMiscLevels(ts, "Average.monthly.fortified.wine.intake...Instance.0", 
                      c("Weekly.drinker.or.never.drinks"))
colnames(ts)
ts <- select(ts, -c("Average.monthly.fortified.wine.intake...Instance.0_Weekly.drinker.or.never.drinks"))
colnames(ts)


table(ts$Average.monthly.intake.of.other.alcoholic.drinks...Instance.0)
table(is.na(ts$Average.monthly.intake.of.other.alcoholic.drinks...Instance.0))
ts$Average.monthly.intake.of.other.alcoholic.drinks...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("1","2","3","6"),
                                                                           "Weekly.drinker.or.never.drinks",ts$Average.monthly.intake.of.other.alcoholic.drinks...Instance.0)
table(ts$Average.monthly.intake.of.other.alcoholic.drinks...Instance.0)
table(is.na(ts$Average.monthly.intake.of.other.alcoholic.drinks...Instance.0))
ts <- splitMiscLevels(ts, "Average.monthly.intake.of.other.alcoholic.drinks...Instance.0", 
                      c("Weekly.drinker.or.never.drinks"))
colnames(ts)
ts <- select(ts, -c("Average.monthly.intake.of.other.alcoholic.drinks...Instance.0_Weekly.drinker.or.never.drinks"))
colnames(ts)


table(ts$Average.weekly.red.wine.intake...Instance.0)
table(is.na(ts$Average.weekly.red.wine.intake...Instance.0))
ts$Average.weekly.red.wine.intake...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("4","5","6"),
                                                         "Monthly.drinker.or.never.drinks",ts$Average.weekly.red.wine.intake...Instance.0)
table(ts$Average.weekly.red.wine.intake...Instance.0)
table(is.na(ts$Average.weekly.red.wine.intake...Instance.0))
ts <- splitMiscLevels(ts, "Average.weekly.red.wine.intake...Instance.0", 
                      c("Monthly.drinker.or.never.drinks"))
colnames(ts)
ts <- select(ts, -c("Average.weekly.red.wine.intake...Instance.0_Monthly.drinker.or.never.drinks"))
colnames(ts)


table(ts$Average.weekly.champagne.plus.white.wine.intake...Instance.0)
table(is.na(ts$Average.weekly.champagne.plus.white.wine.intake...Instance.0))
ts$Average.weekly.champagne.plus.white.wine.intake...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("4","5","6"),
                                                                          "Monthly.drinker.or.never.drinks",ts$Average.weekly.champagne.plus.white.wine.intake...Instance.0)
table(ts$Average.weekly.champagne.plus.white.wine.intake...Instance.0)
table(is.na(ts$Average.weekly.champagne.plus.white.wine.intake...Instance.0))
ts <- splitMiscLevels(ts, "Average.weekly.champagne.plus.white.wine.intake...Instance.0", 
                      c("Monthly.drinker.or.never.drinks"))
colnames(ts)
ts <- select(ts, -c("Average.weekly.champagne.plus.white.wine.intake...Instance.0_Monthly.drinker.or.never.drinks"))
colnames(ts)


table(ts$Average.weekly.beer.plus.cider.intake...Instance.0)
table(is.na(ts$Average.weekly.beer.plus.cider.intake...Instance.0))
ts$Average.weekly.beer.plus.cider.intake...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("4","5","6"),
                                                                "Monthly.drinker.or.never.drinks",ts$Average.weekly.beer.plus.cider.intake...Instance.0)
table(ts$Average.weekly.beer.plus.cider.intake...Instance.0)
table(is.na(ts$Average.weekly.beer.plus.cider.intake...Instance.0))
ts <- splitMiscLevels(ts, "Average.weekly.beer.plus.cider.intake...Instance.0", 
                      c("Monthly.drinker.or.never.drinks"))
colnames(ts)
ts <- select(ts, -c("Average.weekly.beer.plus.cider.intake...Instance.0_Monthly.drinker.or.never.drinks"))
colnames(ts)


table(ts$Average.weekly.spirits.intake...Instance.0)
table(is.na(ts$Average.weekly.spirits.intake...Instance.0))
ts$Average.weekly.spirits.intake...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("4","5","6"),
                                                        "Monthly.drinker.or.never.drinks",ts$Average.weekly.spirits.intake...Instance.0)
table(ts$Average.weekly.spirits.intake...Instance.0)
table(is.na(ts$Average.weekly.spirits.intake...Instance.0))
ts <- splitMiscLevels(ts, "Average.weekly.spirits.intake...Instance.0", 
                      c("Monthly.drinker.or.never.drinks"))
colnames(ts)
ts <- select(ts, -c("Average.weekly.spirits.intake...Instance.0_Monthly.drinker.or.never.drinks"))
colnames(ts)


table(ts$Average.weekly.fortified.wine.intake...Instance.0)
table(is.na(ts$Average.weekly.fortified.wine.intake...Instance.0))
ts$Average.weekly.fortified.wine.intake...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("4","5","6"),
                                                               "Monthly.drinker.or.never.drinks",ts$Average.weekly.fortified.wine.intake...Instance.0)
table(ts$Average.weekly.fortified.wine.intake...Instance.0)
table(is.na(ts$Average.weekly.fortified.wine.intake...Instance.0))
ts <- splitMiscLevels(ts, "Average.weekly.fortified.wine.intake...Instance.0", 
                      c("Monthly.drinker.or.never.drinks"))
colnames(ts)
ts <- select(ts, -c("Average.weekly.fortified.wine.intake...Instance.0_Monthly.drinker.or.never.drinks"))
colnames(ts)


table(ts$Average.weekly.intake.of.other.alcoholic.drinks...Instance.0)
table(is.na(ts$Average.weekly.intake.of.other.alcoholic.drinks...Instance.0))
ts$Average.weekly.intake.of.other.alcoholic.drinks...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("4","5","6"),
                                                                          "Monthly.drinker.or.never.drinks",ts$Average.weekly.intake.of.other.alcoholic.drinks...Instance.0)
table(ts$Average.weekly.intake.of.other.alcoholic.drinks...Instance.0)
table(is.na(ts$Average.weekly.intake.of.other.alcoholic.drinks...Instance.0))
ts <- splitMiscLevels(ts, "Average.weekly.intake.of.other.alcoholic.drinks...Instance.0", 
                      c("Monthly.drinker.or.never.drinks"))
colnames(ts)
ts <- select(ts, -c("Average.weekly.intake.of.other.alcoholic.drinks...Instance.0_Monthly.drinker.or.never.drinks"))
colnames(ts)


table(ts$Alcohol.usually.taken.with.meals...Instance.0)
table(is.na(ts$Alcohol.usually.taken.with.meals...Instance.0))
ts$Alcohol.usually.taken.with.meals...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("6"),
                                                           "Never.drinks",ts$Alcohol.usually.taken.with.meals...Instance.0)
table(ts$Alcohol.usually.taken.with.meals...Instance.0)
table(is.na(ts$Alcohol.usually.taken.with.meals...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Alcohol.usually.taken.with.meals...Instance.0")
colnames(ts)
ts <- select(ts, -c("Alcohol.usually.taken.with.meals...Instance.0_Never.drinks"))
colnames(ts)


table(ts$Alcohol.intake.versus.10.years.previously...Instance.0)
table(is.na(ts$Alcohol.intake.versus.10.years.previously...Instance.0))
ts$Alcohol.intake.versus.10.years.previously...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("6"),
                                                                    "Never.drinks",ts$Alcohol.intake.versus.10.years.previously...Instance.0)
table(ts$Alcohol.intake.versus.10.years.previously...Instance.0)
table(is.na(ts$Alcohol.intake.versus.10.years.previously...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Alcohol.intake.versus.10.years.previously...Instance.0")
colnames(ts)
ts <- select(ts, -c("Alcohol.intake.versus.10.years.previously...Instance.0_Never.drinks"))
colnames(ts)


table(ts$Reason.for.reducing.amount.of.alcohol.drunk...Instance.0)
table(is.na(ts$Reason.for.reducing.amount.of.alcohol.drunk...Instance.0))
ts$Reason.for.reducing.amount.of.alcohol.drunk...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("6"),
                                                                      "Never.drinks",ts$Reason.for.reducing.amount.of.alcohol.drunk...Instance.0)
table(is.na(ts$Reason.for.reducing.amount.of.alcohol.drunk...Instance.0))
ts$Reason.for.reducing.amount.of.alcohol.drunk...Instance.0 <- ifelse(ts_backup$Alcohol.intake.versus.10.years.previously...Instance.0 %in% c("1","2"),
                                                                      "Not.reduced.10.years",ts$Reason.for.reducing.amount.of.alcohol.drunk...Instance.0)
table(ts$Reason.for.reducing.amount.of.alcohol.drunk...Instance.0)
table(is.na(ts$Reason.for.reducing.amount.of.alcohol.drunk...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Reason.for.reducing.amount.of.alcohol.drunk...Instance.0")
colnames(ts)
ts <- select(ts, -c("Reason.for.reducing.amount.of.alcohol.drunk...Instance.0_Not.reduced.10.years", 
                    "Reason.for.reducing.amount.of.alcohol.drunk...Instance.0_Never.drinks"))
colnames(ts)


table(ts$Reason.former.drinker.stopped.drinking.alcohol...Instance.0)
table(is.na(ts$Reason.former.drinker.stopped.drinking.alcohol...Instance.0))
ts$Reason.former.drinker.stopped.drinking.alcohol...Instance.0 <- ifelse(ts_backup$Alcohol.intake.frequency....Instance.0 %in% c("1","2","3","4","5"),
                                                                         "Drinks.alcohol",ts$Reason.former.drinker.stopped.drinking.alcohol...Instance.0)
table(is.na(ts$Reason.former.drinker.stopped.drinking.alcohol...Instance.0))
ts$Reason.former.drinker.stopped.drinking.alcohol...Instance.0 <- ifelse(ts_backup$Former.alcohol.drinker...Instance.0 %in% c("0"),
                                                                         "Not.former.alcohol.drinker",ts$Reason.former.drinker.stopped.drinking.alcohol...Instance.0)
table(ts$Reason.former.drinker.stopped.drinking.alcohol...Instance.0)
table(is.na(ts$Reason.former.drinker.stopped.drinking.alcohol...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Reason.former.drinker.stopped.drinking.alcohol...Instance.0")
colnames(ts)
ts <- select(ts, -c("Reason.former.drinker.stopped.drinking.alcohol...Instance.0_Drinks.alcohol", 
                    "Reason.former.drinker.stopped.drinking.alcohol...Instance.0_Not.former.alcohol.drinker"))
colnames(ts)


#sun exposure
table(ts$Time.spend.outdoors.in.summer...Instance.0)
table(is.na(ts$Time.spend.outdoors.in.summer...Instance.0))


table(ts$Time.spent.outdoors.in.winter...Instance.0)
table(is.na(ts$Time.spent.outdoors.in.winter...Instance.0))


table(ts$Skin.colour...Instance.0)
table(is.na(ts$Skin.colour...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Skin.colour...Instance.0")
colnames(ts)


table(ts$Ease.of.skin.tanning...Instance.0)
table(is.na(ts$Ease.of.skin.tanning...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Ease.of.skin.tanning...Instance.0")
colnames(ts)


table(ts$Childhood.sunburn.occasions...Instance.0)
table(is.na(ts$Childhood.sunburn.occasions...Instance.0))


table(ts$Hair.colour..natural..before.greying....Instance.0)
table(is.na(ts$Hair.colour..natural..before.greying....Instance.0))
ts <- splitLevelsIntoColumns(ts, "Hair.colour..natural..before.greying....Instance.0")
colnames(ts)


table(ts$Facial.ageing...Instance.0)
table(is.na(ts$Facial.ageing...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Facial.ageing...Instance.0")
colnames(ts)


table(ts$Use.of.sun.uv.protection...Instance.0)
table(is.na(ts$Use.of.sun.uv.protection...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Use.of.sun.uv.protection...Instance.0")
colnames(ts)


table(ts$Frequency.of.solarium.sunlamp.use...Instance.0)
table(is.na(ts$Frequency.of.solarium.sunlamp.use...Instance.0))


#sexual factors
table(ts$Answered.sexual.history.questions...Instance.0)
table(is.na(ts$Answered.sexual.history.questions...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Answered.sexual.history.questions...Instance.0")
colnames(ts)


table(ts$Age.first.had.sexual.intercourse...Instance.0)
table(is.na(ts$Age.first.had.sexual.intercourse...Instance.0))
ts <- splitMiscLevels(ts, "Age.first.had.sexual.intercourse...Instance.0", c("-2"))
colnames(ts)


table(ts$Lifetime.number.of.sexual.partners...Instance.0)
table(is.na(ts$Lifetime.number.of.sexual.partners...Instance.0))
ts$Lifetime.number.of.sexual.partners...Instance.0 <- ifelse(ts_backup$Age.first.had.sexual.intercourse...Instance.0 %in% c('-2'),
                                                             "Never.had.intercourse",ts$Lifetime.number.of.sexual.partners...Instance.0)
table(ts$Lifetime.number.of.sexual.partners...Instance.0)
table(is.na(ts$Lifetime.number.of.sexual.partners...Instance.0))
ts <- splitMiscLevels(ts, "Lifetime.number.of.sexual.partners...Instance.0", c("Never.had.intercourse"))
colnames(ts)
ts <- select(ts, -c("Lifetime.number.of.sexual.partners...Instance.0_Never.had.intercourse"))
colnames(ts)


table(ts$Ever.had.same.sex.intercourse...Instance.0)
table(is.na(ts$Ever.had.same.sex.intercourse...Instance.0))
ts$Ever.had.same.sex.intercourse...Instance.0 <- ifelse(ts_backup$Age.first.had.sexual.intercourse...Instance.0 %in% c('-2'),
                                                        "Never.had.intercourse",ts$Ever.had.same.sex.intercourse...Instance.0)
table(ts$Ever.had.same.sex.intercourse...Instance.0)
table(is.na(ts$Ever.had.same.sex.intercourse...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Ever.had.same.sex.intercourse...Instance.0")
colnames(ts)
ts <- select(ts, -c("Ever.had.same.sex.intercourse...Instance.0_Never.had.intercourse"))
colnames(ts)


table(ts$Lifetime.number.of.same.sex.sexual.partners...Instance.0)
table(is.na(ts$Lifetime.number.of.same.sex.sexual.partners...Instance.0))
ts$Lifetime.number.of.same.sex.sexual.partners...Instance.0 <- ifelse(ts_backup$Age.first.had.sexual.intercourse...Instance.0 %in% c('-2'),
                                                                      "Never.had.intercourse",ts$Lifetime.number.of.same.sex.sexual.partners...Instance.0)
table(is.na(ts$Lifetime.number.of.same.sex.sexual.partners...Instance.0))
ts$Lifetime.number.of.same.sex.sexual.partners...Instance.0 <- ifelse(ts_backup$Ever.had.same.sex.intercourse...Instance.0 %in% c('0'),
                                                                      "Never.had.intercourse.same.sex",ts$Lifetime.number.of.same.sex.sexual.partners...Instance.0)
table(ts$Lifetime.number.of.same.sex.sexual.partners...Instance.0)
table(is.na(ts$Lifetime.number.of.same.sex.sexual.partners...Instance.0))
ts <- splitMiscLevels(ts, "Lifetime.number.of.same.sex.sexual.partners...Instance.0", 
                      c("Never.had.intercourse", "Never.had.intercourse.same.sex"))
colnames(ts)
ts <- select(ts, -c("Lifetime.number.of.same.sex.sexual.partners...Instance.0_Never.had.intercourse",
                    "Lifetime.number.of.same.sex.sexual.partners...Instance.0_Never.had.intercourse.same.sex"))
colnames(ts)


#early life factors
table(ts$Breastfed.as.a.baby...Instance.0)
table(is.na(ts$Breastfed.as.a.baby...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Breastfed.as.a.baby...Instance.0")
colnames(ts)


table(ts$Comparative.body.size.at.age.10...Instance.0)
table(is.na(ts$Comparative.body.size.at.age.10...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Comparative.body.size.at.age.10...Instance.0")
colnames(ts)


table(ts$Comparative.height.size.at.age.10...Instance.0)
table(is.na(ts$Comparative.height.size.at.age.10...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Comparative.height.size.at.age.10...Instance.0")
colnames(ts)


table(ts$Handedness..chirality.laterality....Instance.0)
table(is.na(ts$Handedness..chirality.laterality....Instance.0))
ts <- splitLevelsIntoColumns(ts, "Handedness..chirality.laterality....Instance.0")
colnames(ts)


table(ts$Adopted.as.a.child...Instance.0)
table(is.na(ts$Adopted.as.a.child...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Adopted.as.a.child...Instance.0")
colnames(ts)


table(ts$Part.of.a.multiple.birth...Instance.0)
table(is.na(ts$Part.of.a.multiple.birth...Instance.0))
ts$Part.of.a.multiple.birth...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("1"),
                                                   'Adopted',ts$Part.of.a.multiple.birth...Instance.0)
table(ts$Part.of.a.multiple.birth...Instance.0)
table(is.na(ts$Part.of.a.multiple.birth...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Part.of.a.multiple.birth...Instance.0")
colnames(ts)
ts <- select(ts, -c("Part.of.a.multiple.birth...Instance.0_Adopted"))
colnames(ts)


table(ts$Maternal.smoking.around.birth...Instance.0)
table(is.na(ts$Maternal.smoking.around.birth...Instance.0))
ts$Maternal.smoking.around.birth...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("1"),
                                                        'Adopted',ts$Maternal.smoking.around.birth...Instance.0)
table(ts$Maternal.smoking.around.birth...Instance.0)
table(is.na(ts$Maternal.smoking.around.birth...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Maternal.smoking.around.birth...Instance.0")
colnames(ts)
ts <- select(ts, -c("Maternal.smoking.around.birth...Instance.0_Adopted"))
colnames(ts)


#family history
table(ts$Father.still.alive...Instance.0)
table(is.na(ts$Father.still.alive...Instance.0))
ts$Father.still.alive...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("1"),
                                             'Adopted',ts$Father.still.alive...Instance.0)
table(ts$Father.still.alive...Instance.0)
table(is.na(ts$Father.still.alive...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Father.still.alive...Instance.0")
colnames(ts)
ts <- select(ts, -c("Father.still.alive...Instance.0_Adopted"))
colnames(ts)


table(ts$Adopted.father.still.alive...Instance.0)
table(is.na(ts$Adopted.father.still.alive...Instance.0))
ts$Adopted.father.still.alive...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("0"),
                                                     'Not.adopted',ts$Adopted.father.still.alive...Instance.0)
table(ts$Adopted.father.still.alive...Instance.0)
table(is.na(ts$Adopted.father.still.alive...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Adopted.father.still.alive...Instance.0")
colnames(ts)
ts <- select(ts, -c("Adopted.father.still.alive...Instance.0_Not.adopted"))
colnames(ts)


table(ts$Father.s.age...Instance.0)
table(is.na(ts$Father.s.age...Instance.0))
ts$Father.s.age...Instance.0 <- ifelse(ts_backup$Father.still.alive...Instance.0 %in% c("0"),
                                       'Father.not.alive',ts$Father.s.age...Instance.0)
table(is.na(ts$Father.s.age...Instance.0))
ts$Father.s.age...Instance.0 <- ifelse(ts_backup$Adopted.father.still.alive...Instance.0 %in% c("0"),
                                       'Adopted.father.not.alive',ts$Father.s.age...Instance.0)
table(ts$Father.s.age...Instance.0)
table(is.na(ts$Father.s.age...Instance.0))
ts <- splitMiscLevels(ts, "Father.s.age...Instance.0", c("Father.not.alive", "Adopted.father.not.alive"))
colnames(ts)
ts <- select(ts, -c("Father.s.age...Instance.0_Father.not.alive",                                                                
                    "Father.s.age...Instance.0_Adopted.father.not.alive"))
colnames(ts)


table(ts$Father.s.age.at.death...Instance.0)
table(is.na(ts$Father.s.age.at.death...Instance.0))
ts$Father.s.age.at.death...Instance.0 <- ifelse(ts_backup$Father.still.alive...Instance.0 %in% c("1"),
                                                'Father.alive',ts$Father.s.age.at.death...Instance.0)
table(is.na(ts$Father.s.age.at.death...Instance.0))
ts$Father.s.age.at.death...Instance.0 <- ifelse(ts_backup$Adopted.father.still.alive...Instance.0 %in% c("1"),
                                                'Adopted.father.alive',ts$Father.s.age.at.death...Instance.0)
table(ts$Father.s.age.at.death...Instance.0)
table(is.na(ts$Father.s.age.at.death...Instance.0))
ts <- splitMiscLevels(ts, "Father.s.age.at.death...Instance.0", c("Father.alive", "Adopted.father.alive"))
colnames(ts)
ts <- select(ts, -c("Father.s.age.at.death...Instance.0_Father.alive",                                                       
                    "Father.s.age.at.death...Instance.0_Adopted.father.alive" ))
colnames(ts)


table(ts$Mother.still.alive...Instance.0)
table(is.na(ts$Mother.still.alive...Instance.0))
ts$Mother.still.alive...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("1"),
                                             'Adopted',ts$Mother.still.alive...Instance.0)
table(ts$Mother.still.alive...Instance.0)
table(is.na(ts$Mother.still.alive...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Mother.still.alive...Instance.0")
colnames(ts)
ts <- select(ts, -c("Mother.still.alive...Instance.0_Adopted"))
colnames(ts)


table(ts$Adopted.mother.still.alive...Instance.0)
table(is.na(ts$Adopted.mother.still.alive...Instance.0))
ts$Adopted.mother.still.alive...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("0"),
                                                     'Not.adopted',ts$Adopted.mother.still.alive...Instance.0)
table(ts$Adopted.mother.still.alive...Instance.0)
table(is.na(ts$Adopted.mother.still.alive...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Adopted.mother.still.alive...Instance.0")
colnames(ts)
ts <- select(ts, -c("Adopted.mother.still.alive...Instance.0_Not.adopted"))
colnames(ts)



table(ts$Mother.s.age...Instance.0)
table(is.na(ts$Mother.s.age...Instance.0))
ts$Mother.s.age...Instance.0 <- ifelse(ts_backup$Mother.still.alive...Instance.0 %in% c("0"),
                                       'Mother.not.alive',ts$Mother.s.age...Instance.0)
table(is.na(ts$Mother.s.age...Instance.0))
ts$Mother.s.age...Instance.0 <- ifelse(ts_backup$Adopted.mother.still.alive...Instance.0 %in% c("0"),
                                       'Adopted.mother.not.alive',ts$Mother.s.age...Instance.0)
table(ts$Mother.s.age...Instance.0)
table(is.na(ts$Mother.s.age...Instance.0))
ts <- splitMiscLevels(ts, "Mother.s.age...Instance.0", c("Mother.not.alive", "Adopted.mother.not.alive"))
colnames(ts)
ts <- select(ts, -c("Mother.s.age...Instance.0_Mother.not.alive",                                                                
                    "Mother.s.age...Instance.0_Adopted.mother.not.alive"))
colnames(ts)


table(ts$Mother.s.age.at.death...Instance.0)
table(is.na(ts$Mother.s.age.at.death...Instance.0))
ts$Mother.s.age.at.death...Instance.0 <- ifelse(ts_backup$Mother.still.alive...Instance.0 %in% c("1"),
                                                'Mother.alive',ts$Mother.s.age.at.death...Instance.0)
table(is.na(ts$Mother.s.age.at.death...Instance.0))
ts$Mother.s.age.at.death...Instance.0 <- ifelse(ts_backup$Adopted.mother.still.alive...Instance.0 %in% c("1"),
                                                'Adopted.mother.alive',ts$Mother.s.age.at.death...Instance.0)
table(ts$Mother.s.age.at.death...Instance.0)
table(is.na(ts$Mother.s.age.at.death...Instance.0))
ts <- splitMiscLevels(ts, "Mother.s.age.at.death...Instance.0", c("Mother.alive", "Adopted.mother.alive"))
colnames(ts)
ts <- select(ts, -c("Mother.s.age.at.death...Instance.0_Mother.alive",                                                           
                    "Mother.s.age.at.death...Instance.0_Adopted.mother.alive"))
colnames(ts)


table(ts$Number.of.full.brothers...Instance.0)
table(is.na(ts$Number.of.full.brothers...Instance.0))
ts$Number.of.full.brothers...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("1"),
                                                  'Adopted',ts$Number.of.full.brothers...Instance.0)
table(ts$Number.of.full.brothers...Instance.0)
table(is.na(ts$Number.of.full.brothers...Instance.0))
ts <- splitMiscLevels(ts, "Number.of.full.brothers...Instance.0", c("Adopted"))
colnames(ts)
ts <- select(ts, -c("Number.of.full.brothers...Instance.0_Adopted"))
colnames(ts)


table(ts$Number.of.adopted.brothers...Instance.0)
table(is.na(ts$Number.of.adopted.brothers...Instance.0))
ts$Number.of.adopted.brothers...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("0"),
                                                     'Not.adopted',ts$Number.of.adopted.brothers...Instance.0)
table(ts$Number.of.adopted.brothers...Instance.0)
table(is.na(ts$Number.of.adopted.brothers...Instance.0))
ts <- splitMiscLevels(ts, "Number.of.adopted.brothers...Instance.0", c("Not.adopted"))
colnames(ts)
ts <- select(ts, -c("Number.of.adopted.brothers...Instance.0_Not.adopted"))
colnames(ts)


table(ts$Number.of.full.sisters...Instance.0)
table(is.na(ts$Number.of.full.sisters...Instance.0))
ts$Number.of.full.sisters...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("1"),
                                                 'Adopted',ts$Number.of.full.sisters...Instance.0)
table(ts$Number.of.full.sisters...Instance.0)
table(is.na(ts$Number.of.full.sisters...Instance.0))
ts <- splitMiscLevels(ts, "Number.of.full.sisters...Instance.0", c("Adopted"))
colnames(ts)
ts <- select(ts, -c("Number.of.full.sisters...Instance.0_Adopted"))
colnames(ts)


table(ts$Number.of.adopted.sisters...Instance.0)
table(is.na(ts$Number.of.adopted.sisters...Instance.0))
ts$Number.of.adopted.sisters...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("0"),
                                                    'Not.adopted',ts$Number.of.adopted.sisters...Instance.0)
table(ts$Number.of.adopted.sisters...Instance.0)
table(is.na(ts$Number.of.adopted.sisters...Instance.0))
ts <- splitMiscLevels(ts, "Number.of.adopted.sisters...Instance.0", c("Not.adopted"))
colnames(ts)
ts <- select(ts, -c("Number.of.adopted.sisters...Instance.0_Not.adopted"))
colnames(ts)


table(ts$Number.of.older.siblings...Instance.0)
table(is.na(ts$Number.of.older.siblings...Instance.0))
ts$Number.of.older.siblings...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("1"),
                                                   'Adopted',ts$Number.of.older.siblings...Instance.0)
table(ts$Number.of.older.siblings...Instance.0)
table(is.na(ts$Number.of.older.siblings...Instance.0))
ts <- splitMiscLevels(ts, "Number.of.older.siblings...Instance.0", c("Adopted"))
colnames(ts)
ts <- select(ts, -c("Number.of.older.siblings...Instance.0_Adopted"))
colnames(ts)


table(ts$Illnesses.of.adopted.father...Instance.0)
table(is.na(ts$Illnesses.of.adopted.father...Instance.0))
ts$Illnesses.of.adopted.father...Instance.0 <- ifelse(ts$Illnesses.of.adopted.father...Instance.0 %in% c("-23","-21","-13","-11","-13|-23","-11|-23","-13|-21","-11|-21"),
                                                      NA,ts$Illnesses.of.adopted.father...Instance.0)
table(is.na(ts$Illnesses.of.adopted.father...Instance.0))
ts$Illnesses.of.adopted.father...Instance.0 <- ifelse(is.na(ts_backup$Adopted.father.still.alive...Instance.0),
                                                      'No.adopted.father',ts$Illnesses.of.adopted.father...Instance.0)
table(ts$Illnesses.of.adopted.father...Instance.0)
ts$Illnesses.of.adopted.father...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("0"),
                                                      'Not.adopted',ts$Illnesses.of.adopted.father...Instance.0)
table(ts$Illnesses.of.adopted.father...Instance.0)
table(is.na(ts$Illnesses.of.adopted.father...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Illnesses.of.adopted.father...Instance.0")
colnames(ts)

#exclude unnecessary family history columns
ts <- ts %>%
  select(-Illnesses.of.adopted.father...Instance.0_neg11,
         -Illnesses.of.adopted.father...Instance.0_neg21,
         -Illnesses.of.adopted.father...Instance.0_neg23,
         -Illnesses.of.adopted.father...Instance.0_Not.adopted,
         -Illnesses.of.adopted.father...Instance.0_No.adopted.father)
colnames(ts)


table(ts$Illnesses.of.adopted.mother...Instance.0)
table(is.na(ts$Illnesses.of.adopted.mother...Instance.0))
ts$Illnesses.of.adopted.mother...Instance.0 <- ifelse(ts$Illnesses.of.adopted.mother...Instance.0 %in% c("-23","-21","-13","-11","-13|-23","-11|-23","-13|-21","-11|-21"),
                                                      NA,ts$Illnesses.of.adopted.mother...Instance.0)
table(is.na(ts$Illnesses.of.adopted.mother...Instance.0))
ts$Illnesses.of.adopted.mother...Instance.0 <- ifelse(is.na(ts_backup$Adopted.mother.still.alive...Instance.0),
                                                      'No.adopted.mother',ts$Illnesses.of.adopted.mother...Instance.0)
table(ts$Illnesses.of.adopted.mother...Instance.0)
ts$Illnesses.of.adopted.mother...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("0"),
                                                      'Not.adopted',ts$Illnesses.of.adopted.mother...Instance.0)
table(ts$Illnesses.of.adopted.mother...Instance.0)
table(is.na(ts$Illnesses.of.adopted.mother...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Illnesses.of.adopted.mother...Instance.0")
colnames(ts)

#exclude NA family history columns
ts <- ts %>%
  select(-Illnesses.of.adopted.mother...Instance.0_neg11,
         -Illnesses.of.adopted.mother...Instance.0_neg21,
         -Illnesses.of.adopted.mother...Instance.0_neg23,
         -Illnesses.of.adopted.mother...Instance.0_Not.adopted,                                                      
         -Illnesses.of.adopted.mother...Instance.0_No.adopted.mother)
colnames(ts)


table(ts$Illnesses.of.adopted.siblings...Instance.0)
table(is.na(ts$Illnesses.of.adopted.siblings...Instance.0))
ts$Illnesses.of.adopted.siblings...Instance.0 <- ifelse(ts$Illnesses.of.adopted.siblings...Instance.0 %in% c("-23","-21","-13","-11","-13|-23","-11|-23","-13|-21","-11|-21"),
                                                        NA,ts$Illnesses.of.adopted.siblings...Instance.0)
table(is.na(ts$Illnesses.of.adopted.siblings...Instance.0))
ts$Illnesses.of.adopted.siblings...Instance.0 <- ifelse(ts_backup$Number.of.adopted.brothers...Instance.0 %in% c(NA,"0") & ts_backup$Number.of.adopted.sisters...Instance.0 %in% c(NA,"0"),
                                                        'No.adopted.siblings',ts$Illnesses.of.adopted.siblings...Instance.0)
table(ts$Illnesses.of.adopted.siblings...Instance.0)
ts$Illnesses.of.adopted.siblings...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("0"),
                                                        'Not.adopted',ts$Illnesses.of.adopted.siblings...Instance.0)
table(ts$Illnesses.of.adopted.siblings...Instance.0)
table(is.na(ts$Illnesses.of.adopted.siblings...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Illnesses.of.adopted.siblings...Instance.0")
colnames(ts)

#exclude NA family history columns
ts <- ts %>%
  select(-Illnesses.of.adopted.siblings...Instance.0_neg11,
         -Illnesses.of.adopted.siblings...Instance.0_neg21,
         -Illnesses.of.adopted.siblings...Instance.0_neg23,
         -Illnesses.of.adopted.siblings...Instance.0_Not.adopted,                                                   
         -Illnesses.of.adopted.siblings...Instance.0_No.adopted.siblings)
colnames(ts)


table(ts$Illnesses.of.father...Instance.0)
table(is.na(ts$Illnesses.of.father...Instance.0))
ts$Illnesses.of.father...Instance.0 <- ifelse(ts$Illnesses.of.father...Instance.0 %in% c("-23","-21","-13","-11","-13|-23","-11|-23","-13|-21","-11|-21"),
                                              NA,ts$Illnesses.of.father...Instance.0)
table(is.na(ts$Illnesses.of.father...Instance.0))
ts$Illnesses.of.father...Instance.0 <- ifelse(is.na(ts_backup$Father.still.alive...Instance.0),
                                              'No.father',ts$Illnesses.of.father...Instance.0)
table(ts$Illnesses.of.father...Instance.0)
ts$Illnesses.of.father...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("1"),
                                              'Adopted',ts$Illnesses.of.father...Instance.0)
table(ts$Illnesses.of.father...Instance.0)
table(is.na(ts$Illnesses.of.father...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Illnesses.of.father...Instance.0")
colnames(ts)

#exclude NA family history columns
ts <- ts %>%
  select(-Illnesses.of.father...Instance.0_neg11,
         -Illnesses.of.father...Instance.0_neg21,
         -Illnesses.of.father...Instance.0_neg23,
         -Illnesses.of.father...Instance.0_Adopted,                                                                  
         -Illnesses.of.father...Instance.0_No.father)
colnames(ts)


table(ts$Illnesses.of.mother...Instance.0)
table(is.na(ts$Illnesses.of.mother...Instance.0))
ts$Illnesses.of.mother...Instance.0 <- ifelse(ts$Illnesses.of.mother...Instance.0 %in% c("-23","-21","-13","-11","-13|-23","-11|-23","-13|-21","-11|-21"),
                                              NA,ts$Illnesses.of.mother...Instance.0)
table(is.na(ts$Illnesses.of.mother...Instance.0))
ts$Illnesses.of.mother...Instance.0 <- ifelse(is.na(ts_backup$Mother.still.alive...Instance.0),
                                              'No.mother',ts$Illnesses.of.mother...Instance.0)
table(ts$Illnesses.of.mother...Instance.0)
ts$Illnesses.of.mother...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("1"),
                                              'Adopted',ts$Illnesses.of.mother...Instance.0)
table(ts$Illnesses.of.mother...Instance.0)
table(is.na(ts$Illnesses.of.mother...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Illnesses.of.mother...Instance.0")
colnames(ts)

#exclude NA family history columns
ts <- ts %>%
  select(-Illnesses.of.mother...Instance.0_neg11,
         -Illnesses.of.mother...Instance.0_neg21,
         -Illnesses.of.mother...Instance.0_neg23,
         -Illnesses.of.mother...Instance.0_Adopted,
         -Illnesses.of.mother...Instance.0_No.mother)
colnames(ts)


table(ts$Illnesses.of.siblings...Instance.0)
table(is.na(ts$Illnesses.of.siblings...Instance.0))
ts$Illnesses.of.siblings...Instance.0 <- ifelse(ts$Illnesses.of.siblings...Instance.0 %in% c("-23","-21","-13","-11","-13|-23","-11|-23","-13|-21","-11|-21"),
                                                NA,ts$Illnesses.of.siblings...Instance.0)
table(is.na(ts$Illnesses.of.siblings...Instance.0))
ts$Illnesses.of.siblings...Instance.0 <- ifelse(ts_backup$Number.of.full.brothers...Instance.0 %in% c(NA,"0") & ts_backup$Number.of.full.sisters...Instance.0 %in% c(NA,"0"),
                                                'No.siblings',ts$Illnesses.of.siblings...Instance.0)
table(ts$Illnesses.of.siblings...Instance.0)
ts$Illnesses.of.siblings...Instance.0 <- ifelse(ts_backup$Adopted.as.a.child...Instance.0 %in% c("1"),
                                                'Adopted',ts$Illnesses.of.siblings...Instance.0)
table(ts$Illnesses.of.siblings...Instance.0)
table(is.na(ts$Illnesses.of.siblings...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Illnesses.of.siblings...Instance.0")
colnames(ts)

#exclude NA family history columns
ts <- ts %>%
  select(-Illnesses.of.siblings...Instance.0_neg11,
         -Illnesses.of.siblings...Instance.0_neg21,
         -Illnesses.of.siblings...Instance.0_neg23,
         -Illnesses.of.siblings...Instance.0_No.siblings,
         -Illnesses.of.siblings...Instance.0_Adopted)
colnames(ts)


table(ts$Non.accidental.death.in.close.genetic.family...Instance.0)
table(is.na(ts$Non.accidental.death.in.close.genetic.family...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Non.accidental.death.in.close.genetic.family...Instance.0")
colnames(ts)


#social support
table(ts$Frequency.of.friend.family.visits...Instance.0)
table(is.na(ts$Frequency.of.friend.family.visits...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Frequency.of.friend.family.visits...Instance.0")
colnames(ts)


table(ts$Leisure.social.activities...Instance.0)
table(is.na(ts$Leisure.social.activities...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Leisure.social.activities...Instance.0")
colnames(ts)


table(ts$Able.to.confide...Instance.0)
table(is.na(ts$Able.to.confide...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Able.to.confide...Instance.0")
colnames(ts)


#mental health
table(ts$Mood.swings...Instance.0)
table(is.na(ts$Mood.swings...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Mood.swings...Instance.0")
colnames(ts)


table(ts$Miserableness...Instance.0)
table(is.na(ts$Miserableness...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Miserableness...Instance.0")
colnames(ts)


table(ts$Irritability...Instance.0)
table(is.na(ts$Irritability...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Irritability...Instance.0")
colnames(ts)


table(ts$Sensitivity...hurt.feelings...Instance.0)
table(is.na(ts$Sensitivity...hurt.feelings...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Sensitivity...hurt.feelings...Instance.0")
colnames(ts)


table(ts$Fed.up.feelings...Instance.0)
table(is.na(ts$Fed.up.feelings...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Fed.up.feelings...Instance.0")
colnames(ts)


table(ts$Nervous.feelings...Instance.0)
table(is.na(ts$Nervous.feelings...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Nervous.feelings...Instance.0")
colnames(ts)


table(ts$Worrier...anxious.feelings...Instance.0)
table(is.na(ts$Worrier...anxious.feelings...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Worrier...anxious.feelings...Instance.0")
colnames(ts)


table(ts$Tense....highly.strung....Instance.0)
table(is.na(ts$Tense....highly.strung....Instance.0))
ts <- splitLevelsIntoColumns(ts, "Tense....highly.strung....Instance.0")
colnames(ts)


table(ts$Worry.too.long.after.embarrassment...Instance.0)
table(is.na(ts$Worry.too.long.after.embarrassment...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Worry.too.long.after.embarrassment...Instance.0")
colnames(ts)


table(ts$Suffer.from..nerves....Instance.0)
table(is.na(ts$Suffer.from..nerves....Instance.0))
ts <- splitLevelsIntoColumns(ts, "Suffer.from..nerves....Instance.0")
colnames(ts)


table(ts$Loneliness..isolation...Instance.0)
table(is.na(ts$Loneliness..isolation...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Loneliness..isolation...Instance.0")
colnames(ts)


table(ts$Guilty.feelings...Instance.0)
table(is.na(ts$Guilty.feelings...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Guilty.feelings...Instance.0")
colnames(ts)


table(ts$Risk.taking...Instance.0)
table(is.na(ts$Risk.taking...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Risk.taking...Instance.0")
colnames(ts)


table(ts$Happiness...Instance.0)
table(is.na(ts$Happiness...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Happiness...Instance.0")
colnames(ts)


table(ts$Work.job.satisfaction...Instance.0)
table(is.na(ts$Work.job.satisfaction...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Work.job.satisfaction...Instance.0")
colnames(ts)


table(ts$Health.satisfaction...Instance.0)
table(is.na(ts$Health.satisfaction...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Health.satisfaction...Instance.0")
colnames(ts)


table(ts$Family.relationship.satisfaction...Instance.0)
table(is.na(ts$Family.relationship.satisfaction...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Family.relationship.satisfaction...Instance.0")
colnames(ts)


table(ts$Friendships.satisfaction...Instance.0)
table(is.na(ts$Friendships.satisfaction...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Friendships.satisfaction...Instance.0")
colnames(ts)


table(ts$Financial.situation.satisfaction...Instance.0)
table(is.na(ts$Financial.situation.satisfaction...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Financial.situation.satisfaction...Instance.0")
colnames(ts)


table(ts$Frequency.of.depressed.mood.in.last.2.weeks...Instance.0)
table(is.na(ts$Frequency.of.depressed.mood.in.last.2.weeks...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Frequency.of.depressed.mood.in.last.2.weeks...Instance.0")
colnames(ts)


table(ts$Frequency.of.unenthusiasm...disinterest.in.last.2.weeks...Instance.0)
table(is.na(ts$Frequency.of.unenthusiasm...disinterest.in.last.2.weeks...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Frequency.of.unenthusiasm...disinterest.in.last.2.weeks...Instance.0")
colnames(ts)


table(ts$Frequency.of.tenseness...restlessness.in.last.2.weeks...Instance.0)
table(is.na(ts$Frequency.of.tenseness...restlessness.in.last.2.weeks...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Frequency.of.tenseness...restlessness.in.last.2.weeks...Instance.0")
colnames(ts)


table(ts$Frequency.of.tiredness...lethargy.in.last.2.weeks...Instance.0)
table(is.na(ts$Frequency.of.tiredness...lethargy.in.last.2.weeks...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Frequency.of.tiredness...lethargy.in.last.2.weeks...Instance.0")
colnames(ts)


table(ts$Seen.doctor..GP..for.nerves..anxiety..tension.or.depression...Instance.0)
table(is.na(ts$Seen.doctor..GP..for.nerves..anxiety..tension.or.depression...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Seen.doctor..GP..for.nerves..anxiety..tension.or.depression...Instance.0")
colnames(ts)


table(ts$Seen.a.psychiatrist.for.nerves..anxiety..tension.or.depression...Instance.0)
table(is.na(ts$Seen.a.psychiatrist.for.nerves..anxiety..tension.or.depression...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Seen.a.psychiatrist.for.nerves..anxiety..tension.or.depression...Instance.0")
colnames(ts)


table(ts$Ever.depressed.for.a.whole.week...Instance.0)
table(is.na(ts$Ever.depressed.for.a.whole.week...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Ever.depressed.for.a.whole.week...Instance.0")
colnames(ts)


table(ts$Longest.period.of.depression...Instance.0)
table(is.na(ts$Longest.period.of.depression...Instance.0))
ts$Longest.period.of.depression...Instance.0 <- ifelse(ts_backup$Ever.depressed.for.a.whole.week...Instance.0 %in% c("0"),
                                                       'Not.depressed.whole.week',ts$Longest.period.of.depression...Instance.0)
table(ts$Longest.period.of.depression...Instance.0)
table(is.na(ts$Longest.period.of.depression...Instance.0))
ts <- splitMiscLevels(ts, "Longest.period.of.depression...Instance.0", "Not.depressed.whole.week")
colnames(ts)
ts <- select(ts,-c("Longest.period.of.depression...Instance.0_Not.depressed.whole.week"))
colnames(ts)


table(ts$Number.of.depression.episodes...Instance.0)
table(is.na(ts$Number.of.depression.episodes...Instance.0))
ts$Number.of.depression.episodes...Instance.0 <- ifelse(ts_backup$Ever.depressed.for.a.whole.week...Instance.0 %in% c("0"),
                                                        'Not.depressed.whole.week',ts$Number.of.depression.episodes...Instance.0)
table(ts$Number.of.depression.episodes...Instance.0)
table(is.na(ts$Number.of.depression.episodes...Instance.0))
ts <- splitMiscLevels(ts, "Number.of.depression.episodes...Instance.0", "Not.depressed.whole.week")
colnames(ts)
ts <- select(ts,-c("Number.of.depression.episodes...Instance.0_Not.depressed.whole.week"))
colnames(ts)


table(ts$Ever.unenthusiastic.disinterested.for.a.whole.week...Instance.0)
table(is.na(ts$Ever.unenthusiastic.disinterested.for.a.whole.week...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Ever.unenthusiastic.disinterested.for.a.whole.week...Instance.0")
colnames(ts)


table(ts$Longest.period.of.unenthusiasm...disinterest...Instance.0)
table(is.na(ts$Longest.period.of.unenthusiasm...disinterest...Instance.0))
ts$Longest.period.of.unenthusiasm...disinterest...Instance.0 <- ifelse(ts_backup$Ever.unenthusiastic.disinterested.for.a.whole.week...Instance.0 %in% c("0"),
                                                                       'Not.uninterested.depressed.whole.week',ts$Longest.period.of.unenthusiasm...disinterest...Instance.0)
table(ts$Longest.period.of.unenthusiasm...disinterest...Instance.0)
table(is.na(ts$Longest.period.of.unenthusiasm...disinterest...Instance.0))
ts <- splitMiscLevels(ts, "Longest.period.of.unenthusiasm...disinterest...Instance.0", "Not.uninterested.depressed.whole.week")
colnames(ts)
ts <- select(ts,-c("Longest.period.of.unenthusiasm...disinterest...Instance.0_Not.uninterested.depressed.whole.week"))
colnames(ts)


table(ts$Number.of.unenthusiastic.disinterested.episodes...Instance.0)
table(is.na(ts$Number.of.unenthusiastic.disinterested.episodes...Instance.0))
ts$Number.of.unenthusiastic.disinterested.episodes...Instance.0 <- ifelse(ts_backup$Ever.unenthusiastic.disinterested.for.a.whole.week...Instance.0 %in% c("0"),
                                                                          'Not.uninterested.depressed.whole.week',ts$Number.of.unenthusiastic.disinterested.episodes...Instance.0)
table(ts$Number.of.unenthusiastic.disinterested.episodes...Instance.0)
table(is.na(ts$Number.of.unenthusiastic.disinterested.episodes...Instance.0))
ts <- splitMiscLevels(ts, "Number.of.unenthusiastic.disinterested.episodes...Instance.0", "Not.uninterested.depressed.whole.week")
colnames(ts)
ts <- select(ts,-c("Number.of.unenthusiastic.disinterested.episodes...Instance.0_Not.uninterested.depressed.whole.week"))
colnames(ts)


table(ts$Ever.manic.hyper.for.2.days...Instance.0)
table(is.na(ts$Ever.manic.hyper.for.2.days...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Ever.manic.hyper.for.2.days...Instance.0")
colnames(ts)


table(ts$Ever.highly.irritable.argumentative.for.2.days...Instance.0)
table(is.na(ts$Ever.highly.irritable.argumentative.for.2.days...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Ever.highly.irritable.argumentative.for.2.days...Instance.0")
colnames(ts)


table(ts$Manic.hyper.symptoms...Instance.0)
table(is.na(ts$Manic.hyper.symptoms...Instance.0))
ts$Manic.hyper.symptoms...Instance.0 <- ifelse(ts_backup$Ever.manic.hyper.for.2.days...Instance.0 %in% c("0"),
                                               'Not.manic.irritable.two.days',ts$Manic.hyper.symptoms...Instance.0)
table(ts$Manic.hyper.symptoms...Instance.0)
ts$Manic.hyper.symptoms...Instance.0 <- ifelse(ts_backup$Ever.highly.irritable.argumentative.for.2.days...Instance.0 %in% c("0"),
                                               'Not.manic.irritable.two.days',ts$Manic.hyper.symptoms...Instance.0)
table(ts$Manic.hyper.symptoms...Instance.0)
table(is.na(ts$Manic.hyper.symptoms...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Manic.hyper.symptoms...Instance.0")
colnames(ts)
ts <- select(ts,-c("Manic.hyper.symptoms...Instance.0_Not.manic.irritable.two.days"))
colnames(ts)


table(ts$Length.of.longest.manic.irritable.episode...Instance.0)
table(is.na(ts$Length.of.longest.manic.irritable.episode...Instance.0))
ts$Length.of.longest.manic.irritable.episode...Instance.0 <- ifelse(ts_backup$Ever.manic.hyper.for.2.days...Instance.0 %in% c("0"),
                                                                    'Not.manic.irritable.two.days',ts$Length.of.longest.manic.irritable.episode...Instance.0)
table(ts$Length.of.longest.manic.irritable.episode...Instance.0)
ts$Length.of.longest.manic.irritable.episode...Instance.0 <- ifelse(ts_backup$Ever.highly.irritable.argumentative.for.2.days...Instance.0 %in% c("0"),
                                                                    'Not.manic.irritable.two.days',ts$Length.of.longest.manic.irritable.episode...Instance.0)
table(ts$Length.of.longest.manic.irritable.episode...Instance.0)
table(is.na(ts$Length.of.longest.manic.irritable.episode...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Length.of.longest.manic.irritable.episode...Instance.0")
colnames(ts)
ts <- select(ts,-c("Length.of.longest.manic.irritable.episode...Instance.0_Not.manic.irritable.two.days"))
colnames(ts)


table(ts$Severity.of.manic.irritable.episodes...Instance.0)
table(is.na(ts$Severity.of.manic.irritable.episodes...Instance.0))
ts$Severity.of.manic.irritable.episodes...Instance.0 <- ifelse(ts_backup$Ever.manic.hyper.for.2.days...Instance.0 %in% c("0"),
                                                               'Not.manic.irritable.two.days',ts$Severity.of.manic.irritable.episodes...Instance.0)
table(ts$Severity.of.manic.irritable.episodes...Instance.0)
ts$Severity.of.manic.irritable.episodes...Instance.0 <- ifelse(ts_backup$Ever.highly.irritable.argumentative.for.2.days...Instance.0 %in% c("0"),
                                                               'Not.manic.irritable.two.days',ts$Severity.of.manic.irritable.episodes...Instance.0)
table(ts$Severity.of.manic.irritable.episodes...Instance.0)
table(is.na(ts$Severity.of.manic.irritable.episodes...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Severity.of.manic.irritable.episodes...Instance.0")
colnames(ts)
ts <- select(ts,-c("Severity.of.manic.irritable.episodes...Instance.0_Not.manic.irritable.two.days"))
colnames(ts)


table(ts$Illness..injury..bereavement..stress.in.last.2.years...Instance.0)
table(is.na(ts$Illness..injury..bereavement..stress.in.last.2.years...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Illness..injury..bereavement..stress.in.last.2.years...Instance.0")
colnames(ts)


#eyesight
table(ts$Wears.glasses.or.contact.lenses...Instance.0)
table(is.na(ts$Wears.glasses.or.contact.lenses...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Wears.glasses.or.contact.lenses...Instance.0")
colnames(ts)


table(ts$Age.started.wearing.glasses.or.contact.lenses...Instance.0)
table(is.na(ts$Age.started.wearing.glasses.or.contact.lenses...Instance.0))
ts$Age.started.wearing.glasses.or.contact.lenses...Instance.0 <- ifelse(ts_backup$Wears.glasses.or.contact.lenses...Instance.0 %in% c("0"),
                                                                        'No.glasses.or.contact.lens',ts$Age.started.wearing.glasses.or.contact.lenses...Instance.0)
table(ts$Age.started.wearing.glasses.or.contact.lenses...Instance.0)
table(is.na(ts$Age.started.wearing.glasses.or.contact.lenses...Instance.0))
ts <- splitMiscLevels(ts, "Age.started.wearing.glasses.or.contact.lenses...Instance.0", "No.glasses.or.contact.lens")
colnames(ts)
ts <- select(ts, -c("Age.started.wearing.glasses.or.contact.lenses...Instance.0_No.glasses.or.contact.lens"))
colnames(ts)


table(ts$Reason.for.glasses.contact.lenses...Instance.0)
table(is.na(ts$Reason.for.glasses.contact.lenses...Instance.0))
ts$Reason.for.glasses.contact.lenses...Instance.0 <- ifelse(ts_backup$Wears.glasses.or.contact.lenses...Instance.0 %in% c("0"),
                                                            'No.glasses.or.contact.lens',ts$Reason.for.glasses.contact.lenses...Instance.0)
table(ts$Reason.for.glasses.contact.lenses...Instance.0)
table(is.na(ts$Reason.for.glasses.contact.lenses...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Reason.for.glasses.contact.lenses...Instance.0")
colnames(ts)
ts <- select(ts, -c("Reason.for.glasses.contact.lenses...Instance.0_No.glasses.or.contact.lens"))
colnames(ts)


table(ts$Which.eye.s..affected.by.myopia..short.sight....Instance.0)
table(is.na(ts$Which.eye.s..affected.by.myopia..short.sight....Instance.0))
table(ts$Reason.for.glasses.contact.lenses...Instance.0_1 %in% ('FALSE'))
ts$Which.eye.s..affected.by.myopia..short.sight....Instance.0 <- ifelse(ts$Reason.for.glasses.contact.lenses...Instance.0_1 %in% c("FALSE"),
                                                                        "No.myopia",ts$Which.eye.s..affected.by.myopia..short.sight....Instance.0)
table(ts$Which.eye.s..affected.by.myopia..short.sight....Instance.0)
table(is.na(ts$Which.eye.s..affected.by.myopia..short.sight....Instance.0))
ts <- splitLevelsIntoColumns(ts,"Which.eye.s..affected.by.myopia..short.sight....Instance.0")
colnames(ts)
ts <- select(ts, -c("Which.eye.s..affected.by.myopia..short.sight....Instance.0_No.myopia"))
colnames(ts)


table(ts$Which.eye.s..affected.by.hypermetropia..long.sight....Instance.0)
table(is.na(ts$Which.eye.s..affected.by.hypermetropia..long.sight....Instance.0))
table(ts$Reason.for.glasses.contact.lenses...Instance.0_2 %in% ('FALSE'))
ts$Which.eye.s..affected.by.hypermetropia..long.sight....Instance.0 <- ifelse(ts$Reason.for.glasses.contact.lenses...Instance.0_2 %in% c("FALSE"),
                                                                              "No.hypermetropia",ts$Which.eye.s..affected.by.hypermetropia..long.sight....Instance.0)
table(ts$Which.eye.s..affected.by.hypermetropia..long.sight....Instance.0)
table(is.na(ts$Which.eye.s..affected.by.hypermetropia..long.sight....Instance.0))
ts <- splitLevelsIntoColumns(ts,"Which.eye.s..affected.by.hypermetropia..long.sight....Instance.0")
colnames(ts)
ts <- select(ts, -c("Which.eye.s..affected.by.hypermetropia..long.sight....Instance.0_No.hypermetropia"))
colnames(ts)


table(ts$Which.eye.s..affected.by.presbyopia...Instance.0)
table(is.na(ts$Which.eye.s..affected.by.presbyopia...Instance.0))
table(ts$Reason.for.glasses.contact.lenses...Instance.0_3 %in% ('FALSE'))
ts$Which.eye.s..affected.by.presbyopia...Instance.0 <- ifelse(ts$Reason.for.glasses.contact.lenses...Instance.0_3 %in% c("FALSE"),
                                                              "No.presbyopia",ts$Which.eye.s..affected.by.presbyopia...Instance.0)
table(ts$Which.eye.s..affected.by.presbyopia...Instance.0)
table(is.na(ts$Which.eye.s..affected.by.presbyopia...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Which.eye.s..affected.by.presbyopia...Instance.0")
colnames(ts)
ts <- select(ts, -c("Which.eye.s..affected.by.presbyopia...Instance.0_No.presbyopia"))
colnames(ts)


table(ts$Which.eye.s..affected.by.astigmatism...Instance.0)
table(is.na(ts$Which.eye.s..affected.by.astigmatism...Instance.0))
table(ts$Reason.for.glasses.contact.lenses...Instance.0_4 %in% ('FALSE'))
ts$Which.eye.s..affected.by.astigmatism...Instance.0 <- ifelse(ts$Reason.for.glasses.contact.lenses...Instance.0_4 %in% c("FALSE"),
                                                               "No.astigmatism",ts$Which.eye.s..affected.by.astigmatism...Instance.0)
table(ts$Which.eye.s..affected.by.astigmatism...Instance.0)
table(is.na(ts$Which.eye.s..affected.by.astigmatism...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Which.eye.s..affected.by.astigmatism...Instance.0")
colnames(ts)
ts <- select(ts, -c("Which.eye.s..affected.by.astigmatism...Instance.0_No.astigmatism"))
colnames(ts)


table(ts$Which.eye.s..affected.by.strabismus..squint....Instance.0)
table(is.na(ts$Which.eye.s..affected.by.strabismus..squint....Instance.0))
table(ts$Reason.for.glasses.contact.lenses...Instance.0_5 %in% ('FALSE'))
ts$Which.eye.s..affected.by.strabismus..squint....Instance.0 <- ifelse(ts$Reason.for.glasses.contact.lenses...Instance.0_5 %in% c("FALSE"),
                                                                       "No.strabismus",ts$Which.eye.s..affected.by.strabismus..squint....Instance.0)
table(ts$Which.eye.s..affected.by.strabismus..squint....Instance.0)
table(is.na(ts$Which.eye.s..affected.by.strabismus..squint....Instance.0))
ts <- splitLevelsIntoColumns(ts,"Which.eye.s..affected.by.strabismus..squint....Instance.0")
colnames(ts)
ts <- select(ts, -c("Which.eye.s..affected.by.strabismus..squint....Instance.0_No.strabismus"))
colnames(ts)


table(ts$Which.eye.s..affected.by.amblyopia..lazy.eye....Instance.0)
table(is.na(ts$Which.eye.s..affected.by.amblyopia..lazy.eye....Instance.0))
table(ts$Reason.for.glasses.contact.lenses...Instance.0_6 %in% ('FALSE'))
ts$Which.eye.s..affected.by.amblyopia..lazy.eye....Instance.0 <- ifelse(ts$Reason.for.glasses.contact.lenses...Instance.0_6 %in% c("FALSE"),
                                                                        "No.amblyopia",ts$Which.eye.s..affected.by.amblyopia..lazy.eye....Instance.0)
table(ts$Which.eye.s..affected.by.amblyopia..lazy.eye....Instance.0)
table(is.na(ts$Which.eye.s..affected.by.amblyopia..lazy.eye....Instance.0))
ts <- splitLevelsIntoColumns(ts,"Which.eye.s..affected.by.amblyopia..lazy.eye....Instance.0")
colnames(ts)
ts <- select(ts, -c("Which.eye.s..affected.by.amblyopia..lazy.eye....Instance.0_No.amblyopia"))
colnames(ts)


table(ts$Which.eye.s..affected.by.other.eye.condition...Instance.0)
table(is.na(ts$Which.eye.s..affected.by.other.eye.condition...Instance.0))
table(ts$Reason.for.glasses.contact.lenses...Instance.0_7 %in% ('FALSE'))
ts$Which.eye.s..affected.by.other.eye.condition...Instance.0 <- ifelse(ts$Reason.for.glasses.contact.lenses...Instance.0_7 %in% c("FALSE"),
                                                                       "No.other.eye.problems",ts$Which.eye.s..affected.by.other.eye.condition...Instance.0)
table(ts$Which.eye.s..affected.by.other.eye.condition...Instance.0)
table(is.na(ts$Which.eye.s..affected.by.other.eye.condition...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Which.eye.s..affected.by.other.eye.condition...Instance.0")
colnames(ts)
ts <- select(ts, -c("Which.eye.s..affected.by.other.eye.condition...Instance.0_No.other.eye.problems"))
colnames(ts)


table(ts$Other.eye.problems...Instance.0)
table(is.na(ts$Other.eye.problems...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Other.eye.problems...Instance.0")
colnames(ts)


table(ts$Eye.problems.disorders...Instance.0)
table(is.na(ts$Eye.problems.disorders...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Eye.problems.disorders...Instance.0")
colnames(ts)


table(ts$Which.eye.s..affected.by.diabetes.related.eye.disease...Instance.0)
table(is.na(ts$Which.eye.s..affected.by.diabetes.related.eye.disease...Instance.0))
table(ts$Eye.problems.disorders...Instance.0_1 %in% ('FALSE'))
ts$Which.eye.s..affected.by.diabetes.related.eye.disease...Instance.0 <- ifelse(ts$Eye.problems.disorders...Instance.0_1 %in% c("FALSE"),
                                                                                "No.diabetes.related.eye.disease",ts$Which.eye.s..affected.by.diabetes.related.eye.disease...Instance.0)
table(ts$Which.eye.s..affected.by.diabetes.related.eye.disease...Instance.0)
table(is.na(ts$Which.eye.s..affected.by.diabetes.related.eye.disease...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Which.eye.s..affected.by.diabetes.related.eye.disease...Instance.0")
colnames(ts)
ts <- select(ts, -c("Which.eye.s..affected.by.diabetes.related.eye.disease...Instance.0_No.diabetes.related.eye.disease"))
colnames(ts)


table(ts$Which.eye.s..affected.by.glaucoma...Instance.0)
table(is.na(ts$Which.eye.s..affected.by.glaucoma...Instance.0))
table(ts$Eye.problems.disorders...Instance.0_2 %in% ('FALSE'))
ts$Which.eye.s..affected.by.glaucoma...Instance.0 <- ifelse(ts$Eye.problems.disorders...Instance.0_2 %in% c("FALSE"),
                                                            "No.glaucoma",ts$Which.eye.s..affected.by.glaucoma...Instance.0)
table(ts$Which.eye.s..affected.by.glaucoma...Instance.0)
table(is.na(ts$Which.eye.s..affected.by.glaucoma...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Which.eye.s..affected.by.glaucoma...Instance.0")
colnames(ts)
ts <- select(ts, -c("Which.eye.s..affected.by.glaucoma...Instance.0_No.glaucoma"))
colnames(ts)


table(ts$Which.eye.s..affected.by.injury.or.trauma.resulting.in.loss.of.vision...Instance.0)
table(is.na(ts$Which.eye.s..affected.by.injury.or.trauma.resulting.in.loss.of.vision...Instance.0))
table(ts$Eye.problems.disorders...Instance.0_3 %in% ('FALSE'))
ts$Which.eye.s..affected.by.injury.or.trauma.resulting.in.loss.of.vision...Instance.0 <- ifelse(ts$Eye.problems.disorders...Instance.0_3 %in% c("FALSE"),
                                                                                                "No.injury.or.trauma",ts$Which.eye.s..affected.by.injury.or.trauma.resulting.in.loss.of.vision...Instance.0)
table(ts$Which.eye.s..affected.by.injury.or.trauma.resulting.in.loss.of.vision...Instance.0)
table(is.na(ts$Which.eye.s..affected.by.injury.or.trauma.resulting.in.loss.of.vision...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Which.eye.s..affected.by.injury.or.trauma.resulting.in.loss.of.vision...Instance.0")
colnames(ts)
ts <- select(ts, -c("Which.eye.s..affected.by.injury.or.trauma.resulting.in.loss.of.vision...Instance.0_No.injury.or.trauma"))
colnames(ts)


table(ts$Which.eye.s..are.affected.by.cataract...Instance.0)
table(is.na(ts$Which.eye.s..are.affected.by.cataract...Instance.0))
table(ts$Eye.problems.disorders...Instance.0_4 %in% ('FALSE'))
ts$Which.eye.s..are.affected.by.cataract...Instance.0 <- ifelse(ts$Eye.problems.disorders...Instance.0_4 %in% c("FALSE"),
                                                                "No.cataract",ts$Which.eye.s..are.affected.by.cataract...Instance.0)
table(ts$Which.eye.s..are.affected.by.cataract...Instance.0)
table(is.na(ts$Which.eye.s..are.affected.by.cataract...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Which.eye.s..are.affected.by.cataract...Instance.0")
colnames(ts)
ts <- select(ts, -c("Which.eye.s..are.affected.by.cataract...Instance.0_No.cataract"))
colnames(ts)


table(ts$Which.eye.s..affected.by.macular.degeneration...Instance.0)
table(is.na(ts$Which.eye.s..affected.by.macular.degeneration...Instance.0))
table(ts$Eye.problems.disorders...Instance.0_5 %in% ('FALSE'))
ts$Which.eye.s..affected.by.macular.degeneration...Instance.0 <- ifelse(ts$Eye.problems.disorders...Instance.0_5 %in% c("FALSE"),
                                                                        "No.macular.degeneration",ts$Which.eye.s..affected.by.macular.degeneration...Instance.0)
table(ts$Which.eye.s..affected.by.macular.degeneration...Instance.0)
table(is.na(ts$Which.eye.s..affected.by.macular.degeneration...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Which.eye.s..affected.by.macular.degeneration...Instance.0")
colnames(ts)
ts <- select(ts, -c("Which.eye.s..affected.by.macular.degeneration...Instance.0_No.macular.degeneration"))
colnames(ts)


table(ts$Which.eye.s..affected.by.other.serious.eye.condition...Instance.0)
table(is.na(ts$Which.eye.s..affected.by.other.serious.eye.condition...Instance.0))
table(ts$Eye.problems.disorders...Instance.0_6 %in% ('FALSE'))
ts$Which.eye.s..affected.by.other.serious.eye.condition...Instance.0 <- ifelse(ts$Eye.problems.disorders...Instance.0_6 %in% c("FALSE"),
                                                                               "No.other.serious.eye.condition",ts$Which.eye.s..affected.by.other.serious.eye.condition...Instance.0)
table(ts$Which.eye.s..affected.by.other.serious.eye.condition...Instance.0)
table(is.na(ts$Which.eye.s..affected.by.other.serious.eye.condition...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Which.eye.s..affected.by.other.serious.eye.condition...Instance.0")
colnames(ts)
ts <- select(ts, -c("Which.eye.s..affected.by.other.serious.eye.condition...Instance.0_No.other.serious.eye.condition"))
colnames(ts)


table(ts$Age.when.diabetes.related.eye.disease.diagnosed...Instance.0)
table(is.na(ts$Age.when.diabetes.related.eye.disease.diagnosed...Instance.0))
table(ts$Eye.problems.disorders...Instance.0_1 %in% ('FALSE'))
ts$Age.when.diabetes.related.eye.disease.diagnosed...Instance.0 <- ifelse(ts$Eye.problems.disorders...Instance.0_1 %in% c("FALSE"),
                                                                          "No.diabetes.related.eye.disease",ts$Age.when.diabetes.related.eye.disease.diagnosed...Instance.0)
table(ts$Age.when.diabetes.related.eye.disease.diagnosed...Instance.0)
table(is.na(ts$Age.when.diabetes.related.eye.disease.diagnosed...Instance.0))
ts <- splitMiscLevels(ts, "Age.when.diabetes.related.eye.disease.diagnosed...Instance.0", c("No.diabetes.related.eye.disease"))
colnames(ts)
ts <- select(ts, -c("Age.when.diabetes.related.eye.disease.diagnosed...Instance.0_No.diabetes.related.eye.disease" ))
colnames(ts)


table(ts$Age.glaucoma.diagnosed...Instance.0)
table(is.na(ts$Age.glaucoma.diagnosed...Instance.0))
table(ts$Eye.problems.disorders...Instance.0_2 %in% ('FALSE'))
ts$Age.glaucoma.diagnosed...Instance.0 <- ifelse(ts$Eye.problems.disorders...Instance.0_2 %in% c("FALSE"),
                                                 "No.glaucoma",ts$Age.glaucoma.diagnosed...Instance.0)
table(ts$Age.glaucoma.diagnosed...Instance.0)
table(is.na(ts$Age.glaucoma.diagnosed...Instance.0))
ts <- splitMiscLevels(ts, "Age.glaucoma.diagnosed...Instance.0", c("No.glaucoma"))
colnames(ts)
ts <- select(ts, -c("Age.glaucoma.diagnosed...Instance.0_No.glaucoma"))
colnames(ts)


table(ts$Age.when.loss.of.vision.due.to.injury.or.trauma.diagnosed...Instance.0)
table(is.na(ts$Age.when.loss.of.vision.due.to.injury.or.trauma.diagnosed...Instance.0))
table(ts$Eye.problems.disorders...Instance.0_3 %in% ('FALSE'))
ts$Age.when.loss.of.vision.due.to.injury.or.trauma.diagnosed...Instance.0 <- ifelse(ts$Eye.problems.disorders...Instance.0_3 %in% c("FALSE"),
                                                                                    "No.injury.or.trauma",ts$Age.when.loss.of.vision.due.to.injury.or.trauma.diagnosed...Instance.0)
table(ts$Age.when.loss.of.vision.due.to.injury.or.trauma.diagnosed...Instance.0)
table(is.na(ts$Age.when.loss.of.vision.due.to.injury.or.trauma.diagnosed...Instance.0))
ts <- splitMiscLevels(ts, "Age.when.loss.of.vision.due.to.injury.or.trauma.diagnosed...Instance.0", c("No.injury.or.trauma"))
colnames(ts)
ts <- select(ts, -c("Age.when.loss.of.vision.due.to.injury.or.trauma.diagnosed...Instance.0_No.injury.or.trauma"))
colnames(ts)


table(ts$Age.cataract.diagnosed...Instance.0)
table(is.na(ts$Age.cataract.diagnosed...Instance.0))
table(ts$Eye.problems.disorders...Instance.0_4 %in% ('FALSE'))
ts$Age.cataract.diagnosed...Instance.0 <- ifelse(ts$Eye.problems.disorders...Instance.0_4 %in% c("FALSE"),
                                                 "No.cataract",ts$Age.cataract.diagnosed...Instance.0)
table(ts$Age.cataract.diagnosed...Instance.0)
table(is.na(ts$Age.cataract.diagnosed...Instance.0))
ts <- splitMiscLevels(ts, "Age.cataract.diagnosed...Instance.0", c("No.cataract"))
colnames(ts)
ts <- select(ts, -c("Age.cataract.diagnosed...Instance.0_No.cataract"))
colnames(ts)


table(ts$Age.macular.degeneration.diagnosed...Instance.0)
table(is.na(ts$Age.macular.degeneration.diagnosed...Instance.0))
table(ts$Eye.problems.disorders...Instance.0_5 %in% ('FALSE'))
ts$Age.macular.degeneration.diagnosed...Instance.0 <- ifelse(ts$Eye.problems.disorders...Instance.0_5 %in% c("FALSE"),
                                                             "No.macular.degeneration",ts$Age.macular.degeneration.diagnosed...Instance.0)
table(ts$Age.macular.degeneration.diagnosed...Instance.0)
table(is.na(ts$Age.macular.degeneration.diagnosed...Instance.0))
ts <- splitMiscLevels(ts, "Age.macular.degeneration.diagnosed...Instance.0", c("No.macular.degeneration"))
colnames(ts)
ts <- select(ts, -c("Age.macular.degeneration.diagnosed...Instance.0_No.macular.degeneration"))
colnames(ts)


table(ts$Age.other.serious.eye.condition.diagnosed...Instance.0)
table(is.na(ts$Age.other.serious.eye.condition.diagnosed...Instance.0))
table(ts$Eye.problems.disorders...Instance.0_6 %in% ('FALSE'))
ts$Age.other.serious.eye.condition.diagnosed...Instance.0 <- ifelse(ts$Eye.problems.disorders...Instance.0_6 %in% c("FALSE"),
                                                                    "No.other.serious.eye.condition",ts$Age.other.serious.eye.condition.diagnosed...Instance.0)
table(ts$Age.other.serious.eye.condition.diagnosed...Instance.0)
table(is.na(ts$Age.other.serious.eye.condition.diagnosed...Instance.0))
ts <- splitMiscLevels(ts, "Age.other.serious.eye.condition.diagnosed...Instance.0", c("No.other.serious.eye.condition"))
colnames(ts)
ts <- select(ts, -c("Age.other.serious.eye.condition.diagnosed...Instance.0_No.other.serious.eye.condition"))
colnames(ts)


#mouth
table(ts$Mouth.teeth.dental.problems...Instance.0)
table(is.na(ts$Mouth.teeth.dental.problems...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Mouth.teeth.dental.problems...Instance.0")
colnames(ts)


#general health
table(ts$Overall.health.rating...Instance.0)
table(is.na(ts$Overall.health.rating...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Overall.health.rating...Instance.0")
colnames(ts)


table(ts$Long.standing.illness..disability.or.infirmity...Instance.0)
table(is.na(ts$Long.standing.illness..disability.or.infirmity...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Long.standing.illness..disability.or.infirmity...Instance.0")
colnames(ts)


table(ts$Falls.in.the.last.year...Instance.0)
table(is.na(ts$Falls.in.the.last.year...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Falls.in.the.last.year...Instance.0")
colnames(ts)


table(ts$Weight.change.compared.with.1.year.ago...Instance.0)
table(is.na(ts$Weight.change.compared.with.1.year.ago...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Weight.change.compared.with.1.year.ago...Instance.0")
colnames(ts)


#breathing
table(ts$Wheeze.or.whistling.in.the.chest.in.last.year...Instance.0)
table(is.na(ts$Wheeze.or.whistling.in.the.chest.in.last.year...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Wheeze.or.whistling.in.the.chest.in.last.year...Instance.0")
colnames(ts)


table(ts$Shortness.of.breath.walking.on.level.ground...Instance.0)
table(is.na(ts$Shortness.of.breath.walking.on.level.ground...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Shortness.of.breath.walking.on.level.ground...Instance.0")
colnames(ts)


#claudication and peripheral artery disease
table(ts$Leg.pain.on.walking...Instance.0)
table(is.na(ts$Leg.pain.on.walking...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Leg.pain.on.walking...Instance.0")
colnames(ts)


table(ts$Leg.pain.when.standing.still.or.sitting...Instance.0)
table(is.na(ts$Leg.pain.when.standing.still.or.sitting...Instance.0))
ts$Leg.pain.when.standing.still.or.sitting...Instance.0 <- ifelse(ts_backup$Leg.pain.on.walking...Instance.0 %in% c("0"),
                                                                  'No.pain.walking',ts$Leg.pain.when.standing.still.or.sitting...Instance.0)
table(ts$Leg.pain.when.standing.still.or.sitting...Instance.0)
table(is.na(ts$Leg.pain.when.standing.still.or.sitting...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Leg.pain.when.standing.still.or.sitting...Instance.0")
colnames(ts)
ts <- select(ts, -c("Leg.pain.when.standing.still.or.sitting...Instance.0_No.pain.walking"))
colnames(ts)


table(ts$Leg.pain.in.calf.calves...Instance.0)
table(is.na(ts$Leg.pain.in.calf.calves...Instance.0))
ts$Leg.pain.in.calf.calves...Instance.0 <- ifelse(ts_backup$Leg.pain.on.walking...Instance.0 %in% c("0"),
                                                  'No.pain.walking',ts$Leg.pain.in.calf.calves...Instance.0)
table(ts$Leg.pain.in.calf.calves...Instance.0)
table(is.na(ts$Leg.pain.in.calf.calves...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Leg.pain.in.calf.calves...Instance.0")
colnames(ts)
ts <- select(ts, -c("Leg.pain.in.calf.calves...Instance.0_No.pain.walking"))
colnames(ts)


table(ts$Leg.pain.when.walking.uphill.or.hurrying...Instance.0)
table(is.na(ts$Leg.pain.when.walking.uphill.or.hurrying...Instance.0))
ts$Leg.pain.when.walking.uphill.or.hurrying...Instance.0 <- ifelse(ts_backup$Leg.pain.on.walking...Instance.0 %in% c("0"),
                                                                   'No.pain.walking',ts$Leg.pain.when.walking.uphill.or.hurrying...Instance.0)
table(ts$Leg.pain.when.walking.uphill.or.hurrying...Instance.0)
table(is.na(ts$Leg.pain.when.walking.uphill.or.hurrying...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Leg.pain.when.walking.uphill.or.hurrying...Instance.0")
colnames(ts)
ts <- select(ts, -c("Leg.pain.when.walking.uphill.or.hurrying...Instance.0_No.pain.walking"))
colnames(ts)


table(ts$Leg.pain.when.walking.normally...Instance.0)
table(is.na(ts$Leg.pain.when.walking.normally...Instance.0))
ts$Leg.pain.when.walking.normally...Instance.0 <- ifelse(ts_backup$Leg.pain.on.walking...Instance.0 %in% c("0"),
                                                         'No.pain.walking',ts$Leg.pain.when.walking.normally...Instance.0)
table(ts$Leg.pain.when.walking.normally...Instance.0)
table(is.na(ts$Leg.pain.when.walking.normally...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Leg.pain.when.walking.normally...Instance.0")
colnames(ts)
ts <- select(ts, -c("Leg.pain.when.walking.normally...Instance.0_No.pain.walking"))
colnames(ts)


table(ts$Leg.pain.when.walking.ever.disappears.while.walking...Instance.0)
table(is.na(ts$Leg.pain.when.walking.ever.disappears.while.walking...Instance.0))
ts$Leg.pain.when.walking.ever.disappears.while.walking...Instance.0 <- ifelse(ts_backup$Leg.pain.on.walking...Instance.0 %in% c("0"),
                                                                              'No.pain.walking',ts$Leg.pain.when.walking.ever.disappears.while.walking...Instance.0)
table(ts$Leg.pain.when.walking.ever.disappears.while.walking...Instance.0)
ts$Leg.pain.when.walking.ever.disappears.while.walking...Instance.0 <- ifelse(ts_backup$Leg.pain.when.walking.normally...Instance.0 %in% c("0"),
                                                                              'No.pain.walking.normally',ts$Leg.pain.when.walking.ever.disappears.while.walking...Instance.0)
table(ts$Leg.pain.when.walking.ever.disappears.while.walking...Instance.0)
table(is.na(ts$Leg.pain.when.walking.ever.disappears.while.walking...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Leg.pain.when.walking.ever.disappears.while.walking...Instance.0")
colnames(ts)
ts <- select(ts, -c("Leg.pain.when.walking.ever.disappears.while.walking...Instance.0_No.pain.walking.normally",                 
                    "Leg.pain.when.walking.ever.disappears.while.walking...Instance.0_No.pain.walking"))
colnames(ts)


table(ts$Leg.pain.on.walking...action.taken...Instance.0)
table(is.na(ts$Leg.pain.on.walking...action.taken...Instance.0))
ts$Leg.pain.on.walking...action.taken...Instance.0 <- ifelse(ts_backup$Leg.pain.on.walking...Instance.0 %in% c("0"),
                                                             'No.pain.walking',ts$Leg.pain.on.walking...action.taken...Instance.0)
table(ts$Leg.pain.on.walking...action.taken...Instance.0)
table(is.na(ts$Leg.pain.on.walking...action.taken...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Leg.pain.on.walking...action.taken...Instance.0")
colnames(ts)
ts <- select(ts, -c("Leg.pain.on.walking...action.taken...Instance.0_No.pain.walking"))
colnames(ts)


table(ts$Leg.pain.on.walking...effect.of.standing.still...Instance.0)
table(is.na(ts$Leg.pain.on.walking...effect.of.standing.still...Instance.0))
ts$Leg.pain.on.walking...effect.of.standing.still...Instance.0 <- ifelse(ts_backup$Leg.pain.on.walking...Instance.0 %in% c("0"),
                                                                         'No.pain.walking',ts$Leg.pain.on.walking...effect.of.standing.still...Instance.0)
table(ts$Leg.pain.on.walking...effect.of.standing.still...Instance.0)
table(is.na(ts$Leg.pain.on.walking...effect.of.standing.still...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Leg.pain.on.walking...effect.of.standing.still...Instance.0")
colnames(ts)
ts <- select(ts, -c("Leg.pain.on.walking...effect.of.standing.still...Instance.0_No.pain.walking"))
colnames(ts)


table(ts$Surgery.on.leg.arteries..other.than.for.varicose.veins....Instance.0)
table(is.na(ts$Surgery.on.leg.arteries..other.than.for.varicose.veins....Instance.0))
ts$Surgery.on.leg.arteries..other.than.for.varicose.veins....Instance.0 <- ifelse(ts_backup$Leg.pain.on.walking...Instance.0 %in% c("0"),
                                                                                  'No.pain.walking',ts$Surgery.on.leg.arteries..other.than.for.varicose.veins....Instance.0)
table(ts$Surgery.on.leg.arteries..other.than.for.varicose.veins....Instance.0)
table(is.na(ts$Surgery.on.leg.arteries..other.than.for.varicose.veins....Instance.0))
ts <- splitLevelsIntoColumns(ts,"Surgery.on.leg.arteries..other.than.for.varicose.veins....Instance.0")
colnames(ts)
ts <- select(ts, -c("Surgery.on.leg.arteries..other.than.for.varicose.veins....Instance.0_No.pain.walking"))
colnames(ts)


table(ts$Surgery.amputation.of.toe.or.leg...Instance.0)
table(is.na(ts$Surgery.amputation.of.toe.or.leg...Instance.0))
ts$Surgery.amputation.of.toe.or.leg...Instance.0 <- ifelse(ts_backup$Leg.pain.on.walking...Instance.0 %in% c("0"),
                                                           'No.pain.walking',ts$Surgery.amputation.of.toe.or.leg...Instance.0)
table(ts$Surgery.amputation.of.toe.or.leg...Instance.0)
table(is.na(ts$Surgery.amputation.of.toe.or.leg...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Surgery.amputation.of.toe.or.leg...Instance.0")
colnames(ts)
ts <- select(ts, -c("Surgery.amputation.of.toe.or.leg...Instance.0_No.pain.walking"))
colnames(ts)


#pain
table(ts$Pain.type.s..experienced.in.last.month...Instance.0)
table(is.na(ts$Pain.type.s..experienced.in.last.month...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Pain.type.s..experienced.in.last.month...Instance.0")
colnames(ts)


table(ts$Headaches.for.3..months...Instance.0)
table(is.na(ts$Headaches.for.3..months...Instance.0))
table(ts$Pain.type.s..experienced.in.last.month...Instance.0_1 %in% ('FALSE'))
ts$Headaches.for.3..months...Instance.0 <- ifelse(ts$Pain.type.s..experienced.in.last.month...Instance.0_1 %in% c("FALSE"),
                                                  "No.headache",ts$Headaches.for.3..months...Instance.0)
table(ts$Headaches.for.3..months...Instance.0)
table(is.na(ts$Headaches.for.3..months...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Headaches.for.3..months...Instance.0")
colnames(ts)
ts <- select(ts, -c("Headaches.for.3..months...Instance.0_No.headache"))
colnames(ts)


table(ts$Facial.pains.for.3..months...Instance.0)
table(is.na(ts$Facial.pains.for.3..months...Instance.0))
table(ts$Pain.type.s..experienced.in.last.month...Instance.0_2 %in% ('FALSE'))
ts$Facial.pains.for.3..months...Instance.0 <- ifelse(ts$Pain.type.s..experienced.in.last.month...Instance.0_2 %in% c("FALSE"),
                                                     "No.facial.pain",ts$Facial.pains.for.3..months...Instance.0)
table(ts$Facial.pains.for.3..months...Instance.0)
table(is.na(ts$Facial.pains.for.3..months...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Facial.pains.for.3..months...Instance.0")
colnames(ts)
ts <- select(ts, -c("Facial.pains.for.3..months...Instance.0_No.facial.pain"))
colnames(ts)


table(ts$Neck.shoulder.pain.for.3..months...Instance.0)
table(is.na(ts$Neck.shoulder.pain.for.3..months...Instance.0))
table(ts$Pain.type.s..experienced.in.last.month...Instance.0_3 %in% ('FALSE'))
ts$Neck.shoulder.pain.for.3..months...Instance.0 <- ifelse(ts$Pain.type.s..experienced.in.last.month...Instance.0_3 %in% c("FALSE"),
                                                           "No.neck.shoulder.pain",ts$Neck.shoulder.pain.for.3..months...Instance.0)
table(ts$Neck.shoulder.pain.for.3..months...Instance.0)
table(is.na(ts$Neck.shoulder.pain.for.3..months...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Neck.shoulder.pain.for.3..months...Instance.0")
colnames(ts)
ts <- select(ts, -c("Neck.shoulder.pain.for.3..months...Instance.0_No.neck.shoulder.pain"))
colnames(ts)


table(ts$Back.pain.for.3..months...Instance.0)
table(is.na(ts$Back.pain.for.3..months...Instance.0))
table(ts$Pain.type.s..experienced.in.last.month...Instance.0_4 %in% ('FALSE'))
ts$Back.pain.for.3..months...Instance.0 <- ifelse(ts$Pain.type.s..experienced.in.last.month...Instance.0_4 %in% c("FALSE"),
                                                  "No.back.pain",ts$Back.pain.for.3..months...Instance.0)
table(ts$Back.pain.for.3..months...Instance.0)
table(is.na(ts$Back.pain.for.3..months...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Back.pain.for.3..months...Instance.0")
colnames(ts)
ts <- select(ts, -c("Back.pain.for.3..months...Instance.0_No.back.pain"))
colnames(ts)


table(ts$Stomach.abdominal.pain.for.3..months...Instance.0)
table(is.na(ts$Stomach.abdominal.pain.for.3..months...Instance.0))
table(ts$Pain.type.s..experienced.in.last.month...Instance.0_5 %in% ('FALSE'))
ts$Stomach.abdominal.pain.for.3..months...Instance.0 <- ifelse(ts$Pain.type.s..experienced.in.last.month...Instance.0_5 %in% c("FALSE"),
                                                               "No.stomachache",ts$Stomach.abdominal.pain.for.3..months...Instance.0)
table(ts$Stomach.abdominal.pain.for.3..months...Instance.0)
table(is.na(ts$Stomach.abdominal.pain.for.3..months...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Stomach.abdominal.pain.for.3..months...Instance.0")
colnames(ts)
ts <- select(ts, -c("Stomach.abdominal.pain.for.3..months...Instance.0_No.stomachache"))
colnames(ts)


table(ts$Hip.pain.for.3..months...Instance.0)
table(is.na(ts$Hip.pain.for.3..months...Instance.0))
table(ts$Pain.type.s..experienced.in.last.month...Instance.0_6 %in% ('FALSE'))
ts$Hip.pain.for.3..months...Instance.0 <- ifelse(ts$Pain.type.s..experienced.in.last.month...Instance.0_6 %in% c("FALSE"),
                                                 "No.hip.pain",ts$Hip.pain.for.3..months...Instance.0)
table(ts$Hip.pain.for.3..months...Instance.0)
table(is.na(ts$Hip.pain.for.3..months...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Hip.pain.for.3..months...Instance.0")
colnames(ts)
ts <- select(ts, -c("Hip.pain.for.3..months...Instance.0_No.hip.pain"))
colnames(ts)


table(ts$Knee.pain.for.3..months...Instance.0)
table(is.na(ts$Knee.pain.for.3..months...Instance.0))
table(ts$Pain.type.s..experienced.in.last.month...Instance.0_7 %in% ('FALSE'))
ts$Knee.pain.for.3..months...Instance.0 <- ifelse(ts$Pain.type.s..experienced.in.last.month...Instance.0_7 %in% c("FALSE"),
                                                  "No.knee.pain",ts$Knee.pain.for.3..months...Instance.0)
table(ts$Knee.pain.for.3..months...Instance.0)
table(is.na(ts$Knee.pain.for.3..months...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Knee.pain.for.3..months...Instance.0")
colnames(ts)
ts <- select(ts, -c("Knee.pain.for.3..months...Instance.0_No.knee.pain"))
colnames(ts)


table(ts$General.pain.for.3..months...Instance.0)
table(is.na(ts$General.pain.for.3..months...Instance.0))
table(ts$Pain.type.s..experienced.in.last.month...Instance.0_8 %in% ('FALSE'))
ts$General.pain.for.3..months...Instance.0 <- ifelse(ts$Pain.type.s..experienced.in.last.month...Instance.0_8 %in% c("FALSE"),
                                                     "No.general.pain",ts$General.pain.for.3..months...Instance.0)
table(ts$General.pain.for.3..months...Instance.0)
table(is.na(ts$General.pain.for.3..months...Instance.0))
ts <- splitLevelsIntoColumns(ts,"General.pain.for.3..months...Instance.0")
colnames(ts)
ts <- select(ts, -c("General.pain.for.3..months...Instance.0_No.general.pain"))
colnames(ts)


#chest pain
table(ts$Chest.pain.or.discomfort...Instance.0)
table(is.na(ts$Chest.pain.or.discomfort...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Chest.pain.or.discomfort...Instance.0")
colnames(ts)


table(ts$Chest.pain.or.discomfort.walking.normally...Instance.0)
table(is.na(ts$Chest.pain.or.discomfort.walking.normally...Instance.0))
ts$Chest.pain.or.discomfort.walking.normally...Instance.0 <- ifelse(ts_backup$Chest.pain.or.discomfort...Instance.0 %in% c("0"),
                                                                    "No.chest.pain",ts$Chest.pain.or.discomfort.walking.normally...Instance.0)
table(ts$Chest.pain.or.discomfort.walking.normally...Instance.0)
table(is.na(ts$Chest.pain.or.discomfort.walking.normally...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Chest.pain.or.discomfort.walking.normally...Instance.0")
colnames(ts)
ts <- select(ts, -c("Chest.pain.or.discomfort.walking.normally...Instance.0_No.chest.pain"))
colnames(ts)


table(ts$Chest.pain.or.discomfort.when.walking.uphill.or.hurrying...Instance.0)
table(is.na(ts$Chest.pain.or.discomfort.when.walking.uphill.or.hurrying...Instance.0))
ts$Chest.pain.or.discomfort.when.walking.uphill.or.hurrying...Instance.0 <- ifelse(ts_backup$Chest.pain.or.discomfort...Instance.0 %in% c("0"),
                                                                                   "No.chest.pain",ts$Chest.pain.or.discomfort.when.walking.uphill.or.hurrying...Instance.0)
table(ts$Chest.pain.or.discomfort.when.walking.uphill.or.hurrying...Instance.0)
ts$Chest.pain.or.discomfort.when.walking.uphill.or.hurrying...Instance.0 <- ifelse(ts_backup$Chest.pain.or.discomfort.walking.normally...Instance.0 %in% c("1"),
                                                                                   "Chest.pain.walking.normally",ts$Chest.pain.or.discomfort.when.walking.uphill.or.hurrying...Instance.0)
table(ts$Chest.pain.or.discomfort.when.walking.uphill.or.hurrying...Instance.0)
table(is.na(ts$Chest.pain.or.discomfort.when.walking.uphill.or.hurrying...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Chest.pain.or.discomfort.when.walking.uphill.or.hurrying...Instance.0")
colnames(ts)
ts <- select(ts, -c("Chest.pain.or.discomfort.when.walking.uphill.or.hurrying...Instance.0_No.chest.pain",                       
                    "Chest.pain.or.discomfort.when.walking.uphill.or.hurrying...Instance.0_Chest.pain.walking.normally"))
colnames(ts)


table(ts$Chest.pain.due.to.walking.ceases.when.standing.still...Instance.0)
table(is.na(ts$Chest.pain.due.to.walking.ceases.when.standing.still...Instance.0))
ts$Chest.pain.due.to.walking.ceases.when.standing.still...Instance.0 <- ifelse(ts_backup$Chest.pain.or.discomfort...Instance.0 %in% c("0"),
                                                                               "No.chest.pain",ts$Chest.pain.due.to.walking.ceases.when.standing.still...Instance.0)
table(ts$Chest.pain.due.to.walking.ceases.when.standing.still...Instance.0)
ts$Chest.pain.due.to.walking.ceases.when.standing.still...Instance.0 <- ifelse(ts_backup$Chest.pain.or.discomfort.when.walking.uphill.or.hurrying...Instance.0 %in% c("0"),
                                                                               "No.chest.pain",ts$Chest.pain.due.to.walking.ceases.when.standing.still...Instance.0)
table(ts$Chest.pain.due.to.walking.ceases.when.standing.still...Instance.0)
table(is.na(ts$Chest.pain.due.to.walking.ceases.when.standing.still...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Chest.pain.due.to.walking.ceases.when.standing.still...Instance.0")
colnames(ts)
ts <- select(ts, -c("Chest.pain.due.to.walking.ceases.when.standing.still...Instance.0_No.chest.pain"))
colnames(ts)


#cancer screening
table(ts$Ever.had.bowel.cancer.screening...Instance.0)
table(is.na(ts$Ever.had.bowel.cancer.screening...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Ever.had.bowel.cancer.screening...Instance.0")
colnames(ts)


table(ts$Most.recent.bowel.cancer.screening...Instance.0)
table(is.na(ts$Most.recent.bowel.cancer.screening...Instance.0))
ts$Most.recent.bowel.cancer.screening...Instance.0 <- ifelse(ts_backup$Ever.had.bowel.cancer.screening...Instance.0 %in% c("0"),
                                                             "No.bowel.cancer.screening",ts$Most.recent.bowel.cancer.screening...Instance.0)
table(ts$Most.recent.bowel.cancer.screening...Instance.0)
table(is.na(ts$Most.recent.bowel.cancer.screening...Instance.0))
ts <- splitMiscLevels(ts, "Most.recent.bowel.cancer.screening...Instance.0", "No.bowel.cancer.screening")
colnames(ts)
ts <- select(ts, -c("Most.recent.bowel.cancer.screening...Instance.0_No.bowel.cancer.screening"))
colnames(ts)


table(ts$Ever.had.prostate.specific.antigen..PSA..test...Instance.0)
table(is.na(ts$Ever.had.prostate.specific.antigen..PSA..test...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Ever.had.prostate.specific.antigen..PSA..test...Instance.0")
colnames(ts)


table(ts$Time.since.last.prostate.specific.antigen..PSA..test...Instance.0)
table(is.na(ts$Time.since.last.prostate.specific.antigen..PSA..test...Instance.0))
ts$Time.since.last.prostate.specific.antigen..PSA..test...Instance.0 <- ifelse(ts_backup$Sex %in% c(0),
                                                                               "Female",ts$Time.since.last.prostate.specific.antigen..PSA..test...Instance.0)
table(ts$Time.since.last.prostate.specific.antigen..PSA..test...Instance.0)
ts$Time.since.last.prostate.specific.antigen..PSA..test...Instance.0 <- ifelse(ts_backup$Ever.had.prostate.specific.antigen..PSA..test...Instance.0 %in% c("0"),
                                                                               "No.PSA.test",ts$Time.since.last.prostate.specific.antigen..PSA..test...Instance.0)
table(ts$Time.since.last.prostate.specific.antigen..PSA..test...Instance.0)
table(is.na(ts$Time.since.last.prostate.specific.antigen..PSA..test...Instance.0))
ts <- splitMiscLevels(ts, "Time.since.last.prostate.specific.antigen..PSA..test...Instance.0", c("Female", "No.PSA.test"))
colnames(ts)
ts <- select(ts, -c("Time.since.last.prostate.specific.antigen..PSA..test...Instance.0_Female",                                  
                    "Time.since.last.prostate.specific.antigen..PSA..test...Instance.0_No.PSA.test"))
colnames(ts)


#operations
table(ts$Had.major.operations...Instance.0)
table(is.na(ts$Had.major.operations...Instance.0))

table(ts$Had.other.major.operations...Instance.0)
table(is.na(ts$Had.other.major.operations...Instance.0))

#merge two sexed columns
ts$Combined.sex.major.operations <- ifelse(is.na(ts$Had.major.operations...Instance.0), 
                                           "", as.character(ts$Had.major.operations...Instance.0))
table(ts$Combined.sex.major.operations)
ts$Combined.sex.major.operations  <- paste(ts$Combined.sex.major.operations , ifelse(is.na(ts$Had.other.major.operations...Instance.0), 
                                                                                     "", as.character(ts$Had.other.major.operations...Instance.0)), sep = "")
table(ts$Combined.sex.major.operations)
ts$Combined.sex.major.operations <- ifelse(ts$Combined.sex.major.operations %in% c(""),
                                           NA,ts$Combined.sex.major.operations)
table(ts$Combined.sex.major.operations)
table(is.na(ts$Combined.sex.major.operations))

#exclude sexed operation columns
ts <- ts %>%
  select(-Had.major.operations...Instance.0,
         -Had.other.major.operations...Instance.0)
ts <- splitLevelsIntoColumns(ts, "Combined.sex.major.operations")
colnames(ts)


#medical conditions
table(ts$Vascular.heart.problems.diagnosed.by.doctor...Instance.0)
table(is.na(ts$Vascular.heart.problems.diagnosed.by.doctor...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Vascular.heart.problems.diagnosed.by.doctor...Instance.0")
colnames(ts)


table(ts$Age.heart.attack.diagnosed...Instance.0)
table(is.na(ts$Age.heart.attack.diagnosed...Instance.0))
table(ts$Vascular.heart.problems.diagnosed.by.doctor...Instance.0_1 %in% ('FALSE'))
ts$Age.heart.attack.diagnosed...Instance.0 <- ifelse(ts$Vascular.heart.problems.diagnosed.by.doctor...Instance.0_1 %in% c("FALSE"),
                                                     "No.heart.attack",ts$Age.heart.attack.diagnosed...Instance.0)
table(ts$Age.heart.attack.diagnosed...Instance.0)
table(is.na(ts$Age.heart.attack.diagnosed...Instance.0))
ts <- splitMiscLevels(ts, "Age.heart.attack.diagnosed...Instance.0", c("No.heart.attack"))
colnames(ts)
ts <- select(ts, -c("Age.heart.attack.diagnosed...Instance.0_No.heart.attack"))
colnames(ts)


table(ts$Age.angina.diagnosed...Instance.0)
table(is.na(ts$Age.angina.diagnosed...Instance.0))
table(ts$Vascular.heart.problems.diagnosed.by.doctor...Instance.0_2 %in% ('FALSE'))
ts$Age.angina.diagnosed...Instance.0 <- ifelse(ts$Vascular.heart.problems.diagnosed.by.doctor...Instance.0_2 %in% c("FALSE"),
                                               "No.angina",ts$Age.angina.diagnosed...Instance.0)
table(ts$Age.angina.diagnosed...Instance.0)
table(is.na(ts$Age.angina.diagnosed...Instance.0))
ts <- splitMiscLevels(ts, "Age.angina.diagnosed...Instance.0", c("No.angina"))
colnames(ts)
ts <- select(ts, -c("Age.angina.diagnosed...Instance.0_No.angina"))
colnames(ts)


table(ts$Age.stroke.diagnosed...Instance.0)
table(is.na(ts$Age.stroke.diagnosed...Instance.0))
table(ts$Vascular.heart.problems.diagnosed.by.doctor...Instance.0_3 %in% ('FALSE'))
ts$Age.stroke.diagnosed...Instance.0 <- ifelse(ts$Vascular.heart.problems.diagnosed.by.doctor...Instance.0_3 %in% c("FALSE"),
                                               "No.stroke",ts$Age.stroke.diagnosed...Instance.0)
table(ts$Age.stroke.diagnosed...Instance.0)
table(is.na(ts$Age.stroke.diagnosed...Instance.0))
ts <- splitMiscLevels(ts, "Age.stroke.diagnosed...Instance.0", c("No.stroke"))
colnames(ts)
ts <- select(ts, -c("Age.stroke.diagnosed...Instance.0_No.stroke"))
colnames(ts)


table(ts$Age.high.blood.pressure.diagnosed...Instance.0)
table(is.na(ts$Age.high.blood.pressure.diagnosed...Instance.0))
table(ts$Vascular.heart.problems.diagnosed.by.doctor...Instance.0_4 %in% ('FALSE'))
ts$Age.high.blood.pressure.diagnosed...Instance.0 <- ifelse(ts$Vascular.heart.problems.diagnosed.by.doctor...Instance.0_4 %in% c("FALSE"),
                                                            "No.high.BP",ts$Age.high.blood.pressure.diagnosed...Instance.0)
table(ts$Age.high.blood.pressure.diagnosed...Instance.0)
table(is.na(ts$Age.high.blood.pressure.diagnosed...Instance.0))
ts <- splitMiscLevels(ts, "Age.high.blood.pressure.diagnosed...Instance.0", c("No.high.BP"))
colnames(ts)
ts <- select(ts, -c("Age.high.blood.pressure.diagnosed...Instance.0_No.high.BP"))
colnames(ts)


table(ts$Blood.clot..DVT..bronchitis..emphysema..asthma..rhinitis..eczema..allergy.diagnosed.by.doctor...Instance.0)
table(is.na(ts$Blood.clot..DVT..bronchitis..emphysema..asthma..rhinitis..eczema..allergy.diagnosed.by.doctor...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Blood.clot..DVT..bronchitis..emphysema..asthma..rhinitis..eczema..allergy.diagnosed.by.doctor...Instance.0")
colnames(ts)


table(ts$Age.deep.vein.thrombosis..DVT..blood.clot.in.leg..diagnosed...Instance.0)
table(is.na(ts$Age.deep.vein.thrombosis..DVT..blood.clot.in.leg..diagnosed...Instance.0))
table(ts$Blood.clot..DVT..bronchitis..emphysema..asthma..rhinitis..eczema..allergy.diagnosed.by.doctor...Instance.0_5 %in% ('FALSE'))
ts$Age.deep.vein.thrombosis..DVT..blood.clot.in.leg..diagnosed...Instance.0 <- ifelse(ts$Blood.clot..DVT..bronchitis..emphysema..asthma..rhinitis..eczema..allergy.diagnosed.by.doctor...Instance.0_5 %in% c("FALSE"),
                                                                                      "No.DVT",ts$Age.deep.vein.thrombosis..DVT..blood.clot.in.leg..diagnosed...Instance.0)
table(ts$Age.deep.vein.thrombosis..DVT..blood.clot.in.leg..diagnosed...Instance.0)
table(is.na(ts$Age.deep.vein.thrombosis..DVT..blood.clot.in.leg..diagnosed...Instance.0))
ts <- splitMiscLevels(ts, "Age.deep.vein.thrombosis..DVT..blood.clot.in.leg..diagnosed...Instance.0", c("No.DVT"))
colnames(ts)
ts <- select(ts, -c("Age.deep.vein.thrombosis..DVT..blood.clot.in.leg..diagnosed...Instance.0_No.DVT"))
colnames(ts)


table(ts$Age.pulmonary.embolism..blood.clot.in.lung..diagnosed...Instance.0)
table(is.na(ts$Age.pulmonary.embolism..blood.clot.in.lung..diagnosed...Instance.0))
table(ts$Blood.clot..DVT..bronchitis..emphysema..asthma..rhinitis..eczema..allergy.diagnosed.by.doctor...Instance.0_7 %in% ('FALSE'))
ts$Age.pulmonary.embolism..blood.clot.in.lung..diagnosed...Instance.0 <- ifelse(ts$Blood.clot..DVT..bronchitis..emphysema..asthma..rhinitis..eczema..allergy.diagnosed.by.doctor...Instance.0_7 %in% c("FALSE"),
                                                                                "No.PE",ts$Age.pulmonary.embolism..blood.clot.in.lung..diagnosed...Instance.0)
table(ts$Age.pulmonary.embolism..blood.clot.in.lung..diagnosed...Instance.0)
table(is.na(ts$Age.pulmonary.embolism..blood.clot.in.lung..diagnosed...Instance.0))
ts <- splitMiscLevels(ts, "Age.pulmonary.embolism..blood.clot.in.lung..diagnosed...Instance.0", c("No.PE"))
colnames(ts)
ts <- select(ts, -c("Age.pulmonary.embolism..blood.clot.in.lung..diagnosed...Instance.0_No.PE"))
colnames(ts)


table(ts$Age.emphysema.chronic.bronchitis.diagnosed...Instance.0)
table(is.na(ts$Age.emphysema.chronic.bronchitis.diagnosed...Instance.0))
table(ts$Blood.clot..DVT..bronchitis..emphysema..asthma..rhinitis..eczema..allergy.diagnosed.by.doctor...Instance.0_6 %in% ('FALSE'))
ts$Age.emphysema.chronic.bronchitis.diagnosed...Instance.0 <- ifelse(ts$Blood.clot..DVT..bronchitis..emphysema..asthma..rhinitis..eczema..allergy.diagnosed.by.doctor...Instance.0_6 %in% c("FALSE"),
                                                                     "No.emphysema",ts$Age.emphysema.chronic.bronchitis.diagnosed...Instance.0)
table(ts$Age.emphysema.chronic.bronchitis.diagnosed...Instance.0)
table(is.na(ts$Age.emphysema.chronic.bronchitis.diagnosed...Instance.0))
ts <- splitMiscLevels(ts, "Age.emphysema.chronic.bronchitis.diagnosed...Instance.0", c("No.emphysema"))
colnames(ts)
ts <- select(ts, -c("Age.emphysema.chronic.bronchitis.diagnosed...Instance.0_No.emphysema"))
colnames(ts)


table(ts$Age.asthma.diagnosed...Instance.0)
table(is.na(ts$Age.asthma.diagnosed...Instance.0))
table(ts$Blood.clot..DVT..bronchitis..emphysema..asthma..rhinitis..eczema..allergy.diagnosed.by.doctor...Instance.0_8 %in% ('FALSE'))
ts$Age.asthma.diagnosed...Instance.0 <- ifelse(ts$Blood.clot..DVT..bronchitis..emphysema..asthma..rhinitis..eczema..allergy.diagnosed.by.doctor...Instance.0_8 %in% c("FALSE"),
                                               "No.asthma",ts$Age.asthma.diagnosed...Instance.0)
table(ts$Age.asthma.diagnosed...Instance.0)
table(is.na(ts$Age.asthma.diagnosed...Instance.0))
ts <- splitMiscLevels(ts, "Age.asthma.diagnosed...Instance.0", c("No.asthma"))
colnames(ts)
ts <- select(ts, -c("Age.asthma.diagnosed...Instance.0_No.asthma"))
colnames(ts)


table(ts$Age.hay.fever..rhinitis.or.eczema.diagnosed...Instance.0)
table(is.na(ts$Age.hay.fever..rhinitis.or.eczema.diagnosed...Instance.0))
table(ts$Blood.clot..DVT..bronchitis..emphysema..asthma..rhinitis..eczema..allergy.diagnosed.by.doctor...Instance.0_9 %in% ('FALSE'))
ts$Age.hay.fever..rhinitis.or.eczema.diagnosed...Instance.0 <- ifelse(ts$Blood.clot..DVT..bronchitis..emphysema..asthma..rhinitis..eczema..allergy.diagnosed.by.doctor...Instance.0_9 %in% c("FALSE"),
                                                                      "No.hayfever",ts$Age.hay.fever..rhinitis.or.eczema.diagnosed...Instance.0)
table(ts$Age.hay.fever..rhinitis.or.eczema.diagnosed...Instance.0)
table(is.na(ts$Age.hay.fever..rhinitis.or.eczema.diagnosed...Instance.0))
ts <- splitMiscLevels(ts, "Age.hay.fever..rhinitis.or.eczema.diagnosed...Instance.0", c("No.hayfever"))
colnames(ts)
ts <- select(ts, -c("Age.hay.fever..rhinitis.or.eczema.diagnosed...Instance.0_No.hayfever"))
colnames(ts)


table(ts$Diabetes.diagnosed.by.doctor...Instance.0)
table(is.na(ts$Diabetes.diagnosed.by.doctor...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Diabetes.diagnosed.by.doctor...Instance.0")
colnames(ts)


table(ts$Gestational.diabetes.only...Instance.0)
table(is.na(ts$Gestational.diabetes.only...Instance.0))
ts$Gestational.diabetes.only...Instance.0 <- ifelse(ts_backup$Diabetes.diagnosed.by.doctor...Instance.0 %in% c("0"),
                                                    "No.diabetes",ts$Gestational.diabetes.only...Instance.0)
table(ts$Gestational.diabetes.only...Instance.0)
ts$Gestational.diabetes.only...Instance.0 <- ifelse(ts$Sex %in% c(1),
                                                    "Male",ts$Gestational.diabetes.only...Instance.0)
table(ts$Gestational.diabetes.only...Instance.0)
table(is.na(ts$Gestational.diabetes.only...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Gestational.diabetes.only...Instance.0")
colnames(ts)
ts <- select(ts, -c("Gestational.diabetes.only...Instance.0_No.diabetes",                                                             
                    "Gestational.diabetes.only...Instance.0_Male"))
colnames(ts)


table(ts$Age.diabetes.diagnosed...Instance.0)
table(is.na(ts$Age.diabetes.diagnosed...Instance.0))
ts$Age.diabetes.diagnosed...Instance.0 <- ifelse(ts_backup$Diabetes.diagnosed.by.doctor...Instance.0 %in% c("0"),
                                                 "No.diabetes",ts$Age.diabetes.diagnosed...Instance.0)
table(ts$Age.diabetes.diagnosed...Instance.0)
ts$Age.diabetes.diagnosed...Instance.0 <- ifelse(ts_backup$Gestational.diabetes.only...Instance.0 %in% c("1"),
                                                 "Gestational.diabetes",ts$Age.diabetes.diagnosed...Instance.0)
table(ts$Age.diabetes.diagnosed...Instance.0)
table(is.na(ts$Age.diabetes.diagnosed...Instance.0))
ts <- splitMiscLevels(ts, "Age.diabetes.diagnosed...Instance.0", c("No.diabetes", "Gestational.diabetes"))
colnames(ts)
ts <- select(ts, -c("Age.diabetes.diagnosed...Instance.0_No.diabetes",                                                                
                    "Age.diabetes.diagnosed...Instance.0_Gestational.diabetes"))
colnames(ts)


table(ts$Started.insulin.within.one.year.diagnosis.of.diabetes...Instance.0)
table(is.na(ts$Started.insulin.within.one.year.diagnosis.of.diabetes...Instance.0))
ts$Started.insulin.within.one.year.diagnosis.of.diabetes...Instance.0 <- ifelse(ts_backup$Diabetes.diagnosed.by.doctor...Instance.0 %in% c("0"),
                                                                                "No.diabetes",ts$Started.insulin.within.one.year.diagnosis.of.diabetes...Instance.0)
table(ts$Started.insulin.within.one.year.diagnosis.of.diabetes...Instance.0)
ts$Started.insulin.within.one.year.diagnosis.of.diabetes...Instance.0 <- ifelse(ts_backup$Gestational.diabetes.only...Instance.0 %in% c("1"),
                                                                                "Gestational.diabetes",ts$Started.insulin.within.one.year.diagnosis.of.diabetes...Instance.0)
table(ts$Started.insulin.within.one.year.diagnosis.of.diabetes...Instance.0)
table(is.na(ts$Started.insulin.within.one.year.diagnosis.of.diabetes...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Started.insulin.within.one.year.diagnosis.of.diabetes...Instance.0")
colnames(ts)
ts <- select(ts, -c("Started.insulin.within.one.year.diagnosis.of.diabetes...Instance.0_No.diabetes",                                 
                    "Started.insulin.within.one.year.diagnosis.of.diabetes...Instance.0_Gestational.diabetes"))
colnames(ts)


table(ts$Cancer.diagnosed.by.doctor...Instance.0)
table(is.na(ts$Cancer.diagnosed.by.doctor...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Cancer.diagnosed.by.doctor...Instance.0")
colnames(ts)


table(ts$Fractured.broken.bones.in.last.5.years...Instance.0)
table(is.na(ts$Fractured.broken.bones.in.last.5.years...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Fractured.broken.bones.in.last.5.years...Instance.0")
colnames(ts)


table(ts$Fractured.bone.site.s....Instance.0)
table(is.na(ts$Fractured.bone.site.s....Instance.0))
ts$Fractured.bone.site.s....Instance.0 <- ifelse(ts_backup$Fractured.broken.bones.in.last.5.years...Instance.0 %in% c("0"),
                                                 "No.fractures",ts$Fractured.bone.site.s....Instance.0)
table(ts$Fractured.bone.site.s....Instance.0)
table(is.na(ts$Fractured.bone.site.s....Instance.0))
ts <- splitLevelsIntoColumns(ts,"Fractured.bone.site.s....Instance.0")
colnames(ts)
ts <- select(ts, -c("Fractured.bone.site.s....Instance.0_No.fractures"))
colnames(ts)


table(ts$Fracture.resulting.from.simple.fall...Instance.0)
table(is.na(ts$Fracture.resulting.from.simple.fall...Instance.0))
ts$Fracture.resulting.from.simple.fall...Instance.0 <- ifelse(ts_backup$Fractured.broken.bones.in.last.5.years...Instance.0 %in% c("0"),
                                                              "No.fractures",ts$Fracture.resulting.from.simple.fall...Instance.0)
table(ts$Fracture.resulting.from.simple.fall...Instance.0)
table(is.na(ts$Fracture.resulting.from.simple.fall...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Fracture.resulting.from.simple.fall...Instance.0")
colnames(ts)
ts <- select(ts, -c("Fracture.resulting.from.simple.fall...Instance.0_No.fractures"))
colnames(ts)


table(ts$Other.serious.medical.condition.disability.diagnosed.by.doctor...Instance.0)
table(is.na(ts$Other.serious.medical.condition.disability.diagnosed.by.doctor...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Other.serious.medical.condition.disability.diagnosed.by.doctor...Instance.0")
colnames(ts)


#medication
table(ts$Medication.for.cholesterol..blood.pressure.or.diabetes...Instance.0)
table(is.na(ts$Medication.for.cholesterol..blood.pressure.or.diabetes...Instance.0))

table(ts$Medication.for.cholesterol..blood.pressure..diabetes..or.take.exogenous.hormones...Instance.0)
table(is.na(ts$Medication.for.cholesterol..blood.pressure..diabetes..or.take.exogenous.hormones...Instance.0))

#merge two sexed columns
ts$Combined.sex.medication <- ifelse(is.na(ts$Medication.for.cholesterol..blood.pressure.or.diabetes...Instance.0), 
                                     "", as.character(ts$Medication.for.cholesterol..blood.pressure.or.diabetes...Instance.0))
table(ts$Combined.sex.medication)
ts$Combined.sex.medication  <- paste(ts$Combined.sex.medication , ifelse(is.na(ts$Medication.for.cholesterol..blood.pressure..diabetes..or.take.exogenous.hormones...Instance.0), 
                                                                         "", as.character(ts$Medication.for.cholesterol..blood.pressure..diabetes..or.take.exogenous.hormones...Instance.0)), sep = "")
table(ts$Combined.sex.medication)
ts$Combined.sex.medication <- ifelse(ts$Combined.sex.medication %in% c(""),
                                     NA,ts$Combined.sex.medication)
table(ts$Combined.sex.medication)
table(is.na(ts$Combined.sex.medication))
ts <- splitLevelsIntoColumns(ts,"Combined.sex.medication")
colnames(ts)

#exclude sexed operation columns
ts <- ts %>%
  select(-Medication.for.cholesterol..blood.pressure.or.diabetes...Instance.0,
         -Medication.for.cholesterol..blood.pressure..diabetes..or.take.exogenous.hormones...Instance.0)
colnames(ts)


table(ts$Taking.other.prescription.medications...Instance.0)
table(is.na(ts$Taking.other.prescription.medications...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Taking.other.prescription.medications...Instance.0")
colnames(ts)


table(ts$Medication.for.pain.relief..constipation..heartburn...Instance.0)
table(is.na(ts$Medication.for.pain.relief..constipation..heartburn...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Medication.for.pain.relief..constipation..heartburn...Instance.0")
colnames(ts)


table(ts$Vitamin.and.mineral.supplements...Instance.0)
table(is.na(ts$Vitamin.and.mineral.supplements...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Vitamin.and.mineral.supplements...Instance.0")
colnames(ts)


table(ts$Mineral.and.other.dietary.supplements...Instance.0)
table(is.na(ts$Mineral.and.other.dietary.supplements...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Mineral.and.other.dietary.supplements...Instance.0")
colnames(ts)


#hearing
table(ts$Hearing.difficulty.problems...Instance.0)
table(is.na(ts$Hearing.difficulty.problems...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Hearing.difficulty.problems...Instance.0")
colnames(ts)


table(ts$Hearing.difficulty.problems.with.background.noise...Instance.0)
table(is.na(ts$Hearing.difficulty.problems.with.background.noise...Instance.0))
ts$Hearing.difficulty.problems.with.background.noise...Instance.0 <- ifelse(ts_backup$Hearing.difficulty.problems...Instance.0 %in% c("99"),
                                                                            "Completely.deaf",ts$Hearing.difficulty.problems.with.background.noise...Instance.0)
table(ts$Hearing.difficulty.problems.with.background.noise...Instance.0)
table(is.na(ts$Hearing.difficulty.problems.with.background.noise...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Hearing.difficulty.problems.with.background.noise...Instance.0")
colnames(ts)
ts <- select(ts, -c("Hearing.difficulty.problems.with.background.noise...Instance.0_Completely.deaf"))
colnames(ts)


table(ts$Hearing.aid.user...Instance.0)
table(is.na(ts$Hearing.aid.user...Instance.0))
ts$Hearing.aid.user...Instance.0 <- ifelse(ts_backup$Hearing.difficulty.problems...Instance.0 %in% c("99"),
                                           "Completely.deaf",ts$Hearing.aid.user...Instance.0)
table(ts$Hearing.aid.user...Instance.0)
ts$Hearing.aid.user...Instance.0 <- ifelse(ts_backup$Hearing.difficulty.problems...Instance.0 %in% c("0"),
                                           "No.hearing.difficulty",ts$Hearing.aid.user...Instance.0)
table(ts$Hearing.aid.user...Instance.0)
ts$Hearing.aid.user...Instance.0 <- ifelse(ts_backup$Hearing.difficulty.problems.with.background.noise...Instance.0 %in% c("0"),
                                           "No.hearing.difficulty",ts$Hearing.aid.user...Instance.0)
table(ts$Hearing.aid.user...Instance.0)
table(is.na(ts$Hearing.aid.user...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Hearing.aid.user...Instance.0")
colnames(ts)
ts <- select(ts, -c("Hearing.aid.user...Instance.0_No.hearing.difficulty",                                                            
                    "Hearing.aid.user...Instance.0_Completely.deaf"))
colnames(ts)


table(ts$Cochlear.implant...Instance.0)
table(is.na(ts$Cochlear.implant...Instance.0))
ts$Cochlear.implant...Instance.0 <- ifelse(ts_backup$Hearing.difficulty.problems...Instance.0 %in% c("99"),
                                           "Completely.deaf",ts$Cochlear.implant...Instance.0)
table(ts$Cochlear.implant...Instance.0)
table(is.na(ts$Cochlear.implant...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Cochlear.implant...Instance.0")
colnames(ts)
ts <- select(ts, -c("Cochlear.implant...Instance.0_Completely.deaf"))
colnames(ts)


table(ts$Tinnitus...Instance.0)
table(is.na(ts$Tinnitus...Instance.0))
ts$Tinnitus...Instance.0 <- ifelse(ts_backup$Hearing.difficulty.problems...Instance.0 %in% c("99"),
                                   "Completely.deaf",ts$Tinnitus...Instance.0)
table(ts$Tinnitus...Instance.0)
table(is.na(ts$Tinnitus...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Tinnitus...Instance.0")
colnames(ts)
ts <- select(ts, -c("Tinnitus...Instance.0_Completely.deaf"))
colnames(ts)


table(ts$Tinnitus.severity.nuisance...Instance.0)
table(is.na(ts$Tinnitus.severity.nuisance...Instance.0))
ts$Tinnitus.severity.nuisance...Instance.0 <- ifelse(ts_backup$Tinnitus...Instance.0 %in% c("0"),
                                                     "No.tinnitus",ts$Tinnitus.severity.nuisance...Instance.0)
table(ts$Tinnitus.severity.nuisance...Instance.0)
ts$Tinnitus.severity.nuisance...Instance.0 <- ifelse(ts_backup$Hearing.difficulty.problems...Instance.0 %in% c("99"),
                                                     "Completely.deaf",ts$Tinnitus.severity.nuisance...Instance.0)
table(ts$Tinnitus.severity.nuisance...Instance.0)
table(is.na(ts$Tinnitus.severity.nuisance...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Tinnitus.severity.nuisance...Instance.0")
colnames(ts)
ts <- select(ts, -c("Tinnitus.severity.nuisance...Instance.0_No.tinnitus",                                                            
                    "Tinnitus.severity.nuisance...Instance.0_Completely.deaf"))
colnames(ts)


table(ts$Noisy.workplace...Instance.0)
table(is.na(ts$Noisy.workplace...Instance.0))
ts$Noisy.workplace...Instance.0 <- ifelse(ts_backup$Hearing.difficulty.problems...Instance.0 %in% c("99"),
                                          "Completely.deaf",ts$Noisy.workplace...Instance.0)
table(ts$Noisy.workplace...Instance.0)
table(is.na(ts$Noisy.workplace...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Noisy.workplace...Instance.0")
colnames(ts)
ts <- select(ts, -c("Noisy.workplace...Instance.0_Completely.deaf"))
colnames(ts)


table(ts$Loud.music.exposure.frequency...Instance.0)
table(is.na(ts$Loud.music.exposure.frequency...Instance.0))
ts$Loud.music.exposure.frequency...Instance.0 <- ifelse(ts_backup$Hearing.difficulty.problems...Instance.0 %in% c("99"),
                                                        "Completely.deaf",ts$Loud.music.exposure.frequency...Instance.0)
table(ts$Loud.music.exposure.frequency...Instance.0)
table(is.na(ts$Loud.music.exposure.frequency...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Loud.music.exposure.frequency...Instance.0")
colnames(ts)
ts <- select(ts, -c("Loud.music.exposure.frequency...Instance.0_Completely.deaf"))
colnames(ts)


#male-specific factors
table(ts$Relative.age.of.first.facial.hair...Instance.0)
table(is.na(ts$Relative.age.of.first.facial.hair...Instance.0))
ts$Relative.age.of.first.facial.hair...Instance.0 <- ifelse(ts$Sex %in% c("0"),
                                                            "Female",ts$Relative.age.of.first.facial.hair...Instance.0)
table(ts$Relative.age.of.first.facial.hair...Instance.0)
table(is.na(ts$Relative.age.of.first.facial.hair...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Relative.age.of.first.facial.hair...Instance.0")
colnames(ts)
ts <- select(ts, -c("Relative.age.of.first.facial.hair...Instance.0_Female"))
colnames(ts)


table(ts$Relative.age.voice.broke...Instance.0)
table(is.na(ts$Relative.age.voice.broke...Instance.0))
ts$Relative.age.voice.broke...Instance.0 <- ifelse(ts$Sex %in% c("0"),
                                                   "Female",ts$Relative.age.voice.broke...Instance.0)
table(ts$Relative.age.voice.broke...Instance.0)
table(is.na(ts$Relative.age.voice.broke...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Relative.age.voice.broke...Instance.0")
colnames(ts)
ts <- select(ts, -c("Relative.age.voice.broke...Instance.0_Female"))
colnames(ts)


table(ts$Hair.balding.pattern...Instance.0)
table(is.na(ts$Hair.balding.pattern...Instance.0))
ts$Hair.balding.pattern...Instance.0 <- ifelse(ts$Sex %in% c("0"),
                                               "Female",ts$Hair.balding.pattern...Instance.0)
table(ts$Hair.balding.pattern...Instance.0)
table(is.na(ts$Hair.balding.pattern...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Hair.balding.pattern...Instance.0")
colnames(ts)
ts <- select(ts, -c("Hair.balding.pattern...Instance.0_Female"))
colnames(ts)


table(ts$Number.of.children.fathered...Instance.0)
table(is.na(ts$Number.of.children.fathered...Instance.0))
ts$Number.of.children.fathered...Instance.0 <- ifelse(ts$Sex %in% c("0"),
                                                      "Female",ts$Number.of.children.fathered...Instance.0)
table(ts$Number.of.children.fathered...Instance.0)
table(is.na(ts$Number.of.children.fathered...Instance.0))
ts <- splitMiscLevels(ts, "Number.of.children.fathered...Instance.0", c("Female"))
colnames(ts)
ts <- select(ts, -c("Number.of.children.fathered...Instance.0_Female"))
colnames(ts)


#female-specific factors
table(ts$Ever.had.breast.cancer.screening...mammogram...Instance.0)
table(is.na(ts$Ever.had.breast.cancer.screening...mammogram...Instance.0))
ts$Ever.had.breast.cancer.screening...mammogram...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                                       "Male",ts$Ever.had.breast.cancer.screening...mammogram...Instance.0)
table(ts$Ever.had.breast.cancer.screening...mammogram...Instance.0)
table(is.na(ts$Ever.had.breast.cancer.screening...mammogram...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Ever.had.breast.cancer.screening...mammogram...Instance.0")
colnames(ts)
ts <- select(ts, -c("Ever.had.breast.cancer.screening...mammogram...Instance.0_Male"))
colnames(ts)


table(ts$Years.since.last.breast.cancer.screening...mammogram...Instance.0)
table(is.na(ts$Years.since.last.breast.cancer.screening...mammogram...Instance.0))
ts$Years.since.last.breast.cancer.screening...mammogram...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                                               "Male",ts$Years.since.last.breast.cancer.screening...mammogram...Instance.0)
table(ts$Years.since.last.breast.cancer.screening...mammogram...Instance.0)
ts$Years.since.last.breast.cancer.screening...mammogram...Instance.0 <- ifelse(ts_backup$Ever.had.breast.cancer.screening...mammogram...Instance.0 %in% c("0"),
                                                                               "No.mammogram",ts$Years.since.last.breast.cancer.screening...mammogram...Instance.0)
table(ts$Years.since.last.breast.cancer.screening...mammogram...Instance.0)
table(is.na(ts$Years.since.last.breast.cancer.screening...mammogram...Instance.0))
ts <- splitMiscLevels(ts, "Years.since.last.breast.cancer.screening...mammogram...Instance.0", c("Male", "No.mammogram"))
colnames(ts)
ts <- select(ts, -c("Years.since.last.breast.cancer.screening...mammogram...Instance.0_Male",                                         
                    "Years.since.last.breast.cancer.screening...mammogram...Instance.0_No.mammogram"))
colnames(ts)


table(ts$Ever.had.cervical.smear.test...Instance.0)
table(is.na(ts$Ever.had.cervical.smear.test...Instance.0))
ts$Ever.had.cervical.smear.test...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                       "Male",ts$Ever.had.cervical.smear.test...Instance.0)
table(ts$Ever.had.cervical.smear.test...Instance.0)
table(is.na(ts$Ever.had.cervical.smear.test...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Ever.had.cervical.smear.test...Instance.0")
colnames(ts)
ts <- select(ts, -c("Ever.had.cervical.smear.test...Instance.0_Male"))
colnames(ts)


table(ts$Years.since.last.cervical.smear.test...Instance.0)
table(is.na(ts$Years.since.last.cervical.smear.test...Instance.0))
ts$Years.since.last.cervical.smear.test...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                               "Male",ts$Years.since.last.cervical.smear.test...Instance.0)
table(ts$Years.since.last.cervical.smear.test...Instance.0)
ts$Years.since.last.cervical.smear.test...Instance.0 <- ifelse(ts_backup$Ever.had.breast.cancer.screening...mammogram...Instance.0 %in% c("0"),
                                                               "No.cervical.smear",ts$Years.since.last.cervical.smear.test...Instance.0)
table(ts$Years.since.last.cervical.smear.test...Instance.0)
table(is.na(ts$Years.since.last.cervical.smear.test...Instance.0))
ts <- splitMiscLevels(ts, "Years.since.last.cervical.smear.test...Instance.0", c("Male", "No.cervical.smear"))
colnames(ts)
ts <- select(ts, -c("Years.since.last.cervical.smear.test...Instance.0_Male",                                                         
                    "Years.since.last.cervical.smear.test...Instance.0_No.cervical.smear"))
colnames(ts)


table(ts$Age.when.periods.started..menarche....Instance.0)
table(is.na(ts$Age.when.periods.started..menarche....Instance.0))
ts$Age.when.periods.started..menarche....Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                              "Male",ts$Age.when.periods.started..menarche....Instance.0)
table(ts$Age.when.periods.started..menarche....Instance.0)
table(is.na(ts$Age.when.periods.started..menarche....Instance.0))
ts <- splitMiscLevels(ts, "Age.when.periods.started..menarche....Instance.0", c("Male"))
colnames(ts)
ts <- select(ts, -c("Age.when.periods.started..menarche....Instance.0_Male"))
colnames(ts)


table(ts$Had.menopause...Instance.0)
table(is.na(ts$Had.menopause...Instance.0))
ts$Had.menopause...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                        "Male",ts$Had.menopause...Instance.0)
table(ts$Had.menopause...Instance.0)
table(is.na(ts$Had.menopause...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Had.menopause...Instance.0")
colnames(ts)
ts <- select(ts, -c("Had.menopause...Instance.0_Male"))
colnames(ts)
table(ts$Had.menopause...Instance.0_0)
table(is.na(ts$Had.menopause...Instance.0_0))


table(ts$Age.at.menopause..last.menstrual.period....Instance.0)
table(is.na(ts$Age.at.menopause..last.menstrual.period....Instance.0))
ts$Age.at.menopause..last.menstrual.period....Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                                   "Male",ts$Age.at.menopause..last.menstrual.period....Instance.0)
table(ts$Age.at.menopause..last.menstrual.period....Instance.0)
ts$Age.at.menopause..last.menstrual.period....Instance.0 <- ifelse(ts_backup$Had.menopause...Instance.0 %in% c("0","2","3"),
                                                                   "No.menopause",ts$Age.at.menopause..last.menstrual.period....Instance.0)
table(ts$Age.at.menopause..last.menstrual.period....Instance.0)
table(is.na(ts$Age.at.menopause..last.menstrual.period....Instance.0))
ts <- splitMiscLevels(ts, "Age.at.menopause..last.menstrual.period....Instance.0", c("Male", "No.menopause"))
colnames(ts)
ts <- select(ts, -c("Age.at.menopause..last.menstrual.period....Instance.0_Male",                                                     
                    "Age.at.menopause..last.menstrual.period....Instance.0_No.menopause"))
colnames(ts)


table(ts$Time.since.last.menstrual.period...Instance.0)
table(is.na(ts$Time.since.last.menstrual.period...Instance.0))
ts$Time.since.last.menstrual.period...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                           "Male",ts$Time.since.last.menstrual.period...Instance.0)
table(ts$Time.since.last.menstrual.period...Instance.0)
ts$Time.since.last.menstrual.period...Instance.0 <- ifelse(ts_backup$Had.menopause...Instance.0 %in% c("1","2","3"),
                                                           "Menopause",ts$Time.since.last.menstrual.period...Instance.0)
table(ts$Time.since.last.menstrual.period...Instance.0)
table(is.na(ts$Time.since.last.menstrual.period...Instance.0))
ts <- splitMiscLevels(ts, "Time.since.last.menstrual.period...Instance.0", c("Male", "Menopause"))
colnames(ts)
ts <- select(ts, -c("Time.since.last.menstrual.period...Instance.0_Male",                                                             
                    "Time.since.last.menstrual.period...Instance.0_Menopause"))
colnames(ts)


table(ts$Length.of.menstrual.cycle...Instance.0)
table(is.na(ts$Length.of.menstrual.cycle...Instance.0))
ts$Length.of.menstrual.cycle...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                    "Male",ts$Length.of.menstrual.cycle...Instance.0)
table(ts$Length.of.menstrual.cycle...Instance.0)
ts$Length.of.menstrual.cycle...Instance.0 <- ifelse(ts_backup$Had.menopause...Instance.0 %in% c("1","2","3"),
                                                    "Menopause",ts$Length.of.menstrual.cycle...Instance.0)
table(ts$Length.of.menstrual.cycle...Instance.0)
table(is.na(ts$Length.of.menstrual.cycle...Instance.0))
ts <- splitMiscLevels(ts, "Length.of.menstrual.cycle...Instance.0", c("-6","Male", "Menopause"))
colnames(ts)
ts <- select(ts, -c("Length.of.menstrual.cycle...Instance.0_Male",                                                                    
                    "Length.of.menstrual.cycle...Instance.0_Menopause"))
colnames(ts)


table(ts$Menstruating.today...Instance.0)
table(is.na(ts$Menstruating.today...Instance.0))
ts$Menstruating.today...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                             "Male",ts$Menstruating.today...Instance.0)
table(ts$Menstruating.today...Instance.0)
ts$Menstruating.today...Instance.0 <- ifelse(ts_backup$Had.menopause...Instance.0 %in% c("1","2","3"),
                                             "Menopause",ts$Menstruating.today...Instance.0)
table(ts$Menstruating.today...Instance.0)
table(is.na(ts$Menstruating.today...Instance.0))
ts <- splitLevelsIntoColumns(ts,"Menstruating.today...Instance.0")
colnames(ts)
ts <- select(ts, -c("Menstruating.today...Instance.0_Menopause",                                                                      
                    "Menstruating.today...Instance.0_Male"))
colnames(ts)


table(ts$Number.of.live.births...Instance.0)
table(is.na(ts$Number.of.live.births...Instance.0))
ts$Number.of.live.births...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                "Male",ts$Number.of.live.births...Instance.0)
table(ts$Number.of.live.births...Instance.0)
table(is.na(ts$Number.of.live.births...Instance.0))
ts <- splitMiscLevels(ts, "Number.of.live.births...Instance.0", c("Male"))
colnames(ts)
ts <- select(ts, -c("Number.of.live.births...Instance.0_Male"))
colnames(ts)


table(ts$Birth.weight.of.first.child...Instance.0)
table(is.na(ts$Birth.weight.of.first.child...Instance.0))
ts$Birth.weight.of.first.child...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                      "Male",ts$Birth.weight.of.first.child...Instance.0)
table(ts$Birth.weight.of.first.child...Instance.0)
ts$Birth.weight.of.first.child...Instance.0 <- ifelse(ts_backup$Number.of.live.births...Instance.0 %in% c("0"),
                                                      "No.children",ts$Birth.weight.of.first.child...Instance.0)
table(ts$Birth.weight.of.first.child...Instance.0)
table(is.na(ts$Birth.weight.of.first.child...Instance.0))
ts <- splitMiscLevels(ts, "Birth.weight.of.first.child...Instance.0", c("-2","Male","No.children"))
colnames(ts)
ts <- select(ts, -c("Birth.weight.of.first.child...Instance.0_Male",                                                                  
                    "Birth.weight.of.first.child...Instance.0_No.children"))
colnames(ts)


table(ts$Age.of.primiparous.women.at.birth.of.child...Instance.0)
table(is.na(ts$Age.of.primiparous.women.at.birth.of.child...Instance.0))
ts$Age.of.primiparous.women.at.birth.of.child...Instance.0 <- ifelse(ts$Age.of.primiparous.women.at.birth.of.child...Instance.0 %in% c("-4","-3"),
                                                                     NA,ts$Age.of.primiparous.women.at.birth.of.child...Instance.0)
table(is.na(ts$Age.of.primiparous.women.at.birth.of.child...Instance.0))
ts$Age.of.primiparous.women.at.birth.of.child...Instance.0 <- ifelse(!ts_backup$Number.of.live.births...Instance.0 %in% c("1") & !is.na(ts_backup$Number.of.live.births...Instance.0),
                                                                     "Not.para1",ts$Age.of.primiparous.women.at.birth.of.child...Instance.0)
table(ts$Age.of.primiparous.women.at.birth.of.child...Instance.0)
ts$Age.of.primiparous.women.at.birth.of.child...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                                     "Male",ts$Age.of.primiparous.women.at.birth.of.child...Instance.0)
table(ts$Age.of.primiparous.women.at.birth.of.child...Instance.0)
table(is.na(ts$Age.of.primiparous.women.at.birth.of.child...Instance.0))
ts <- splitMiscLevels(ts, "Age.of.primiparous.women.at.birth.of.child...Instance.0", c("Male","Not.para1"))
colnames(ts)
ts <- select(ts, -c("Age.of.primiparous.women.at.birth.of.child...Instance.0_Male",                                                  
                    "Age.of.primiparous.women.at.birth.of.child...Instance.0_Not.para1"))
colnames(ts)


table(ts$Age.at.first.live.birth...Instance.0)
table(is.na(ts$Age.at.first.live.birth...Instance.0))
ts$Age.at.first.live.birth...Instance.0 <- ifelse(ts$Age.at.first.live.birth...Instance.0 %in% c("-4","-3"),
                                                  NA,ts$Age.at.first.live.birth...Instance.0)
table(is.na(ts$Age.at.first.live.birth...Instance.0))
ts$Age.at.first.live.birth...Instance.0 <- ifelse(ts_backup$Number.of.live.births...Instance.0 %in% c("0","1"),
                                                  "Less.than.2.children",ts$Age.at.first.live.birth...Instance.0)
table(ts$Age.at.first.live.birth...Instance.0)
ts$Age.at.first.live.birth...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                  "Male",ts$Age.at.first.live.birth...Instance.0)
table(ts$Age.at.first.live.birth...Instance.0)
table(is.na(ts$Age.at.first.live.birth...Instance.0))
ts <- splitMiscLevels(ts, "Age.at.first.live.birth...Instance.0", c("Less.than.2.children","Male"))
colnames(ts)
ts <- select(ts, -c("Age.at.first.live.birth...Instance.0_Less.than.2.children",                                                      
                    "Age.at.first.live.birth...Instance.0_Male"))
colnames(ts)


table(ts$Age.at.last.live.birth...Instance.0)
table(is.na(ts$Age.at.last.live.birth...Instance.0))
ts$Age.at.last.live.birth...Instance.0 <- ifelse(ts$Age.at.last.live.birth...Instance.0 %in% c("-4","-3"),
                                                 NA,ts$Age.at.last.live.birth...Instance.0)
table(is.na(ts$Age.at.last.live.birth...Instance.0))
ts$Age.at.last.live.birth...Instance.0 <- ifelse(ts_backup$Number.of.live.births...Instance.0 %in% c("0","1"),
                                                 "Less.than.2.children",ts$Age.at.last.live.birth...Instance.0)
table(ts$Age.at.last.live.birth...Instance.0)
ts$Age.at.last.live.birth...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                 "Male",ts$Age.at.last.live.birth...Instance.0)
table(ts$Age.at.last.live.birth...Instance.0)
table(is.na(ts$Age.at.last.live.birth...Instance.0))
ts <- splitMiscLevels(ts, "Age.at.last.live.birth...Instance.0", c("Less.than.2.children","Male"))
colnames(ts)
ts <- select(ts, -c("Age.at.last.live.birth...Instance.0_Less.than.2.children",                                                       
                    "Age.at.last.live.birth...Instance.0_Male"))
colnames(ts)


table(ts$Ever.had.stillbirth..spontaneous.miscarriage.or.termination...Instance.0)
table(is.na(ts$Ever.had.stillbirth..spontaneous.miscarriage.or.termination...Instance.0))
ts$Ever.had.stillbirth..spontaneous.miscarriage.or.termination...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                                                      "Male",ts$Ever.had.stillbirth..spontaneous.miscarriage.or.termination...Instance.0)
table(ts$Ever.had.stillbirth..spontaneous.miscarriage.or.termination...Instance.0)
table(is.na(ts$Ever.had.stillbirth..spontaneous.miscarriage.or.termination...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Ever.had.stillbirth..spontaneous.miscarriage.or.termination...Instance.0")
colnames(ts)
ts <- select(ts, -c("Ever.had.stillbirth..spontaneous.miscarriage.or.termination...Instance.0_Male"))
colnames(ts)


table(ts$Number.of.stillbirths...Instance.0)
table(is.na(ts$Number.of.stillbirths...Instance.0))
ts$Number.of.stillbirths...Instance.0 <- ifelse(ts_backup$Ever.had.stillbirth..spontaneous.miscarriage.or.termination...Instance.0 %in% c("0"),
                                                "No.stillbirths",ts$Number.of.stillbirths...Instance.0)
table(ts$Number.of.stillbirths...Instance.0)
ts$Number.of.stillbirths...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                "Male",ts$Number.of.stillbirths...Instance.0)
table(ts$Number.of.stillbirths...Instance.0)
table(is.na(ts$Number.of.stillbirths...Instance.0))
ts <- splitMiscLevels(ts, "Number.of.stillbirths...Instance.0", c("No.stillbirths","Male"))
colnames(ts)
ts <- select(ts, -c("Number.of.stillbirths...Instance.0_No.stillbirths",                                                              
                    "Number.of.stillbirths...Instance.0_Male"))
colnames(ts)


table(ts$Number.of.spontaneous.miscarriages...Instance.0)
table(is.na(ts$Number.of.spontaneous.miscarriages...Instance.0))
ts$Number.of.spontaneous.miscarriages...Instance.0 <- ifelse(ts_backup$Ever.had.stillbirth..spontaneous.miscarriage.or.termination...Instance.0 %in% c("0"),
                                                             "No.miscarriages",ts$Number.of.spontaneous.miscarriages...Instance.0)
table(ts$Number.of.spontaneous.miscarriages...Instance.0)
ts$Number.of.spontaneous.miscarriages...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                             "Male",ts$Number.of.spontaneous.miscarriages...Instance.0)
table(ts$Number.of.spontaneous.miscarriages...Instance.0)
table(is.na(ts$Number.of.spontaneous.miscarriages...Instance.0))
ts <- splitMiscLevels(ts, "Number.of.spontaneous.miscarriages...Instance.0", c("No.miscarriages","Male"))
colnames(ts)
ts <- select(ts, -c("Number.of.spontaneous.miscarriages...Instance.0_No.miscarriages",                                                
                    "Number.of.spontaneous.miscarriages...Instance.0_Male"))
colnames(ts)


table(ts$Number.of.pregnancy.terminations...Instance.0)
table(is.na(ts$Number.of.pregnancy.terminations...Instance.0))
ts$Number.of.pregnancy.terminations...Instance.0 <- ifelse(ts_backup$Ever.had.stillbirth..spontaneous.miscarriage.or.termination...Instance.0 %in% c("0"),
                                                           "No.terminations",ts$Number.of.pregnancy.terminations...Instance.0)
table(ts$Number.of.pregnancy.terminations...Instance.0)
ts$Number.of.pregnancy.terminations...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                           "Male",ts$Number.of.pregnancy.terminations...Instance.0)
table(ts$Number.of.pregnancy.terminations...Instance.0)
table(is.na(ts$Number.of.pregnancy.terminations...Instance.0))
ts <- splitMiscLevels(ts, "Number.of.pregnancy.terminations...Instance.0", c("No.terminations","Male"))
colnames(ts)
ts <- select(ts, -c("Number.of.pregnancy.terminations...Instance.0_No.terminations",                                                  
                    "Number.of.pregnancy.terminations...Instance.0_Male"))
colnames(ts)


table(ts$Ever.taken.oral.contraceptive.pill...Instance.0)
table(is.na(ts$Ever.taken.oral.contraceptive.pill...Instance.0))
ts$Ever.taken.oral.contraceptive.pill...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                             "Male",ts$Ever.taken.oral.contraceptive.pill...Instance.0)
table(ts$Ever.taken.oral.contraceptive.pill...Instance.0)
table(is.na(ts$Ever.taken.oral.contraceptive.pill...Instance.0))
ts <- splitLevelsIntoColumns(ts, "Ever.taken.oral.contraceptive.pill...Instance.0")
colnames(ts)
ts <- select(ts, -c("Ever.taken.oral.contraceptive.pill...Instance.0_Male"))
colnames(ts)


table(ts$Age.started.oral.contraceptive.pill...Instance.0)
table(is.na(ts$Age.started.oral.contraceptive.pill...Instance.0))
ts$Age.started.oral.contraceptive.pill...Instance.0 <- ifelse(ts_backup$Ever.taken.oral.contraceptive.pill...Instance.0 %in% c("0"),
                                                              "No.OCP",ts$Age.started.oral.contraceptive.pill...Instance.0)
table(ts$Age.started.oral.contraceptive.pill...Instance.0)
ts$Age.started.oral.contraceptive.pill...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                              "Male",ts$Age.started.oral.contraceptive.pill...Instance.0)
table(ts$Age.started.oral.contraceptive.pill...Instance.0)
table(is.na(ts$Age.started.oral.contraceptive.pill...Instance.0))
ts <- splitMiscLevels(ts, "Age.started.oral.contraceptive.pill...Instance.0", c("No.OCP","Male"))
colnames(ts)
ts <- select(ts, -c("Age.started.oral.contraceptive.pill...Instance.0_No.OCP",                                                   
                    "Age.started.oral.contraceptive.pill...Instance.0_Male"))
colnames(ts)


table(ts$Age.when.last.used.oral.contraceptive.pill...Instance.0)
table(is.na(ts$Age.when.last.used.oral.contraceptive.pill...Instance.0))
ts$Age.when.last.used.oral.contraceptive.pill...Instance.0 <- ifelse(ts_backup$Ever.taken.oral.contraceptive.pill...Instance.0 %in% c("0"),
                                                                     "No.OCP",ts$Age.when.last.used.oral.contraceptive.pill...Instance.0)
table(ts$Age.when.last.used.oral.contraceptive.pill...Instance.0)
ts$Age.when.last.used.oral.contraceptive.pill...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                                     "Male",ts$Age.when.last.used.oral.contraceptive.pill...Instance.0)
table(ts$Age.when.last.used.oral.contraceptive.pill...Instance.0)
table(is.na(ts$Age.when.last.used.oral.contraceptive.pill...Instance.0))
ts <- splitMiscLevels(ts, "Age.when.last.used.oral.contraceptive.pill...Instance.0", c("No.OCP","Male"))
colnames(ts)
ts <- select(ts, -c("Age.when.last.used.oral.contraceptive.pill...Instance.0_No.OCP",                                                
                    "Age.when.last.used.oral.contraceptive.pill...Instance.0_Male"))
colnames(ts)


table(ts$Ever.used.hormone.replacement.therapy..HRT....Instance.0)
table(is.na(ts$Ever.used.hormone.replacement.therapy..HRT....Instance.0))
ts$Ever.used.hormone.replacement.therapy..HRT....Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                                      "Male",ts$Ever.used.hormone.replacement.therapy..HRT....Instance.0)
table(ts$Ever.used.hormone.replacement.therapy..HRT....Instance.0)
table(is.na(ts$Ever.used.hormone.replacement.therapy..HRT....Instance.0))
ts <- splitLevelsIntoColumns(ts, "Ever.used.hormone.replacement.therapy..HRT....Instance.0")
colnames(ts)
ts <- select(ts, -c("Ever.used.hormone.replacement.therapy..HRT....Instance.0_Male"))
colnames(ts)


table(ts$Age.started.hormone.replacement.therapy..HRT....Instance.0)
table(is.na(ts$Age.started.hormone.replacement.therapy..HRT....Instance.0))
ts$Age.started.hormone.replacement.therapy..HRT....Instance.0 <- ifelse(ts_backup$Ever.used.hormone.replacement.therapy..HRT....Instance.0 %in% c("0"),
                                                                        "No.HRT",ts$Age.started.hormone.replacement.therapy..HRT....Instance.0)
table(ts$Age.started.hormone.replacement.therapy..HRT....Instance.0)
ts$Age.started.hormone.replacement.therapy..HRT....Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                                        "Male",ts$Age.started.hormone.replacement.therapy..HRT....Instance.0)
table(ts$Age.started.hormone.replacement.therapy..HRT....Instance.0)
table(is.na(ts$Age.started.hormone.replacement.therapy..HRT....Instance.0))
ts <- splitMiscLevels(ts, "Age.started.hormone.replacement.therapy..HRT....Instance.0", c("No.HRT","Male"))
colnames(ts)
ts <- select(ts, -c("Age.started.hormone.replacement.therapy..HRT....Instance.0_No.HRT",                                              
                    "Age.started.hormone.replacement.therapy..HRT....Instance.0_Male"))
colnames(ts)


table(ts$Age.last.used.hormone.replacement.therapy..HRT....Instance.0)
table(is.na(ts$Age.last.used.hormone.replacement.therapy..HRT....Instance.0))
ts$Age.last.used.hormone.replacement.therapy..HRT....Instance.0 <- ifelse(ts_backup$Ever.used.hormone.replacement.therapy..HRT....Instance.0 %in% c("0"),
                                                                          "No.HRT",ts$Age.last.used.hormone.replacement.therapy..HRT....Instance.0)
table(ts$Age.last.used.hormone.replacement.therapy..HRT....Instance.0)
ts$Age.last.used.hormone.replacement.therapy..HRT....Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                                          "Male",ts$Age.last.used.hormone.replacement.therapy..HRT....Instance.0)
table(ts$Age.last.used.hormone.replacement.therapy..HRT....Instance.0)
table(is.na(ts$Age.last.used.hormone.replacement.therapy..HRT....Instance.0))
ts <- splitMiscLevels(ts, "Age.last.used.hormone.replacement.therapy..HRT....Instance.0", c("-11","No.HRT","Male"))
colnames(ts)
ts <- select(ts, -c("Age.last.used.hormone.replacement.therapy..HRT....Instance.0_No.HRT",                                           
                    "Age.last.used.hormone.replacement.therapy..HRT....Instance.0_Male"))
colnames(ts)


table(ts$Ever.had.hysterectomy..womb.removed....Instance.0)
table(is.na(ts$Ever.had.hysterectomy..womb.removed....Instance.0))
ts$Ever.had.hysterectomy..womb.removed....Instance.0 <- ifelse(ts$Ever.had.hysterectomy..womb.removed....Instance.0 %in% c("-5","-3"),
                                                               NA,ts$Ever.had.hysterectomy..womb.removed....Instance.0)
table(is.na(ts$Ever.had.hysterectomy..womb.removed....Instance.0))
ts$Ever.had.hysterectomy..womb.removed....Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                               "Male",ts$Ever.had.hysterectomy..womb.removed....Instance.0)
table(ts$Ever.had.hysterectomy..womb.removed....Instance.0)
ts$Ever.had.hysterectomy..womb.removed....Instance.0 <- ifelse(ts_backup$Had.menopause...Instance.0 %in% c("2"),
                                                               "Had.hysterectomy",ts$Ever.had.hysterectomy..womb.removed....Instance.0)
table(ts$Ever.had.hysterectomy..womb.removed....Instance.0)
table(is.na(ts$Ever.had.hysterectomy..womb.removed....Instance.0))
ts <- splitLevelsIntoColumns(ts, "Ever.had.hysterectomy..womb.removed....Instance.0")
colnames(ts)
ts <- select(ts, -c("Ever.had.hysterectomy..womb.removed....Instance.0_Male",                                                         
                    "Ever.had.hysterectomy..womb.removed....Instance.0_Had.hysterectomy"))
colnames(ts)


table(ts$Age.at.hysterectomy...Instance.0)
table(is.na(ts$Age.at.hysterectomy...Instance.0))
ts$Age.at.hysterectomy...Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                              "Male",ts$Age.at.hysterectomy...Instance.0)
table(ts$Age.at.hysterectomy...Instance.0)
ts$Age.at.hysterectomy...Instance.0 <- ifelse(ts_backup$Had.menopause...Instance.0 %in% c("0","1","3"),
                                              "No.hysterectomy",ts$Age.at.hysterectomy...Instance.0)
table(ts$Age.at.hysterectomy...Instance.0)
ts$Age.at.hysterectomy...Instance.0 <- ifelse(ts_backup$Ever.had.hysterectomy..womb.removed....Instance.0 %in% c("0"),
                                              "No.hysterectomy",ts$Age.at.hysterectomy...Instance.0)
table(ts$Age.at.hysterectomy...Instance.0)
table(is.na(ts$Age.at.hysterectomy...Instance.0))
ts <- splitMiscLevels(ts, "Age.at.hysterectomy...Instance.0", c("No.hysterectomy","Male"))
colnames(ts)
ts <- select(ts, -c("Age.at.hysterectomy...Instance.0_No.hysterectomy",                                                               
                    "Age.at.hysterectomy...Instance.0_Male"))
colnames(ts)


table(ts$Bilateral.oophorectomy..both.ovaries.removed....Instance.0)
table(is.na(ts$Bilateral.oophorectomy..both.ovaries.removed....Instance.0))
ts$Bilateral.oophorectomy..both.ovaries.removed....Instance.0 <- ifelse(ts$Bilateral.oophorectomy..both.ovaries.removed....Instance.0 %in% c("-5","-3"),
                                                                        NA,ts$Bilateral.oophorectomy..both.ovaries.removed....Instance.0)
table(is.na(ts$Bilateral.oophorectomy..both.ovaries.removed....Instance.0))
ts$Bilateral.oophorectomy..both.ovaries.removed....Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                                        "Male",ts$Bilateral.oophorectomy..both.ovaries.removed....Instance.0)
table(ts$Bilateral.oophorectomy..both.ovaries.removed....Instance.0)
table(is.na(ts$Bilateral.oophorectomy..both.ovaries.removed....Instance.0))
ts <- splitLevelsIntoColumns(ts, "Bilateral.oophorectomy..both.ovaries.removed....Instance.0")
colnames(ts)
ts <- select(ts, -c("Bilateral.oophorectomy..both.ovaries.removed....Instance.0_Male"))
colnames(ts)


table(ts$Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0)
table(is.na(ts$Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0))
ts$Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0 <- ifelse(ts$Sex %in% c("1"),
                                                                               "Male",ts$Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0)
table(ts$Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0)
ts$Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0 <- ifelse(ts_backup$Bilateral.oophorectomy..both.ovaries.removed....Instance.0 %in% c("0"),
                                                                               "No.bilateral.oophorectomy",ts$Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0)
table(ts$Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0)
table(is.na(ts$Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0))
ts <- splitMiscLevels(ts, "Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0", c("No.bilateral.oophorectomy","Male"))
colnames(ts)
ts <- select(ts, -c("Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0_No.bilateral.oophorectomy",                    
                    "Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0_Male" ))
colnames(ts)


#exclude sex column
ts <- ts %>%
  select(-Sex)


# Function to select logical columns with more than or equal to 0.1% & <= 99.9 % TRUE/FALSE ratio (equivalent of 500 measurements)
table(sapply(ts,class))
true_false_ratio <- function(x) { if (is.logical(x)) { return(mean(x, na.rm = TRUE)) } else { return(NA) } }
true_ratios <- sapply(ts, true_false_ratio)
cols_to_remove <- names(true_ratios[!is.na(true_ratios) & (true_ratios < 0.001 | true_ratios > 0.999)])
df_cleaned <- ts %>%
  select(-all_of(cols_to_remove))
colnames(df_cleaned)
# Calculate the number of NA values per column 
na_counts <- sapply(df_cleaned, function(x) sum(is.na(x))) 
# Convert the results into a data frame 
na_df <- data.frame(column = names(na_counts), na_count = na_counts) 
# Sort the data frame in descending order based on the NA count 
na_df_sorted <- na_df[order(-na_df$na_count), ] 
# View the sorted data frame
head(na_df_sorted)


write.table(df_cleaned,'Touchscreen_v1_participant.tsv', sep = '\t')
