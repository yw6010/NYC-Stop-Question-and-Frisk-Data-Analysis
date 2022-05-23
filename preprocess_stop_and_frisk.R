source('library.R')


# IMPORT RAW DATA
sqf.data.full <- foreach(year=2008:2016, .combine='rbind') %do% {
  filename <- paste0('C:/Users/wangy/WD/',year, '.csv')
  this.data <- read_csv(filename)
  # convert the last three colnames in 2013,2014,2015,2016 to lower case
  colnames<-colnames(this.data)
  if ("dettypCM" %in% colnames) {
  this.data.new <- this.data %>% 
                  rename_at(c("dettypCM","lineCM","detailCM"),.funs=tolower)
  } else{
    this.data.new<-this.data
  }
  # Also, remove the forceuse column in 2011-2016
  if ("forceuse" %in% colnames){
    this.data.new <- select(this.data.new,-forceuse)
  }
  this.data.new
}


sqf.data <- sqf.data.full
# PARSE COLUMNS
# 0) drop columns that won't be used, either because they're irrelevant or 
# seem to be coded badly
sqf.data <- sqf.data %>% select(-recstat, -officrid, -sumoffen, -compyear, 
                                -comppct, -dettypcm, -linecm, -dettypcm)

# 1) date and time (except for weird times) #### filter out rows with weird time?
sqf.data <- sqf.data %>% mutate(datestop = sprintf("%08d", as.integer(datestop)),
                                timestop = sprintf("%04d", as.integer(timestop)),
                                timestamp = mdy_hm(paste(datestop, timestop))) %>% 
  select(-datestop, -timestop)

# 2) location information and serial number
sqf.data <- sqf.data %>% mutate(precinct = as.factor(pct), xcoord = as.integer(xcoord),
                                ycoord = as.integer(ycoord), serial = ser_num) %>% 
  select(-pct, -ser_num)

# 3) recode y/n variables 
sqf.data <- sqf.data %>% mutate(frisked = recode.yn(frisked), 
                                searched = recode.yn(searched), 
                                extra.reports = recode.yn(adtlrept),
                                reason.explained = recode.yn(explnstp), 
                                others.stopped = recode.yn(othpers),
                                arrested = recode.yn(arstmade),
                                summons.issued = recode.yn(sumissue),
                                radio.run = recode.yn(radio))

# Note, in 2014.csv, adtlrept and radio still contain NAs need to replace with FALSE by assumption
sqf.data = sqf.data %>% mutate( extra.reports = replace(extra.reports,is.na(extra.reports),FALSE),
                                radio.run = replace(radio.run, is.na(radio.run),FALSE))                                

# 4) recode other binary variables and drop irrelevant variables
sqf.data <- sqf.data %>% mutate(inside = recode.io(inout), observation.period = perobs,
                                suspected.crime = convert.offense.code(detailcm),
                                officer.verbal = recode.0V(offverb),
                                officer.shield = recode.0S(offshld),
                                arrested.reason = convert.arrest.reasons(arstoffn)) %>% 
  select(-adtlrept, -explnstp, -othpers, -arstmade, -sumissue, -radio, -inout, 
         -perobs, -detailcm, -offverb, -offshld, -arstoffn)
                                
  
# 5) clean up other variables 
# location: recode 'P' (for Pedestrian, which occurs mainly after 2008) and blank as 'neither'.
sqf.data <- sqf.data %>% mutate(location.housing = 
                                  recode.factor(sqf.data.full$trhsloc, c('P', 'H', 'T'), 
                                                c('neither', 'housing', 'transit')))
sqf.data <- sqf.data %>% 
  mutate(location.housing = replace(location.housing, is.na(location.housing), 'neither')) %>% 
  select(-trhsloc)

# period of stop (in minutes)
sqf.data <- sqf.data %>% mutate(stop.length = perstop) %>% select(-perstop)

# type of id and officer in uniform
sqf.data <- sqf.data %>% 
  mutate(identification = recode.factor(typeofid, c('O','P','R','V'),
                                        c('other', 'photo', 'refused', 'verbal')),
         officer.uniform = recode.factor(offunif, c('M', 'N', 'Y'),
                                         c('N', 'N', 'Y')),
         officer.uniform = recode.yn(officer.uniform)) %>% 
  select(-typeofid, -offunif)


# 6) physical force variables
# factor Y 1, N to Y Y N and then recode them to TRUE and FALSE
sqf.data <- sqf.data %>% mutate(force.hands = recode.yn(recode.factor(pf_hands,c("Y","1","N"),c("Y","Y","N"))),
                                  force.wall = recode.yn(recode.factor(pf_wall,c("Y","1","N"),c("Y","Y","N"))),
                                  force.ground = recode.yn(recode.factor(pf_grnd,c("Y","1","N"),c("Y","Y","N"))),
                                  force.drawn = recode.yn(recode.factor(pf_drwep,c("Y","1","N"),c("Y","Y","N"))),
                                  force.pointed = recode.yn(recode.factor(pf_ptwep,c("Y","1","N"),c("Y","Y","N"))),
                                  force.baton = recode.yn(recode.factor(pf_baton,c("Y","1","N"),c("Y","Y","N"))),
                                  force.handcuffs = recode.yn(recode.factor(pf_hcuff,c("Y","1","N"),c("Y","Y","N"))),
                                  force.pepper = recode.yn(recode.factor(pf_pepsp,c("Y","1","N"),c("Y","Y","N"))),
                                  force.other = recode.yn(recode.factor(pf_other,c("Y","1","N"),c("Y","Y","N")))) %>% 
  select(-pf_hands, -pf_wall, -pf_grnd, -pf_drwep, -pf_ptwep, -pf_baton, -pf_hcuff,
         -pf_pepsp, -pf_other)

# Then, we simply replace all the NA with FALSE
sqf.data = sqf.data %>% mutate(force.hands = replace(force.hands,is.na(force.hands),FALSE),
                               force.wall = replace(force.wall,is.na(force.wall),FALSE),
                               force.ground = replace(force.ground,is.na(force.ground),FALSE),
                               force.drawn = replace(force.drawn,is.na(force.drawn),FALSE),
                               force.pointed = replace(force.pointed,is.na(force.pointed),FALSE),
                               force.baton = replace(force.baton,is.na(force.baton),FALSE),
                               force.handcuffs = replace(force.handcuffs, is.na(force.handcuffs),FALSE),
                               force.pepper = replace(force.pepper, is.na(force.pepper),FALSE),
                               force.other = replace(force.other, is.na(force.other),FALSE))

# 7) primary circumstances of stop
# factor Y 1, N to Y Y N and then recode them to TRUE and FALSE
sqf.data <- sqf.data %>% mutate(stopped.bc.object = recode.yn(recode.factor(cs_objcs,c("Y","1","N"),c("Y","Y","N"))),
                                stopped.bc.desc = recode.yn(recode.factor(cs_descr,c("Y","1","N"),c("Y","Y","N"))),
                                stopped.bc.casing = recode.yn(recode.factor(cs_casng,c("Y","1","N"),c("Y","Y","N"))),
                                stopped.bc.lookout = recode.yn(recode.factor(cs_lkout,c("Y","1","N"),c("Y","Y","N"))),
                                stopped.bc.clothing = recode.yn(recode.factor(cs_cloth,c("Y","1","N"),c("Y","Y","N"))),
                                stopped.bc.drugs = recode.yn(recode.factor(cs_drgtr,c("Y","1","N"),c("Y","Y","N"))),
                                stopped.bc.furtive = recode.yn(recode.factor(cs_furtv,c("Y","1","N"),c("Y","Y","N"))),
                                stopped.bc.violent = recode.yn(recode.factor(cs_vcrim,c("Y","1","N"),c("Y","Y","N"))),
                                stopped.bc.bulge = recode.yn(recode.factor(cs_bulge,c("Y","1","N"),c("Y","Y","N"))),
                                stopped.bc.other = recode.yn(recode.factor(cs_other,c("Y","1","N"),c("Y","Y","N")))) %>% 
  select(-cs_objcs, -cs_descr, -cs_casng, -cs_lkout, -cs_cloth, - cs_drgtr, 
         -cs_furtv, -cs_vcrim, -cs_bulge, -cs_other)

# Then, we simply replace all the NA with FALSE
sqf.data = sqf.data %>% mutate(stopped.bc.object = replace(stopped.bc.object, is.na(stopped.bc.object),FALSE),
                               stopped.bc.desc = replace(stopped.bc.desc, is.na(stopped.bc.desc),FALSE),
                               stopped.bc.casing = replace(stopped.bc.casing, is.na(stopped.bc.casing),FALSE),
                               stopped.bc.lookout =  replace(stopped.bc.lookout, is.na(stopped.bc.lookout),FALSE),
                               stopped.bc.clothing = replace(stopped.bc.clothing, is.na(stopped.bc.clothing), FALSE),
                               stopped.bc.drugs =  replace(stopped.bc.drugs, is.na(stopped.bc.drugs),FALSE),
                               stopped.bc.furtive = replace(stopped.bc.furtive, is.na(stopped.bc.furtive),FALSE),
                               stopped.bc.violent = replace(stopped.bc.violent, is.na(stopped.bc.violent),FALSE),
                               stopped.bc.bulge =  replace(stopped.bc.bulge, is.na(stopped.bc.bulge),FALSE),
                               stopped.bc.other = replace(stopped.bc.other, is.na(stopped.bc.other),FALSE))

# 8) reasons for frisk
# factor Y 1, N to Y Y N and then recode them to TRUE and FALSE
sqf.data <- sqf.data %>% mutate(frisked.bc.suspected.crime = recode.yn(recode.factor(rf_vcrim,c("Y","1","N"),c("Y","Y","N"))),
                                frisked.bc.weapons = recode.yn(recode.factor(rf_othsw,c("Y","1","N"),c("Y","Y","N"))),
                                frisked.bc.attire = recode.yn(recode.factor(rf_attir,c("Y","1","N"),c("Y","Y","N"))),
                                frisked.bc.actual.crime = recode.yn(recode.factor(rf_vcact,c("Y","1","N"),c("Y","Y","N"))),
                                frisked.bc.noncompliance = recode.yn(recode.factor(rf_rfcmp,c("Y","1","N"),c("Y","Y","N"))),
                                frisked.bc.threats = recode.yn(recode.factor(rf_verbl,c("Y","1","N"),c("Y","Y","N"))),
                                frisked.bc.prior = recode.yn(recode.factor(rf_knowl,c("Y","1","N"),c("Y","Y","N"))),
                                frisked.bc.furtive = recode.yn(recode.factor(rf_furt,c("Y","1","N"),c("Y","Y","N"))),
                                frisked.bc.bulge = recode.yn(recode.factor(rf_bulg,c("Y","1","N"),c("Y","Y","N")))) %>% 
  select(-rf_vcrim, -rf_othsw, -rf_attir, -rf_vcact, -rf_rfcmp, -rf_verbl, -rf_knowl,
         -rf_furt, -rf_bulg)

# Then, we simply replace all the NA with FALSE
sqf.data =  sqf.data %>% mutate(frisked.bc.suspected.crime = replace(frisked.bc.suspected.crime, is.na(frisked.bc.suspected.crime),FALSE),
                                frisked.bc.weapons =  replace(frisked.bc.weapons, is.na(frisked.bc.weapons),FALSE),
                                frisked.bc.attire = replace(frisked.bc.attire, is.na(frisked.bc.attire),FALSE),
                                frisked.bc.actual.crime =  replace(frisked.bc.actual.crime, is.na(frisked.bc.actual.crime),FALSE),
                                frisked.bc.noncompliance = replace(frisked.bc.noncompliance, is.na(frisked.bc.noncompliance),FALSE),
                                frisked.bc.threats = replace(frisked.bc.threats, is.na(frisked.bc.threats), FALSE),
                                frisked.bc.prior =  replace(frisked.bc.prior, is.na(frisked.bc.prior),FALSE),
                                frisked.bc.furtive = replace(frisked.bc.furtive, is.na(frisked.bc.furtive), FALSE),
                                frisked.bc.bulge = replace(frisked.bc.bulge, is.na(frisked.bc.bulge), FALSE)
                                )

# 9) secondary circumstances of stop
# factor Y 1, N to Y Y N and then recode them to TRUE and FALSE
sqf.data <- sqf.data %>% mutate(additional.report = recode.yn(recode.factor(ac_rept,c("Y","1","N"),c("Y","Y","N"))),
                                additional.investigation = recode.yn(recode.factor(ac_inves, c("Y","1","N"),c("Y","Y","N"))),
                                additional.proximity = recode.yn(recode.factor(ac_proxm,c("Y","1","N"),c("Y","Y","N"))),
                                additional.evasive = recode.yn(recode.factor(ac_evasv,c("Y","1","N"),c("Y","Y","N"))),
                                additional.associating = recode.yn(recode.factor(ac_assoc,c("Y","1","N"),c("Y","Y","N"))),
                                additional.direction = recode.yn(recode.factor(ac_cgdir,c("Y","1","N"),c("Y","Y","N"))),
                                additional.highcrime = recode.yn(recode.factor(ac_incid,c("Y","1","N"),c("Y","Y","N"))),
                                additional.time = recode.yn(recode.factor(ac_time,c("Y","1","N"),c("Y","Y","N"))),
                                additional.sights = recode.yn(recode.factor(ac_stsnd,c("Y","1","N"),c("Y","Y","N"))),
                                additional.other = recode.yn(recode.factor(ac_other,c("Y","1","N"),c("Y","Y","N")))) %>% 
  select(-ac_rept, -ac_inves, -ac_proxm, -ac_evasv, -ac_assoc, -ac_cgdir, -ac_incid,
         -ac_time, -ac_stsnd, -ac_other)

# Then, we simply replace all the NA with FALSE by assumption 
sqf.data = sqf.data %>% mutate(additional.report =  replace(additional.report, is.na(additional.report),FALSE),
                               additional.investigation = replace(additional.investigation, is.na(additional.investigation), FALSE),
                               additional.proximity = replace(additional.proximity, is.na(additional.proximity),FALSE),
                               additional.evasive = replace(additional.evasive, is.na(additional.evasive),FALSE),
                               additional.associating = replace(additional.associating, is.na(additional.associating),FALSE),
                               additional.direction = replace(additional.direction, is.na(additional.direction),FALSE),
                               additional.highcrime = replace(additional.highcrime, is.na(additional.highcrime),FALSE),
                               additional.time = replace(additional.time, is.na(additional.time),FALSE),
                               additional.sights = replace(additional.sights, is.na(additional.sights),FALSE),
                               additional.other = replace(additional.other, is.na(additional.other),FALSE))

# 10) basis of search
# factor Y 1, N to Y Y N and then recode them to TRUE and FALSE
sqf.data <- sqf.data %>% mutate(searched.hardobject = recode.yn(recode.factor(sb_hdobj,c("Y","1","N"),c("Y","Y","N"))),
                                searched.outline = recode.yn(recode.factor(sb_outln,c("Y","1","N"),c("Y","Y","N"))),
                                searched.admission = recode.yn(recode.factor(sb_admis,c("Y","1","N"),c("Y","Y","N"))),
                                searched.other = recode.yn(recode.factor(sb_other,c("Y","1","N"),c("Y","Y","N")))) %>% 
  select(-sb_hdobj, -sb_outln, -sb_admis, -sb_other)

# Then, we simply replace all the NA with FALSE by assumption
sqf.data = sqf.data %>% mutate(searched.hardobject = replace(searched.hardobject, is.na(searched.hardobject),FALSE),
                               searched.outline = replace(searched.outline, is.na(searched.outline),FALSE),
                               searched.admission = replace(searched.admission, is.na(searched.admission),FALSE),
                               searched.other = replace(searched.other, is.na(searched.other),FALSE))


# 11) results of frisk/search
# factor Y 1, N to Y Y N and then recode them to TRUE and FALSE
sqf.data <- sqf.data %>% mutate(found.contraband = recode.yn(recode.factor(contrabn,c("Y","1","N"),c("Y","Y","N"))),
                                found.pistol = recode.yn(recode.factor(pistol,c("Y","1","N"),c("Y","Y","N"))),
                                found.rifle = recode.yn(recode.factor(riflshot,c("Y","1","N"),c("Y","Y","N"))),
                                found.assault = recode.yn(recode.factor(asltweap,c("Y","1","N"),c("Y","Y","N"))),
                                found.knife = recode.yn(recode.factor(knifcuti,c("Y","1","N"),c("Y","Y","N"))),
                                found.machinegun = recode.yn(recode.factor(machgun,c("Y","1","N"),c("Y","Y","N"))),
                                found.other = recode.yn(recode.factor(othrweap,c("Y","1","N"),c("Y","Y","N")))) %>% 
  select(-contrabn, -pistol, -riflshot, -asltweap, -knifcuti, -machgun, -othrweap)


# Then, we simply replace all the NA with FALSE by assumption
sqf.data = sqf.data %>% mutate(found.contraband = replace(found.contraband, is.na(found.contraband),FALSE),
                               found.pistol = replace(found.pistol, is.na(found.pistol),FALSE),
                               found.rifle = replace(found.rifle, is.na(found.rifle),FALSE),
                               found.assault = replace(found.assault, is.na(found.assault),FALSE),
                               found.knife = replace(found.knife, is.na(found.knife),FALSE),
                               found.machinegun = replace(found.machinegun, is.na(found.machinegun),FALSE),
                               found.other = replace(found.other, is.na(found.other),FALSE))

# 12) demographics of stop subject
# sex, race, and Hispanic/non-Hispanic
sqf.data <- sqf.data %>% mutate(suspect.sex = recode.factor(sex, c('M', 'F'),
                                                            c('male', 'female')),
                                suspect.race = recode.factor(race, c('A','B','I','P','Q','W','Z'),
                                                             c('asian','Black','native american','Black Hispanic','white Hispanic','white','other')),
                                suspect.hispanic = (suspect.race %in% c('Black Hispanic','white Hispanic'))) %>% 
  select(-sex, -race)


# age and DOB
sqf.data <- sqf.data %>% mutate(suspect.age = as.integer(age), 
                                suspect.age = replace(suspect.age, suspect.age > 100, NA),
                                dob = sprintf("%08d", as.integer(dob)),
                                suspect.dob = lubridate::mdy(dob),
                                suspect.dob = replace(suspect.dob, suspect.dob=='1900-12-31', NA)) %>%
  select(-age, -dob)


# height (in feet) and weight (in lbs)
sqf.data <- sqf.data %>% mutate(suspect.height = (ht_feet + as.numeric(ht_inch)/12),
                                suspect.weight = weight,
                                suspect.weight = replace(suspect.weight, suspect.weight >= 700, NA)) %>% 
  select(-ht_feet, -ht_inch, -weight)


# hair color, eye color, and build
sqf.data <- sqf.data %>% mutate(suspect.hair = recode.factor(haircolr, 
                                                             c('BA','BK','BL','BR','DY','FR','GY', 'RD', 'SN', 'SP', 'WH', 'XX', 'ZZ'),
                                                             c('bald', 'black', 'blond', 'brown', 'dyed', 'frosted', 'gray', 'red', 'sandy', 'salt and pepper', 'white', 'unknown', 'other')),
                                suspect.eye = recode.factor(eyecolor,
                                                            c('BK','BL','BR','GY','GR','HA', 'MA', 'Z', 'ZZ', 'P', 'PK','DF', 'XX',  'MC', 'VI'),
                                                            c('black','blue','brown','gray','green','hazel', 'maroon',  'other', 'other','pink','pink', 'two different','unknown', 'unknown','violet')),
                                suspect.build = recode.factor(build,
                                                              c('H', 'M', 'T', 'U', 'Z'),
                                                              c('heavy', 'medium', 'thin', 'muscular', 'unknown'))) %>% 
  select(-haircolr, -eyecolor, -build)


# 13) add extra useful fields and filter data

# fields for weapon found or gun found
sqf.data <- sqf.data %>% mutate(found.gun = (found.pistol|found.rifle|found.assault|found.machinegun),
                                found.weapon = (found.pistol|found.rifle|found.assault|found.machinegun|found.knife|found.other))
# add a unique id
sqf.data$id <- 1:nrow(sqf.data)

# eliminate all ages except for those between 10 and 80.
sqf.data <- sqf.data %>% filter(suspect.age >= 10 & suspect.age <= 80)


# convert coordinates to lat/lon
coords <- proj4::project(list(sqf.data$xcoord, sqf.data$ycoord), nyc.proj, inverse=TRUE)
sqf.data$lat <- coords$y
sqf.data$lon <- coords$x


# 14) final useful additions/changes
# recode suspect.race for "white Hispanic" and "Black Hispanic" to "Hispanic"
levels(sqf.data$suspect.race) <- c("asian", "Black", "native.american", "Hispanic", "Hispanic", "white", "other")

# add weekday, month, and time (6 four-hour-bins denoted by 1 through 6)
sqf.data <- sqf.data %>% mutate(day = wday(timestamp, label = T, abbr = F),
                                month = month(timestamp, label = T, abbr = F),
                                time.period = case_when(
                                  hour(timestamp) < 4 ~ '1',
                                  hour(timestamp) >= 4 & hour(timestamp) < 8 ~ '2',
                                  hour(timestamp) >= 8 & hour(timestamp) < 12 ~ '3',
                                  hour(timestamp) >= 12 & hour(timestamp) < 16 ~ '4',
                                  hour(timestamp) >= 16 & hour(timestamp) < 20 ~ '5',
                                  hour(timestamp) >= 20 ~ '6',
                                ))

# drop remaining irrelevant columns
sqf.data <- sqf.data %>% select(-crimsusp, -repcmd, -revcmd, -othfeatr, -addrtyp, 
                                -rescode, -premtype, -premname, -addrnum, -stname,
                                -stinter, -crossst, -aptnum, -state, -zip, -addrpct,
                                -post, -serial)

write_csv(sqf.data, 'sqf_08_16.csv')

# check proportion of nas in each column
proportion.nas <- sqf.data %>%
  dplyr::group_by(year) %>%
  dplyr::summarize_all(funs(nas = sum(is.na(.))/dplyr::n()))
