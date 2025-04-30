library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidygeocoder)

#andrewbcooper

#Sys.setenv(MAPBOX_API_KEY="MAPBOX_API_KEY")

####. This section pulls the data from ClinicalTrials.gov 
####. and saves each page as a list within a list called 'data'
#### as of 4/29/25 the data consists of 49 pages.  Each list, except the last
#### has 1000 elements.  Each element is a study

data <- list()

silly <- GET("https://clinicaltrials.gov/api/v2/studies",query=list(pageSize=10000,
                                                                    filter.advanced="AREA[StudyType]INTERVENTIONAL AND AREA[OverallStatus]RECRUITING"))
data[[1]] = fromJSON(rawToChar(silly$content),simplifyVector = TRUE)
mytoken = data[[1]]$nextPageToken



i = 2
while(is.null(mytoken)==FALSE){
  silly <- GET("https://clinicaltrials.gov/api/v2/studies",query=list(pageSize=10000,
                                                                      pageToken=mytoken,
                                                                      filter.advanced="AREA[StudyType]INTERVENTIONAL AND AREA[OverallStatus]RECRUITING"))
  data[[i]] <- fromJSON(rawToChar(silly$content),,simplifyVector = TRUE)
  mytoken =  data[[i]]$nextPageToken
  i=i+1
}




temp2 <- list()
data_part2 <- list()

for(j in 1:length(data)){
  
  # Use 'pluck' to extract the data from each study when that data will
  # consist of only 1 line
  
  # Use map to extract the data frome each study when that data may 
  # consist of more than one line (e.g., multiple locations for each study)
  
  study <- pluck(data[[j]],"studies")
  protocol_section = pluck(study,"protocolSection")
  identification_module = pluck(protocol_section,"identificationModule")
  nct_id = pluck(identification_module,"nctId")
  org_study_id = pluck(pluck(identification_module,"orgStudyIdInfo"),"id")
  organization = pluck(pluck(identification_module,"organization"),"fullName")
  brief_title = pluck(identification_module,"briefTitle")
  #official_title = pluck(identification_module,"officialTitle")
  #record_verification_date = pluck(pluck(protocol_section,"statusModule"),"statusVerifiedDate")
  #overall_status = pluck(pluck(protocol_section,"statusModule"),"overallStatus")
  #responsible_party_investigator_full_name = pluck(pluck(pluck(protocol_section,"sponsorCollaboratorsModule"),"responsibleParty"),"investigatorFullName")
  #responsible_party_investigator_title = pluck(pluck(pluck(protocol_section,"sponsorCollaboratorsModule"),"responsibleParty"),"investigatorTitle")
  #responsible_party_investigator_affiliation = pluck(pluck(pluck(protocol_section,"sponsorCollaboratorsModule"),"responsibleParty"),"investigatorAffiliation")
  brief_summary = pluck(pluck(protocol_section,"descriptionModule"),"briefSummary")
  #detailed_description = pluck(pluck(protocol_section,"descriptionModule"),"detailedDescription")
  conditions = pluck(pluck(protocol_section,"conditionsModule"),"conditions")
  condition_str = lapply(conditions, function(x) paste0(x,collapse =", " ))
  keyword_arr = pluck(pluck(protocol_section,"conditionsModule"),"keywords")
  keyword = lapply(keyword_arr, function(x) paste0(x,collapse =", " ))
  study_type = pluck(pluck(protocol_section,"designModule"),"studyType")
  study_phase = pluck(pluck(protocol_section,"designModule"),"phases")
  study_phase2 = lapply(study_phase, function(x) paste0(x,collapse =", " ))
  interventions_list = pluck(pluck(protocol_section,"armsInterventionsModule"),"interventions")
  intervention_types = lapply(interventions_list, function(x) x %>% dplyr::select("type")) #[intervention.get("type", "") for intervention in interventions_list]
  intervention_type = lapply(intervention_types, function(x) paste0(x,collapse =", " ))
  
  minimum_age = pluck(pluck(protocol_section,"eligibilityModule"),"minimumAge")
  maximum_age = pluck(pluck(protocol_section,"eligibilityModule"),"maximumAge")
  locations = pluck(pluck(protocol_section,"contactsLocationsModule"),"locations")
  facility = map(locations,"facility")
  city = map(locations,"city")
  state = map(locations,"state")
  zip = map(locations,"zip")
  country = map(locations,"country")
  status = map(locations,"status")
  geoPoint = map(locations,"geoPoint")
  #contacts = map(locations,"contacts")

  # Go through each study from the list of studies in data[[]]
  # First unlist the 1+ rows for the location-specific info and turn that
  # into a dataframe
  # Then appen the info that is constant accross locations using mutate
  
  # Temp2 will be a list of dataframes, one for each study
  
  for(loc in 1:length(locations)){
    temp2[[loc]] <- data.frame(
      country=if(is.null(country[[loc]])==TRUE) {NA} else {unlist(country[loc])},
      facility = if(is.null(facility[[loc]])==TRUE) {NA} else {unlist(facility[loc])},
      city = if(is.null(city[[loc]])==TRUE) {NA} else {unlist(city[loc])},
      state = if(is.null(state[[loc]])==TRUE) {NA} else {unlist(state[loc])},
      zip = if(is.null(zip[[loc]])==TRUE) {NA} else {unlist(zip[loc])},
      status = if(is.null(status[[loc]])==TRUE) {NA} else {unlist(status[loc])},
      geoPoint.lat = if(is.null(geoPoint[[loc]]$lat)==TRUE) {NA} else {geoPoint[[loc]]$lat},
      geoPoint.lon = if(is.null(geoPoint[[loc]]$lon)==TRUE) {NA} else {geoPoint[[loc]]$lon}
    ) %>% 
      mutate(
        # apply the 
        nct_id = nct_id[loc],
        org_study_id = org_study_id[[loc]],
        
        organization = organization[loc],
        brieftitle = brief_title[loc],
        briefsummary = brief_summary[loc],
        condition = unlist(condition_str[loc]),
        keyword = unlist(keyword[loc]),
        studytype = study_type[loc],
        intervention_type = unlist(intervention_type[loc]),
        minimum_age = minimum_age[loc],
        maximum_age = maximum_age[loc],
        phase = study_phase2[loc]
        
      ) %>%
      mutate(studyurl = paste0("https://clinicaltrials.gov/study/",nct_id),)
  }
  
  # Combine all the dataframes in temp2 into a single dataframe 
  temp2_df <- do.call(rbind,temp2)
  
  # create a dataframe of all unique study-facility-contact combinations
  locations_df <- map_df(locations,dplyr::bind_rows,.id="study_number") %>% 
    mutate(study_number = as.numeric(study_number),
           rownumber = seq(1,nrow(.)),
           geoPoint.lat = geoPoint$lat,
           geoPoint.lon = geoPoint$lon)
  
  nct_id_df <- data.frame(nct_id = nct_id,
                          study_number = seq(1:length(nct_id)))
  locations_df2 <- left_join(locations_df,nct_id_df)
  contacts_df <- map_df(locations_df$contacts,bind_rows,.id="facility_number") %>% 
    filter(role=="CONTACT") %>% 
    mutate(facility_number = as.numeric(facility_number))
  FullContactInfo <- left_join(locations_df2,contacts_df,by=c( "rownumber"="facility_number")) %>%
    select(facility,nct_id,zip,name,phone,email,geoPoint.lat,geoPoint.lon)
  
  # join the contact info to the study/facility info and save to a list
  temp2_df2 <- left_join(temp2_df,FullContactInfo,
                         by=c("nct_id"="nct_id","facility"="facility","zip"="zip","geoPoint.lat"="geoPoint.lat","geoPoint.lon"="geoPoint.lon"),
                         relationship = "many-to-many")
  data_part2[[j]] <- temp2_df2
}

finaldata <- do.call(rbind,data_part2) %>% filter(country == "United States")
finaldata$phase = as.vector(do.call(rbind,finaldata$phase))
finaldata$state <- ifelse(finaldata$state == "Washington" & is.na(finaldata$zip) == FALSE & as.numeric(substr(finaldata$zip, start = 1, stop = 5)) < 30000, "District of Columbia",finaldata$state)


mysilly <- finaldata %>% filter(is.na(geoPoint.lat)==TRUE)

sillygeo = tryCatch(geo(address = unique(mysilly$zip),method="mapbox"),error=function(e) {
  Sys.sleep(3)
  geo(address = unique(mysilly$zip),method="mapbox")
})


mysilly <- left_join(mysilly,sillygeo,by=c("zip"="address"))
mysilly <- mysilly %>% 
  mutate(geoPoint.lat = lat,
         geoPoint.lon = long) %>%
  select(-c("lat","long"))

finaldata = rbind(finaldata[is.na(finaldata$geoPoint.lat) == FALSE,],mysilly) 

finaldata <- finaldata %>% mutate(
  
  MINAGE = case_when( 
    str_detect(tolower(minimum_age),"year") == TRUE ~ parse_number(minimum_age),
    str_detect(tolower(minimum_age),"month") == TRUE ~ parse_number(minimum_age)/12,
    str_detect(tolower(minimum_age),"week") == TRUE ~ parse_number(minimum_age)/52,
    str_detect(tolower(minimum_age),"day") == TRUE ~ parse_number(minimum_age)/365.25,
    str_detect(tolower(minimum_age),"hour") == TRUE ~ parse_number(minimum_age)/8760,
    TRUE ~ 0),
  
  MAXAGE = case_when( 
    str_detect(tolower(maximum_age),"year") == TRUE ~ parse_number(maximum_age),
    str_detect(tolower(maximum_age),"month") == TRUE ~ parse_number(maximum_age)/12,
    str_detect(tolower(maximum_age),"week") == TRUE ~ parse_number(maximum_age)/52,
    str_detect(tolower(maximum_age),"day") == TRUE ~ parse_number(maximum_age)/365.25,
    str_detect(tolower(maximum_age),"hour") == TRUE ~ parse_number(maximum_age)/8760,
    TRUE ~ 100),
  
  phase = str_replace_all(str_replace_all(phase,"PHASE","Phase "),"EARLY_","Early "),
  
  CityState = paste0(city,", ",state),
  FacilityLoc = paste0(facility,"; ",city,", ",state),
  LATEST_REFRESH = Sys.time()
)
  
  # Things to code that were in the view or original Rmd
  
#is org the facility in the original data?
finaldata <- finaldata %>%
  mutate(
  SCH = ifelse(str_detect(tolower(facility),"seattle children") | 
                 str_detect(tolower(facility),regex("children.+hospital.+regional.+medical.+center")),1,0),
  FHCC = ifelse(str_detect(tolower(facility),"fhcc|fred hutch|seattle cancer care alliance|scca"),1,0 ),
  UW = ifelse(str_detect(tolower(facility),"university of wa|harborview|uw med|uw-fred|univ of wash") |
                str_detect(tolower(facility),regex("university.+washington")) |
                str_detect(facility,"UW "),1,0),
  SARCOMA = ifelse(str_detect(tolower(condition),"sarcoma"),1,0),
  GERM_CELL_TUMOR = ifelse(str_detect(tolower(condition),regex("germ.+cell.+tumor")),1,0),
  THYROID_CANCER = ifelse(str_detect(tolower(condition),regex("thyroid.+cancer")),1,0),
  LEUKEMIA = ifelse(str_detect(tolower(condition),"leukemia"),1,0),
  LYMPHOMA = ifelse(str_detect(tolower(condition),"lymphoma"),1,0),
  BRAIN_TUMOR = ifelse(str_detect(tolower(condition),regex("brain.+tumor")) |
                         str_detect(tolower(condition),regex("central.+nervous.+tumor")) |
                         str_detect(tolower(condition),regex("central.+nervous.+carcinoma")) |
                         str_detect(tolower(condition),"central nervous carcinoma|glioma|craniophary|medullo"),1,0)
    ) %>%
  mutate(OncologyCondition = 
           case_when(
             BRAIN_TUMOR > 0  ~ "Brain Tumor",
             GERM_CELL_TUMOR > 0 ~ "Germ Cell Tumor",
             LEUKEMIA > 0 ~ "Leukemia",
             LYMPHOMA > 0 ~ "Lymphoma",
             SARCOMA > 0 ~ "Sarcoma",
             THYROID_CANCER > 0 ~ "Thyroid Cancer",
             TRUE ~ "Not Cancer"
           ),
         ConsortiumSite = 
           case_when(
             SCH > 0 ~ "Seattle Children's",
             FHCC > 0 ~ "FHCC",
             UW > 0 ~ 'UW',
             TRUE ~ "Non-Consortium"
           ),
         
         dummy = 0
  ) %>% 
    mutate(NOT_CANCER = ifelse(OncologyCondition == "Not Cancer",1,0)) %>% 
  filter(geoPoint.lon < 0 &
           
           country == 'United States' & 
           
         str_detect(tolower(state),'washington|alaska|montana|idaho|oregon')    
           
           )

names(finaldata) <- toupper(names(finaldata))
saveRDS(finaldata,file="trials.RDS")
