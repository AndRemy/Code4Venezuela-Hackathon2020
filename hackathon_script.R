library(sqldf)
library(dplyr)

# Library dependencies
if (!require("bigrquery")) install.packages("bigrquery")

# Authenticate user
bq_auth(path="https://storage.googleapis.com/angostura-public/hult-hackathon-key.json")

# SQL request and store into dataframe
project_id <- "event-pipeline"
sql <- 'SELECT * from `angostura_dev.eh_health_survey_response`'
df <- query_exec(sql, project_id, use_legacy_sql = FALSE)

#Removing duplicates
unique_df <- sqldf("SELECT max(timestamp) AS timestamp, report_week, hospital_code
                    FROM df
                    GROUP BY hospital_code, report_week") %>%
    left_join(df, c("hospital_code", "report_week", "timestamp"))


##########################################################
#                    DATA CLEANING                       #
##########################################################
# DROP THE RECORD THAT HAS A YES IN rrt_avail AND rrt_operability IS EMPTY
unique_df <- unique_df[!(unique_df$rrt_avail != "No" & unique_df$rrt_operability == ""),]

# DROP THE RECORD THAT HAS A YES IN nutr_avail AND nutr_operability IS EMPTY
unique_df <- unique_df[!(unique_df$nutr_avail != "No" & unique_df$nutr_operability == ""),]

#IF QUESTION 49 > 0, QUESTION 48 SHOULD BE YES. IF 49 = 0, 48 SHOULD BE NO.
unique_df <- unique_df[!(unique_df$power_outage_avg_failures_per_day > 0 & unique_df$power_outage == 'No'),]
unique_df <- unique_df[!(unique_df$power_outage_avg_failures_per_day == 0 & unique_df$power_outage != 'No'),]

#IF QUESTION 49 > 0, QUESTION 48 SHOULD BE YES. IF 49 = 0, 48 SHOULD BE NO.
unique_df[unique_df$power_outage_avg_failures_per_day == 0, 'power_outage'] <- 'No'
unique_df[unique_df$power_outage_avg_failures_per_day > 0, 'power_outage']  <- 'S�'



#RELATED QUESTIONS
#IF THEY ANSWER NO IN 12, AND IF THERE'S NOTHING UNTIL 15, IT'S OK. OTHERWISE, FLAG IT.
#Q.12: rrt_avail

#Q.13: rrt_operability
unique_df$m_rrt_operability <- 0
unique_df[unique_df$rrt_avail != "No" & nchar(unique_df$rrt_operability) == 0, "m_rrt_operability"] <- 1

#Q.14: rrt_date_stopped_operability
unique_df$m_rrt_date_stopped_operability <- 0
unique_df[unique_df$rrt_avail != "No" & nchar(unique_df$rrt_date_stopped_operability) == 0, "m_rrt_date_stopped_operability"] <- 1

#Q.15: rrt_reason_stopped_operability
unique_df$m_rrt_reason_stopped_operability <- 0
unique_df[unique_df$rrt_avail != "No" & nchar(unique_df$rrt_reason_stopped_operability) == 0, "m_rrt_reason_stopped_operability"] <- 1


#IF THEY ANSWER NO IN 27 AND IF THERE'S NOTHING UNTIL 34, IT'S OK. OTHERWISE, FLAG IT.
#Q.27: nutr_avail

#Q.28: nutr_operability
unique_df$m_nutr_operability <- 0
unique_df[unique_df$nutr_avail != "No" & nchar(unique_df$nutr_operability) == 0, "m_nutr_operability"] <- 1

#Q.29: nutr_date_stopped_operability
unique_df$m_nutr_date_stopped_operability <- 0
unique_df[unique_df$nutr_avail != "No" & nchar(unique_df$nutr_date_stopped_operability) == 0, "m_nutr_date_stopped_operability"] <- 1

#Q.30: nutr_reason_stopped_operability
unique_df$m_nutr_reason_stopped_operability <- 0
unique_df[unique_df$nutr_avail != "No" & nchar(unique_df$nutr_reason_stopped_operability) == 0, "m_nutr_reason_stopped_operability"] <- 1

#Q.31: nutr_num
unique_df$m_nutr_num <- 0
unique_df[unique_df$nutr_avail != "No" & nchar(unique_df$nutr_num) == 0, "m_nutr_num"] <- 1

#Q.32: nutr_daily_freq_meal
unique_df$m_nutr_daily_freq_meal <- 0
unique_df[unique_df$nutr_avail != "No" & nchar(unique_df$nutr_daily_freq_meal) == 0, "m_nutr_daily_freq_meal"] <- 1

#Q.33: nutr_quality
unique_df$m_nutr_quality <- 0
unique_df[nchar(unique_df[unique_df["nutr_avail"] != "No", "nutr_quality"]) == 0, "m_nutr_quality"] <- 1

#Q.34: nutr_freq_milk_formulas
unique_df$m_nutr_freq_milk_formulas <- 0
unique_df[unique_df$nutr_avail != "No" & nchar(unique_df$nutr_freq_milk_formulas) == 0, "m_nutr_freq_milk_formulas"] <- 1


#IF 55 IS YES, 56 SHOULD HAVE A VALUE. IF NOT, FLAG IT
#Q.55: power_outage_mortatility
#Q.56: power_outage_deaths_count
unique_df$m_power_outage_deaths_count <- 0
unique_df[unique_df$power_outage_mortatility != "No" & is.na(unique_df$power_outage_deaths_count) == TRUE, "m_power_outage_deaths_count"] <- 1

#FROM QUESTIONS 64-66, FLAG THEM IF THERES MISSING VALUE.
#Q.64: nCoV_face_mask_avail
unique_df$m_nCoV_face_mask_avail <- 0
unique_df[nchar(unique_df$nCoV_face_mask_avail) == 0, "m_nCoV_face_mask_avail"] <- 1

#Q.65: nCoV_respiratory_isolation_protocol_avail
unique_df$m_nCoV_respiratory_isolation_protocol_avail <- 0
unique_df[nchar(unique_df$nCoV_respiratory_isolation_protocol_avail) == 0, "m_nCoV_respiratory_isolation_protocol_avail"] <- 1

#Q.66: nCoV_isolation_area_avail
unique_df$m_nCoV_isolation_area_avail <- 0
unique_df[nchar(unique_df$nCoV_isolation_area_avail) == 0, "m_nCoV_isolation_area_avail"] <- 1

#RECOMMENDATION: ADD CORONAVIRUS AS AN OPTION IN QUESTION 63.


#Creating dummie variables from multiple options lists
unique_df$mortality_hospital_failure_cardiovascular_cause_equipos=as.integer(grepl(pattern="Equipos",x=unique_df$mortality_hospital_failure_cardiovascular_cause))
unique_df$mortality_hospital_failure_cardiovascular_cause_medicamentos=as.integer(grepl(pattern="Medicamentos",x=unique_df$mortality_hospital_failure_cardiovascular_cause))
unique_df$mortality_hospital_failure_cardiovascular_cause_recursohumano=as.integer(grepl(pattern="Recurso Humano",x=unique_df$mortality_hospital_failure_cardiovascular_cause))
unique_df$mortality_hospital_failure_cardiovascular_cause_luzelectrica=as.integer(grepl(pattern="Luz el�ctrica",x=unique_df$mortality_hospital_failure_cardiovascular_cause))
unique_df$mortality_hospital_failure_cardiovascular_cause_restoestructurales=as.integer(grepl(pattern="Resto estructurales ( agua , O2 , succi�n)",x=unique_df$mortality_hospital_failure_cardiovascular_cause))
#
unique_df$mortality_hospital_failure_trauma_cause_equipos=as.integer(grepl(pattern="Equipos",x=unique_df$mortality_hospital_failure_cardiovascular_cause))
unique_df$mortality_hospital_failure_trauma_cause_medicamentos=as.integer(grepl(pattern="Medicamentos",x=unique_df$mortality_hospital_failure_cardiovascular_cause))
unique_df$mortality_hospital_failure_trauma_cause_recursohumano=as.integer(grepl(pattern="Recurso Humano",x=unique_df$mortality_hospital_failure_cardiovascular_cause))
unique_df$mortality_hospital_failure_trauma_cause_luzelectrica=as.integer(grepl(pattern="Luz el�ctrica",x=unique_df$mortality_hospital_failure_cardiovascular_cause))
unique_df$mortality_hospital_failure_trauma_cause_restoestructurales=as.integer(grepl(pattern="Resto estructurales ( agua , O2 , succi�n)",x=unique_df$mortality_hospital_failure_cardiovascular_cause))
#
unique_df$staff_violence_affected_reasons_violencia_familiares=as.integer(grepl(pattern="Violencia contra personal de hospital por familiares",x=unique_df$staff_violence_affected_reasons))
unique_df$staff_violence_affected_reasons_violencia_paramilitar=as.integer(grepl(pattern="Violencia contra personal por  grupos paramilitares .",x=unique_df$staff_violence_affected_reasons))
unique_df$staff_violence_affected_reasons_violencia_fuerzas=as.integer(grepl(pattern="Violencia contra personal por fuerzas de seguridad",x=unique_df$staff_violence_affected_reasons))
unique_df$staff_violence_affected_reasons_robos=as.integer(grepl(pattern="Robos , hurtos o disparos dentro del centro asistencial.",x=unique_df$staff_violence_affected_reasons))
unique_df$staff_violence_affected_reasons_homicidios=as.integer(grepl(pattern="Homicidios (muertes violentas de personal de salud)",x=unique_df$staff_violence_affected_reasons))
#
unique_df$epidemiological_emergency_suspected_diseases_difteria=as.integer(grepl(pattern="Difteria",x=unique_df$epidemiological_emergency_suspected_diseases))
unique_df$epidemiological_emergency_suspected_diseases_sarampion=as.integer(grepl(pattern="Sarampi�n",x=unique_df$epidemiological_emergency_suspected_diseases))
unique_df$epidemiological_emergency_suspected_diseases_TBC=as.integer(grepl(pattern="TBC en personal de salud",x=unique_df$epidemiological_emergency_suspected_diseases))
unique_df$epidemiological_emergency_suspected_diseases_influenza=as.integer(grepl(pattern="Influenza (UTI)",x=unique_df$epidemiological_emergency_suspected_diseases))
unique_df$epidemiological_emergency_suspected_diseases_fiebrehemorragica=as.integer(grepl(pattern="Fiebre hemorr�gica severa (UTI)",x=unique_df$epidemiological_emergency_suspected_diseases))
unique_df$epidemiological_emergency_suspected_diseases_zika=as.integer(grepl(pattern="S�ndrome Cong�nito Zika",x=unique_df$epidemiological_emergency_suspected_diseases))
unique_df$epidemiological_emergency_suspected_diseases_gillanbarre=as.integer(grepl(pattern="S�ndrome Gillan Barr�",x=unique_df$epidemiological_emergency_suspected_diseases))

#New Features
no_list <- c("No existe", "No operativa", "Nunca ha existido")
unique_df$metric_operability_icu_p <- 1
unique_df[unique_df$operability_icu_p %in% no_list, "metric_operability_icu_p"] <- 0

unique_df$week <- 0
unique_df$year <- 0
unique_df[, c('week', 'year')] <- data.frame(do.call('rbind', strsplit(as.character(unique_df$report_week),' del ',fixed=TRUE)))



#Columns that will be used for general public
public <- c(
  'timestamp',
  'report_week',
  'week',
  'year',
  'hospital_code',
  'federal_entity',
  'hospital_type',
  'administrative_entity',
  'arch_beds_count',
  'op_beds_count',
  'op_beds_er_count',
  'op_pavilions_count',
  'operability_icu',
  'operability_icu_p',
  'operability_er',
  'operability_sx',
  'operability_lab',
  'operability_uls',
  'operability_ct_mri',
  'operability_xr',
  'er_avail_adrenalin',
  'er_avail_atropine',
  'er_avail_dopamine',
  'er_avail_cephalosporins_betalactams',
  'er_avail_aminoglycosides_quinolone',
  'er_avail_vancomycin_clindamycin',
  'er_avail_lidocaine',
  'er_avail_minor_opioids',
  'er_avail_major_opioids',
  'er_avail_iv_fluids',
  'er_avail_diazepam_dph',
  'er_avail_heparin',
  'er_avail_steroids',
  'er_avail_insulin',
  'er_avail_asthma',
  'er_avail_blood_pressure',
  'er_avail_defibrillator',
  'er_avail_ott_intubation',
  'er_avail_catheter',
  'er_avail_oxygen_suction',
  'sx_avail_minor_opioids',
  'sx_avail_major_opioids',
  'sx_avail_anesthetic_gases',
  'sx_avail_anesthetics_iv',
  'sx_avail_relaxants',
  'sx_avail_ott_intubation',
  'sx_avail_patient_lingerie_kit',
  'sx_avail_disposables_mask_gloves_gown',
  'sx_avail_oxygen_suction',
  'rrt_avail',
  'rrt_operability',
  'rrt_hemodialysis_avail_filter',
  'rrt_hemodialysis_avail_lines',
  'rrt_hemodialysis_avail_kit_hemodialysis',
  'rrt_hemodialysis_avail_iron',
  'rrt_hemodialysis_avail_b_complex',
  'rrt_hemodialysis_avail_calcium',
  'rrt_hemodialysis_avail_zemblar',
  'rrt_avail_high_flow_catheters',
  'rrt_avail_blood_tests_hiv_hvb_hvc_vdr',
  'rrt_avail_immediate_access_urea_reduction_bun',
  'nutr_avail',
  'nutr_operability',
  'nutr_date_stopped_operability',
  'nutr_daily_freq_meal',
  'nutr_freq_milk_formulas',
  'pneumonia_antibiotic_therapy_latency',
  'mi_thrombolytic_treatment_latency',
  'wash_failure_icu',
  'wash_failure_er',
  'wahs_failure_sx',
  'power_outage',
  'power_outage_avg_failures_per_day',
  'metric_operability_icu_p',
  'mortality_hospital_failure_cardiovascular_cause_equipos'           ,
  'mortality_hospital_failure_cardiovascular_cause_medicamentos'      ,
  'mortality_hospital_failure_cardiovascular_cause_recursohumano'     ,
  'mortality_hospital_failure_cardiovascular_cause_luzelectrica'      ,
  'mortality_hospital_failure_cardiovascular_cause_restoestructurales',
  'mortality_hospital_failure_trauma_cause_equipos'                   ,
  'mortality_hospital_failure_trauma_cause_medicamentos'              ,
  'mortality_hospital_failure_trauma_cause_recursohumano'             ,
  'mortality_hospital_failure_trauma_cause_luzelectrica'              ,
  'mortality_hospital_failure_trauma_cause_restoestructurales'        ,
  'staff_violence_affected_reasons_violencia_familiares'              ,
  'staff_violence_affected_reasons_violencia_paramilitar'             ,
  'staff_violence_affected_reasons_violencia_fuerzas'                 ,
  'staff_violence_affected_reasons_robos'                             ,
  'staff_violence_affected_reasons_homicidios'                        ,
  'epidemiological_emergency_suspected_diseases_difteria'             ,
  'epidemiological_emergency_suspected_diseases_sarampion'            ,
  'epidemiological_emergency_suspected_diseases_TBC'                  ,
  'epidemiological_emergency_suspected_diseases_influenza'            ,
  'epidemiological_emergency_suspected_diseases_fiebrehemorragica'    ,
  'epidemiological_emergency_suspected_diseases_zika'                 ,
  'epidemiological_emergency_suspected_diseases_gillanbarre'
)

#Columns that will be used for press
news <- c(
  'timestamp',
  'report_week',
  'week',
  'year',
  'hospital_code',
  'federal_entity',
  'hospital_type',
  'administrative_entity',
  'arch_beds_count',
  'op_beds_count',
  'op_beds_er_count',
  'op_pavilions_count',
  'operability_icu',
  'operability_icu_p',
  'operability_er',
  'operability_sx',
  'operability_lab',
  'operability_uls',
  'operability_ct_mri',
  'operability_xr',
  'er_avail_adrenalin',
  'er_avail_atropine',
  'er_avail_dopamine',
  'er_avail_cephalosporins_betalactams',
  'er_avail_aminoglycosides_quinolone',
  'er_avail_vancomycin_clindamycin',
  'er_avail_lidocaine',
  'er_avail_minor_opioids',
  'er_avail_major_opioids',
  'er_avail_iv_fluids',
  'er_avail_diazepam_dph',
  'er_avail_heparin',
  'er_avail_steroids',
  'er_avail_insulin',
  'er_avail_asthma',
  'er_avail_blood_pressure',
  'er_avail_defibrillator',
  'er_avail_ott_intubation',
  'er_avail_catheter',
  'er_avail_oxygen_suction',
  'sx_avail_minor_opioids',
  'sx_avail_major_opioids',
  'sx_avail_anesthetic_gases',
  'sx_avail_anesthetics_iv',
  'sx_avail_relaxants',
  'sx_avail_ott_intubation',
  'sx_avail_patient_lingerie_kit',
  'sx_avail_disposables_mask_gloves_gown',
  'sx_avail_oxygen_suction',
  'rrt_date_stopped_operability',
  'rrt_reason_stopped_operability',
  'rrt_reverse_osmosis_unit_operability',
  'nutr_reason_stopped_operability',
  'nutr_freq_milk_formulas',
  'pneumonia_antibiotic_therapy_latency',
  'mi_thrombolytic_treatment_latency',
  'power_outage_days_count',
  'power_outage_equipment_failure',
  'power_outage_mortatility',
  'power_outage_deaths_count',
  'strike_medical_staff_affected',
  'strike_nurses_affected',
  'strike_other_staff_affected',
  'strike_patients_affected',
  'strike_other_affected',
  'staff_violence_affected_reasons',
  'mortality_hospital_failure_cardiovascular_cause_equipos'           ,
  'mortality_hospital_failure_cardiovascular_cause_medicamentos'      ,
  'mortality_hospital_failure_cardiovascular_cause_recursohumano'     ,
  'mortality_hospital_failure_cardiovascular_cause_luzelectrica'      ,
  'mortality_hospital_failure_cardiovascular_cause_restoestructurales',
  'mortality_hospital_failure_trauma_cause_equipos'                   ,
  'mortality_hospital_failure_trauma_cause_medicamentos'              ,
  'mortality_hospital_failure_trauma_cause_recursohumano'             ,
  'mortality_hospital_failure_trauma_cause_luzelectrica'              ,
  'mortality_hospital_failure_trauma_cause_restoestructurales'        ,
  'staff_violence_affected_reasons_violencia_familiares'              ,
  'staff_violence_affected_reasons_violencia_paramilitar'             ,
  'staff_violence_affected_reasons_violencia_fuerzas'                 ,
  'staff_violence_affected_reasons_robos'                             ,
  'staff_violence_affected_reasons_homicidios'                        ,
  'epidemiological_emergency_suspected_diseases_difteria'             ,
  'epidemiological_emergency_suspected_diseases_sarampion'            ,
  'epidemiological_emergency_suspected_diseases_TBC'                  ,
  'epidemiological_emergency_suspected_diseases_influenza'            ,
  'epidemiological_emergency_suspected_diseases_fiebrehemorragica'    ,
  'epidemiological_emergency_suspected_diseases_zika'                 ,
  'epidemiological_emergency_suspected_diseases_gillanbarre'
)

#subsetting the dataframe that will be used for general public
df_public <- unique_df[public]

df_public <- sqldf(
  "SELECT *
  FROM df_public
  WHERE (hospital_code, year, week) IN
  (
    SELECT hospital_code, year, MAX(week)
    FROM   df_public
    WHERE (hospital_code, year) IN
    (
      SELECT hospital_code, MAX(year)
      FROM   df_public
      GROUP BY hospital_code
    )
    GROUP BY hospital_code, year
  )"
)

write.csv(df_public, 'public_data.csv')

#subsetting the dataframe that will be used for press
df_news   <- unique_df[news]

cols <- c(
  "operability_icu",
  "operability_icu_p",
  "operability_er",
  "operability_sx",
  "operability_lab",
  "operability_uls",
  "operability_ct_mri",
  "operability_xr",
  "er_avail_adrenalin",
  "er_avail_atropine",
  "er_avail_dopamine",
  "er_avail_cephalosporins_betalactams",
  "er_avail_aminoglycosides_quinolone",
  "er_avail_vancomycin_clindamycin",
  "er_avail_lidocaine",
  "er_avail_minor_opioids",
  "er_avail_major_opioids",
  "er_avail_iv_fluids",
  "er_avail_diazepam_dph",
  "er_avail_heparin",
  "er_avail_steroids",
  "er_avail_insulin",
  "er_avail_asthma",
  "er_avail_blood_pressure",
  "er_avail_defibrillator",
  "er_avail_ott_intubation",
  "er_avail_catheter",
  "er_avail_oxygen_suction",
  "sx_avail_minor_opioids",
  "sx_avail_major_opioids",
  "sx_avail_anesthetic_gases",
  "sx_avail_anesthetics_iv",
  "sx_avail_relaxants",
  "sx_avail_ott_intubation",
  "sx_avail_patient_lingerie_kit",
  "sx_avail_disposables_mask_gloves_gown",
  "sx_avail_oxygen_suction"
)

write.csv(df_news, "news_data.csv")
write.csv(df_public, "public_data.csv")
