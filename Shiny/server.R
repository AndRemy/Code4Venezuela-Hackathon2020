# Library dependencies
if (!require("bigrquery")) install.packages("bigrquery")
library(shiny)
library(sqldf)
library(dplyr)
library(bigrquery)

load_data <- function(){
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
    unique_df[unique_df$power_outage_avg_failures_per_day > 0, 'power_outage']  <- 'Si'
    
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
        'metric_operability_icu_p'
        #,
        #'mortality_hospital_failure_cardiovascular_cause_equipos'           ,
        #'mortality_hospital_failure_cardiovascular_cause_medicamentos'      ,
        #'mortality_hospital_failure_cardiovascular_cause_recursohumano'     ,
        #'mortality_hospital_failure_cardiovascular_cause_luzelectrica'      ,
        #'mortality_hospital_failure_cardiovascular_cause_restoestructurales',
        #'mortality_hospital_failure_trauma_cause_equipos'                   ,
        #'mortality_hospital_failure_trauma_cause_medicamentos'              ,
        #'mortality_hospital_failure_trauma_cause_recursohumano'             ,
        #'mortality_hospital_failure_trauma_cause_luzelectrica'              ,
        #'mortality_hospital_failure_trauma_cause_restoestructurales'        ,
        #'staff_violence_affected_reasons_violencia_familiares'              ,
        #'staff_violence_affected_reasons_violencia_paramilitar'             ,
        #'staff_violence_affected_reasons_violencia_fuerzas'                 ,
        #'staff_violence_affected_reasons_robos'                             ,
        #'staff_violence_affected_reasons_homicidios'                        ,
        #'epidemiological_emergency_suspected_diseases_difteria'             ,
        #'epidemiological_emergency_suspected_diseases_sarampion'            ,
        #'epidemiological_emergency_suspected_diseases_TBC'                  ,
        #'epidemiological_emergency_suspected_diseases_influenza'            ,
        #'epidemiological_emergency_suspected_diseases_fiebrehemorragica'    ,
        #'epidemiological_emergency_suspected_diseases_zika'                 ,
        #'epidemiological_emergency_suspected_diseases_gillanbarre'
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
        'staff_violence_affected_reasons'
        #,
        #'mortality_hospital_failure_cardiovascular_cause_equipos'           ,
        #'mortality_hospital_failure_cardiovascular_cause_medicamentos'      ,
        #'mortality_hospital_failure_cardiovascular_cause_recursohumano'     ,
        #'mortality_hospital_failure_cardiovascular_cause_luzelectrica'      ,
        #'mortality_hospital_failure_cardiovascular_cause_restoestructurales',
        #'mortality_hospital_failure_trauma_cause_equipos'                   ,
        #'mortality_hospital_failure_trauma_cause_medicamentos'              ,
        #'mortality_hospital_failure_trauma_cause_recursohumano'             ,
        #'mortality_hospital_failure_trauma_cause_luzelectrica'              ,
        #'mortality_hospital_failure_trauma_cause_restoestructurales'        ,
        #'staff_violence_affected_reasons_violencia_familiares'              ,
        #'staff_violence_affected_reasons_violencia_paramilitar'             ,
        #'staff_violence_affected_reasons_violencia_fuerzas'                 ,
        #'staff_violence_affected_reasons_robos'                             ,
        #'staff_violence_affected_reasons_homicidios'                        ,
        #'epidemiological_emergency_suspected_diseases_difteria'             ,
        #'epidemiological_emergency_suspected_diseases_sarampion'            ,
        #'epidemiological_emergency_suspected_diseases_TBC'                  ,
        #'epidemiological_emergency_suspected_diseases_influenza'            ,
        #'epidemiological_emergency_suspected_diseases_fiebrehemorragica'    ,
        #'epidemiological_emergency_suspected_diseases_zika'                 ,
        #'epidemiological_emergency_suspected_diseases_gillanbarre'
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
    return(df_public)
}

get_regions <- function(df_public){
    region <- unique(df_public$federal_entity)
    return(region)
}

get_hospitals <- function(df_public, region){
    df_public[df_public$federal_entity == "region"]
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    df_public <- load_data()
    
    observe({
        regions <- get_regions(df_public)
        updateSelectInput(session = session,
                          inputId = "region",
                          choices = regions,
                          selected = regions[1])
    })
    
    output$numberHospitals <- renderText({
        numberHospitals <- length(unique(df_public$hospital_code))
        paste0(numberHospitals, " Hospitals")
    })
    
    output$hospitalsPE <- renderText({
        hospitalsPE <- length(unique(df_public[df_public$metric_operability_icu_p == 1, "hospital_code"]))
        paste0(hospitalsPE, " Hospitals with Pediatric Care")
    })
    
    output$regionalInfo <- renderUI({
        regions <- get_regions(df_public)
        region_info <- ""
        for(reg in regions){
            df_analysis <- df_public[df_public$federal_entity == reg,]
            text_1      <- paste0("<b>Region ", reg, "</b>")
            
            num_hospitals <- length(unique(df_analysis[, "hospital_code"]))
            text_2        <- paste0(num_hospitals, " Hospitals in Total")
            
            with_energy <- length(unique(df_analysis[df_analysis$power_outage != "No","hospital_code"]))
            text_3      <- paste0(with_energy, " Hospitals with Power Energy")
            
            with_water <- 0
            with_water <- with_water + length(unique(df_analysis[df_analysis$wash_failure_er != "No hubo agua ningún dia" | df_analysis$wash_failure_icu != "No hubo agua ningún dia" | df_analysis$wahs_failure_sx != "No hubo agua ningún dia", "hospital_code"]))
            text_4     <- paste0(with_water, " Hospitals with Clean Water")
            
            with_nutrition <- length(unique(df_analysis[df_analysis$nutr_operability != "No","hospital_code"]))
            text_5 <- paste0(with_nutrition, " Hospitals with Nutrition Service")
            
            region_info <- paste(region_info, text_1, text_2, text_3, text_4, text_5, '<br/>', sep = '<br/>')
        }
        HTML(region_info)
    })
    
    output$generalRegion <- renderUI({
        region <- input$region
        
        df_analysis <- df_public[df_public$federal_entity == region,]
        
        num_hospitals <- length(unique(df_analysis[, "hospital_code"]))
        text_2        <- paste0(num_hospitals, " Hospitals in Total")
        
        with_energy <- length(unique(df_analysis[df_analysis$power_outage != "No","hospital_code"]))
        text_3      <- paste0(with_energy, " Hospitals with Power Energy")
        
        with_water <- 0
        with_water <- with_water + length(unique(df_analysis[df_analysis$wash_failure_er != "No hubo agua ningún dia" | df_analysis$wash_failure_icu != "No hubo agua ningún dia" | df_analysis$wahs_failure_sx != "No hubo agua ningún dia", "hospital_code"]))
        text_4     <- paste0(with_water, " Hospitals with Clean Water")
        
        with_nutrition <- length(unique(df_analysis[df_analysis$nutr_operability != "No","hospital_code"]))
        text_5 <- paste0(with_nutrition, " Hospitals with Nutrition Service")
        
        HTML(paste(text_2, text_3, text_4, text_5, sep = '<br/>'))
    })
    
    output$hospitalInfo <- renderUI({
        hospital_info <- ""
        df_regions <- df_public[df_public$federal_entity == input$region, ]
        hospitals <- df_regions$hospital_code
        
        for(hospital in hospitals){
            df_analysis <- df_regions[df_regions$hospital_code == hospital, ]
            
            hospital_name <- df_analysis$hospital_code
            text_1 <- paste0("<b>Hospital ", hospital_name, "</b>")
            
            result <- ""
            child_care <- length(unique(df_analysis[df_analysis$operability_icu_p == "No operativa" | df_analysis$operability_icu_p == "Nunca ha existido", "hospital_code"]))
            if(child_care > 0){
                result <- "No"
            }
            else{
                result <- "Yes"
            }
            text_2 <- paste0("Provides childcare: ", result)
            
            type <- df_analysis$hospital_type
            if(type == "Tipo II")
                result <- "General"
            else{
                if(type == "Tipo III")
                    result <- "Intermediate"
                else{
                    if(type == "Tipo IV")
                        result <- "Specialized"
                }
            }
            text_3 <- paste0("Type of Care: ", result)
            
            water <- length(unique(df_analysis[df_analysis$wash_failure_er == "No hubo agua ningún dia" | df_analysis$wash_failure_icu == "No hubo agua ningún dia" | df_analysis$wahs_failure_sx == "No hubo agua ningún dia", "hospital_code"]))
            if(water > 0)
                result <- "No"
            else
                result <- "Yes"
            text_4 <- paste0("Regular Clean Water: ", result)
            
            hospital_info <- paste(hospital_info, text_1, text_2, text_3, text_4, "<br/>", sep="<br/>")
        }
        
        HTML(hospital_info)
    })
})
