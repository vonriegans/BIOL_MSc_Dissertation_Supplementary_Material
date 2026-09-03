###### Load libraries and database #######
  library(readr)
  library(pccc)
  library(tidyverse)
  library(data.table)
# library(caret)

## Initial data cleaning
  test_data <- read.csv("full_data.csv", header = TRUE, sep =",")

  ## Remove rows which have no data related to laterality + if verified is FALSE
  ## BC I'm using the patient_verified column, there are some that are just blank
  ## I'm making this into NAs so I can also just drop the NAs.
  test_data[test_data == "" | test_data == " "] <- NA
  clean_test<- test_data %>% 
               filter_out(, patient_verified == "FALSE") %>%
               drop_na(patient_verified) %>%
               mutate(across(everything(), as.character))
  
  
  rm(test_data)
  
  ## Changing column names for uniformity (useful for later)
  setnames(clean_test, 
           c("PrimaryDiagnosis", "Primary_Procedure_Date", "procedurePrimary"),
           c("Diagnosis_1", "Proc_Date", "Procedure_1"), 
           skip_absent = TRUE)

##### Formatting the data - LONGFORM OPCS ######
# Currently the HES data is arranged with one patient episode per row.
# We need it to be one knee operation/row so we can classify the indication for each operation and find PO-PPF.
  
  # OPCS code analysis
  knees_longopcs <- clean_test %>%
    select(Row, starts_with("Procedure_"), starts_with("Proc_"))
  
  # Make long form operation codes
  longopcs1 <- knees_longopcs %>% 
    select(Row, starts_with("Procedure_")) %>% 
    pivot_longer(-Row) %>%
    filter(!is.na(value))
  
  # Make long form dates for operation codes
  longopcs2 <- knees_longopcs %>% 
    select(Row, starts_with("Proc_")) %>% 
    pivot_longer(-Row) %>%
    filter(!is.na(value))
  
  # Put codes and date together:
  # As there is only a PRIMARY PROC. DATE, we're assuming it is all the same date.
  knees_longopcs <- inner_join(longopcs1, longopcs2, by = "Row")
  
  # Change column names again bc it made it into x/y value/names (duplicated names before)
  # Also so now they all have the same name theme.
  setnames(knees_longopcs, 
           c("name.x", "value.x", "value.y"),
           c("OPERTN_NO", "OPERTN", "OP_DATE"), 
           skip_absent = TRUE)

  knees_longopcs <- knees_longopcs %>% 
                # This was just some cleaning, just in case we need the Procedure "number".
                mutate(OPERTN_NO = as.numeric(gsub("\\D", "", OPERTN_NO))) %>%
                select(-name.y)  %>%
                # Here we are filling a new "Side" column based on existing laterality codes
                # Again, we assume the patient will have procedures on the same side every time.
                mutate(Side = case_when(str_detect(OPERTN, "Z941")~"bilateral",
                                        str_detect(OPERTN, "Z942")~"right",
                                        str_detect(OPERTN, "Z943")~"left",
                                        str_detect(OPERTN, "Z944")~"unilateral",
                                        str_detect(OPERTN, "Z948")~"specified lat NEC",
                                        str_detect(OPERTN, "Z949")~"laterality NEC"))  %>% 
                group_by(Row) %>%
                fill(Side, .direction="updown") %>%
                ungroup() %>%
                mutate(Side = case_when(is.na(Side)~"none", T~Side)) %>%
                group_nest(Row, Side) %>%
                group_by(Row) %>%
                mutate(opgrp = 1:n())%>%
                unnest(cols = c(data)) %>% 
                ungroup 
  
    longopcs1 <- knees_longopcs %>%
                 group_by(Row, opgrp, Side, OP_DATE) %>%
                 summarise(OPERTN = toString(OPERTN), .groups = 'drop')

### Listing OPCS codes for revision operations and fixations ###
  fix_im <-c("W19", "W191", "W192", "W193", "W194",
             "W195", "W196", "W197", "W198", "W199")
  fix_em <- c("W201","W202","W203","W204","W208","W209")
  fix_closed <- c("W24", "O17")
  fix_orif <- c("W22","W23","W24","W25","W26","W65","W66", "W67")
  
  fixation <- paste(c(fix_im, fix_em, fix_closed, fix_orif), collapse = " ") %>%
              str_replace_all(., " ", "\\|")
  
  # This for "location" codes
  knee <- c("Z846")
  femur <- c("Z76")
  patella <- c("Z787|Z846")
  tibia <- c("Z77")


###### OPCS CODES -- integration of a lookup table #####
  # I got the list of knee revision related codes from the NJR, basis of the table:
  # Importing and doing some formatting:
  lookup_knees <- read.csv("Knee_Revision_NJR_codes.csv", sep = ",", header = TRUE) 
  lookup_knees <- lookup_knees %>%
                   # There is a description column but we don't need it here
                   select(,-Description) %>% 
                   setnames(c("Item", "Operation.OPCS.codes"), 
                            c("NJR", "OPCS"), skip_absent = TRUE)
  
  # Splitting the codes bc I'm not sure if they have to occur right after e/o in the main string to be flagged.
  # Note: K2.81 had no space after the comma, so I manually edited it before importing.
  lookup_knees[c("OPCS_1", "OPCS_2")] <- str_split_fixed(lookup_knees$OPCS, ", ", 2) 

  lookup_knees <- lookup_knees[, -2]
  # Replaced with NAs just in case it will flag spaces
  lookup_knees[lookup_knees == "" | lookup_knees == " "] <- NA
    
# Identifying the unique codes in the table to make a TRUE/FALSE map (not including NAs)
  unique <- na.omit(unique(c(lookup_knees$OPCS_1, lookup_knees$OPCS_2)))
  tf_map <- sapply(unique, function(i) str_detect(longopcs1$OPERTN, i))

## MAKING THE LOOP:
# Kind of like Python, make an empty object first for the loop that we are going to make:
  res <- rep(NA, nrow(longopcs1)) 

  for (i in seq_len(nrow(lookup_knees))) {
    # Since the NJR codes only ever have max. two OPCS codes for one given combination,
    # The if else here checks if we only have to look for 1 or both codes
    match <- if (is.na(lookup_knees$OPCS_2[i])) { tf_map[, lookup_knees$OPCS_1[i]] } 
    else { 
      tf_map[, lookup_knees$OPCS_1[i]] & tf_map[, lookup_knees$OPCS_2[i]]    
      }
    res[is.na(res) & match] <- lookup_knees$NJR[i]
  }
  
  longopcs1$NJR_code <- res
  longopcs1$Revision_Pres<- if_else(!is.na(longopcs1$NJR_code), 1, NA)
  
## Double checked in a separate script using the same style of code as the original
  ## It gives the same result, so this works as far as I know.
  
### Now we extract the ones with  fixation (original method since not as many conditions)
longopcs1 <- longopcs1 %>%
             mutate(Fixation_Pres = case_when(
               str_detect(OPERTN, fixation) & str_detect(OPERTN, knee) ~1,
               str_detect(OPERTN, fixation) & str_detect(OPERTN, femur) ~1,
               str_detect(OPERTN, fixation) & str_detect(OPERTN, tibia) ~1,
               str_detect(OPERTN, fixation) & str_detect(OPERTN, patella) ~1))

# This extracts the ones that don't have an NA (so have done rev/fix)
longopcs2 <- longopcs1 %>% 
             filter(!is.na(NJR_code)|!is.na(Fixation_Pres))

RF_knees_opcs <- longopcs2
# There are two items that are here but have the side "none"? ID: 238, 346
rm(longopcs2)

##### IF THERE ARE BILATERAL CASES ######
# Duplicate bilateral cases and rename side `right` and `left`
# There were no bilateral cases in this set though.
# longopcs_bilat <- bind_rows(
#  longopcs2 %>% filter(Side=="bilateral") %>% mutate(Side="left"),
#  longopcs2 %>% filter(Side=="bilateral") %>% mutate(Side="right")) %>%
#  ungroup

# Remove bilat and add opcs_bilat
# longopcs3 <- longopcs2 %>% ungroup %>%
#  filter(!side== "bilateral") %>%
#  bind_rows(longopcs_bilat)


##### Formatting the data - LONGFORM ICD ####
knees_longicd <- clean_test %>%
                 select(Row, starts_with("Diagnosis_")) %>%
                 mutate(across(everything(), ~na_if(.x,""))) %>%
                 unite(col = "value", -Row, sep = ", ", na.rm = T, remove = T)

# Manually identifying strings:
# Will try making a loop for this later too so that you could technically just swap out as many CSV with the codes you want and their description (like with OPCS).

# The code should be the exact same, but I just reordered the cases so that the less specific one will be lower (so that the others may hit a TRUE first)
knees_longicd <- knees_longicd %>%
              mutate(fracture_icd = case_when(
                str_detect(value,"M966")& str_detect(value,"S72") ~ "Fracture after implant insertion & fractured femur",
                str_detect(value,"M966")& str_detect(value,"S820") ~ "Fracture after implant insertion & fractured patella",
                str_detect(value,"M966")& str_detect(value,"S821") ~ "Fracture after implant insertion & fractured tibia",
                str_detect(value,"M966") ~ "Fracture after implant insertion",
                str_detect(value,"M971")& str_detect(value,"S72") ~ "Periprosthetic fracture around knee joint & fractured femur",
                str_detect(value,"M971")& str_detect(value,"S820") ~ "Periprosthetic fracture around knee joint & fractured patella",
                str_detect(value,"M971")& str_detect(value,"S821") ~ "Periprosthetic fracture around knee joint & fractured tibia",
                str_detect(value,"M971")~ "Periprosthetic fracture around knee joint",
                str_detect(value,"M979")~ "Periprosthetic fracture unspecified",
                str_detect(value,"S72") ~ "Fractured femur",
                str_detect(value,"S820") ~ "Fractured patella",
                str_detect(value,"S821") ~ "Fractured tibia",
                str_detect(value,"M8445") ~ "Pathological fracture of femur",
                str_detect(value,"M8446") ~ "Pathological fracture of tibia/fibula",
                str_detect(value,"M8005|M8085|M8095") ~ "Osteoporotic fracture of femur",
                str_detect(value,"M8006|M8086|M8096") ~ "Osteoporotic fracture of tibia/fibula",
                str_detect(value,"M8435") ~ "Stress fracture of femur",
                str_detect(value,"M8436") ~ "Stress fracture of tibia/fibula",  
                str_detect(value,"M8455") ~ "Pathological (neoblastic) fracture of femur", 
                str_detect(value,"M8456") ~ "Pathological (neoblastic) fracture of tibia/fibula",
                str_detect(value,"M8465") ~ "Pathological (NOS) fracture of femur",
                str_detect(value,"M8466") ~ "Pathological (NOS) fracture of tibia/fibula",
                str_detect(value,"M8475") ~ "Atypical fracture of femur",
                str_detect(value,"M9075") ~ "Pelvis or thigh neoplastic fracture"),
                fracture_icdL = ifelse(!is.na(fracture_icd),1,NA))

thr_tkr_pres <- clean_test %>%
                select(Row, starts_with("thr_"), starts_with("tkr_"))

longall <- merge(RF_knees_opcs, knees_longicd, by = "Row")

###Identifying HIP-RELATED procedures/PFFs ####

hip_codes <- read.csv("Hip_Rev_NJR.csv", sep = ",", header = TRUE)
hip_opcs <- hip_codes %>%
  select(,-Description) %>% 
  setnames(c("Item", "Operation.OPCS.codes"), 
           c("NJR", "OPCS"), skip_absent = TRUE)
hip_opcs[c("OPCS_1", "OPCS_2")] <- str_split_fixed(hip_opcs$OPCS, ", ", 2) 
hip_opcs <- hip_opcs[, -2]
hip_opcs[hip_opcs == "" | hip_opcs == " "] <- NA

# Here I am extracting the OPCS codes that are unique to the hip NJR revision codes;
uniquehip <- na.omit(unique(c(hip_opcs$OPCS_1, hip_opcs$OPCS_2)))
common <- intersect(unique, uniquehip)
uniquehip_codes <- setdiff(uniquehip, common)
string_hipopcs <- paste(uniquehip_codes, collapse = " ") %>%
  str_replace_all(., " ", "\\|")

# In theory, if any of these are present in OPERTN then, even if the patient has a TKR, they were admitted and a procedure was done because of the THR, not the TKR.

########## Final setup #############

longall2 <- longall  %>%
  mutate(surghes = case_when( 
    # any revision but fixation only when fracture ICD exists
    Revision_Pres == 1 & Fixation_Pres== 1 & fracture_icdL == 1 ~ "Revision_Fixation",
    # opcs fixation and icd femur fracture == fixation
    is.na(Revision_Pres) & Fixation_Pres == 1 & fracture_icdL == 1 ~ "Fixation",
    # Revisions as per NJR
    Revision_Pres == 1 ~ "Revision")) %>% 
  # keep only cases with a revision, fixation or both
  filter(!is.na(surghes)) %>%
  left_join(thr_tkr_pres, longall2, by = "Row") %>%
  # Cases where there is a tkr and it's on the same side as the operation reported
  filter(tkr_right == "TRUE" & Side == "right" |tkr_left == "TRUE" & Side == "left") %>%
  # I'm removing cases where there is absolutely no replacement of any type present.
  filter_out(tkr_right == "FALSE" & tkr_left == "FALSE" & thr_right == "FALSE" & thr_left == "FALSE") %>%
  # Finding any entry with hip related OPCS codes in the OPERTN column
  mutate(hip_related = case_when(
                       str_detect(OPERTN, string_hipopcs) & 
                      (str_detect(thr_left, "TRUE") | str_detect(thr_right, "TRUE")) ~ 1)) %>%
  select(Row, OP_DATE, thr_right, thr_left, tkr_right, tkr_left, Side, surghes, 
         fracture_icd, fracture_icdL, NJR_code, Revision_Pres, Fixation_Pres, hip_related) %>%
  distinct()

longall2$Row <- as.numeric(longall2$Row)

##### Identification of POPFF  &  cleanup #####
final_set <- longall2 %>%
             mutate(PFF = case_when((Revision_Pres==1|Fixation_Pres==1) 
                                    & fracture_icdL==1 ~1)) %>%
            # Even if someone has a PFF it might be because of the hip, not knee, so filter out any entries that are flagged as having a hip revision related procedure:
             filter_out(hip_related == 1) %>%
             select(Row, OP_DATE, tkr_right, tkr_left, Side, surghes, fracture_icd,
                    fracture_icdL, NJR_code, Revision_Pres, Fixation_Pres, PFF) %>%
             arrange(Row)

write.csv(final_set, "final_set.csv", na = "0", row.names = FALSE)
message("The final_set.CSV file should now be found in your working directory.")

# Cleanup
rm(knees_longicd, knees_longopcs, longopcs1, longall2,
   longall, lookup_knees, RF_knees_opcs, tf_map,
   hip_codes, hip_opcs, thr_tkr_pres)

                
                
                