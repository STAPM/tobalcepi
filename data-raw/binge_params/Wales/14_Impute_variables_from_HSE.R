### read in HSE imputed dataset for the English binge model parameters and 
### combine with the NSW data. Use English data to impute edu4end and social_grade
### variables in the NSW

data_imp_hse <- readRDS(paste0("X:/ScHARR/PR_STAPM/Code/R_packages/tobalcepi/data-raw/binge_params/England/tobalc_consumption_eng_national_2011-2018_v1_2023-10-26_hseclean_1.11.3_imputed.rds"))
#data_imp_hse[, country := "England"]
data_imp_hse <- data_imp_hse[year %in% 2016:2018]

data_imp_nsw <- readRDS(paste0("X:/ScHARR/PR_STAPM/Code/R_packages/tobalcepi/data-raw/binge_params/Wales/tobalc_consumption_wales_2016-2022_v1_2025-03-12_hseclean_1.14.0_imputed.rds"))
#data_imp_nsw[, country := "Wales"]
data_imp_nsw <- data_imp_nsw[!is.na(sex)]

#####################################################
#### Set up the HSE for the matching process ########

set.seed(42)
ids <- 1:nrow(data_imp_hse)

sample_size <- 100000

# population is going to be created by duplicating rows according to the survey weights

# Standardise survey weights to sum to 1
probs <- data_imp_hse$wt_int / sum(data_imp_hse$wt_int, na.rm = T)

# Calculate the number of duplicates required of each row
dups <- round(probs * sample_size, 0)

# The fact that the number of row duplicates has to be rounded to an integer number
# means that the final sample size will not be exactly equal to sample_size
# In this case the size of the starting population sample produced should be recorded
#sum(probs * sample_size)

# Get the vector of required ids
ids_samp <- unlist(sapply(ids, function(z) rep(ids[z], dups[z])))

# Construct the population
data_sample_new <- data_imp_hse[ids_samp]

data_sample_new[ , wt_int := NULL]


# Remove objects not needed any more
rm(ids_samp, ids, probs)
gc()

#############################################################################################
#### by matching variable subgroups, get a list of values of the variables to impute in NSW

data_match <- data_sample_new[ , .(eduend4cat = list(eduend4cat),
                                   social_grade = list(social_grade)), by = c("sex", "age_cat", "imd_quintile")]


domain <- data.frame(expand.grid(
  sex = unique(data_sample_new$sex),
  age_cat = unique(data_sample_new$age_cat),
  imd_quintile = unique(data_sample_new$imd_quintile)
)) 

setDT(domain)

data_match2 <- merge(domain, data_match, by = c("sex", "age_cat", "imd_quintile"), all.x = T, all.y = F)

data_match <- data_sample_new[ , .(eduend4cat2 = list(eduend4cat),
                                   social_grade2 = list(social_grade)), by = c("sex", "imd_quintile")]

data_match2 <- merge(data_match2, data_match, by = c("sex", "imd_quintile"), all.x = T, all.y = F)

data_match2[is.null(social_grade)]

rm(domain, data_match)
gc()

######################################################################
### Step 3. Do the matching to the National Survey for Wales data 

# read imputed National Survey for Wales data 
wales_data <- copy(data_imp_nsw)

# merge the HSE data into the National Survey for Wales data
# using the subgroups and index numbers

data_imp <- merge(data_imp_nsw, data_match2, by = c("sex", "age_cat", "imd_quintile"), all.x = T, all.y = F)

#data_imp[ran_i >= p_comb, prop_handrolled := 0]

data_imp[ , ran_seed := rpois(nrow(data_imp), 1000)]

n <- nrow(data_imp)

### loop over every row and sample from the vector of possible 
### edu4end and social_grade values from the HSE data for 
### that subgroup combination

for (ix in 1:n){
  
set.seed(data_imp[ix, ran_seed])
  
z1 <- as.vector(unlist(data_imp[ix, eduend4cat]))
z2 <- as.vector(unlist(data_imp[ix, social_grade]))

if(is.null(z1)) {
  
  z1 <- as.vector(unlist(data_imp[ix, eduend4cat2]))
  
}
if(is.null(z2)) {
  
  z2 <- as.vector(unlist(data_imp[ix, social_grade2]))
  
}

data_imp[ix, eduend4cat_imp := sample(z1, 1)]
data_imp[ix, social_grade_imp := sample(z2, 1)]  

cat(ix, "of", n, "\r")
utils::flush.console()
  
}

data_imp[, c("imputation","eduend4cat","social_grade","eduend4cat2","social_grade2","ran_seed") := NULL]

setnames(data_imp, c("eduend4cat_imp","social_grade_imp"), c("eduend4cat","social_grade"))


saveRDS(data_imp, paste0("X:/ScHARR/PR_STAPM/Code/R_packages/tobalcepi/data-raw/binge_params/Wales/tobalc_consumption_wales_2016-2022_v1_FINAL_imputed.rds"))

