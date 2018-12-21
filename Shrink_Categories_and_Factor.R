# @author: Amandeep, Philip Fang
# "We are drowning in information, while starving for wisdom - E. O. Wilson"

max.category.cnt <- 51

# This function will merge the categories with lower frequency into one category called "others"
# This function also takes care about the missing values and puts those in "missing" data category
shrink_categories_and_factor <- function(trainingData, column){
  temp <- unique(trainingData[, freq := .N, by = get(column)][, c(column,"freq"), with = FALSE])
  setorder(temp, -freq)
  max_cat_allowed_freq <- temp[max.category.cnt, freq]
  if(!is.na(max_cat_allowed_freq)){
    trainingData[, eval(column) := as.character(get(column))]
    trainingData[get(column) == "-1", eval(column) := "missing"]
    trainingData[trainingData$freq <= max_cat_allowed_freq, eval(column) := "others"] 
  }
  trainingData[, eval(column) := factor(get(column))]
  #trainingData[, get(column)]
}

not__to_factor <- c("id", "click","site_id","app_id","device_id","device_ip","site_domain","device_model")
# C14, C19, C20, C21 might be deleted if necessary

categorical_variables <- setdiff(colnames(trainingData), not__to_factor)
for(column in categorical_variables){
  shrink_categories_and_factor(trainingData, column)
}

trainingData[, freq := NULL]

