# @author: Amandeep
# "We are drowning in information, while starving for wisdom - E. O. Wilson"

trainingData[, week_day := wday(ymd_h(as.character(hour)))]
trainingData[, is_week_day := 1]
trainingData[week_day %in% c(1,7), is_week_day := 0]
trainingData[, week_day := NULL]

#keeping only hour
trainingData[, hour := (hour %% 100)]
