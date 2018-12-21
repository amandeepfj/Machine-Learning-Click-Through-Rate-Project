# @author: Philip

#This file is to explore the variables, actual merging happens in Shrink_Categories_Factor.R file


colnames(trainingData)

max.category.cnt <- 32

# see if there's NA ----------------------------------------------------------
cat("Number of Missing Values in the Training data\n\n")
sapply(trainingData,FUN=function(x) {sum(is.na(x))})

# see class
sapply(trainingData,class)

# look at overall CTR=0.168 -------------------------------------------------
mean(trainingData$click)


# Check out this function this wil merge based on max categories usable ----------------------
shrink_categories_and_factor <- function(trainingData, column){
  temp <- unique(trainingData[, freq := .N, by = get(column)][, c(column,"freq"), with = FALSE])
  setorder(temp, -freq)
  max_cat_allowed_freq <- temp[max.category.cnt, freq]
  if(!is.na(max_cat_allowed_freq)){
    trainingData[, eval(column) := as.character(get(column))]
    trainingData[trainingData$freq <= max_cat_allowed_freq, eval(column) := "others"] 
  }
  trainingData[, eval(column) := factor(get(column))]
  trainingData[, get(column)]
}

categorical_variables <- setdiff(colnames(trainingData), c("id", "click", "hour", "week_day"))
for(column in categorical_variables){
  shrink_categories_and_factor(trainingData, column)
}



# Categorical variable exploration (31)---------------------------------------

# Banner_pos * 3
temp<-unique(trainingData[,avg:=mean(click),by="banner_pos"][,freq:=.N,by="banner_pos"][,c("banner_pos","avg","freq")])
setorder(temp,-freq)
temp
trainingData$banner_pos[trainingData$freq<=101]<-"banner_pos_others"
trainingData$banner_pos<-factor(trainingData$banner_pos)
# table(trainingData[, get("banner_pos")])
# barplot(table(trainingData[, get("banner_pos")]))


# C1 * 3
temp<-unique(trainingData[,avg:=mean(click),by="C1"][,freq:=.N,by="C1"][,c("C1","avg","freq")])
setorder(temp,-freq)
temp
trainingData$C1[trainingData$freq<=5000]<-"C1_others"
trainingData$C1<-factor(trainingData$C1)
# table(trainingData[, get("C1")])
# barplot(table(trainingData[, get("C1")]))


# SiteDomain ?
# temp<-unique(trainingData[,avg:=mean(click),by="site_domain"][,freq:=.N,by="site_domain"][,c("site_domain","avg","freq")])
# setorder(temp,-freq)
# temp
# trainingData$site_domain[trainingData$freq<=8000]<-"site_domain_others"
# trainingData$site_domain<-factor(trainingData$site_domain)
# table(trainingData[, get("site_domain")])
# barplot(table(trainingData[, get("site_domain")]))


# SiteCategory *4
temp<-unique(trainingData[,avg:=mean(click),by="site_category"][,freq:=.N,by="site_category"][,c("site_category","avg","freq")])
setorder(temp,-freq)
temp[max.category.cnt:2]
trainingData$site_category[trainingData$freq<=8000]<-"site_category_others"
trainingData$site_category<-factor(trainingData$site_category)
# table(trainingData[, get("site_category")])
# barplot(table(trainingData[, get("site_category")]))
column <- "site_category"


# appdomain * 3
temp<-unique(trainingData[,avg:=mean(click),by="app_domain"][,freq:=.N,by="app_domain"][,c("app_domain","avg","freq")])
setorder(temp,-freq)
temp
trainingData$app_domain[trainingData$freq<=8000]<-"app_domain_others"
trainingData$app_domain<-factor(trainingData$app_domain)
# table(trainingData[, get("app_domain")])
# barplot(table(trainingData[, get("app_domain")]))

# appcategory *3
temp<-unique(trainingData[,avg:=mean(click),by="app_category"][,freq:=.N,by="app_category"][,c("app_category","avg","freq")])
setorder(temp,-freq)
temp
trainingData$app_category[trainingData$freq<=20000]<-"app_category_others"
trainingData$app_category<-factor(trainingData$app_category)
# table(trainingData[, get("app_category")])
# barplot(table(trainingData[, get("app_category")]))

# device_model  ?
# temp<-unique(trainingData[,avg:=mean(click),by="device_model"][,freq:=.N,by="device_model"][,c("device_model","avg","freq")])
# setorder(temp,-freq)
# temp
# trainingData$device_model[trainingData$freq<=2000]<-"device_model_others"
# table(trainingData[, get("device_model")])
# barplot(table(trainingData[, get("device_model")]))


# device_type * 3
temp<-unique(trainingData[,avg:=mean(click),by="device_type"][,freq:=.N,by="device_type"][,c("device_type","avg","freq")])
setorder(temp,-freq)
temp
trainingData$device_type[trainingData$freq<=5000]<-"device_type_others"
trainingData$device_type<-factor(trainingData$device_type)
# table(trainingData[, get("device_type")])
# barplot(table(trainingData[, get("device_type")]))

# device_conn_type *3
temp<-unique(trainingData[,avg:=mean(click),by="device_conn_type"][,freq:=.N,by="device_conn_type"][,c("device_conn_type","avg","freq")])
setorder(temp,-freq)
temp
trainingData$device_conn_type[trainingData$freq<=7000]<-"device_conn_type_others"
trainingData$device_conn_type<-factor(trainingData$device_conn_type)
# table(trainingData[, get("device_conn_type")])
# barplot(table(trainingData[, get("device_conn_type")]))


# C14  ?
# temp<-unique(trainingData[,avg:=mean(click),by="C14"][,freq:=.N,by="C14"][,c("C14","avg","freq")])
# setorder(temp,-freq)
# temp
# trainingData$C14[trainingData$freq<=2000]<-"C14_others"
# table(trainingData[, get("C14")])
# barplot(table(trainingData[, get("C14")]))


# C15 *2
temp<-unique(trainingData[,avg:=mean(click),by="C15"][,freq:=.N,by="C15"][,c("C15","avg","freq")])
setorder(temp,-freq)
temp
trainingData$C15[trainingData$freq<=6000]<-"C15_others"
trainingData$C15<-factor(trainingData$C15)
# table(trainingData[, get("C15")])
# barplot(table(trainingData[, get("C15")]))


# C16 * 2
temp<-unique(trainingData[,avg:=mean(click),by="C16"][,freq:=.N,by="C16"][,c("C16","avg","freq")])
setorder(temp,-freq)
temp
trainingData$C16[trainingData$freq<=5000]<-"C16_others"
trainingData$C16<-factor(trainingData$C16)
# table(trainingData[, get("C16")])
# barplot(table(trainingData[, get("C16")]))


# C17 *2
temp<-unique(trainingData[,avg:=mean(click),by="C17"][,freq:=.N,by="C17"][,c("C17","avg","freq")])
setorder(temp,-freq)
temp
trainingData$C17[trainingData$freq<=5000]<-"C17_others"
trainingData$C17<-factor(trainingData$C17)
# table(trainingData[, get("C17")])
# barplot(table(trainingData[, get("C17")]))


# C18 * 3
temp<-unique(trainingData[,avg:=mean(click),by="C18"][,freq:=.N,by="C18"][,c("C18","avg","freq")])
setorder(temp,-freq)
temp
trainingData$C18[trainingData$freq<=20000]<-"C18_others"
trainingData$C18<-factor(trainingData$C18)
# table(trainingData[, get("C18")])
# barplot(table(trainingData[, get("C18")]))


# C19  ?
# temp<-unique(trainingData[,avg:=mean(click),by="C19"][,freq:=.N,by="C19"][,c("C19","avg","freq")])
# setorder(temp,-freq)
# temp
# trainingData$C19[trainingData$freq<=5000]<-"C19_others"
# table(trainingData[, get("C19")])
# barplot(table(trainingData[, get("C19")]))


# C20  ? -1 means NA?
# temp<-unique(trainingData[,avg:=mean(click),by="C20"][,freq:=.N,by="C20"][,c("C20","avg","freq")])
# setorder(temp,-freq)
# temp
# trainingData$C20[trainingData$freq<=5000]<-"C20_others"
# table(trainingData[, get("C20")])
# barplot(table(trainingData[, get("C20")]))


# C21  ?
# temp<-unique(trainingData[,avg:=mean(click),by="C21"][,freq:=.N,by="C21"][,c("C21","avg","freq")])
# setorder(temp,-freq)
# temp
# trainingData$C21[trainingData$freq<=10000]<-"C21_others"
# trainingData$C21<-factor(trainingData$C21)
# table(trainingData[, get("C21")])
# barplot(table(trainingData[, get("C21")]))


# Work on Time variable ---------- ------------------------------------------

# change time to hour * 24
temp <- trainingData[, week_day := wday(ymd_h(as.character(hour)))]
temp[, hour := (hour %% 100)]
temp<-unique(temp[,avg:=mean(click),by="hour"][,freq:=.N,by="hour"][,c("hour","avg","freq")])
setorder(temp,-freq)
temp
trainingData$hour<-factor(trainingData$hour)

trainingData[, click := factor(trainingData[, click])]




