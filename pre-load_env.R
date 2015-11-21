library(jsonlite)

# Load user data
juser <- fromJSON(sprintf("[%s]", paste(readLines("yelp_academic_dataset_user.json"), 
                                    collapse=",")))

# Load and pre-process tip data
jtip <- fromJSON(sprintf("[%s]", paste(readLines("yelp_academic_dataset_tip.json"), 
                                  collapse=",")))
ntips <- jtip[,c(1,3)]
# write.csv(ntips, "ntips.csv")

# Load and pre-process review data
jreview <- fromJSON(sprintf("[%s]", paste(readLines("yelp_academic_dataset_review.json"),
                                  collapse=",")))
nreviews <- jreview[,c(2,8)]
# write.csv(nreviews, "nreviews.csv")

# Load user data
jbusiness <- fromJSON(sprintf("[%s]", paste(readLines("yelp_academic_dataset_business.json"), 
                                 collapse=",")))
# Reading the pre-processed files
#@@@ntips <- read.csv("ntips.csv")
#@@@nreviews <- read.csv("nreviews.csv")
rm(jreview)
rm(jtip)

# Building list for the hotel category 
subcat <- jbusiness$categories
catlodge <- grepl( "^(.*[Hh]otel.*)",  subcat)
length(catlodge[catlodge==TRUE])
subamb <- jbusiness$attributes$"Ambience"
busid <- jbusiness[,1]
catplus <- data.frame(busid,catlodge)
# Build DF with observations
df_amb <- cbind(catplus,subamb)
df_amb <- na.omit(df_amb)

lodge_amb <- df_amb[(df_amb$catlodge == "TRUE"), ]
lodge_amb <- na.omit(lodge_amb)

# Building list for the restaurants
catrest <- grepl( "^(.*[Rr]estaurants.*)",  subcat)
catplus <- data.frame(busid,catrest)

df_amb <- cbind(catplus,subamb)
df_amb <- na.omit(df_amb)

rest_amb <- df_amb[(df_amb$catrest == "TRUE"), ]
rest_amb <- na.omit(rest_amb)

# Focus now on trendy
trendy <- rest_amb[(rest_amb$trendy == "TRUE"), ]
trendy$busid <- as.character(trendy$busid)
ntips$business_id <- as.character(ntips$business_id)
nreviews$business_id <- as.character(nreviews$business_id)
ntips$user_id <- as.character(ntips$user_id)
nreviews$user_id <- as.character(nreviews$user_id)

for (obser in 1:nrow(trendy)) {
  tips <- nrow(ntips[(ntips$business_id == trendy[obser,1]),])
  reviews <- nrow(ntips[(nreviews$business_id == trendy[obser,1]),])
  business <- trendy[obser,1]
  if(obser == 1){
    bus_map <- cbind(business, tips, reviews)
  }  else { 
    rhold <- cbind(business, tips, reviews)
    bus_map <- rbind(bus_map, rhold)
  }
}
bus_map[is.na(bus_map)] <- 0
bus_map <- as.data.frame(bus_map)

# examine the tips
for (obser in 1:nrow(trendy)) {
  tips <- ntips[(ntips$business_id == trendy[obser,1]),]
  if(obser == 1){
    user_map_tips <- rbind(tips)
  }  else { 
    user_map_tips <- rbind(user_map_tips, tips)
  }
}

#user_map_tips <- user_map_tips[,2-3]
user_plot_tips <- as.data.frame(table(user_map_tips$user_id))
business_plot_tips <- as.data.frame(table(user_map_tips$business_id))

# examine the reviews
for (obser in 1:nrow(trendy)) {
  reviews <- nreviews[(nreviews$business_id == trendy[obser,1]),]
  if(obser == 1){
    user_map_reviews <- rbind(reviews)
  }  else { 
    user_map_reviews <- rbind(user_map_reviews, reviews)
  }
}
#user_map_reviews <- user_map_reviews[,2-3]
user_plot_reviews <- as.data.frame(table(user_map_reviews$user_id))
business_plot_reviews <- as.data.frame(table(user_map_reviews$business_id))
colnames(user_plot_tips)[1] <- "user_id"
colnames(user_plot_reviews)[1] <- "user_id"
gdata <- merge(user_plot_reviews, user_plot_tips, by="user_id")

# focus on users
juser <- flatten(juser)
user_plot_tips$user_id <- as.character(user_plot_tips$user_id)
user_plot_reviews$user_id <- as.character(user_plot_reviews$user_id)
user_top_reviewers <- tail(user_plot_reviews[order(user_plot_reviews$Freq),], 100)
user_top_tipers <- tail(user_plot_tips[order(user_plot_tips$Freq),], 100)

# look at the top tipers and reviewers
for (obser in 1:nrow(user_top_tipers)) {
  top_dog <- juser[(juser$user_id == user_top_tipers[obser,1]),c(4,2,6)]
  if(obser == 1){
    tip_top_profile <- rbind(top_dog)
  }  else { 
    tip_top_profile <- rbind(tip_top_profile, top_dog)
  }
}
for (obser in 1:nrow(user_top_reviewers)) {
  top_dog <- juser[(juser$user_id == user_top_reviewers[obser,1]),c(4,2,6)]
  if(obser == 1){
    review_top_profile <- rbind(top_dog)
  }  else { 
    review_top_profile <- rbind(review_top_profile, top_dog)
  }
}

for (obser in 1:nrow(user_plot_tips)) {
  tip_dog <- juser[(juser$user_id == user_plot_tips[obser,1]),c(4,2,6)]
  if(obser == 1){
    tip_profile <- rbind(tip_dog)
  }  else { 
    tip_profile <- rbind(tip_profile, tip_dog)
  }
}

# Build data frame for trendy user reviewers (all population)
for (obser in 1:nrow(user_plot_reviews)) {
  rev_dog <- juser[(juser$user_id == user_plot_reviews[obser,1]),c(4,2,6)]
  if(obser == 1){
    review_profile <- rbind(rev_dog)
  }  else { 
    review_profile <- rbind(review_profile, rev_dog)
  }
}
# Look at the fans
tip_sum <- group_by(tip_profile, fans)
review_sum <- group_by(review_profile, fans)
tip_sum <- summarize(tip_sum, mean = mean(review_count))
review_sum <- summarize(review_sum, mean = mean(review_count))

# slide prep
amb_names <- lodge_amb[1,3:11]
saveRDS(amb_names, "amb_names.rds")

# Clean up!
rm(catlodge)
rm(busid)
rm(catrest)
rm(subcat)
rm(catplus)
rm(df_amb)
rm(subamb)
rm(rhold)
rm(obser)
rm(user_map_reviews)
rm(user_map_tips)
rm(rev_dog)
rm(tip_dog)
rm(business)
rm(tips)
rm(reviews)
rm(top_dog)
rm(trendy)
