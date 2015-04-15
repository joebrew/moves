library(dplyr)

#####
# READ IN PHONE DATA
#####
setwd('../phone_data/')
for (i in dir()[which(grepl('.csv', dir()))]){
  assign(gsub('.csv', '', i), read.csv(i, stringsAsFactors = FALSE))
}

#####
# COMBINE ALL DATAFRAMES INTO ONE
#####
dfs <- names(which(sapply(.GlobalEnv, is.data.frame))) 
for (i in 1:length(dfs)){
  
  temp <- get(dfs[i])
  temp$Name <- dfs[i]
  
  if(i == 1){
    df <- temp
  } else {
    df <- rbind(df, temp)
  }
  # remove individuals
  rm( list = c(dfs[i]))
}

#####
# FORMAT DATE AND REMOVE UNECESSARY INFO
#####
df$Date <- gsub('/2015', '/15', df$Date)
df$Date <- as.Date(df$Date, '%m/%d/%y')

df$Group <- NULL

# Make minutes instead of seconds
df$Duration <- df$Duration / 60

############################################################
############################################################
############################################################

#####
# READ IN DIARY DATA
#####
setwd('../diary_data/')
for (i in dir()[which(grepl('.csv', dir()))]){
  assign(gsub('.csv', '', i), read.csv(i, stringsAsFactors = FALSE))
}

#####
# CLEAN UP
#####

# diary2 ##### ##### ##### #####

# select only relevant columns
diary2 <- diary2[,c('Date', 'Name', 'Activity', 'Time.Duration', 
                    'Time.Duration.1', 'Calculated.Time.Duration')]
names(diary2) <- c('Date', 'Name', 'Activity', 'Start', 'Finish', 'Duration')

# address times
diary2$start_hour <- as.numeric(gsub(':.*', '', diary2$Start))
diary2$start_hour <- ifelse(grepl('AM', diary2$Start), diary2$start_hour,
                            diary2$start_hour + 12)

# estimate time of day (period)
diary2$Period <- ifelse(diary2$start_hour <=12,
                        'Morning',
                        ifelse(diary2$start_hour <= 17,
                               'Afternoon',
                               'Evening'))

# whittle down a bit
diary2$Start <- diary2$Finish <- diary2$start_hour <- NULL

# dates
diary2$Date <- as.Date(diary2$Date, '%m/%d/%Y')

# diary1 ##### ##### ##### #####

# Clean up a bit
diary1 <- diary1[,c('Date', 'Name', 'Walk', 'Run', 'Bike', 'Time.Duration')]
diary1$Date <- as.Date(diary1$Date, '%m/%d/%Y')
diary1$Period <- diary1$Time.Duration
diary1$Time.Duration <- NULL

# Get format to match with df and diary2
d1 <- diary2[-c(1:nrow(diary2)),]
new_row_format = data.frame(Date = NA,
                            Name = NA, 
                            Activity = NA,
                            Duration = NA,
                            Period = NA)


for (i in 1:nrow(diary1)){
  old_row <- diary1[i,]
  
  # Walk
  if(old_row$Walk > 0 & !is.na(old_row$Walk)){
    new_row <- new_row_format
    new_row$Date <- old_row$Date
    new_row$Name <- old_row$Name
    new_row$Activity <- 'Walking'
    new_row$Duration <- old_row$Walk
    new_row$Period <- old_row$Period
    d1 <- rbind(d1, new_row)
  }
  
  # Run
  if(old_row$Run > 0 & !is.na(old_row$Run)){
    new_row <- new_row_format
    new_row$Date <- old_row$Date
    new_row$Name <- old_row$Name
    new_row$Activity <- 'Running'
    new_row$Duration <- old_row$Run
    new_row$Period <- old_row$Period
    d1 <- rbind(d1, new_row)
  }
  
  # Bike
  if(old_row$Bike > 0 & !is.na(old_row$Bike)){
    new_row <- new_row_format
    new_row$Date <- old_row$Date
    new_row$Name <- old_row$Name
    new_row$Activity <- 'Biking'
    new_row$Duration <- old_row$Bike
    new_row$Period <- old_row$Period
    d1 <- rbind(d1, new_row)
  }
}

diary1 <- d1


##### ##### ##### ##### 
# Join together diaries
diary <- rbind(diary1, diary2)

# Remove garbage
rm(d1, diary1, diary2, new_row, new_row_format, old_row, temp, dfs, i)

# Clean duration
for (i in 1:nrow(diary)){
  val <- diary$Duration[i]
  if (grepl(':', val)){
    hours <- as.numeric(gsub(':.*', '', val))
    minutes <- as.numeric(gsub('.*:', '', val))
    new_val <- as.numeric((hours*60) + minutes)
  } else {
    new_val <- as.numeric(val)
  }
  diary$Duration[i] <- new_val
}
diary$Duration <- as.numeric(diary$Duration)

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################


#####
# JOIN TOGETHER MASTER
#####
df <- df[,c('Date', 'Activity', 'Duration', 'Name')]
df$method <- 'app'

diary <- diary[,c('Date', 'Activity', 'Duration', 'Name')]
diary$method <- 'diary'
diary$Activity <- tolower(diary$Activity)
diary$Activity[which(diary$Activity == 'biking')] <- 'cycling'

master <- rbind(diary, df)

# Clean up names
master$Name <- ifelse(master$Name == 'joe', 'Joe',
                      ifelse(master$Name == 'jake', 'Jake',
                             ifelse(substr(tolower(master$Name),1,1) == 's',
                                    'Sheldon',
                                    ifelse(substr(tolower(master$Name),1,1) == 'x',
                                           'Xu',
                                           ifelse(substr(tolower(master$Name),1,1) == 'y',
                                                  'Yun', master$Name)))))

#####
# FILTER ONLY GOOD DATES
#####
master <- master[which(master$Date >= '2015-03-30' &
                         master$Date <= '2015-04-10'),]

# Remove transport
master <- master[which(master$Activity != 'transport'),]

# Capitalization in activity
master$Activity <- tolower(master$Activity)

#####
# SEPARATE AND AGGREGATE
#####
app_df <- master %>%
  filter(method == 'app') %>%
  group_by(Name, Date, Activity) %>%
  summarise(app_time = sum(Duration))
diary_df <- master %>%
  filter(method == 'diary') %>%
  group_by(Name, Date, Activity) %>%
  summarise(diary_time = sum(Duration))
agg <- left_join(app_df, diary_df,
                 by = c('Name', 'Date', 'Activity'))

# Fill NA's with 0s
agg$diary_time[which(is.na(agg$diary_time))] <- 0
agg$app_time[which(is.na(agg$app_time))] <- 0

#####
# VISUALIZE
#####
agg$color <- ifelse(agg$Activity == 'walking', 'darkgreen', 
                    ifelse(agg$Activity == 'running', 'darkred',
                           ifelse(agg$Activity == 'cycling', 'darkblue', NA)))
agg$color <- adjustcolor(agg$color, alpha.f = 0.6)
agg$shape <- ifelse(agg$Name == 'Joe', 21,
                    ifelse(agg$Name == 'Xu', 15,
                           ifelse(agg$Name == 'Sheldon', 16,
                                  ifelse(agg$Name == 'Jake', 17,
                                         ifelse(agg$Name == 'Yun', 18, 3)))))

plot(agg$diary_time,
     agg$app_time,
    xlim = c(0, 150),
    ylim = c(0, 150),
     pch = agg$shape,
     cex = 1,
     col = agg$color,
    xlab = 'Minutes (per diary)',
    ylab = 'Minutes (per phone)',
    main = 'Agreement between diary and phone app')
grid()


legend('bottomright',
       pch = c(21, 15:18),
       legend = c('Joe', 'Xu', 'Sheldon', 'Jake', 'Yun'),
       col = 'grey',
       title = 'Who',
       cex = 0.8,
       ncol = 2)

legend('bottom',
       fill = adjustcolor(c('darkgreen', 'darkred', 'darkblue'), alpha.f = 0.6),
       legend = c('Walking', 'Running', 'Cycling'),
       title = 'What', cex = 0.8,
       border = NA)

abline(a = 0, b = 1)
text(x = 120, y = 120,
     labels = 'Line of\nperfect match',
     col = adjustcolor('black', alpha.f = 0.6))

#####
#
#####
