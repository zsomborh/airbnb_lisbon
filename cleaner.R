rm(list = ls())

library(tidyverse)
library(data.table)


df <- read_csv('listings.csv')

# First sense checks ------------------------------------------------------

summary(df)
unique(df$last_scraped) # all listings are downloaded in an interval of 4 days between 20th and 24th of Dec, 2020
sum(rowSums(is.na(df)) == ncol(df)) # There are no only NA rows
nrow(df[duplicated(df),]) # There are no duplicates either

# Reduce scope to fit for exercise ----------------------------------------

# 1) Between 2 and 6 accomodates
df <- df[df$accommodates >= 2 & df$accommodates <= 6,]


# 2) Apartmants - I will reduce this category sizeably

properties_df <- df %>%  group_by(property_type) %>%  summarise(count = n())
properties_df <- 
    properties_df %>%  mutate(
    property_type = tolower(property_type))

#We check those cases when we don't have apartment in the name - they are different entities to what we are interested in
View(properties_df[!grepl('(apartment)|(condominium)',x = properties_df$property_type ),])
properties_df  <- properties_df[grepl('(apartment)|(condominium)',x = properties_df$property_type ),]
#We still have a lot of accomodations that we don't want to include 
View(properties_df[!grepl('(room)',x = properties_df$property_type ),])
View(properties_df[grepl('(room)',x = properties_df$property_type ),])

# I will have three categories - rooms, apartments (based on few checks condominimum is close to apartments)
rooms  = c(
'private room in apartment',
'private room in condominium',
'room in serviced apartment')

apartments = c(
    'entire apartment',
    'entire condominium',
    'entire serviced apartment'
)


df <- df %>%  mutate(
        property_type = tolower(property_type),
        property_type = ifelse(property_type %in% rooms,'room',ifelse(property_type %in% apartments,'apartment', 'not_used'))) 

df <- df %>% filter(property_type != 'not_used')


# Remove variables with too many missing values ---------------------------

plot_Missing <- function(data_in, title = NULL){
    temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
    temp_df <- temp_df[,order(colSums(temp_df))]
    data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
    data_temp$m <- as.vector(as.matrix(temp_df))
    data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
    ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}

plot_Missing(df[,colSums(is.na(df))>0])

temp <- sort(colSums(is.na(df[,colSums(is.na(df)) >0])))
temp_df <- df[names(temp[temp>.1*nrow(df)])] 

# there are quite a lot of missing values for some cols, we will remvoe those that have more than 20% missing + firt a
df<-df %>% select(-c(names(temp[ temp >.2*nrow(df)]) ,'last_review', 'first_review'))


# Remove useless variables ------------------------------------------------

# The below is just some justification why I left them out
colnames(df)[20] # This is the same as host_listings_count as seen above
colnames(df)[12] # There are just too many categories, it won't make that much of a difference
colnames(df)[21] # too complex, yet not so much benefit coming from this 
colnames(df)[24] # too many categories, grouped neighbourhood group cleansed has less categories
colnames(df)[44] # very low variation - everything is available, maybe due to
colnames(df)[51:52] # number of reviews already captures this
# host_has_profile_pic - it won't affect the price probably - it will juts bring in noise
# room type doesn't matter since variable is already taken care of 
# host_id - we have host listing which is better
# maximum_nights - minimum nihts is enough
#  'review_scores_accuracy',"review_scores_cleanliness","review_scores_checkin","review_scores_communication", "review_scores_location","review_scores_value,
#   these are all captured in the ratings column 

drop_cols = c(colnames(df)[c(2:5,7,9:10,12,17:18,20,21,24,26,27, 38:49, 51:52, 60:64)],
              'description', 'host_id', 'room_type', 'host_has_profile_pic', 'host_acceptance_rate', 'host_response_rate', 'host_response_time',
              'calculated_host_listings_count_shared_rooms', 'maximum_nights', 'review_scores_accuracy',"review_scores_cleanliness",
              "review_scores_checkin","review_scores_communication", "review_scores_location","review_scores_value")
keep_cols = subset(colnames(df), !(colnames(df) %in% drop_cols))

df = df[,keep_cols]


# Clean the useful variables, drop or impute NAs --------------------------


df <- df %>%  mutate(
    bathrooms = as.numeric(gsub('[^[:digit:].]','',bathrooms_text)),
    price = as.numeric(gsub('[^[:digit:].]','',price)),
    bathrooms_text = NULL)

#checking prices
check_price <- df %>% 
    arrange(desc(price)) %>% 
    head(100)

ggplot(data = df, aes(x = price)) +
    geom_histogram(aes(x = price), binwidth = 100 )

# I will drop observations with prices higher than 1000
df <- df %>% filter(price < 1000)

#transform host since to how many years host is active
colnames(df)
unique(df$host_since)

df <- df %>% filter(!is.na(host_since)) # there is one variable with NA here, that we removed

df %>% mutate(
    host_since = as.numeric(round((as.Date('2020-12-20')- as.Date(host_since))/365,0)))

# neighbourhoods show that there are places that are not actually Lisbon but close neighbourhoods
# we narrow the scope to Lisboa only 
df %>% group_by(neighbourhood_group_cleansed) %>% summarise(n())

df<- df %>% filter(neighbourhood_group_cleansed == 'Lisboa') %>% mutate(neighbourhood_group_cleansed = NULL)

# let's look at bedrooms - there are missing values
unique(df$beds)

sum(is.na(df$beds))
# we will inpute missing values with assumption that every two guest has 1 bedroom, 1 guest has 1 bed
df<- df %>% mutate(bedrooms = ifelse(is.na(bedrooms),accommodates/2,bedrooms ),
                   beds = ifelse(is.na(beds),accommodates,beds ))

# min nights if more than 3, it stays at 3
df %>% group_by(minimum_nights) %>% summarise(n())

df <- df %>%
    mutate(minimum_nights= cut(minimum_nights, c(1,2,3,max(minimum_nights)), labels=c(1,2,3), right = F))

# review scores - impute with median, leave flag

df<- df %>% mutate(
    flag_review_scores_rating=ifelse(is.na(review_scores_rating),1, 0),
    review_scores_rating =  ifelse(is.na(review_scores_rating), median(review_scores_rating, na.rm = T), review_scores_rating))

# removing missing observations
unique(df$neighbourhood_group_cleansed)

write_csv(df,'cleaned_first_without_dummies.csv')

# from here amenities needs to be broken out