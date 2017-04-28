pacman::p_load(magrittr, dplyr)
install_github("ggbiplot", "vqv")
setwd("~/Documents/STAT/DC Case Study")

load("case_study_dt1.RData")
load("case_study_dt2.RData")

# Pull out the infinity g37 sedan
car1 <- case_study_dt1[case_study_dt1$source_Vehicle_Id==
                      "e4095762-6216-4efa-9605-405dc158959e" , ]

car1_speed <-  car1[!duplicated(car1$source_Vehicle_Speed_Timestamp), ]

car1_speed$source_Vehicle_Speed_Timestamp <- (car1_speed$source_Vehicle_Speed_Timestamp)

range(car1_speed$source_Vehicle_Speed_Timestamp, na.rm=TRUE)
##### Select rpm, speed, accel, decel info
## add make


library(dplyr)
car_combined <- rbind(case_study_dt1, case_study_dt2)

# first data set
grouped_data <- select(car_combined,
                       source_Vehicle_Id,
                       source_Vehicle_VinDetails_Make,
                       source_Vehicle_VinDetails_Model,
                       source_Vehicle_Acceleration_Value,
                       source_Vehicle_Deceleration_Value,
                       source_Vehicle_RPM_Value,
                       source_Vehicle_Speed_Value)

grouped_data[,4:7] <- apply(grouped_data[,4:7], 2, function(x) as.numeric(as.character(x))) 

# grouped_data <- na.omit(grouped_data)

grouped_data$decel_big <- 0
grouped_data[which(grouped_data$source_Vehicle_Deceleration_Value >10), "decel_big" ] <- 1
grouped_data$acel_big <- 0
grouped_data[which(grouped_data$source_Vehicle_Acceleration_Value >10), "acel_big" ] <- 1


grouped_data[which(grouped_data$source_Vehicle_Deceleration_Value==0), 5] <- NA 
grouped_data[which(grouped_data$source_Vehicle_Acceleration_Value==0), 4] <- NA 




# grouped_data[grouped_data$source_Vehicle_Deceleration_Value==0, 5] <- NA 
# grouped_data <- filter(grouped_data, 
#       source_Vehicle_Acceleration_Value > 0,
#       source_Vehicle_Deceleration_Value > 0 
# )


grouped_by_car <- grouped_data %>%
  group_by(source_Vehicle_Id, source_Vehicle_VinDetails_Model, source_Vehicle_VinDetails_Make) %>%
  summarise(
    
    avg_acel = mean(source_Vehicle_Acceleration_Value, na.rm=T)  %>% round(., digits=3),
    var_acel = var(source_Vehicle_Acceleration_Value, na.rm=T)   %>% round(., digits=3),
    avg_decel = mean(source_Vehicle_Deceleration_Value, na.rm=T) %>% round(., digits=3),
    var_decel = var(source_Vehicle_Deceleration_Value, na.rm=T)  %>% round(., digits=3),
    avg_rpm = mean(source_Vehicle_RPM_Value)                     %>% round(., digits=3),
    var_rpm = var(source_Vehicle_RPM_Value)                      %>% round(., digits=3),
    avg_speed = mean(source_Vehicle_Speed_Value)                 %>% round(., digits=3),
    total_big_decel = sum(decel_big),
    total_big_acel = sum(acel_big)
    ) 

grouped_by_car_good <- grouped_by_car

## perform pca
grouped_by_car <- na.omit(grouped_by_car)
pca_object <- prcomp(grouped_by_car[, 4:10], center=T, scale=T)
print(pca_object)
summary(pca_object)
## da plots

source_Vehicle_Id <- grouped_by_car$source_Vehicle_Id
model <- grouped_by_car$source_Vehicle_VinDetails_Model
cluster <- grouped_by_car$cluster %>% as.factor()

# clustering
carCluster <- kmeans(grouped_by_car[,4:10], 4, nstart=20)
carCluster
cluster_viz(carCluster)
grouped_by_car$cluster <- carCluster$cluster

# vis for clustering and PCA
library(ggbiplot)
g <- ggbiplot(pca_object, obs.scale = 1, var.scale = 1, 
              groups = cluster, ellipse = T, 
              circle = TRUE) 
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.position = "bottom")
print(g)

# histogram for clusters:
ggplot(data=grouped_by_car, aes(x=cluster))+ geom_histogram(binwidth=1, colour="#FF6666", fill="white") +
  ggtitle("Cluster Count, n=365")

