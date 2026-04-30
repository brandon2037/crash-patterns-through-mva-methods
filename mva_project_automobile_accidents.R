# Multivariate Project
df <- read.csv("traffic_accidents.csv")
########################################
library(dplyr)
library(car)       
library(psych)
library(MASS)      
library(VGAM)     
library(pscl)     
library(ggplot2)
df$most_severe_injury <- factor(df$most_severe_injury, 
                                levels = c("NO INDICATION OF INJURY", "REPORTED, NOT EVIDENT", 
                                           "NONINCAPACITATING INJURY", "INCAPACITATING INJURY", "FATAL"),
                                ordered = TRUE)

df$intersection_related_i <- ifelse(df$intersection_related_i == "Y", 1, 0)
df$trafficway_type <- as.factor(df$trafficway_type)
df$alignment <- as.factor(df$alignment)
df$crash_type <- as.factor(df$crash_type)

df$damage <- recode(df$damage, 
                    "'$500 OR LESS' = 0; '$501 - $1,500' = 1; 'OVER $1,500' = 2")
categorical_vars <- c("weather_condition", "lighting_condition", "roadway_surface_cond",
                      "damage","traffic_control_device","intersection_related_i",
                      "road_defect","most_severe_injury","trafficway_type","alignment","crash_type"
)

df <- df %>%
  mutate(across(all_of(categorical_vars), as.factor))
df <- cbind(df, model.matrix(~ weather_condition + lighting_condition + 
                               roadway_surface_cond + first_crash_type + trafficway_type+alignment+crash_type+most_severe_injury- 1, data=df))
df <- df %>% select(-all_of(categorical_vars))

############################# EDA 1

hist(df$injuries_total, main="Histogram of Total Injuries", xlab="Total Injuries", col="blue")
mean(data$injuries_total)
var(data$injuries_total)
range(data$injuries_total)

hist(df$injuries_fatal, main="Histogram of Fatal Injuries", xlab="Total Fatal Injuries", col="red")
mean(data$injuries_fatal)
var(data$injuries_fatal)
range(data$injuries_fatal)

################################### EDA 2
data <- read.csv("traffic_accidents.csv")
injuries_by_hour <- aggregate(injuries_total ~ crash_hour, data=data, sum)
ggplot(injuries_by_hour, aes(x=crash_hour, y=injuries_total)) +
  geom_line(color="black", size=1) +
  geom_point(color="red", size=2) + 
  labs(title="Total Injuries by Hour",
       x="Crash Hour",
       y="Total Injuries") +
  theme_bw()

injuries_by_day <- aggregate(injuries_total ~ crash_day_of_week, data=data, sum)
ggplot(injuries_by_day, aes(x=crash_day_of_week, y=injuries_total)) +
  geom_line(color="black", size=1) +
  geom_point(color="red", size=2) + 
  labs(title="Total Injuries by Day",
       x="Crash Day",
       y="Total Injuries") +
  theme_bw()

fatal_by_hour <- aggregate(injuries_fatal ~ crash_hour, data=data, sum)
ggplot(fatal_by_hour, aes(x=crash_hour, y=injuries_fatal)) +
  geom_line(color="black", size=1) +
  geom_point(color="red", size=2) + 
  labs(title="Fatal Injuries by Hour",
       x="Crash Hour",
       y="Fatal Injuries") +
  theme_bw()

fatal_by_day <- aggregate(injuries_fatal ~ crash_day_of_week, data=data, sum)
ggplot(fatal_by_day, aes(x=crash_day_of_week, y=injuries_fatal)) +
  geom_line(color="black", size=1) +
  geom_point(color="red", size=2) + 
  labs(title="Fatal Injuries by Day",
       x="Crash Day",
       y="Fatal Injuries") +
  theme_bw()

# VIF
model <- glm(injuries_total ~ num_units + crash_hour + damage, data=data, family=poisson)
# Our only numerical variables
vif_val <- vif(model)
print("Variance Inflation Factors (VIF):")
print(vif_val)

#### First Problem

## variables to select

vars_to_keep <- c("most_severe_injury", "injuries_total", "injuries_fatal",
                  "weather_condition", "lighting_condition", "roadway_surface_cond",
                  "alignment", "road_defect", "traffic_control_device",
                  "trafficway_type", "intersection_related_i", "crash_hour",
                  "crash_day_of_week", "first_crash_type", "crash_type",
                  "num_units", "prim_contributory_cause")

df_subset <- df %>% dplyr::select(all_of(vars_to_keep))
categorical_vars <- c("most_severe_injury", "weather_condition", "lighting_condition",
                      "roadway_surface_cond", "alignment", "road_defect",
                      "traffic_control_device", "trafficway_type", "intersection_related_i",
                      "first_crash_type", "crash_type", "prim_contributory_cause",
                      "crash_day_of_week")

df_subset <- df_subset %>%
  mutate(across(all_of(categorical_vars), as.factor))

df_subset$most_severe_injury <- factor(df_subset$most_severe_injury,
                                       levels = c("NO INDICATION OF INJURY", 
                                                  "REPORTED, NOT EVIDENT", 
                                                  "NONINCAPACITATING INJURY", 
                                                  "INCAPACITATING INJURY", 
                                                  "FATAL"),
                                       ordered = TRUE)

categorical_vars <- c("weather_condition", "lighting_condition", "roadway_surface_cond",
                      "alignment", "road_defect", "traffic_control_device", 
                      "trafficway_type", "intersection_related_i", "first_crash_type",
                      "crash_type", "prim_contributory_cause", "crash_day_of_week")

df_subset <- df_subset %>%
  mutate(across(all_of(categorical_vars), as.factor))

# Crash severity: ordinal logistic
f1 <- bf(most_severe_injury ~ weather_condition + lighting_condition + roadway_surface_cond +
           alignment + road_defect + traffic_control_device + trafficway_type + 
           intersection_related_i + crash_hour + crash_day_of_week + 
           first_crash_type + crash_type + num_units + prim_contributory_cause,
         family = cumulative())

# Total injuries: zero-inflated negative binomial
f2 <- bf(injuries_total ~ weather_condition + lighting_condition + roadway_surface_cond +
           alignment + road_defect + traffic_control_device + trafficway_type + 
           intersection_related_i + crash_hour + crash_day_of_week + 
           first_crash_type + crash_type + num_units + prim_contributory_cause,
         family = zero_inflated_negbinomial())

# Fatal injuries: also ZINB
f3 <- bf(injuries_fatal ~ weather_condition + lighting_condition + roadway_surface_cond +
           alignment + road_defect + traffic_control_device + trafficway_type + 
           intersection_related_i + crash_hour + crash_day_of_week + 
           first_crash_type + crash_type + num_units + prim_contributory_cause,
         family = zero_inflated_negbinomial())

multivar_model <- brm(
  f1 + f2 + f3,
  data = df_subset,
  chains = 4,
  cores = 4,
  iter = 20,
  seed = 123
)
################################################################################ NEW CODE
df$weather_condition <- as.factor(df$weather_condition)
df$lighting_condition <- as.factor(df$lighting_condition)
df$trafficway_type <- as.factor(df$trafficway_type)
df$intersection_related_i <- as.factor(df$intersection_related_i)
df$roadway_surface_cond <- as.factor(df$roadway_surface_cond)
df$first_crash_type <- as.factor(df$first_crash_type)

df$prim_contributory_cause <- as.factor(df$prim_contributory_cause)
df$cause_grouped <- dplyr::case_when(
  df$prim_contributory_cause %in% c("FOLLOWING TOO CLOSELY", "IMPROPER TURNING/NO SIGNAL", "IMPROPER BACKING", "IMPROPER OVERTAKING/PASSING", "DRIVING ON WRONG SIDE/WRONG WAY", "IMPROPER LANE USAGE", "TURNING RIGHT ON RED") ~ "Driver Behavior",
  df$prim_contributory_cause %in% c("FAILING TO YIELD RIGHT-OF-WAY", "DISREGARDING TRAFFIC SIGNALS", "DISREGARDING STOP SIGN", "DISREGARDING YIELD SIGN", "DISREGARDING ROAD MARKINGS", "DISREGARDING OTHER TRAFFIC SIGNS") ~ "Signal/Right-of-Way",
  df$prim_contributory_cause %in% c("EXCEEDING SAFE SPEED FOR CONDITIONS", "EXCEEDING AUTHORIZED SPEED LIMIT", "FAILING TO REDUCE SPEED TO AVOID CRASH") ~ "Speeding",
  df$prim_contributory_cause %in% c("DISTRACTION - FROM INSIDE VEHICLE", "DISTRACTION - FROM OUTSIDE VEHICLE", "TEXTING", "CELL PHONE USE OTHER THAN TEXTING", "DISTRACTION - OTHER ELECTRONIC DEVICE (NAVIGATION DEVICE, DVD PLAYER, ETC.)") ~ "Distraction",
  df$prim_contributory_cause %in% c("UNDER THE INFLUENCE OF ALCOHOL/DRUGS (USE WHEN ARREST IS EFFECTED)", "HAD BEEN DRINKING (USE WHEN ARREST IS NOT MADE)", "PHYSICAL CONDITION OF DRIVER") ~ "Impairment",
  df$prim_contributory_cause %in% c("WEATHER", "VISION OBSCURED (SIGNS, TREE LIMBS, BUILDINGS, ETC.)", "EVASIVE ACTION DUE TO ANIMAL, OBJECT, NONMOTORIST", "ANIMAL", "ROAD ENGINEERING/SURFACE/MARKING DEFECTS", "ROAD CONSTRUCTION/MAINTENANCE") ~ "Environmental",
  df$prim_contributory_cause == "EQUIPMENT - VEHICLE CONDITION" ~ "Vehicle/Equipment",
  TRUE ~ "Other/Unknown"
)

df$cause_grouped <- as.factor(df$cause_grouped)

df$most_severe_injury <- as.factor(df$most_severe_injury)

new.df <- df[,c("most_severe_injury","injuries_total","num_units","weather_condition","lighting_condition",
                "trafficway_type","intersection_related_i","roadway_surface_cond",
                "first_crash_type","cause_grouped","crash_hour")]

library(brms)
library(rstudioapi)

small_data <- new.df[sample(nrow(new.df),2500),]

multi_model <- brm(
  formula = mvbind(injuries_total, num_units) ~ 
    weather_condition + lighting_condition + trafficway_type + 
    crash_hour + cause_grouped,
  data = small_data,
  family = list(  # for most_severe_injury (multinomial logistic)
    zero_inflated_negbinomial(),        # for injuries_total (count data)
    negbinomial()         # for num_units (count data)
  ),
  chains = 4,
  cores = 4,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.95),
)

howpredicted <- predict(multi_model,resp="numunits")
table(predicted[,"Estimate"],new.df$num_units[1:1000])

########################################################################### NEW CODE
library(dplyr)
library(forcats)
library(FactoMineR)
library(factoextra)

clean.df <- df %>% 
  filter(across(c(weather_condition, lighting_condition, traffic_control_device,
                  trafficway_type, roadway_surface_cond, prim_contributory_cause)
                , ~!.x %in% c("OTHER","UNKNOWN","UNABLE TO DETERMINE")))

clean.df <- clean.df %>% 
  filter(prim_contributory_cause != c("UNABLE TO DETERMINE", "NOT APPLICABLE"))

clean.df$weather_condition <- fct_collapse(clean.df$weather_condition,
                                           CLEAR = "CLEAR",
                                           RAINY = c("RAIN", "FREEZING RAIN/DRIZZLE", "SLEET/HAIL"),
                                           SNOWY = c("SNOW", "BLOWING SNOW"),
                                           FOGGY = c("FOG/SMOKE/HAZE"),
                                           EXTREME = c("SEVERE CROSS WIND GATE", "BLOWING SAND, SOIL, DIRT"),
                                           CLOUDY = "CLOUDY/OVERCAST",
                                           OTHER = c("OTHER", "UNKNOWN")
)

clean.df$lighting_condition <- fct_collapse(clean.df$lighting_condition,
                                            DAYLIGHT = "DAYLIGHT",
                                            DARK = c("DARKNESS", "DARKNESS, LIGHTED ROAD"),
                                            TWILIGHT = c("DAWN", "DUSK"),
                                            OTHER = "UNKNOWN"
)

clean.df$traffic_control_device <- fct_collapse(clean.df$traffic_control_device,
                                                SIGNAL = "TRAFFIC SIGNAL",
                                                STOP = "STOP SIGN/FLASHER",
                                                NONE = "NO CONTROLS",
                                                WARNING = c("FLASHING CONTROL SIGNAL", "DELINEATORS", 
                                                            "OTHER WARNING SIGN", "RR CROSSING SIGN", 
                                                            "RAILROAD CROSSING GATE", "SCHOOL ZONE", 
                                                            "PEDESTRIAN CROSSING SIGN", "YIELD", "LANE USE MARKING", 
                                                            "NO PASSING", "OTHER REG. SIGN", "POLICE/FLAGMAN",
                                                            "BICYCLE CROSSING SIGN")
)

clean.df$trafficway_type <- fct_collapse(clean.df$trafficway_type,
                                         DIVIDED = c("DIVIDED - W/MEDIAN BARRIER", "CENTER TURN LANE DIVIDED - W/MEDIAN (NOT RAISED)"),
                                         NOT_DIVIDED = c("NOT DIVIDED", "ONE-WAY"),
                                         INTERSECTION = c("FOUR WAY", "T-INTERSECTION", "Y-INTERSECTION", 
                                                          "FIVE POINT, OR MORE", "L-INTERSECTION", "UNKNOWN INTERSECTION TYPE"),
                                         LOCAL = c("DRIVEWAY", "PARKING LOT", "ALLEY", "TRAFFIC ROUTE"),
                                         OTHER = c("RAMP", "ROUNDABOUT", "NOT REPORTED", "UNKNOWN", "OTHER")
)

clean.df$roadway_surface_cond <- fct_collapse(clean.df$roadway_surface_cond,
                                              DRY = "DRY",
                                              WET = "WET",
                                              WINTRY = c("SNOW OR SLUSH", "ICE"),
                                              OBSTRUCTED = "SAND, MUD, DIRT",
                                              OTHER = c("UNKNOWN")
)

clean.df$prim_contributory_cause <- fct_collapse(clean.df$prim_contributory_cause,
                                                 SPEEDING = c("EXCEEDING AUTHORIZED SPEED LIMIT", "EXCEEDING SAFE SPEED FOR CONDITIONS", 
                                                              "FAILURE TO REDUCE SPEED TO AVOID CRASH"),
                                                 DISTRACTED = c("DISTRACTION - FROM INSIDE VEHICLE", "DISTRACTION - FROM OUTSIDE VEHICLE", 
                                                                "DISTRACTION - OTHER ELECTRONIC DEVICE (NAVIGATION DEVICE, DVD PLAYER, ETC.)", 
                                                                "TEXTING", "CELL PHONE USE OTHER THAN TEXTING"),
                                                 DISREGARD = c("DISREGARDING TRAFFIC SIGNALS", "DISREGARDING STOP SIGN", 
                                                               "DISREGARDING OTHER TRAFFIC SIGNS", "DISREGARDING ROAD MARKINGS",
                                                               "DISREGARDING YIELD SIGN"),
                                                 IMPROPER_MANEUVER = c("IMPROPER TURNING/NO SIGNAL", "IMPROPER BACKING", "IMPROPER LANE USAGE", 
                                                                       "IMPROPER OVERTAKING/PASSING", "TURNING RIGHT ON RED", "PASSING STOPPED SCHOOL BUS"),
                                                 RIGHT_OF_WAY = c("FAILING TO YIELD RIGHT-OF-WAY"),
                                                 FOLLOWING = "FOLLOWING TOO CLOSELY",
                                                 DRIVER_ISSUE = c("DRIVING SKILLS/KNOWLEDGE/EXPERIENCE", "OPERATING VEHICLE IN ERRATIC,
                                                            RECKLESS, CARELESS, NEGLIGENT OR AGGRESSIVE MANNER", 
                                                                  "DRIVING ON WRONG SIDE/WRONG WAY"),
                                                 ALCOHOL_DRUG = c("UNDER THE INFLUENCE OF ALCOHOL/DRUGS (USE WHEN ARREST IS EFFECTED)", 
                                                                  "HAD BEEN DRINKING (USE WHEN ARREST IS NOT MADE)", "PHYSICAL CONDITION OF DRIVER"),
                                                 ENVIRONMENTAL = c("WEATHER", "VISION OBSCURED (SIGNS, TREE LIMBS, BUILDINGS, ETC.)",
                                                                   "ROAD ENGINEERING/SURFACE/MARKING DEFECTS", "ROAD CONSTRUCTION/MAINTENANCE",
                                                                   "EQUIPMENT - VEHICLE CONDITION", "EVASIVE ACTION DUE TO ANIMAL, OBJECT, NONMOTORIST", 
                                                                   "ANIMAL"),
                                                 OTHER = c("NOT APPLICABLE", "OBSTRUCTED CROSSWALKS", "RELATED TO BUS STOP", 
                                                           "BICYCLE ADVANCING LEGALLY ON RED LIGHT", 
                                                           "MOTORCYCLE ADVANCING LEGALLY ON RED LIGHT")
)

clean.df$season <- factor(case_when(
  clean.df$crash_month %in% c(12, 1, 2)  ~ "Winter",
  clean.df$crash_month %in% c(3, 4, 5)   ~ "Spring",
  clean.df$crash_month %in% c(6, 7, 8)   ~ "Summer",
  clean.df$crash_month %in% c(9, 10, 11) ~ "Fall"
))

day_labels <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

clean.df$crash_day_of_week <- factor(clean.df$crash_day_of_week, 
                                     levels = 1:7, 
                                     labels = day_labels, 
                                     ordered = FALSE)

clean.df <- clean.df %>% 
  mutate(across(where(is.factor), droplevels))

clean.df <- na.omit(clean.df)
clean.df$hour_group <- cut(clean.df$crash_hour,
                           breaks = c(-1, 5, 11, 17, 20, 23),
                           labels = c("Night", "Morning", "Afternoon", "Evening", "Night"),
                           include.lowest = TRUE,  # ensures hour 0 is included
                           right = TRUE)           # right-inclusive (e.g., 5 belongs to Night)

labels = c("Night", "Morning", "Afternoon", "Evening")

ready.df <- clean.df %>% 
  dplyr::select(weather_condition,
                trafficway_type, prim_contributory_cause,
                roadway_surface_cond,crash_day_of_week, trafficway_type, hour_group, num_units, injuries_total
  )

ready.df <- ready.df %>% 
  dplyr::filter(if_all(where(~ is.factor(.) || is.character(.)), ~ . != "OTHER"))

fviz_contrib(fmd_result, "var", axes = 4)
keep.sake <- ready.df

ready.df <- ready.df %>% 
  dplyr::select(weather_condition,
                trafficway_type, prim_contributory_cause, 
                roadway_surface_cond,crash_day_of_week, trafficway_type, hour_group)


fmd_result <- FAMD(ready.df,ncp=10,graph = FALSE)
get_eigenvalue(fmd_result)
fviz_screeplot(fmd_result)

#var <- get_famd_var(fmd_result) head(var$cos2) head(var$coord) head(var$contrib)

famd_coords <- fmd_result$ind$coord[, 1:4]  # use top 5–6 dimensions

library(cluster)

set.seed(123)
clara_clusters <- clara(famd_coords, k = 4)

ready.df$cluster <- as.factor(clara_clusters$clustering)
ready.df$injuries_total <- keep.sake$injuries_total
ready.df$num_units <- keep.sake$num_units


ready.df %>%
  group_by(cluster) %>%
  summarise(
    mean_injuries = mean(injuries_total, na.rm = TRUE),
    mean_units = mean(num_units, na.rm = TRUE),
    common_weather = names(sort(table(weather_condition), decreasing = TRUE))[1]
  )


fviz_cluster(clara_clusters, data = famd_coords,
             ellipse.type = "convex", geom = "point",
             palette = "jco", ggtheme = theme_minimal())

fviz_silhouette(clara_clusters)
table(clara_clusters$clustering)
barplot(table(clara_clusters$clustering),
        main = "Number of Observations per Cluster",
        col = "steelblue", ylab = "Count")

clara_clusters$medoids
clean.df[clara_clusters$medoids, ]

library(dplyr)

ready.df %>%
  group_by(cluster) %>%
  summarise(
    mean_injuries = mean(injuries_total, na.rm = TRUE),
    mean_units = mean(num_units, na.rm = TRUE),
    most_common_cause = names(sort(table(prim_contributory_cause), decreasing = TRUE))[1],
    common_weather = names(sort(table(weather_condition), decreasing = TRUE))[1]
  )




################################################################################
library(tidyr)
library(dplyr)
# Step 1: Filter the main df and store surviving row indices
filter_mask <- df %>%
  filter(across(c(weather_condition, lighting_condition, traffic_control_device,
                  trafficway_type, roadway_surface_cond, prim_contributory_cause),
                ~!.x %in% c("OTHER", "UNKNOWN", "UNABLE TO DETERMINE"))) %>%
  filter(!(prim_contributory_cause %in% c("UNABLE TO DETERMINE", "NOT APPLICABLE"))) %>%
  drop_na() %>%
  rownames() %>%
  as.integer()

# Step 2: Use those indices to subset df
clean.df <- df[filter_mask, ]

# (apply your fct_collapse and other transformations to clean.df here — unchanged)

# Step 3: Create ready.df from clean.df and retain injuries_total & num_units
ready.df <- clean.df %>%
  select(weather_condition, trafficway_type, prim_contributory_cause,
         roadway_surface_cond, crash_day_of_week, hour_group,
         injuries_total, num_units)

# Step 4: Filter out levels still marked as "OTHER"

# Step 5: Create version for FAMD (exclude outcomes)
ready.df_famd <- ready.df %>%
  select(-injuries_total, -num_units)

# Step 6: Run FAMD
fmd_result <- FAMD(ready.df_famd, ncp = 10, graph = FALSE)

# Step 7: Run CLARA on top 5 dimensions
famd_coords <- fmd_result$ind$coord[, 1:4]

library(cluster)
set.seed(123)
clara_clusters <- clara(famd_coords, k = 4, samples = 5, sampsize = 5000)

# Step 8: Add cluster and outcomes back
ready.df$cluster <- as.factor(clara_clusters$clustering)

ready.df %>%
  count(cluster, weather_condition) %>%
  group_by(cluster) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = cluster, y = prop, fill = weather_condition)) +
  geom_bar(stat = "identity", position = "fill")
ready.df %>%
  count(cluster, roadway_surface_cond) %>%
  group_by(cluster) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = cluster, y = prop, fill = roadway_surface_cond)) +
  geom_bar(stat = "identity", position = "fill")


###############
library(cluster)
set.seed(123)
sample_idx <- sample(1:nrow(ready.df), 5000)
ready_sample <- ready.df[sample_idx, ]
gower_sample <- daisy(ready_sample, metric = "gower")

# Use the full set of selected variables for clustering
hclust_result <- hclust(as.dist(gower_sample), method = "ward.D2")
plot(hclust_result, labels = FALSE, main = "Hierarchical Clustering Dendrogram")
ready_sample$cluster_hier <- cutree(hclust_result, k = 4)  # choose k
table(ready.df$cluster_hier)
ready_sample %>%
  group_by(cluster_hier) %>%
  summarise(
    mean_injuries = mean(keep.sake$injuries_total, na.rm = TRUE),
    mean_units = mean(keep.sake$num_units, na.rm = TRUE),
    common_weather = names(sort(table(weather_condition), decreasing = TRUE))[1],
    common_cause = names(sort(table(prim_contributory_cause), decreasing = TRUE))[1]
  )

famd_coords_sample <- famd_coords[sample_idx, ]  # match same sample

fviz_cluster(list(data = famd_coords_sample, cluster = ready_sample$cluster_hier),
             ellipse.type = "convex", geom = "point",
             palette = "jco", ggtheme = theme_minimal(),
             main = "Hierarchical Clusters on FAMD Dimensions")
ready_sample %>%
  group_by(cluster_hier) %>%
  summarise(
    count = n(),
    common_weather = names(sort(table(weather_condition), decreasing = TRUE))[1],
    common_cause = names(sort(table(prim_contributory_cause), decreasing = TRUE))[1]
  )

#####
# Distance matrix on FAMD coordinates
dist_mat <- dist(famd_coords)

# Hierarchical clustering (Ward's method recommended)
hc <- hclust(dist_mat, method = "ward.D2")

# Plot the dendrogram
plot(hc, labels = FALSE, hang = -1, main = "Hierarchical Clustering Dendrogram")

# Add rectangle around chosen number of clusters (e.g., k = 4)
rect.hclust(hc, k = 4, border = 2:5)

# Weather condition frequency
ggplot(df, aes(x = injuries_total)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Total Injuries") +
  theme_minimal()


table(CLARA = clara_clusters$clustering, Hierarchical = ready.df$cluster)
fviz_cluster(clara_clusters, data = famd_coords,
             geom = "point", ellipse.type = "convex",
             palette = "jco", ggtheme = theme_minimal())
fviz_silhouette(clara_clusters)
barplot(table(clara_clusters$clustering),
        main = "Number of Observations per Cluster",
        col = "steelblue")
ready.df %>%
  group_by(cluster) %>%
  summarise(
    mean_injuries = mean(injuries_total),
    common_cause = names(sort(table(prim_contributory_cause), decreasing = TRUE))[1],
    common_weather = names(sort(table(weather_condition), decreasing = TRUE))[1]
  )
ggplot(ready.df, aes(x = prim_contributory_cause, fill = cluster)) +
  geom_bar(position = "fill") +
  theme_minimal()

library(ggplot2)


ggplot(clean.df, aes(x = weather_condition)) +
  geom_bar() +
  labs(
    title = "Weather Conditions Ordered by Frequency",
    x = "Weather Condition",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(clean.df, aes(x = weather_condition)) +
  geom_bar(aes(y = (..count..) / sum(..count..)), fill = "steelblue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Weather Conditions in Crashes",
    x = "Weather Condition",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

ggplot(ready.df, aes(x = prim_contributory_cause)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Frequency of Primary Contributory Causes",
       x = "Cause", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######################
fviz_cluster(clara_clusters, data = famd_coords,
             ellipse.type = "convex", geom = "point",
             palette = "jco", ggtheme = theme_minimal(),
             main = "Crash Clusters from FAMD + CLARA")
fviz_silhouette(clara_clusters)
barplot(table(clara_clusters$clustering),
        main = "Cluster Sizes (CLARA)",
        ylab = "Number of Cases",
)
ggplot(ready.df, aes(x = weather_condition, fill = cluster)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Weather Conditions by Cluster",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(ready.df, aes(x = prim_contributory_cause, fill = cluster)) +
  geom_bar(position = "fill") +
  labs(title = "Primary Contributory Causes by Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(ready.df, aes(x = cluster, y = injuries_total, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Injury Counts by Cluster") +
  theme_minimal()

ready.df %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    mean_injuries = mean(injuries_total, na.rm = TRUE),
    mean_units = mean(num_units, na.rm = TRUE),
    common_weather = names(sort(table(weather_condition), decreasing = TRUE))[1],
    common_cause = names(sort(table(prim_contributory_cause), decreasing = TRUE))[1]
  )
################################################################################
set.seed(123)
kmeans_model <- kmeans(famd_coords, centers = 4, nstart = 25)

ready.df$kmeans_cluster <- as.factor(kmeans_model$cluster)

# Visualize
fviz_cluster(kmeans_model, data = famd_coords,
             geom = "point", ellipse.type = "convex",
             palette = "jco", ggtheme = theme_minimal())

library(cluster)

# K-means silhouette
sil_kmeans <- silhouette(kmeans_model$cluster, dist(famd_coords))


# K-means
ready.df %>%
  group_by(kmeans_cluster) %>%
  summarise(
    mean_injuries = mean(injuries_total),
    mean_units = mean(num_units),
    common_weather = names(sort(table(weather_condition), decreasing = TRUE))[1],
    common_cause = names(sort(table(prim_contributory_cause), decreasing = TRUE))[1]
  )

# CLARA (if not already done)
ready.df %>%
  group_by(cluster) %>%
  summarise(
    mean_injuries = mean(injuries_total),
    mean_units = mean(num_units),
    common_weather = names(sort(table(weather_condition), decreasing = TRUE))[1],
    common_cause = names(sort(table(prim_contributory_cause), decreasing = TRUE))[1]
  )

silhouette_scores <- numeric()

set.seed(3)
sample_idx <- sample(1:nrow(famd_coords), 10000)  # or smaller if needed
sampled_coords <- famd_coords[sample_idx, ]
sampled_clusters <- clara(sampled_coords, k = 5)$clustering

sil <- silhouette(sampled_clusters, dist(sampled_coords))
fviz_silhouette(sil)





barplot(table(ready.df$cluster),
        main = "Number of Observations per Cluster",
        , ylab = "Count")
library(cluster)
silhouette_obj <- silhouette(clara_clusters$clustering, dist(famd_coords))
fviz_silhouette(silhouette_obj)

ready.df %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    mean_injuries = mean(injuries_total, na.rm = TRUE),
    sd_injuries = sd(injuries_total, na.rm = TRUE),
    mean_units = mean(num_units, na.rm = TRUE),
    top_weather = names(sort(table(weather_condition), decreasing = TRUE))[1],
    top_cause = names(sort(table(prim_contributory_cause), decreasing = TRUE))[1],
    top_road = names(sort(table(roadway_surface_cond), decreasing = TRUE))[1]
  )

set.seed(123)
sample_idx <- sample(1:nrow(famd_coords), size = 5000)
sample_coords <- famd_coords[sample_idx, ]
sample_cluster <- clara_clusters$clustering[sample_idx]

library(cluster)
sil_obj <- silhouette(sample_cluster, dist(sample_coords))

library(factoextra)
fviz_silhouette(sil_obj)

sil_widths <- numeric()

for (k in 2:6) {
  clara_k <- clara(sample_coords, k = k)
  sil_k <- silhouette(clara_k$clustering, dist(sample_coords))
  sil_widths[k] <- mean(sil_k[, 3])
}

# Plot silhouette widths for sampled data
plot(2:6, sil_widths[2:6], type = "b", pch = 19,
     xlab = "Number of clusters (k)", ylab = "Avg silhouette width",
     main = "Silhouette width (sampled data)")

set.seed(123)
clara_k3 <- clara(famd_coords, k = 4)
ready.df$cluster <- as.factor(clara_k3$clustering)
library(factoextra)

fviz_cluster(clara_k3, data = famd_coords,
             ellipse.type = "convex",
             palette = "jco",
             geom = "point",
             ggtheme = theme_minimal())
library(dplyr)

ready.df %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    mean_injuries = mean(injuries_total),
    sd_injuries = sd(injuries_total),
    mean_units = mean(num_units),
    top_weather = names(sort(table(weather_condition), decreasing = TRUE))[1],
    top_cause = names(sort(table(prim_contributory_cause), decreasing = TRUE))[1],
    top_road = names(sort(table(roadway_surface_cond), decreasing = TRUE))[1]
  )
library(cluster)
library(factoextra)

)
barplot(table(clara_k3$clustering),
        main = "Number of Observations per Cluster",
        col = "steelblue", ylab = "Count")
library(ggplot2)

ggplot(ready.df, aes(x = cluster, fill = weather_condition)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Weather Condition Proportion by Cluster") +
  theme_minimal()
ggplot(ready.df, aes(x = cluster, fill = roadway_surface_cond)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Road Surface Condition by Cluster") +
  theme_minimal()
######################

# Required packages
library(cluster)
library(ggplot2)

# Sample 10,000 points from your FAMD-reduced dataset
set.seed(14)
sample_rows <- sample(nrow(famd_coords), size = 5000)
sample_data <- famd_coords[sample_rows, ]  # famd_coords should be your FAMD coordinate matrix

# Choose a range of cluster values
k_range <- 2:6
sil_width <- numeric(length(k_range))

# Loop through k and compute average silhouette using pam (faster for small samples)
for (i in seq_along(k_range)) {
  pam_fit <- pam(sample_data, k = k_range[i])
  sil <- silhouette(pam_fit)
  sil_width[i] <- mean(sil[, 3])  # average silhouette width
}

# Store results
sil_df <- data.frame(k = k_range, silhouette = sil_width)

# Plot silhouette width vs k
ggplot(sil_df, aes(x = k, y = silhouette)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Silhouette Width by Number of Clusters",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Width") +
  theme_minimal()

# Print best k
optimal_k <- sil_df$k[which.max(sil_df$silhouette)]
cat("Optimal number of clusters (based on sample):", optimal_k, "\n")
################

# Assuming sil_df is your data frame with k and silhouette
optimal_k <- 4

sil_df$silhouette <- c(0.3279951, 0.325579, 0.3323863, 0.3275394,0.3257326)

ggplot(sil_df, aes(x = k, y = silhouette)) +
  geom_line() +
  geom_point() +  # highlight optimal k
  labs(title = "Average Silhouette Width by Number of Clusters",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Width") +
  theme_minimal()





