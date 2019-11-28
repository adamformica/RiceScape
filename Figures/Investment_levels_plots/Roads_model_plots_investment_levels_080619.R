library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggpubr)

# 1 - process data

setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Investment_levels_plots/")

# fixed layouts

data <- read.csv("Ricescape_senegal_workshop_121718 investment_levels-spreadsheet.csv",header=FALSE,skip=6,nrows=13)

data <- t(data)

colnames(data) <- data[1,]

data <- data[-1,]

rownames(data) <- seq(1:nrow(data))

data <- as.data.frame(data,stringsAsFactors=FALSE)

data <- data %>% 
  mutate(`village-weight` = na_if(`village-weight`, ""),
         `storage-weight` = na_if(`storage-weight`, ""),
         `roads-investment` = na_if(`roads-investment`, ""),
         environment = na_if(environment, "")) %>%
  fill(`village-weight`,
       `storage-weight`,
       `roads-investment`,
       environment)

data <- data %>%
  mutate(scenarios = ifelse((data$`village-weight`==1 & data$`storage-weight`==0),"villages_prioritized",
                            ifelse((data$`village-weight`==0 & data$`storage-weight`==1),"storage_prioritized",
                                   ifelse((data$`village-weight`==1 & data$`storage-weight`==1),"equal_priority","other"))))

drops <- c("scenarios","[reporter]","environment")

data[,!(names(data) %in% drops)] <- sapply(data[,!(names(data) %in% drops)],as.numeric)

data <- data %>%
  filter(scenarios!="other")

col_names <- colnames(data)

col_names <- gsub("\\[|\\]|\"", "", col_names)

col_names <- gsub("-| ", "_", col_names)

colnames(data) <- col_names

# variable layouts

# worst case trade off for grid, radial, and random layouts

seed_data <- read.csv("Ricescape_senegal_workshop_121718 spatial_layouts_scaled.csv")

seed_data_filter <- filter(seed_data,(environment=="grid" & seed_value==26)|
                             (environment=="radial" & seed_value==95)|
                             (environment=="random" & seed_value==8))

# combine fixed and variable layouts data

data_combined <- rbind(data,seed_data_filter)

# continue preprocessing

data_combined <- rename(data_combined, Scenarios = scenarios)

data_combined$Scenarios <- recode_factor(data_combined$Scenarios,"villages_prioritized" = "Villages prioritized",
                                "equal_priority" = "Equal priority",
                                "storage_prioritized" = "Storage prioritized")

data_combined$environment <- recode_factor(data_combined$environment,"grid_distributed" = "grid distributed",
                                "radial_distributed" = "radial distributed",
                                "random_distributed" = "random distributed")

data_combined$environment <- paste0(toupper(substring(data_combined$environment, 1, 1)),substring(data_combined$environment,2))

data_combined$environment <- as.factor(data_combined$environment)


# set environments to plot

# environments <- c("Bandafassi", "Ndorna", "Makacoulibantang")

environments <- c("Grid", "Radial", "Random")

# environments <- c("Grid distributed", "Radial distributed", "Random distributed")


data_env_sub <- data_combined[data_combined$environment %in% environments,]

# 2 - plot data

# crop expansion plots

if ("Grid" %in% environments) {
  
  y_labs <- c("Scaled villages connected","Scaled storage connected","Scaled crop expansion")
  
} else {
  
  y_labs <- c("Villages connected","Storage connected (T)","Crop expansion (ha)")
  
}

villages_connected <- data_env_sub[data_env_sub$reporter=="villages-along-paved" ,]

villages_plot <- ggplot(villages_connected,aes(x=roads_investment,y=final,color=Scenarios)) +
  geom_smooth() +
  geom_point(position="jitter",alpha=0.1) +
  # ylim(0,1) +
  # facet_wrap(vars(environment)) +
  facet_wrap(vars(environment), scales="free") +
  ylab(y_labs[1]) +
  xlab("") +
  theme_bw() +
  theme(legend.position="bottom")

storage_connected <- data_env_sub[data_env_sub$reporter=="storage-connected" ,]

storage_plot <- ggplot(storage_connected,aes(x=roads_investment,y=final,color=Scenarios)) +
  geom_smooth() +
  geom_point(position="jitter",alpha=0.1) +
  # ylim(0,1) +
  # facet_wrap(vars(environment)) +
  facet_wrap(vars(environment), scales="free") +
  ylab(y_labs[2]) +
  xlab("") +
  theme_bw()

crop_expansion <- data_env_sub[data_env_sub$reporter=="( count patches with [ farm > 0 ] - initial-farm-count ) * hectares-per-cell",]

expansion_plot <- ggplot(crop_expansion,aes(x=roads_investment,y=final,color=Scenarios)) +
  geom_smooth() +
  geom_point(position="jitter",alpha=0.1) +
  # ylim(0,1) +
  # facet_wrap(vars(environment)) +
  facet_wrap(vars(environment), scales="free") +
  ylab(y_labs[3]) +
  xlab("Investment level (M CFA per year to 2035)") +
  theme_bw()

dev.new(width=8,height=8,noRStudioGD = TRUE)

ggarrange(villages_plot,storage_plot,expansion_plot, ncol=1, nrow=3, labels="AUTO", common.legend = TRUE, legend="bottom")
