library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggpubr)
library(data.table)
library(scales)
library(zoo)

# 1 - process data

setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Investment_levels_plots/")

# crop expansion data

data <- fread("Ricescape_senegal_workshop_121718 spatial_layouts-spreadsheet.csv",skip=6,nrows=13)

data <- t(data)

colnames(data) <- data[1,]

data <- data[-1,]

rownames(data) <- seq(1:nrow(data))

data <- as.data.frame(data,stringsAsFactors=FALSE)

data <- data %>% 
  mutate(`village-weight` = na_if(`village-weight`, ""),
         `storage-weight` = na_if(`storage-weight`, ""),
         `roads-investment` = na_if(`roads-investment`, ""),
         environment = na_if(environment, ""),
         `seed-value` = na_if(`seed-value`, "")) %>%
  fill(`village-weight`,
       `storage-weight`,
       `roads-investment`,
       environment,
       `seed-value`)

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

write.csv(data,"Ricescape_senegal_workshop_121718 spatial_layouts.csv",row.names = FALSE)

# 2 - scale data

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

environments <- c("grid", "radial", "random")

reporters <- c("villages-along-paved",
               "storage-connected",
               "( count patches with [ farm > 0 ] - initial-farm-count ) * hectares-per-cell")

for (k in 1:length(environments)) {
  
  for (j in 1:length(environments)) {
    
    environment_data <- data %>% filter(environment == environments[j])
    
    for (i in 1:max(data$seed_value)) {
      
      storage_scenario <- environment_data %>% filter(seed_value == i,
                                                      scenarios == "storage_prioritized",
                                                      reporter == reporters[k]) %>%
        
        mutate(final = range01(final)) %>%
        
        pull(final)
      
      storage_scenario[is.na(storage_scenario)] <- 0
      
      data[(data$environment == environments[j] & data$seed_value == i & data$scenarios == "storage_prioritized" & data$reporter == reporters[k]), ]$final <- storage_scenario
      
      villages_scenario <- environment_data %>% filter(seed_value == i,
                                                       scenarios == "villages_prioritized",
                                                       reporter == reporters[k]) %>%
        
        mutate(final = range01(final)) %>%
        
        pull(final)
      
      villages_scenario[is.na(villages_scenario)] <- 0
      
      data[(data$environment == environments[j] & data$seed_value == i & data$scenarios == "villages_prioritized" & data$reporter == reporters[k]), ]$final <- villages_scenario
      
      equal_scenario <- environment_data %>% filter(seed_value == i,
                                                    scenarios == "equal_priority",
                                                    reporter == reporters[k]) %>%
        
        mutate(final = range01(final)) %>%
        
        pull(final)
      
      equal_scenario[is.na(equal_scenario)] <- 0
      
      data[(data$environment == environments[j] & data$seed_value == i & data$scenarios == "equal_priority" & data$reporter == reporters[k]), ]$final <- equal_scenario
      
    }
    
  }
  
}

write.csv(data,"Ricescape_senegal_workshop_121718 spatial_layouts_scaled.csv",row.names = FALSE)

# 3 - vary seed for spatial layout and calculate AUC for different priorities across outcomes

storage_prioritized_AUC_dfs <- rep(list(data.frame(matrix(NA, nrow=max(data$seed_value), ncol=length(environments)))),length(environments))

villages_prioritized_AUC_dfs <- storage_prioritized_AUC_dfs 

for (i in 1:length(storage_prioritized_AUC_dfs)) {
  colnames(storage_prioritized_AUC_dfs[[i]]) <- environments
  colnames(villages_prioritized_AUC_dfs[[i]]) <- environments
}

for (k in 1:length(storage_prioritized_AUC_dfs)) {
  
  for (j in 1:length(environments)) {
    
    environment_data <- data %>% filter(environment == environments[j])
    
    for (i in 1:max(data$seed_value)) {
      
      storage_scenario <- environment_data %>% filter(seed_value == i,
                                                      scenarios == "storage_prioritized",
                                                      reporter == reporters[k])
      
      storage_loess <- loess(final~roads_investment,data=storage_scenario)
      
      storage_loess_preds <- predict(storage_loess,storage_scenario$roads_investment)
      
      
      villages_scenario <- environment_data %>% filter(seed_value == i,
                                                      scenarios == "villages_prioritized",
                                                      reporter == reporters[k])
      
      villages_loess <- loess(final~roads_investment,data=villages_scenario)
      
      villages_loess_preds <- predict(villages_loess,villages_scenario$roads_investment)
      
      
      
      storage_x <- storage_scenario$roads_investment
      
      storage_x <-  storage_x[seq(1,length(storage_x),5)]
      
      storage_y <- storage_loess_preds
      
      storage_y <- storage_y[seq(1,length(storage_y),5)]
      
      storage_id <- order(storage_x)
      
      storage_prioritized_AUC <- sum(diff(storage_x[storage_id])*rollmean(storage_y[storage_id],2))
      
      storage_prioritized_AUC_dfs[[k]][i,j] <- storage_prioritized_AUC
      
      
      villages_x <- villages_scenario$roads_investment
      
      villages_x <-  villages_x[seq(1,length(villages_x),5)]
      
      villages_y <- villages_loess_preds
      
      villages_y <- villages_y[seq(1,length(villages_y),5)]
      
      villages_id <- order(villages_x)
      
      villages_prioritized_AUC <- sum(diff(villages_x[villages_id])*rollmean(villages_y[villages_id],2))
      
      villages_prioritized_AUC_dfs[[k]][i,j] <- villages_prioritized_AUC
      
    }
    
  }
  
  storage_prioritized_AUC_dfs[[k]]$seed <- seq(1,max(data$seed_value))
  
  villages_prioritized_AUC_dfs[[k]]$seed <- seq(1,max(data$seed_value))
  
  storage_prioritized_AUC_dfs[[k]] <- storage_prioritized_AUC_dfs[[k]][,c(4,1,2,3)]
  
  villages_prioritized_AUC_dfs[[k]] <- villages_prioritized_AUC_dfs[[k]][,c(4,1,2,3)]
  
}

# 4 - show AUCs across outcomes for each environment

outcomes <- c("villages_connected","storage_connected","crop_expansion")

storage_outcomes_dfs <- rep(list(data.frame(matrix(NA, nrow=max(data$seed_value), ncol=length(environments)))),length(environments))

villages_outcomes_dfs <- storage_outcomes_dfs

for (i in 1:length(storage_outcomes_dfs)) {
  colnames(storage_outcomes_dfs[[i]]) <- paste0(outcomes,"_storage_prioritized_AUC")
  colnames(villages_outcomes_dfs[[i]]) <- paste0(outcomes,"_villages_prioritized_AUC")
}

for(i in 1:3) {
  
  storage_outcomes_dfs[[i]]$villages_connected_storage_prioritized_AUC <- pull(storage_prioritized_AUC_dfs[[1]],environments[i])
  
  storage_outcomes_dfs[[i]]$storage_connected_storage_prioritized_AUC <- pull(storage_prioritized_AUC_dfs[[2]],environments[i])
  
  storage_outcomes_dfs[[i]]$crop_expansion_storage_prioritized_AUC <- pull(storage_prioritized_AUC_dfs[[3]],environments[i])
  
  storage_outcomes_dfs[[i]]$seed <- storage_prioritized_AUC_dfs[[1]]$seed
  
  storage_outcomes_dfs[[i]] <- storage_outcomes_dfs[[i]][,c(4,1,2,3)]
  
}

for(i in 1:3) {
  
  villages_outcomes_dfs[[i]]$villages_connected_villages_prioritized_AUC <- pull(villages_prioritized_AUC_dfs[[1]],environments[i])
  
  villages_outcomes_dfs[[i]]$storage_connected_villages_prioritized_AUC <- pull(villages_prioritized_AUC_dfs[[2]],environments[i])
  
  villages_outcomes_dfs[[i]]$crop_expansion_villages_prioritized_AUC <- pull(villages_prioritized_AUC_dfs[[3]],environments[i])
  
  villages_outcomes_dfs[[i]]$seed <- villages_prioritized_AUC_dfs[[1]]$seed
  
  villages_outcomes_dfs[[i]] <- villages_outcomes_dfs[[i]][,c(4,1,2,3)]
  
}

# 5 - merge AUC data tables

grid_outcomes <- left_join(storage_outcomes_dfs[[1]],villages_outcomes_dfs[[1]],by="seed")

radial_outcomes <- left_join(storage_outcomes_dfs[[2]],villages_outcomes_dfs[[2]],by="seed")

random_outcomes <- left_join(storage_outcomes_dfs[[3]],villages_outcomes_dfs[[3]],by="seed")

# 6 - find optimal policy for each seed

# add optimal priority columns

network_outcomes_list <- list(grid_outcomes,radial_outcomes,random_outcomes)

for (i in 1:3) {
  network_outcomes_list[[i]] <- network_outcomes_list[[i]] %>%
    mutate(optimal_priority_villages = NA, optimal_priority_storage = NA, optimal_priority_crop_expansion = NA) 
}

# AUC when prioritizing villages or storage for each outcome

outcomes_villages_prioritized_AUC <- c("villages_connected_villages_prioritized_AUC","storage_connected_villages_prioritized_AUC","crop_expansion_villages_prioritized_AUC")

outcomes_storage_prioritized_AUC <- c("villages_connected_storage_prioritized_AUC","storage_connected_storage_prioritized_AUC","crop_expansion_storage_prioritized_AUC")

# overall optimal priority

outcomes_optimal <- paste0("optimal_priority_",c("villages","storage","crop_expansion"))

# network structure

for (j in 1:3) {
  
  # villages and storage AUC and optimal policies
  
  for (k in 1:3) {
    
    # seeds
    
    for (i in 1:nrow(network_outcomes_list[[j]])) {
      
      if (network_outcomes_list[[j]][i,outcomes_villages_prioritized_AUC[k]] == network_outcomes_list[[j]][i,outcomes_storage_prioritized_AUC[k]]) {
        
        network_outcomes_list[[j]][i,outcomes_optimal[k]] <- "neither"
        
      } else {
        
        if (outcomes_villages_prioritized_AUC[k] != "crop_expansion_villages_prioritized_AUC") {
          
          if (network_outcomes_list[[j]][i,outcomes_villages_prioritized_AUC[k]] < network_outcomes_list[[j]][i,outcomes_storage_prioritized_AUC[k]]) {
            
            network_outcomes_list[[j]][i,outcomes_optimal[k]] <- "storage"
            
          } else {
            
            network_outcomes_list[[j]][i,outcomes_optimal[k]] <- "villages"
            
          }
          
        } else {
          
          if (network_outcomes_list[[j]][i,outcomes_villages_prioritized_AUC[k]] < network_outcomes_list[[j]][i,outcomes_storage_prioritized_AUC[k]]) {
            
            network_outcomes_list[[j]][i,outcomes_optimal[k]] <- "villages"
            
          } else {
            
            network_outcomes_list[[j]][i,outcomes_optimal[k]] <- "storage"
            
          }
          
        }
        
      }
      
    }
    
  }
  
}

# calculate the absolute value of the difference in AUC values

for (i in 1:3) {
  
  network_outcomes_list[[i]]$villages_connected_priority_AUC_difference <- with(network_outcomes_list[[i]],abs(villages_connected_villages_prioritized_AUC - villages_connected_storage_prioritized_AUC))
  
  network_outcomes_list[[i]]$storage_connected_priority_AUC_difference <- with(network_outcomes_list[[i]],abs(storage_connected_villages_prioritized_AUC - storage_connected_storage_prioritized_AUC))
  
  network_outcomes_list[[i]]$crop_expansion_priority_AUC_difference <- with(network_outcomes_list[[i]],abs(crop_expansion_villages_prioritized_AUC - crop_expansion_storage_prioritized_AUC))
  
}

# find whether there is an overall optimal policy or trade-offs

# if storage and villages are optimal for different outcomes, 
# then there are trade-offs overall

# if storage is optimal for at least one outcome and villages
# are never optimal, storage is optimal overall

# if villages are optimal for at least one outcome and storage
# is never optimal, villages are optimal overall

# add column for optimal priority overall

for (i in 1:3) {
  network_outcomes_list[[i]] <- network_outcomes_list[[i]] %>%
    mutate(optimal_priority_overall = NA) 
}

for (j in 1:3) {
  
  for (i in 1:nrow(network_outcomes_list[[j]])) {
    
    priorities <- network_outcomes_list[[j]][i,] %>% select(contains("optimal"))
    
    if ("storage" %in% priorities & "villages" %in% priorities) {
      
      network_outcomes_list[[j]][i,]$optimal_priority_overall <- "trade offs"
      
    } else if ("storage" %in% priorities & !("villages" %in% priorities)) {
      
      network_outcomes_list[[j]][i,]$optimal_priority_overall <- "storage"
      
    } else if (!("storage" %in% priorities) & "villages" %in% priorities) {
      
      network_outcomes_list[[j]][i,]$optimal_priority_overall <- "villages"
      
    } else {
      
      network_outcomes_list[[j]][i,]$optimal_priority_overall <- "none"
      
    }
    
  }
  
}

# 7 - organize network outcome tables 

for (i in 1:3) {
  network_outcomes_list[[i]] <- network_outcomes_list[[i]] %>%
    
    # organize by seeds for spatial layouts with max aggregate RMSE across outcomes
    arrange(desc(crop_expansion_priority_AUC_difference)) %>%
    
    # organize columns for identifying optimal policy for each seed
    select(seed,
           crop_expansion_villages_prioritized_AUC,crop_expansion_storage_prioritized_AUC,crop_expansion_priority_AUC_difference,optimal_priority_crop_expansion,
           villages_connected_villages_prioritized_AUC,villages_connected_storage_prioritized_AUC,villages_connected_priority_AUC_difference,optimal_priority_villages,
           storage_connected_villages_prioritized_AUC,storage_connected_storage_prioritized_AUC,storage_connected_priority_AUC_difference,optimal_priority_storage,
           optimal_priority_overall) %>%
    
    # round outcomes
    mutate_at(vars(ends_with("AUC")),~round(.)) %>%
    
    mutate_at(vars(ends_with("priority_AUC_difference")),~round(.)) %>%
    
    
    # filter out seeds with RMSE = 0 because of lack of farms
    filter(crop_expansion_priority_AUC_difference>0)
}

# 8 - write seeds illustrating layouts more likely to lead to trade offs

grid_auc <- network_outcomes_list[[1]]

radial_auc <- network_outcomes_list[[2]]

random_auc <- network_outcomes_list[[3]]

maps_path <- "C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Intermediate_and_trade_off_maps/"

grid_seeds_maps <- grid_auc[seq(1,60,10),]$seed

write(grid_seeds_maps,paste0(maps_path,"grid_seeds_maps"))

radial_seeds_maps <- radial_auc[seq(1,60,10),]$seed

write(radial_seeds_maps,paste0(maps_path,"radial_seeds_maps"))

random_seeds_maps <- random_auc[seq(1,60,10),]$seed

write(random_seeds_maps,paste0(maps_path,"random_seeds_maps"))

network_seeds_maps <- data.frame(grid=grid_seeds_maps,
                                 radial=radial_seeds_maps,
                                 random=random_seeds_maps)

write.csv(network_seeds_maps,paste0(maps_path,"network_seeds_maps.csv"),row.names = FALSE)

# 9 - write tables

# AUC

grid_auc_condensed <- grid_auc %>%
  select(-ends_with("prioritized_AUC"))

radial_auc_condensed <- radial_auc %>%
  select(-ends_with("prioritized_AUC"))

random_auc_condensed <- random_auc %>%
  select(-ends_with("prioritized_AUC"))

headers <- gsub("_"," ",names(grid_auc_condensed))

names(grid_auc_condensed) <- paste("Grid",headers)

names(radial_auc_condensed) <- paste("Radial",headers)

names(random_auc_condensed) <- paste("Random",headers)

write.csv(grid_auc_condensed,"Grid_variable_layout_outcomes_AUC.csv",row.names = FALSE)

write.csv(radial_auc_condensed,"Radial_variable_layout_outcomes_AUC.csv",row.names = FALSE)

write.csv(random_auc_condensed,"Random_variable_layout_outcomes_AUC.csv",row.names = FALSE)