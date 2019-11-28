setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Storage_by_village_tables")

communities <- c("Bandafassi","Makacoulibantang","Ndorna")

df_sums_list <- list()

for (i in 1:length(communities)) {
  
  community <- communities[i]
  
  # export from output on NL interface
  
  data <- read.delim(paste0("Ricescape_senegal_121718 output_",tolower(community),".txt"),header=FALSE)
  
  attributes <- read.csv(paste0(community,"_villages_attributes.csv"))
  
  attributes <- attributes[attributes$village=="c"|attributes$village=="cl",]
  
  attributes <- attributes[order(attributes$Y),]
  
  num_villages <- nrow(attributes)
  
  years <- 17
  
  data <- data[1:((num_villages+1)*years),1]
  
  mat <- matrix(data,nrow=(num_villages+1),ncol=years)
  
  df <- data.frame(mat)
  
  colnames(df) <- as.character(unlist(df[1,]))
  
  df <- df[-1,]
  
  rownames(df) <- 1:nrow(df)
  
  var1 <- c(1,6,11)
  
  var2 <- c(4,4,6)
  
  sum_rows <- function(var1,var2){rowSums(df[,var1:(var1+var2)])}
  
  df_sums <- mapply(sum_rows,var1,var2)
  
  df_sums <- as.data.frame(df_sums)
  
  colnames(df_sums) <- c("2024 storage (T)","2029 storage (T)","2035 storage (T)")
  
  df_sums$"2019 storage (T)" <- attributes$capacite
  
  df_sums$Village <- attributes$Localites
  
  df_sums$Community <- communities[i]
  
  df_sums <- df_sums[,c(6,5,4,1,2,3)]
  
  df_sums_list[[i]] <- df_sums
  
}

df_sums_bind <- bind_rows(df_sums_list)

write.csv(df_sums_bind,"Community_storage_output.csv",row.names = FALSE)
