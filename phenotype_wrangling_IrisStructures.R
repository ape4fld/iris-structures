library(tidyverse)
library(here)

home=here()

# read iris structures grades
iris_grades <- read.table(paste(home,"/iris-structures/eyestructures_graded.csv", sep=""), 
           sep=",", header=TRUE, fill = TRUE) %>%
  rename(ID_1 = IID)

# convert to numeric:
cols.num <- c("CRYPT", "PIGRING", "FURROW", "NEVI")
iris_grades[cols.num] <- sapply(iris_grades[cols.num],as.numeric)
sapply(iris_grades, class)
  
head(iris_grades)

# read camera body covariate
camera <- read.table(paste(home,"/iris-structures/Eur-iris-pigm-2.sample",sep=""), 
                     sep = " ", header = TRUE, fill = TRUE)[-1,] %>%
  select(id_1, camera) %>%
  rename(ID_1 = id_1)
camera$camera <- as.numeric(camera$camera)
head(camera)

# read other covariates (sex, PCs)
covariates <- read.table(paste(home,"/iris-structures/eur-iris-str3.sample",sep=""),
                         sep=" ", header=TRUE, fill=TRUE)[-1,] %>%
  select(ID_1, ID_2, missing, sex, eye_color_L, eye_color_a, eye_color_b, pc1, pc2, pc3, pc4, pc5) %>%
  rename(EC_a = eye_color_a, EC_L = eye_color_L, EC_b = eye_color_b)

# make coviariates numeric:
cols.num <- c("sex","EC_L","EC_a","EC_b","pc1","pc2","pc3","pc4","pc5")
covariates[cols.num] <- sapply(covariates[cols.num],as.numeric)
sapply(covariates, class)
head(covariates)

# read eye colour categories
ec_cat <- read.table(paste(home,"/iris-structures/eur-iris-str2.sample",sep=""),
                     sep=" ", header = TRUE, fill=TRUE)[-1,] %>%
  select(ID_1, eye_color) %>%
  rename(EC_cat = eye_color)
ec_cat$EC_cat <- as.numeric(ec_cat$EC_cat)
head(ec_cat)

# read age for covariate
age <- read.table(paste(home,"/iris-structures/iris_structure_data_full.txt",sep=""),
                  sep="\t", header=TRUE, fill=TRUE) %>%
  select(Participant, Age) %>%
  rename(ID_1 = Participant)
head(age)

# join different tables into one
phenotype <- left_join(covariates, camera, by = "ID_1") %>% 
  left_join(., age, by = "ID_1") %>%
  left_join(., ec_cat, by = "ID_1") %>%
  left_join(., iris_grades, by = "ID_1")

head(phenotype)

write.table(phenotype, here("/iris-structures/", "iris-structures.sample"), 
            sep = " ", row.names = FALSE, quote = FALSE)

#########################################
######################################### Correlations among phenotypes and traits
#########################################


results_cor_pearson <- data.frame(matrix(0, ncol = 4, nrow = 12)) %>%
  rename(CRYPT = X1, PIGRING = X2, FURROW = X3, NEVI = X4)
results_pval_pearson <- data.frame(matrix(0, ncol = 4, nrow = 12)) %>%
  rename(CRYPT = X1, PIGRING = X2, FURROW = X3, NEVI = X4)
results_cor_spearman <- data.frame(matrix(0, ncol = 4, nrow = 12)) %>%
  rename(CRYPT = X1, PIGRING = X2, FURROW = X3, NEVI = X4)
results_pval_spearman <- data.frame(matrix(0, ncol = 4, nrow = 12)) %>%
  rename(CRYPT = X1, PIGRING = X2, FURROW = X3, NEVI = X4)

row.names(results_cor_pearson) <- c("sex","EC_L","EC_a","EC_b","pc1","pc2","pc3","pc4","pc5","camera","Age","EC_cat")
row.names(results_pval_pearson) <- c("sex","EC_L","EC_a","EC_b","pc1","pc2","pc3","pc4","pc5","camera","Age","EC_cat")
row.names(results_cor_spearman) <- c("sex","EC_L","EC_a","EC_b","pc1","pc2","pc3","pc4","pc5","camera","Age","EC_cat")
row.names(results_pval_spearman) <- c("sex","EC_L","EC_a","EC_b","pc1","pc2","pc3","pc4","pc5","camera","Age","EC_cat")

y=1
for (i in c(16:19)) {  #x
  x=1
  for (j in c(4:15)) { #y
  pear <- cor.test(phenotype[,i], phenotype[,j], method = "pearson")
  spear <- cor.test(phenotype[,i], phenotype[,j], method = "spearman")
  results_cor_pearson[x,y] <- pear$estimate
  results_pval_pearson[x,y] <- pear$p.value
  results_cor_spearman[x,y] <- spear$estimate
  results_pval_spearman[x,y] <- spear$p.value
  x=x+1
  }
  y=y+1
}

write.table(results_cor_pearson, here("/iris-structures", "results_cor_pearson.txt"), 
            sep = "\t", row.names = TRUE, quote = FALSE)
write.table(results_pval_pearson, here("/iris-structures", "results_pval_pearson.txt"), 
            sep = "\t", row.names = TRUE, quote = FALSE)
write.table(results_cor_spearman, here("/iris-structures", "results_cor_spearman.txt"), 
            sep = "\t", row.names = TRUE, quote = FALSE)
write.table(results_pval_spearman, here("/iris-structures", "results_pval_spearman.txt"), 
            sep = "\t", row.names = TRUE, quote = FALSE)