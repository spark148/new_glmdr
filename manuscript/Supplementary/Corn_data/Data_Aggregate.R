library(tidyverse)

#Importing data
## Code.xlsx
Code_dat <- readxl::read_xlsx("./Code.xlsx", sheet=1) # 2815 x 8
#sapply(Code_dat, class)
Code_dat$`Avg. IBS` <- round(as.numeric(Code_dat$`Avg. IBS`),4) #Char -> Numeric

## Kernel_Color_Data.xlsx
### Final_Product - 1st sheet
Final_Product <- readxl::read_xlsx("./Kernel_Color_Data.xlsx",sheet=1)
Final_Product$`Accesion N` <- str_split(Final_Product$Complete_name,":",simplify=TRUE)[,1]
#head(sort(table(Final_Product$`Accesion N`),decreasing=TRUE))
### Final_Product - 1st sheet
Heavy_Lfiting <- readxl::read_xlsx("./Kernel_Color_Data.xlsx",sheet=2, skip=1) # In the original data, 1st line is garbage value so we should skip.
Heavy_Lfiting$`Accesion N` <- str_split(Heavy_Lfiting$Complete_name,":",simplify=TRUE)[,1]
#head(sort(table(Heavy_Lfiting$`Accesion N`),decreasing=TRUE)) 
### Final_Product - 1st sheet
Genotype_data <- readxl::read_xlsx("./Kernel_Color_Data.xlsx",sheet=3)
Genotype_data$`Accesion N` <- str_split(Genotype_data$Complete_name,":",simplify=TRUE)[,1]
#head(sort(table(Genotype_data$`Accesion N`),decreasing=TRUE),10) 
### Final_Product - 1st sheet
Phenotype_data <- readxl::read_xlsx("./Kernel_Color_Data.xlsx",sheet=4)
Phenotype_data$`Accesion N` <- str_split(Phenotype_data$Complete_name,":",simplify=TRUE)[,1]
#head(sort(table(Phenotype_data$`Accesion N`),decreasing=TRUE)) 

# end of Imprting data

#Cross duplicated values
Code_dup_tab <- sort(table(Code_dat$`Accesion N`),decreasing=TRUE)
Code_dup_lst <- names(Code_dup_tab[Code_dup_tab > 1])
Code_Final <- Code_dup_lst[Code_dup_lst %in% Final_Product$`Accesion N`]
Code_Heavy <- Code_dup_lst[Code_dup_lst %in% Heavy_Lfiting$`Accesion N`]
Code_Genotype <- Code_dup_lst[Code_dup_lst %in% Genotype_data$`Accesion N`]
Code_Phenotype <- Code_dup_lst[Code_dup_lst %in% Phenotype_data$`Accesion N`]

Code_Final_Product <- inner_join(Code_dat,Final_Product,by="Accesion N")
Code_Heavy <- inner_join(Code_dat,Heavy_Lfiting,by="Accesion N")
Code_Phenotype <- inner_join(Code_dat,Phenotype_data,by="Accesion N")

Code_Final_Product %>% openxlsx::write.xlsx("Combined_Final_Product.xlsx")
Code_Heavy %>% openxlsx::write.xlsx("Combined_Heavy_Lfiting.xlsx")
Code_Phenotype %>% openxlsx::write.xlsx("Combined_Phenotype_Data.xlsx")
