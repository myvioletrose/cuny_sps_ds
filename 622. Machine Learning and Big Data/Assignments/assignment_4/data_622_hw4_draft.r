# set wd
setwd("C:/development/school_of_professional_studies/622. Machine Learning and Big Data/Assignments/assignment_4")

# load packages
packages <- c(
    'tidyverse', 
    'corrplot', 
    'palmerpenguins',
    'class',
    'kableExtra',
    'naniar',
    'DataExplorer',
    'caret',
    'tidymodels',
    'rsample',
    'themis',
    'randomForest',
    'car',
    'xgboost',
    'broom',
    "readr",
    "MachineShop",
    "readit",
    "janitor",
    "recipes",
    "factoextra",
    "wrapr"
)

for (pkg in packages) {
    suppressPackageStartupMessages(suppressWarnings(
        library(
            pkg, character.only = TRUE, 
            warn.conflicts = FALSE, quietly = TRUE)
    ))
}

# read data
df = readit("ADHD_data.xlsx")
df <- janitor::clean_names(df)
dim(df)

# metadata
dfMeta <- data.frame(missing = colSums(is.na(df)),
                     dtype = sapply(df, class),
                     num_of_unique = sapply(df, function(x) length(unique(x)))) %>%
    tibble::rownames_to_column() %>%
    dplyr::select(columns = rowname, dtype, missing, num_of_unique)

# Spearman correlation on ADHD, MD
dfDisorder <- df %>% 
    dplyr::select(matches("age|sex|race|adhd|md"))

cor(dfDisorder, method = "spearman") %>% corrplot(type = "upper")

# data prep 
numeric_var = c("age", "adhd_total", "md_total", "education")

dfReady <-df %>% recipe(~.) %>% 
    step_rm(initial, psych_meds) %>% 
    step_impute_knn(all_predictors()) %>% 
    step_mutate_at(-all_of(numeric_var), fn = ~ as.factor(.)) %>% 
    step_dummy(all_nominal(), one_hot = TRUE) %>% 
    step_normalize(all_predictors()) %>%
    step_nzv(all_predictors()) %>%
    step_corr(all_predictors()) %>%
    prep()

dfReady <- dfReady %>% 
    bake(df)

dfReady <- dfReady %>%
    dplyr::mutate_at(all_of(numeric_var), function(x) scale(x) %>% as.numeric)

# check missing again
if(!any(colSums(is.na(dfReady)) >0)){print("No more missing variable.")}

# pca - ADHD
adhdPCA <- prcomp(dfReady %>% dplyr::select(contains("adhd_")))
summary(adhdPCA)

# variance
pr_var = (adhdPCA$sdev)^2 

# % of variance
prop_varex = pr_var / sum(pr_var)
cumulative_var = cumsum(prop_varex)

sd <- adhdPCA$sdev
loadings <- adhdPCA$rotation
rownames(loadings) <- colnames(dfReady %>% dplyr::select(contains("adhd_")))
scores <- adhdPCA$x

# scree plot
screeplot(adhdPCA, type = "l", npcs = 30, main = "Screeplot of the first 30 PCs")
abline(h = 1, col = "red", lty = 5)
legend("topright", legend = c("Eigenvalue = 1"), col = c("red"), lty = 5, cex = 0.6)

# cumulative variance plot
cumpro <- cumsum(adhdPCA$sdev^2 / sum(adhdPCA$sdev^2))
plot(cumpro[0:30], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
#abline(v = 6, col = "blue", lty = 5)
#abline(h = 0.88759, col = "blue", lty = 5)
#legend("topleft", legend = c("Cut-off @ PC6"), col = c("blue"), lty = 5, cex = 0.6) 

# loadings

# cutoff for 'important' loadings
cut_off <- sqrt(1/ncol(dfReady %>% dplyr::select(contains("adhd_")))) 

loadingsDf <- loadings %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column() %>%
    dplyr::select(variables = rowname, everything())

pc1_important <- loadingsDf %>% 
    dplyr::filter(abs(PC1) > cut_off) %>%
    dplyr::select(variables, PC1) %>%
    arrange(desc(abs(PC1)))

pc1_important

pc2_important <- loadingsDf %>% 
    dplyr::filter(abs(PC2) > cut_off) %>%
    dplyr::select(variables, PC2) %>%
    arrange(desc(abs(PC2)))

pc2_important

# extract and visualize results for individuals:
fviz_pca_ind(adhdPCA, 
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)

# extract and visualize results for variables:
fviz_pca_var(adhdPCA, 
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)

# biplot
biplot(adhdPCA, scale = 0, cex = 0.5)

fviz_pca_biplot(adhdPCA, 
                col.ind = "cos2", 
                col.var="contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE)



# plot just 2 components
# plot(adhdPCA$x[,1], adhdPCA$x[,2], xlab = "PC1 (13.9%)", ylab = "PC2 (8.7%)", main = "PC1 / PC2 - plot")

# Graph of variables: default plot
#fviz_pca_var(adhdPCA, col.var = "black")

###############################################################################################

# pca - MD
mdPCA <- prcomp(dfReady %>% dplyr::select(contains("md_")))
summary(mdPCA)

# variance
pr_var = (mdPCA$sdev)^2 

# % of variance
prop_varex = pr_var / sum(pr_var)
cumulative_var = cumsum(prop_varex)

sd2 <- mdPCA$sdev
loadings2 <- mdPCA$rotation
rownames(loadings2) <- colnames(dfReady %>% dplyr::select(contains("md_")))
scores2 <- mdPCA$x

# scree plot
screeplot(mdPCA, type = "l", npcs = 10, main = "Screeplot of the first 10 PCs")
abline(h = 1, col = "red", lty = 5)
legend("topright", legend = c("Eigenvalue = 1"), col = c("red"), lty = 5, cex = 0.6)

# cumulative variance plot
cumpro <- cumsum(mdPCA$sdev^2 / sum(mdPCA$sdev^2))
plot(cumpro[0:10], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
#abline(v = 6, col = "blue", lty = 5)
#abline(h = 0.88759, col = "blue", lty = 5)
#legend("topleft", legend = c("Cut-off @ PC6"), col = c("blue"), lty = 5, cex = 0.6) 

# loadings

# cutoff for 'important' loadings
cut_off2 <- sqrt(1/ncol(dfReady %>% dplyr::select(contains("md_")))) 

loadingsDf2 <- loadings2 %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column() %>%
    dplyr::select(variables = rowname, everything())

pc1_important2 <- loadingsDf2 %>% 
    dplyr::filter(abs(PC1) > cut_off2) %>%
    dplyr::select(variables, PC1) %>%
    arrange(desc(abs(PC1)))

pc1_important2

pc2_important2 <- loadingsDf2 %>% 
    dplyr::filter(abs(PC2) > cut_off2) %>%
    dplyr::select(variables, PC2) %>%
    arrange(desc(abs(PC2)))

pc2_important2

# extract and visualize results for individuals:
fviz_pca_ind(mdPCA, 
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)

# extract and visualize results for variables:
fviz_pca_var(mdPCA, 
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)

# biplot
biplot(mdPCA, scale = 0, cex = 0.5)

fviz_pca_biplot(mdPCA, 
                col.ind = "cos2", 
                col.var="contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE)

################################################################################
################################################################################
################################################################################

# pca - all (minus suicide)
allPCA <- prcomp(dfReady %>% dplyr::select(-suicide_X1))
summary(allPCA)

# variance
pr_var = (allPCA$sdev)^2 

# % of variance
prop_varex = pr_var / sum(pr_var)
cumulative_var = cumsum(prop_varex)

sd3 <- allPCA$sdev
loadings3 <- allPCA$rotation
rownames(loadings3) <- colnames(dfReady %>% dplyr::select(-suicide_X1))
scores3 <- allPCA$x

# scree plot
screeplot(allPCA, type = "l", npcs = 30, main = "Screeplot of the first 30 PCs")
abline(h = 1, col = "red", lty = 5)
legend("topright", legend = c("Eigenvalue = 1"), col = c("red"), lty = 5, cex = 0.6)

# cumulative variance plot
cumpro <- cumsum(allPCA$sdev^2 / sum(allPCA$sdev^2))
plot(cumpro[0:30], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
#abline(v = 6, col = "blue", lty = 5)
#abline(h = 0.88759, col = "blue", lty = 5)
#legend("topleft", legend = c("Cut-off @ PC6"), col = c("blue"), lty = 5, cex = 0.6) 

# loadings

# cutoff for 'important' loadings
cut_off3 <- sqrt(1/ncol(dfReady %>% dplyr::select(-suicide_X1)))

loadingsDf3 <- loadings3 %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column() %>%
    dplyr::select(variables = rowname, everything())

pc1_important3 <- loadingsDf3 %>% 
    dplyr::filter(abs(PC1) > cut_off3) %>%
    dplyr::select(variables, PC1) %>%
    arrange(desc(abs(PC1)))

pc1_important3

pc2_important3 <- loadingsDf3 %>% 
    dplyr::filter(abs(PC2) > cut_off3) %>%
    dplyr::select(variables, PC2) %>%
    arrange(desc(abs(PC2)))

pc2_important3

# extract and visualize results for individuals:
fviz_pca_ind(allPCA, 
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)

# extract and visualize results for variables:
fviz_pca_var(allPCA, 
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)

# biplot
biplot(allPCA, scale = 0, cex = 0.5)

fviz_pca_biplot(allPCA, 
                col.ind = "cos2", 
                col.var="contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE)

# fviz_pca_ind
fviz_pca_ind(allPCA,
             geom.ind = "point",
             pointshape = 21,
             pointsize = 2,
             fill.ind = dfReady$suicide_X1 %>% as.factor(),
             col.ind = "black",
             palette = "jco",
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Suicide") +
    ggtitle("2D PCA-plot") +
    theme(plot.title = element_text(hjust = 0.5))

###############################################################################################
###############################################################################################
###############################################################################################

