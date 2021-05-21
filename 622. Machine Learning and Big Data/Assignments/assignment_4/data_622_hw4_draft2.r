# baseline distribution
table(dfReady %>% 
          dplyr::mutate(suicide_X1 = dplyr::case_when(suicide_X1 >0 ~1, TRUE ~0) %>% as.factor()) %>% 
          dplyr::select(suicide_X1))

# split data
set.seed(1234)
index <- floor(sample(1:nrow(dfReady), nrow(dfReady) * .75, replace = FALSE))
trainSet <- dfReady[index, ] %>% dplyr::mutate(suicide_X1 = dplyr::case_when(suicide_X1 >0 ~1, TRUE ~0) %>% 
                                                   factor(., levels = c("1", "0"), labels = c("Yes", "No")))
testSet <- dfReady[-index, ] %>% dplyr::mutate(suicide_X1 = dplyr::case_when(suicide_X1 >0 ~1, TRUE ~0) %>% 
                                                   factor(., levels = c("1", "0"), labels = c("Yes", "No")))

# distribution
trainSet$suicide_X1 %>% table()
testSet$suicide_X1 %>% table()

# oversampling the minority class
oversampling_by = 1.5
oversample_index = sample(1:nrow(trainSet %>% dplyr::filter(suicide_X1 == "Yes")),
                          floor(nrow(trainSet %>% dplyr::filter(suicide_X1 == "Yes")) * oversampling_by), 
                          replace = TRUE)

trainSet2 = dplyr::bind_rows(
    trainSet %>% dplyr::filter(suicide_X1 == "Yes") %>% 
        .[oversample_index, ],
    trainSet %>% dplyr::filter(suicide_X1 == "No")
    )

trainSet2$suicide_X1 %>% table()

# trainSetControl
trainSet.control <- caret::trainControl(method = "cv", number = 5, savePredictions = "final")

#################################################################
# Note that, there is a tuning parameter C, also known as Cost, that determines the possible misclassifications. It essentially imposes a penalty to the model for making an error: the higher the value of C, the less likely it is that the SVM algorithm will misclassify a point.
# 
# By default caret builds the SVM linear classifier using C = 1.

# svm linear
tic()
svm_lin <- caret::train(suicide_X1 ~., 
                        data = trainSet, 
                        method = "svmLinear", 
                        trControl = trainSet.control,
                        tuneGrid = expand.grid(C = seq(0.1, 2, length = 20)))

svm_lin2 <- caret::train(suicide_X1 ~., 
                        data = trainSet2, 
                        method = "svmLinear", 
                        trControl = trainSet.control,
                        tuneGrid = expand.grid(C = seq(0.1, 2, length = 20)))
toc()

# overview of the model
svm_lin
#plot(svm_lin)

# print best model
svm_lin$bestTune
#as_tibble(svm_lin$results[which.min(svm_lin$results[, 2]), ])

# testSet
cm_svm_lin = caret::confusionMatrix(predict(svm_lin, testSet, type = "raw"), testSet$suicide_X1)
cm_svm_lin2 = caret::confusionMatrix(predict(svm_lin2, testSet, type = "raw"), testSet$suicide_X1)

cm_svm_lin
cm_svm_lin2

#################################################################
# To build a non-linear SVM classifier, we can use either polynomial kernel or radial kernel function. Again, the caret package can be used to easily computes the polynomial and the radial SVM non-linear models.
# 
# The package automatically choose the optimal values for the model tuning parameters, where optimal is defined as values that maximize the model accuracy.

# Computing SVM using radial basis kernel
tic()
svm_k <- caret::train(suicide_X1 ~., 
                      data = trainSet, 
                      method = "svmRadial", 
                      trControl = trainSet.control,
                      tuneLength = 20)

svm_k2 <- caret::train(suicide_X1 ~., 
                      data = trainSet2, 
                      method = "svmRadial", 
                      trControl = trainSet.control,
                      tuneLength = 20)
toc()

# overview of the model
svm_k
#plot(svm_k)

# print best model
svm_k$bestTune

# testSet
cm_svm_k = caret::confusionMatrix(predict(svm_k, testSet, type = "raw"), testSet$suicide_X1)
cm_svm_k2 = caret::confusionMatrix(predict(svm_k2, testSet, type = "raw"), testSet$suicide_X1)

cm_svm_k
cm_svm_k2

#################################################################
# Computing SVM using polynomial basis kernel

tic()
svm_p <- caret::train(suicide_X1 ~., 
                      data = trainSet, 
                      method = "svmPoly", 
                      trControl = trainSet.control,
                      tuneLength = 5)

svm_p2 <- caret::train(suicide_X1 ~., 
                       data = trainSet2, 
                       method = "svmPoly", 
                       trControl = trainSet.control,
                       tuneLength = 5)
toc()

# overview of the model
svm_p
#plot(svm_p)

# print best model
svm_p$bestTune
svm_p2$bestTune

# testSet
cm_svm_p = caret::confusionMatrix(predict(svm_p, testSet, type = "raw"), testSet$suicide_X1)
cm_svm_p2 = caret::confusionMatrix(predict(svm_p2, testSet, type = "raw"), testSet$suicide_X1)

cm_svm_p
cm_svm_p2

##################################
cmResultList = list(cm_svm_lin, cm_svm_lin2, cm_svm_k, cm_svm_k2, cm_svm_p, cm_svm_p2)
names(cmResultList) <- wrapr::qc(cm_svm_lin, cm_svm_lin2, cm_svm_k, cm_svm_k2, cm_svm_p, cm_svm_p2)

cmResultDf = lapply(1:length(cmResultList), function(x) broom::tidy(cmResultList[[x]]) %>%
                        dplyr::mutate(type = names(cmResultList)[x]) %>%
                        dplyr::filter(term != "mcnemar") %>%
                        dplyr::select(type, term, estimate)) %>%
    dplyr::bind_rows() %>%
    tidyr::spread(type, estimate) %>%
    dplyr::select(term, cm_svm_lin, cm_svm_lin2, cm_svm_k, cm_svm_k2, cm_svm_p, cm_svm_p2)

cmResultDf

#####################################################################################################
#####################################################################################################
#####################################################################################################

allPCA <- prcomp(dfReady %>% dplyr::select(-suicide_X1))
summary(allPCA)

# Creating a new dataset
new_trainSet = data.frame(dfReady %>% 
                              dplyr::select(suicide_X1) %>% 
                              dplyr::mutate(suicide_X1 = dplyr::case_when(suicide_X1 >0 ~1, TRUE ~0) %>% 
                                                factor(., levels = c("1", "0"), 
                                                       labels = c("Yes", "No")))) %>%
    dplyr::select(class = suicide_X1) %>%
    dplyr::bind_cols(., allPCA$x[, 1:45] %>% as.data.frame())

new_testSet = dplyr::bind_cols(class = testSet$suicide_X1, 
                               predict(allPCA, newdata = testSet)[, 1:45] %>% as.data.frame())

#####################################################################################
tic()

# svm linear
svm_lin_new <- caret::train(class ~., 
                            data = new_trainSet, 
                            method = "svmLinear", 
                            trControl = trainSet.control,
                            tuneGrid = expand.grid(C = seq(0.1, 2, length = 20)))

# svm basis kernel
svm_k_new <- caret::train(class ~., 
                          data = new_trainSet, 
                          method = "svmRadial", 
                          trControl = trainSet.control,
                          tuneLength = 20)

# svm polynomial basis kernel
svm_p_new <- caret::train(class ~., 
                          data = new_trainSet,
                          method = "svmPoly", 
                          trControl = trainSet.control,
                          tuneLength = 5)

toc()

# testSet
cm_svm_lin_new = caret::confusionMatrix(predict(svm_lin_new, new_testSet, type = "raw"), new_testSet$class)
cm_svm_k_new = caret::confusionMatrix(predict(svm_k_new, new_testSet, type = "raw"), new_testSet$class)
cm_svm_p_new = caret::confusionMatrix(predict(svm_p_new, new_testSet, type = "raw"), new_testSet$class)

cmResultList2 = list(cm_svm_lin_new, cm_svm_k_new, cm_svm_p_new)
names(cmResultList2) <- wrapr::qc(cm_svm_lin_new, cm_svm_k_new, cm_svm_p_new)

cmResultDf2 = lapply(1:length(cmResultList2), function(x) broom::tidy(cmResultList2[[x]]) %>%
                        dplyr::mutate(type = names(cmResultList2)[x]) %>%
                        dplyr::filter(term != "mcnemar") %>%
                        dplyr::select(type, term, estimate)) %>%
    dplyr::bind_rows() %>%
    tidyr::spread(type, estimate) %>%
    dplyr::select(term, cm_svm_lin_new, cm_svm_k_new, cm_svm_p_new)


finalComparison = dplyr::inner_join(cmResultDf, cmResultDf2, by = "term") %>%
    dplyr::select(term, 
                  cm_svm_lin, cm_svm_lin2, cm_svm_lin_new,
                  cm_svm_k, cm_svm_k2, cm_svm_k_new,
                  cm_svm_p, cm_svm_p2, cm_svm_p_new)

finalComparison

cm_svm_p_new

