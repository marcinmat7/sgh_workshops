# ##############################################################################
# SGH lecture, part 2 (PD modelling)
# ##############################################################################
# Filename:     sgh_part2_pd_model.R
# Author(s):    Marcin Matuszewski (matuszewskimarcin7@gmail.com)
# Version:      1.0
#
# Changelog
# 03-2021   v1.0 Final version
# ##############################################################################

library("ggplot2")
library("dplyr")

plot_roc <- function(probs, true_y, draw = TRUE) {
    
    getROC_AUC <- function(probs, true_y){
        probsSort = sort(probs, decreasing = TRUE, index.return = TRUE)
        val = unlist(probsSort$x)
        idx = unlist(probsSort$ix)  
        
        roc_y = true_y[idx];
        stack_x = cumsum(roc_y == 0)/sum(roc_y == 0)
        stack_y = cumsum(roc_y == 1)/sum(roc_y == 1)    
        
        auc = sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])
        return(list(stack_x=stack_x, stack_y=stack_y, auc=auc))
    }
    aList = getROC_AUC(probs, true_y) 
    
    stack_x = unlist(aList$stack_x)
    stack_y = unlist(aList$stack_y)
    auc = unlist(aList$auc)
    
    if (draw) {
        plot(stack_x, stack_y, 
             type = "l", col = "blue", 
             xlab = "False Positive Rate", 
             ylab = "True Positive Rate", main = "ROC", ylim = c(0,1),
             xlim = c(0,1))
    }
    return(auc)
}

# Read dataset

    setwd("C:/Users/garri/Desktop/sgh - wyklad")
    credit_records <- read.csv("credit_record.csv")
    app_record <- read.csv("application_record.csv")


# Labels (credit_records)
    {
        credit_records$STATUS[credit_records$STATUS == "C"] <- 0
        credit_records$STATUS[credit_records$STATUS == "X"] <- 0
        
        credit_records$def <- 0
        credit_records$def[credit_records$STATUS >= 2] <- 1
        
        credit_records2 <- credit_records %>% 
            group_by(ID) %>%
            summarise(def = max(def))
    }
    table(credit_records2$def)

    
# Labels (credit_records)

    {
        head(app_record)
        app_record2 <- app_record[app_record$ID %in% credit_records2$ID, ]
        
        length(unique(app_record2$ID)) / length(app_record2$ID)
        length(app_record2$ID)
        length(unique(credit_records2$ID))
        
        credit_records3 <- credit_records2[credit_records2$ID %in% app_record2$ID, ]
        table(sort(credit_records3$ID) == sort(app_record2$ID))
        
        credit_records3 <- credit_records3[order(credit_records3$ID), ]
        app_record2 <- app_record2[order(app_record2$ID), ]
        table(credit_records3$ID == app_record2$ID)
        
        data1 <- data.frame(app_record2, credit_records3)
        names(data1)
    }
    
    tab <- table(data1$def, data1$CODE_GENDER)
    round(prop.table(tab, 2), 2)
    barplot(prop.table(tab, 2), col = c("blue", "red"))

    tab <- table(data1$def, data1$FLAG_OWN_CAR)
    round(prop.table(tab, 2), 2)
    barplot(prop.table(tab, 2), col = c("blue", "red"))
    
    tab <- table(data1$def, data1$FLAG_OWN_REALTY)
    round(prop.table(tab, 2), 2)
    barplot(prop.table(tab, 2), col = c("blue", "red"))
    
    tab <- table(data1$def, data1$CNT_CHILDREN)
    round(prop.table(tab, 2), 2)
    barplot(prop.table(tab, 2), col = c("blue", "red"))

    table(data1$NAME_EDUCATION_TYPE)
    
# How to transform nominal variables??

    logit_f <- function(prob) {return(log(x = prob / (1 - prob), base = exp(1)))}
    add_logits <- function(data, col) {
        
        data[[paste0(col, "_logit")]] <- 1
        nams <- unique(data1[[col]])
        logits <- c()
        
        for(i in nams) {
            logits <- c(logits, logit_f(mean(data[['def']][data[[col]] == i])))
        }
        
        for(i in nams) {
            data[[paste0(col, "_logit")]][data[[col]] == i] <- logits[i == nams]
        }
        return(data)
    }

    {
        names(data1)
        data2 <- add_logits(data1, "CODE_GENDER")
        data2 <- add_logits(data2, "FLAG_OWN_CAR")
        data2 <- add_logits(data2, "FLAG_OWN_REALTY")
        data2 <- add_logits(data2, "NAME_INCOME_TYPE")
        data2 <- add_logits(data2, "NAME_EDUCATION_TYPE")
        data2 <- add_logits(data2, "NAME_FAMILY_STATUS")
        data2 <- add_logits(data2, "NAME_HOUSING_TYPE")
        data2 <- add_logits(data2, "FLAG_WORK_PHONE")
        data2 <- add_logits(data2, "FLAG_PHONE")
        data2 <- add_logits(data2, "FLAG_EMAIL")
        data2 <- add_logits(data2, "OCCUPATION_TYPE")
    }


    names(data2) <- tolower(names(data2))
    
    features <- c("code_gender_logit", "flag_own_car_logit", "flag_own_realty_logit",
                  "name_income_type_logit", "name_education_type_logit", 
                  "name_family_status_logit", "name_housing_type_logit", 
                  "flag_work_phone_logit", "flag_phone_logit", "flag_email_logit",
                  "occupation_type_logit",
                  
                  "amt_income_total", "days_birth", "days_employed",
                  "cnt_fam_members")
    
    
    y_label <- "def"
    data3 <- data2[, c(features, "def")]


# Split into training and test set

# Model 1

    set.seed(123)
    ind <- sample(x = c(TRUE, FALSE), size = nrow(data3), prob = c(0.2, 0.8), replace = TRUE)
    data3_test <- data3[ind, ]
    data3_train <- data3[!ind, ]
    
    model_glm <- glm(formula = paste0("def ~ ", paste(features, collapse='+')), 
                     family = "binomial", 
                     data = data3_train)
    
    for(i in names(data3_train)) {
        data3_train[[i]][data3_train[[i]] == -Inf] <- min(data3_train[[i]][data3_train[[i]] != -Inf])
        data3_train[[i]][data3_train[[i]] == Inf] <- max(data3_train[[i]][data3_train[[i]] != Inf])
    }
    
    model_glm <- glm(formula = paste0("def ~ ", paste(features, collapse='+')), 
                     family = "binomial", 
                     data = data3_train)
    
    plot_roc(model_glm$fitted.values, true_y = data3_train$def)
    y_pred_test <- predict(model_glm, newdata = data3_test, type = "response")
    plot_roc(y_pred_test, true_y = data3_test$def)
    summary(model_glm)


# # Model 2 (oversampling)

    set.seed(123)
    ind <- sample(x = c(TRUE, FALSE), size = nrow(data3), prob = c(0.2, 0.8), replace = TRUE)
    data3_test <- data3[ind, ]
    data3_train <- data3[!ind, ]
    data3_train <- data3_train[c(sample(which(data3_train$def == 1), size = 5000, replace = TRUE),
                                 sample(which(data3_train$def == 0), size = 5000, replace = TRUE)), ]
    
    for(i in names(data3_train)) {
        data3_train[[i]][data3_train[[i]] == -Inf] <- min(data3_train[[i]][data3_train[[i]] != -Inf])
        data3_train[[i]][data3_train[[i]] == Inf] <- max(data3_train[[i]][data3_train[[i]] != Inf])
    }
    
    model_glm <- glm(formula = paste0("def ~ ", paste(features, collapse='+')), 
                     family = "binomial", 
                     data = data3_train)
    
    plot_roc(model_glm$fitted.values, true_y = data3_train$def)
    y_pred_test <- predict(model_glm, newdata = data3_test, type = "response")
    plot_roc(y_pred_test, true_y = data3_test$def)
    summary(model_glm)

    
# Model 3 (num features standardised)
    
    set.seed(123)
    ind <- sample(x = c(TRUE, FALSE), size = nrow(data3), prob = c(0.2, 0.8), replace = TRUE)
    data3_test <- data3[ind, ]
    data3_train <- data3[!ind, ]
    
    for(i in names(data3_train)) {
        data3_train[[i]][data3_train[[i]] == -Inf] <- min(data3_train[[i]][data3_train[[i]] != -Inf])
        data3_train[[i]][data3_train[[i]] == Inf] <- max(data3_train[[i]][data3_train[[i]] != Inf])
    }
    
    num_cols <- c("amt_income_total", "days_birth", "days_employed", "cnt_fam_members")
    
    for(i in num_cols) {
        data3_train[[i]][data3_train[[i]] < quantile(x = data3_train[[i]], probs = 0.05)] <- 
            quantile(x = data3_train[[i]], probs = 0.05)
        
        data3_train[[i]][data3_train[[i]] > quantile(x = data3_train[[i]], probs = 0.95)] <- 
            quantile(x = data3_train[[i]], probs = 0.95)
        
        
        data3_test[[i]][data3_test[[i]] < quantile(x = data3_train[[i]], probs = 0.05)] <- 
            quantile(x = data3_train[[i]], probs = 0.05)
        
        data3_test[[i]][data3_test[[i]] > quantile(x = data3_train[[i]], probs = 0.95)] <- 
            quantile(x = data3_train[[i]], probs = 0.95)
        
        
        data3_test[[i]] <- (data3_test[[i]] - mean(data3_train[[i]])) / sd(data3_train[[i]])
        data3_train[[i]] <- (data3_train[[i]] - mean(data3_train[[i]])) / sd(data3_train[[i]])     
    }
    
    data3_train <- data3_train[c(sample(which(data3_train$def == 1), size = 5000, replace = TRUE),
                                 sample(which(data3_train$def == 0), size = 5000, replace = TRUE)), ]
    
    model_glm <- glm(formula = paste0("def ~ ", paste(features, collapse='+')), 
                     family = "binomial", 
                     data = data3_train)
    
    plot_roc(model_glm$fitted.values, true_y = data3_train$def)
    y_pred_test <- predict(model_glm, newdata = data3_test, type = "response")
    plot_roc(y_pred_test, true_y = data3_test$def)


### Model 4: decision tree
    
    library("rpart")
    library("rpart.plot")
    
    fit <- rpart(def~., data = data3_train, method = 'class')
    rpart.plot(fit, extra = 106)
    
    plot_roc(data.frame(predict(fit, data3_train, type = 'prob'))$X1, 
             true_y = data3_train$def)
    
    plot_roc(data.frame(predict(fit, data3_test, type = 'prob'))$X1, 
             true_y = data3_test$def)

    
    # winsoryzacja
    # standaryzacja
    # buckety
    # obsluga nulli
    

### Second dataset

    setwd("C:/Users/garri/Desktop/sgh - wyklad")
    credit_records <- read.csv("dataset_2/application_data.csv")
    