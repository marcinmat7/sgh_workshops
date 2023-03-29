# ##############################################################################
# SGH lecture, part 1 (AUC - basics)
# ##############################################################################
# Filename:     sgh_part1_auc.R
# Author(s):    Marcin Matuszewski (matuszewskimarcin7@gmail.com)
# Version:      1.0
#
# Changelog
# 03-2021   v1.0 Final version
# ##############################################################################

x <- c(0/7, 1/7, 1/7, 2/7, 2/7, 3/7, 3/7, 4/7, 5/7, 6/7, 7/7)
y <- c(0/3, 0/3, 1/3, 1/3, 2/3, 2/3, 3/3, 3/3, 3/3, 3/3, 3/3)

plot(x, y, xlab = "False Positive Rate", 
     ylab = "True Positive Rate", main = "ROC Curve")

probs <- c(1:9/10, 0.99)
true_Y <- c(0, 0,0,0,1,0,1,0,1,0)

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

# Example 

    probs <- c(1:9/10, 0.99)
    true_Y <- c(0, 0,0,0,1,0,1,0,1,0)
    
    plot_roc(probs, true_Y)


# Perfect model:

    probs <- c(1:9/10, 0.99)
    true_Y <- c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
    plot_roc(probs, true_Y)
    
# Worst possible model:
    
    probs <- c(1:9/10, 0.99)
    true_Y <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
    plot_roc(probs, true_Y)
    
# Random model:
    
    probs <- runif(n = 1000, min = 0, max = 1) 
    true_Y <- sample(x = c(0, 1), size = 1000, replace = TRUE) 
    plot_roc(probs, true_Y)
    
# Insensitive for monotonic transformation:

    probs <- c(1:9/10, 0.99)
    true_Y <- c(0, 0, 0, 0, 1, 0, 1, 0, 1, 0)
    plot_roc(probs, true_Y)

    probs <- 20 * c(1:9/10, 0.99) + 401
    true_Y <- c(0, 0, 0, 0, 1, 0, 1, 0, 1, 0)
    plot_roc(probs, true_Y)
    plot_roc(2 * probs + 100, true_Y)
    plot_roc(log(x = 2 * probs + 100), true_Y)
    
    
# Standard deviation of AUC
    
    set.seed(120)
    probs <- c(runif(1000, min = 0.1, max = 0.9))
    true_Y <- rbinom(n = 1000, size = 1, prob = probs)
    plot_roc(probs, true_Y)
    
    # 0.749807
    
    # Sample size = 10
    aucs <- c()
    for(i in 1:1000) {
        ind <- sample(1:1000, size = 10, replace = FALSE)     
        probs_a <- probs[ind]
        true_Y_a <- true_Y[ind]
        
        aucs <- c(aucs, plot_roc(probs = probs_a, true_y = true_Y_a, draw = FALSE))
    }    
    hist(aucs, 100)
    mean(aucs, na.rm = TRUE)
    sd(aucs, na.rm = TRUE)
    
    # Sample size = 100
    aucs <- c()
    for(i in 1:1000) {
        print(i)
        ind <- sample(1:1000, size = 100, replace = FALSE)     
        probs_a <- probs[ind]
        true_Y_a <- true_Y[ind]
        
        aucs <- c(aucs, plot_roc(probs = probs_a, true_y = true_Y_a, draw = FALSE))
    }    
    hist(aucs, 100)
    mean(aucs)
    sd(aucs)
    
    # Sample size = 500
    aucs <- c()
    for(i in 1:1000) {
        print(i)
        ind <- sample(1:1000, size = 500, replace = FALSE)     
        probs_a <- probs[ind]
        true_Y_a <- true_Y[ind]
        
        aucs <- c(aucs, plot_roc(probs = probs_a, true_y = true_Y_a, draw = FALSE))
    }    
    hist(aucs, 100)
    mean(aucs)
    sd(aucs)
    
# Probability interpretation
    
    probs <- c(11.2, 21.3, 32.1, 44.3, 52.5, 60)
    true_Y <- c(0,0,1,0,1,0)
    plot_roc(probs, true_Y)
    
    
    
