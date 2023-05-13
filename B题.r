library("psych")
library("nFactors")
library("e1071")

# 第一问:完成?
x_train <- read.csv("X_train.csv")
x_coefficient <- corr.test(x_train) # 警告: 这个函数会执行大约15min!
# 将得到的所有数据进行输出
write.csv(x_coefficient$r, "x_coefficient_r.csv")
print(x_coefficient$n)
write.csv(x_coefficient$t, "x_coefficient_t.csv")
write.csv(x_coefficient$p, "x_coefficient_p.csv")
write.csv(x_coefficient$p.adj, "x_coefficient_p.adj.csv")
write.csv(x_coefficient$se, "x_coefficient_se.csv")
print(x_coefficient$sef)
print(x_coefficient$adjust)
print(x_coefficient$sym)
write.csv(x_coefficient$ci, "x_coefficient_ci.csv")
write.csv(x_coefficient$ci2, "x_coefficient_ci2.csv")
write.csv(x_coefficient$ci.adj, "x_coefficient_ci.adj.csv")
write.csv(x_coefficient$stars, "x_coefficient_stars.csv")
print(x_coefficient$Call)
# 确定应提取的因子个数
x_eigen <- eigen(cor(x_train))
x_parallel <- parallel(nrow(x_train), ncol(x_train)) # 4min
x_factor_number <- nScree(aparallel = x_parallel$eigen$qevpea, x = x_eigen$values)
plot(x_factor_number$Analysis[1:20, 1], type = "b")
plotnScree(x_factor_number)
# 特征矩阵
x_coefficient_r <- read.csv("x_coefficient_r.csv")
x_matrix <- as.matrix(x_coefficient_r[, 2:ncol(x_coefficient_r)])
row.names(x_matrix) <- x_coefficient_r[, 1]
x_factor_7 <- fa(x_matrix, 100, fm = "pa", rotate = "varimax")
x_factor_7 <- fa(x_matrix, 107, fm = "pa", rotate = "varimax")
write.csv(x_factor_7$loadings, "x_loadings.csv")
write.csv(x_factor_7$Vaccounted, "x_vaccount.csv")
x_factor_7
plot(x_factor_7$loadings, type = "n")
text(x_factor_7$loadings, labels = names(x_train))

# 第二问:完成!
xy_train <- read.csv("xy_train_filtered.csv")
y_train <- as.factor(xy_train$y)
xy_train$y <- y_train
xy_test <- read.csv("xy_test_filtered.csv")
y_test <- as.factor(xy_test$y)
xy_test$y <- y_test
svmWithTimer <- function(kernel = "radial", data = xy_train) {
    start_time <- Sys.time()
    xy_svm <- svm(y ~ ., data, kernel = kernel)
    end_time <- Sys.time()
    print(end_time - start_time)
    return(xy_svm)
}
xy_svm <- svmWithTimer()
xy_svm <- svmWithTimer("linear") # 最好
xy_svm <- svmWithTimer("polynomial")
xy_svm <- svmWithTimer("sigmoid")
xy_svm
plot(xy_svm, data = xy_train, tBodyAcc.mean...X ~ tBodyAcc.mean...Y)
predictWithTimer <- function(data, model, output = NULL) {
    start_time <- Sys.time()
    xy_predict <- predict(model, data)
    end_time <- Sys.time()
    print(end_time - start_time)
    xy_table <- table(Actual = data$y, Predicted = xy_predict)
    if (!is.null(output)) {
        write.csv(xy_table, output)
    }
    return(sum(diag(xy_table)) / sum(xy_table))
}
xy_accuracy <- predictWithTimer(xy_train, xy_svm)
xy_accuracy <- predictWithTimer(xy_test, xy_svm)
# 筛选
xy_filtered <- xy_train[, c(1:6, 41:46, 81:86, 121:126, 200, 201, 214, 215, 227, 228, 253, 254, 555:562)]
xy_svm <- svmWithTimer("linear", xy_filtered)
xy_svm <- svmWithTimer("radial", xy_filtered)
xy_svm <- svmWithTimer("polynomial", xy_filtered)
xy_svm <- svmWithTimer("sigmoid", xy_filtered)
xy_accuracy <- predictWithTimer(xy_train, xy_svm)
xy_accuracy <- predictWithTimer(xy_test, xy_svm)

# 第三问:完成
xy_train <- read.csv("xy_train.csv")
y_train <- as.factor(xy_train$y)
xy_train$y <- y_train
xy_test <- read.csv("xy_test.csv")
y_test <- as.factor(xy_test$y)
xy_test$y <- y_test
xy_svm <- svmWithTimer("linear")
xy_svm <- svmWithTimer()
xy_svm <- svmWithTimer("polynomial")
xy_svm <- svmWithTimer("sigmoid")
xy_accuracy <- predictWithTimer(xy_train, xy_svm)
xy_accuracy <- predictWithTimer(xy_test, xy_svm)
xy_predict <- predict(xy_svm, xy_train)
xy_table <- table(Actual = xy_train$y, Predicted = xy_predict)
# 筛选
xy_filtered <- xy_train[, c(1:6, 41:46, 81:86, 121:126, 200, 201, 214, 215, 227, 228, 253, 254, 555:562)]
xy_svm <- svmWithTimer("linear", xy_filtered)
xy_svm <- svmWithTimer("radial", xy_filtered)
xy_svm <- svmWithTimer("polynomial", xy_filtered)
xy_svm <- svmWithTimer("sigmoid", xy_filtered)
xy_accuracy <- predictWithTimer(xy_train, xy_svm)
xy_accuracy <- predictWithTimer(xy_test, xy_svm)
# 批量输出
for (kernel in c("linear", "radial", "polynomial", "sigmoid")) {
    xy_svm <- svmWithTimer(kernel)
    predictWithTimer(xy_train, xy_svm, paste0("第三问-预测结果/训练集全集", kernel, ".csv"))
    predictWithTimer(xy_test, xy_svm, paste0("第三问-预测结果/测试集全集", kernel, ".csv"))
    xy_svm <- svmWithTimer(kernel, xy_filtered)
    predictWithTimer(xy_train, xy_svm, paste0("第三问-预测结果/训练集子集", kernel, ".csv"))
    predictWithTimer(xy_test, xy_svm, paste0("第三问-预测结果/测试集子集", kernel, ".csv"))
}
# 改进
xy_improved <- xy_train[, c(1:6, 41:46, 81:86, 121:126, 161:166, 200, 201, 214, 215, 227, 228, 240, 241, 253, 254, 555:562)]
xy_svm <- svmWithTimer("linear", xy_improved)
xy_predict <- predict(xy_svm, xy_train)
xy_table <- table(Actual = xy_train$y, Predicted = xy_predict)
xy_svm <- svmWithTimer("linear", xy_improved)
xy_svm <- svmWithTimer("radial", xy_improved)
xy_svm <- svmWithTimer("polynomial", xy_improved)
xy_svm <- svmWithTimer("sigmoid", xy_improved)
xy_accuracy_train <- predictWithTimer(xy_train, xy_svm)
xy_accuracy_test <- predictWithTimer(xy_test, xy_svm)
xy_svm
for (kernel in c("linear", "radial", "polynomial", "sigmoid")) {
    xy_svm <- svmWithTimer(kernel, xy_improved)
    predictWithTimer(xy_train, xy_svm, paste0("第三问-预测结果/训练集改进", kernel, ".csv"))
    predictWithTimer(xy_test, xy_svm, paste0("第三问-预测结果/测试集改进", kernel, ".csv"))
}
