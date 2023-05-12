library("psych")
library("nFactors")
# 第一问
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
x_factor_7 <- fa(x_matrix, 7, fm = "pa", rotate = "varimax") #TODO: 因子个数与旋转方法有待商榷
write.csv(x_factor_7$loadings, "x_loadings.csv")
x_factor_7
plot(x_factor_7$loadings, type = "n")
text(x_factor_7$loadings, labels = names(x_train))