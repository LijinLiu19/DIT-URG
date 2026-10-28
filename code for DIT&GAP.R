#1.Baseline
library(readxl)
data<- read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data.xlsx")
set.seed(1234)
dat<-data
install.packages("stargazer")
library(stargazer)
stargazer(dat[, 1:16], 
          type = "text",          # 输出类型：text / latex / html
          title = "控制变量描述性统计",
          digits = 4,              # 保留4位小数
          out = "descriptive.txt") # 可选：直接输出文件
library(grf)
X1<-dat[,3:16]
W<-dat$did
Y<-dat$gap
f1 <- causal_forest(X1, Y, W,num.trees = 2000, sample.fraction = 0.5, min.node.size = 5,
                    honesty = TRUE, honesty.fraction = 0.5,alpha = 0.05)#This is the default setting; in line with current practice, we are using this default setting. The model settings below are the same, so they are not shown here.
f2 <- causal_forest(X1, Y, W,clusters = X1$pov_lab)
f3 <- causal_forest(X1, Y, W,clusters = X1$city_lab)
f4 <- causal_forest(X1, Y, W,clusters = X1$cou_lab)
average_treatment_effect(f1)
average_treatment_effect(f1,"overalap")
average_treatment_effect(f2)
average_treatment_effect(f2,"overlap")
average_treatment_effect(f3)
average_treatment_effect(f3,"overlap")
average_treatment_effect(f4)
average_treatment_effect(f4,"overlap")
# 2.Omitted Variables and Confounding Factors
library(dplyr)
library(ggplot2)
vi_raw<-variable_importance(f3)
vi_df <- data.frame(
  variable = colnames(X1),
  importance = vi_raw
) %>% 
  arrange(desc(importance)) %>% 
  mutate(
    importance_scaled = importance , # 标准化
    variable = factor(variable, levels = variable) # 按重要性排序因子
  )
ggplot(vi_df, aes(x = variable, y = importance_scaled, fill = importance_scaled)) +
  geom_col(color = "white", width = 0.9) + # 白色边框区分不同扇形，width调小可加间隙
  coord_polar(start = -pi/2) + # start=-pi/2让最重要的变量在正上方，更符合阅读习惯
  scale_fill_gradient(low = "#2E86AB", high = "#FF6B6B") + # 自定义渐变色
  labs(
    title = NULL,
    x = NULL, # 极坐标下x轴无意义，隐藏
    y = "重要性",
    fill = "重要性"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(), # 隐藏半径轴刻度
    axis.ticks = element_blank(), # 隐藏刻度线
    panel.grid = element_blank(), # 隐藏网格线
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold") # 标题居中加粗
  )
X2<-X1[,-8]
X3<-dat[,3:17]
X4<-dat[,3:18]
f5 <- causal_forest(X3, Y, W,clusters = X3$city_lab)
f6 <- causal_forest(X4, Y, W,clusters = X4$city_lab)

f8 <- causal_forest(X2, Y, W,clusters = X2$city_lab)
chars <- c("基准", "检验1","检验2","检验3")  #Painting a Landscape of Mountains
repeated_chars_strings <- rep(chars, each=21975) 
dathz<- data.frame(
  组别=repeated_chars_strings,
  value = c(f3$predictions, f5$predictions,f6$predictions,f8$predictions)
)

library(ggridges)
library(ggplot2)
p4<-ggplot(dathz, aes(x = value, y = 组别,fill=组别 )) +
  geom_density_ridges(alpha = 0.7) +
  xlab("处理效应") + ylab("密度") 
plot(p4)

#3.Robustness checks

#3.1 SUVTA checks
dat_suvta<- read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_suvta.xlsx")
W_sutva<-dat_suvta$did
x_sutva<-dat_suvta[,3:16]
Y_sutva<-dat_suvta$gap
f_sutva<-causal_forest(x_sutva, Y_sutva, W_sutva,clusters = x_sutva$city_lab)
average_treatment_effect(f_sutva)

#3.1_2 Randomised tests
set.seed(2132)
num_iter <- 300
placebo_ates <- numeric(num_iter)

for (i in 1:num_iter) {
  
  # 【核心步骤】随机打乱处理变量 W 的顺序（即虚构处理组）
  # sample(W) 会将 W 的数值原地打乱，切断与协变量 X 的真实关联
  W_permuted <- sample(W)
  
  # 运行因果森林（使用打乱后的 W）
  # 注意：为了节省时间，可以适当减少树木数量，但为了稳定建议保持 num.trees 不变
  cf_placebo <- causal_forest(
    X = X1,
    Y = Y,
    W = W_permuted,
    num.trees = 250,  # 可适当降低以加快循环速度，
    seed = i           # 为每次循环单独设置种子，增加随机性
  )
  
  # 计算并存储本次虚构样本的平均处理效应 (ATE)
  # average_treatment_effect 返回一个包含 estimate 和 std.err 的列表
  ate_placebo <- average_treatment_effect(cf_placebo)
  placebo_ates[i] <- ate_placebo["estimate"]
  
  # 可选：打印进度，避免等待时焦虑
  if (i %% 50 == 0) print(paste("完成第", i, "次迭代"))
}
plot_data <- data.frame(placebo_ate = placebo_ates)
p <- ggplot(plot_data, aes(x = placebo_ate)) +
  # 绘制虚构效应的核密度曲线（填充浅蓝色，边框深蓝）
  geom_density(fill = "#A6CEE3", color = "#1F78B4", alpha = 0.7, size = 0.8) +
  # 添加零线参考线（表示无效应），放在底层，颜色浅灰
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.5) +
  # 主题与标签设置
  labs(
    x = "Average treatment effect",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text = element_text(size = 11)
  )
plot(p)

#3.2 Macroeconomic policy
newdat<-dat[,-c(17:19)]
X6<-newdat[,3:17]
X61<-newdat[,3:18]
X7<-X61[,-15]
X71<-newdat[,3:19]
X8<-X71[,-c(15:16)]
f9 <- causal_forest(X6, Y, W,clusters = X6$city_lab)
f10<- causal_forest(X7, Y, W,clusters = X7$city_lab)
f11<- causal_forest(X8, Y, W,clusters = X8$city_lab)
average_treatment_effect(9)
average_treatment_effect(10)
average_treatment_effect(11)
#3.3 teil indicator
dat1<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_teil.xlsx")
X9<-dat1[,2:15]
W1<-dat1$did
Y1<-dat1$tiel
f12<-causal_forest(X9, Y1, W1,clusters = X9$city_lab)
average_treatment_effect(12)

#3.4 2020-2023
dat2<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_20_23.xlsx")
X10<-dat2[,3:16]
W2<-dat2$did
Y2<-dat2$gap
f13<-causal_forest(X10, Y2, W2,clusters = X10$city_lab)
average_treatment_effect(13)

# 3.5  Remove municipalities directly under the central government
dat3<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_remove.xlsx")
X11<-dat3[,3:16]
W3<-dat3$did
Y3<-dat3$gap
f14<-causal_forest(X11, Y3, W3,clusters = X11$city_lab)
average_treatment_effect(14)

#3.6 1% double-ended truncation
dat5<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_truncation.xlsx")
W5<-dat5$did
Y5<-dat5$gap
X13<-dat5[,3:16]
f16<-causal_forest(X13, Y5, W5,clusters = X13$city_lab)
average_treatment_effect(16)

# 4 Mechanism Analysis

#4.1 income 
dat_inc<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_income.xlsx")
X_inc<-dat_inc[,6:19]
W_inc<-dat_inc$did
Y_inc_r<-dat_inc$rural_inc
Y_inc_t<-dat_inc$town_inc 
f_r_inc<-causal_forest(X_inc, Y_inc_r,W_inc,clusters = X_inc$city_lab)
f_t_inc<-causal_forest(X_inc, Y_inc_t,W_inc,clusters = X_inc$city_lab)
average_treatment_effect(f_r_inc)
average_treatment_effect(f_t_inc)
forest_data2 <- list(
  label = c("城镇居民收入", "农村居民收入"),
  mean = c(0.067676, 0.199247),
  lower = c(0.0043974, 0.1029875),
  upper = c( 0.1309546, 0.2955065)
)
p <- forestplot(
  labeltext = forest_data2$label,
  mean = forest_data2$mean,
  lower = forest_data2$lower,
  upper = forest_data2$upper,
  xlab = "处理效应 (95% 置信区间)",
  clip = c(0, 0.35),
  zero = 0,
  col = fpColors(line = "steelblue",box="skyblue"),
  lineheight = unit(1, "cm"),
  boxsize = 0.05,
  txt_gp = fpTxtGp(label = gpar(cex = 1))
)
plot(p)

#4.2 Labour Factor Mobility
dat_labm<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_labm.xlsx")
Y=dat_labm$pop_m
W=dat_labm$did
X=dat_labm[,4:17]
set.seed(1221)
f_labm<-causal_forest(X, Y, W,clusters =X$city_lab)
average_treatment_effect(f_labm)

#4.2.1 The flow of capital
dat_capm<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_capm.xlsx")
Y1<-dat_capm$cap_m
W2<-dat_capm$did
X2<-dat_capm[,5:18]
f_cqpm<-causal_forest(X2, Y1, W2,clusters =X2$city_lab)
average_treatment_effect(f_cqpm)

#4.3 Agricultural development
#4.3.1 Labour productivity
dat_agr<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_agr.xlsx")
X5<-dat_agr[,5:18]
W5<-dat_agr$did
Y_agr1<-dat_agr$lab_p1
f_labpo<-causal_forest(X5, Y_agr1, W5,clusters =X6$city_lab)
average_treatment_effect(f_labpo)

#4.3.2 Value added in the primary sector
dat_agr2<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_agr2.xlsx")
Y_agr2<-dat_agr2$fir_ind
W6<-dat_agr2$did
X6<-dat_agr2[,6:19]
f_firind<-causal_forest(X6, Y_agr2, W6,clusters =X6$city_lab)
average_treatment_effect(f_firind)

# 4.3.3 Total labour-machine power per capita
dat_mach<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_mach.xlsx")
Y<-dat_mach$machine
W<-dat_mach$did
X<-dat_mach[,4:17]
set.seed(1221)
f_mach<-causal_forest(X, Y, W,clusters =X$city_lab)
average_treatment_effect(f_mach)

#4.4 Entrepreneurial Spirit
dat_firm<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_firm.xlsx")
Y_firm<-dat_firm$ln_firm
X_firm<-dat_firm[,10:23]
W_firm<-dat_firm$did
Y_firm1<-dat_firm$ln_firm1
Y_firm2<-dat_firm$ln_firm2
Y_firm3<-dat_firm$ln_firm3
f_firm<-causal_forest(X_firm, Y_firm, W_firm,clusters =X_firm$city_lab)
f_firm1<-causal_forest(X_firm, Y_firm1, W_firm,clusters =X_firm$city_lab)
f_firm2<-causal_forest(X_firm, Y_firm2, W_firm,clusters =X_firm$city_lab)
f_firm3<-causal_forest(X_firm3, Y_firm3, W_firm3,clusters =X_firm3$city_lab)



plot_data <- data.frame(
  Subgroup = factor(
    c("Newly_established", "primary_sector", "secondary_sector", "tertiary_sector"),
    levels = c("Newly_established", "primary_sector", "secondary_sector", "tertiary_sector")
  ),
  Group = c("Newly_established", "primary_sector", "secondary_sector", "tertiary_sector"),
  Estimate = c(0.1352, 0.5329, 0.2349, 0.0775),
  Lower = c(0.1001,0.3466, 0.1610, 0.0354),
  Upper = c(0.1703, 0.7190, 0.3089, 0.1196)
)

group_colors <- c(
  "Newly_established" = "#FF7F00",       # 红色
  "primary_sector" = "#4DAF4A",   # 绿色
  "secondary_sector" = "#377EB8",   # 蓝色
  "tertiary_sector" = "blue"    # 橙色
)

p_ggplot <- ggplot(plot_data, aes(x = Estimate, y = Subgroup)) +
  
  # 参考线（x=0，回归系数用0）
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 0.8) +
  
  # 置信区间线（按Group着色）
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, color = Group), 
                 height = 0.1, size = 1.5) +
  
  # 效应值点（按Group着色）
  geom_point(aes(color = Group, shape = Group), size = 4) +
  
  
  # 自定义颜色
  scale_color_manual(values = group_colors) +
  
  # 添加数值标签（保留4位小数）
  geom_text(aes(label = sprintf("%.4f (%.4f, %.4f)", Estimate, Lower, Upper), 
                color = Group),
            hjust = -0.1, size = 2.5) +
  
  # X轴范围
  coord_cartesian(xlim = c(-0.05, 0.75)) +
  
  # 标签
  labs(
    x = "Treatment effect (95% CI)",
    y = "General and Industry Breakdown"
  ) +
  
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12, color = "black"),   # Y
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "none"
  )

print(p_ggplot)

#4.5  Social and economic assistance
dat_assit<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_assit.xlsx")
dat_rural<-read.table("clipboard",header=T)
dat_town<-read.table("clipboard",header=T)
Y_pub<-dat_assit$lnpub
W_pub<-dat_assit$did
X_pub<-dat_assit[,4:20]
Y_rural<-dat_rural$lnpub
Y_twon<-dat_town$lnpub
W_rural<-dat_rural$did
W_twon<-dat_town$did
X_rural<-dat_rural[,4:19]
X_twon<-dat_town[,4:19]
f_pub<-causal_forest(X_pub, Y_pub, W_pub,clusters =X_pub$hhid)
f_rural<-causal_forest(X_rural, Y_rural, W_rural,clusters =X_rural$hhid)
f_twon<-causal_forest(X_twon, Y_twon, W_twon,clusters =X_twon$hhid)
average_treatment_effect(f_pub,"overlap")
average_treatment_effect(f_rural,"overlap")
average_treatment_effect(f_pub,"overlap")

#5 Further Analysis
#5.1 Common Prosperity
dat_gf<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_gf.xlsx")
Y_GF<-dat_gf$gf
W_GF<-dat_gf$did
X_GF<-dat_gf[,3:16]
f_GF<-causal_forest(X_GF, Y_GF, W_GF,clusters =X_GF$city_lab)
average_treatment_effect(f_GF)
best_linear_projection(f_GF)

#5.2 Institutional discrimination
dat_qs<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_qs.xlsx")
install.packages("oaxaca")
library(oaxaca)

ob_decomp <- oaxaca(
  formula = ln_lncome~sex+age+edu+party+married+health|rural, # 自变量为禀赋变量
  data = dat_qs,
  group = dat_qs$rural, # 分组变量
  reference = "0", # 参考组：0=城镇，即反事实为农村群体的禀赋对应城镇的回报率
  R = 200
)
summary(counterfactual_wage)
counterfactual_wage <-ob_decomp$y
model_urban <-ob_decomp$reg$reg.A   # 城镇（参照组）
model_rural <-ob_decomp$reg$reg.B   # 农村（实验组）
dat_qs$pred_ln_wage <- NA

dat_qs$discrimination <- NA
dat_qs$pred_ln_wage[dat_qs$rural == 0] <- predict(model_urban, newdata = dat_qs[dat_qs$rural== 0, ])
dat_qs$discrimination <-dat_qs$ln_wage - dat_qs$pred_ln_wage
dat_qs$pred_ln_wage2 <- NA
dat_qs$discrimination2<-NA 
dat_qs$pred_ln_wage2 <- predict(model_urban, newdata = dat_qs)
head(da)
dat_qs$discrimination2 <-dat_qs$ln_wage2 - dat_qs$pred_ln_wage
write.csv(dat_qs, "ob_decomposition_results.csv", row.names = F)
dat_ob<-read.table("clipboard",header=T)
X_ds<-dat_ob[,6:21]
W_qs<-dat_ob$did
Y_ds<-dat_ob$discrimination2
f_qs<-causal_forest(X_ds, Y_ds, W_qs,clusters =X_ds$hhid)
average_treatment_effect(f_qs)

#5.3 Standard International Index of Occupational and Socio-Economic Status
dat_isei<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_isei.xlsx")
Y_isei<-dat_isei$isei
X_isei<-dat_isei[,4:17]
W_isei<-dat_isei$did
f_isei<-causal_forest(X_isei, Y_isei, W_isei,clusters =X_isei$city_alb)
average_treatment_effect(f_isei)
best_linear_projection(f_isei)

#6 Heterogeneity Analysis
# 6.1 Heterogeneity Analysis Based on Regional Economic Characteristics
library(readxl)
data<- read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data.xlsx")
set.seed(1234)
dat<-data
X1<-dat[,3:16]
W<-dat$did
Y<-dat$gap
f3 <- causal_forest(X1, Y, W,clusters = X1$city_lab)
p4<-ggplot(mapping=aes(x=scale(X1$net),y=f3$predictions))+geom_smooth(method="lm",formula = y ~ x 
     + pmax(x-10, 0),se=TRUE,color = "blue")+xlab('信息技术')+
  ylab('处理效应')+theme(
    axis.title = element_text(size = 10),   # 坐标轴标题
    axis.text = element_text(size = 10),    # 坐标轴刻度数字
    plot.title = element_text(size = 10)    # 图形标题
  )
p5<-ggplot(mapping=aes(x=scale(X1$reve),y=f3$predictions))+geom_smooth(method="lm",formula = y ~ x 
    + pmax(x - 10, 0),se=TRUE, color = "blue")+xlab('财政收入')+
  ylab('处理效应')+theme(
    axis.title = element_text(size = 10),   # 坐标轴标题
    axis.text = element_text(size = 10),    # 坐标轴刻度数字
    plot.title = element_text(size = 10)    # 图形标题
  )

p6<-ggplot(mapping=aes(x=scale(X1$gdp),y=f3$predictions))+geom_smooth(method="lm",formula = y ~ x 
    + pmax(x -3.484, 0),se=TRUE, color = "blue")+xlab('经济发展')+
  ylab('处理效应')+theme(
    axis.title = element_text(size = 10),   # 坐标轴标题
    axis.text = element_text(size = 10),    # 坐标轴刻度数字
    plot.title = element_text(size = 10)    # 图形标题
  )
library(cowplot)
plot_grid(p4,p5,p6,nrow = 1)

#6.2  Heterogeneity Analysis Based on Regional Cultural Characteristics(Dialects and Confucian Culture)
#6.2.1 Dialects 
datfy<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_fy.xlsx")
XFY<-datfy[,3:16]
YFY<-datfy$gap
WFY<-datfy$did
f_fy<-causal_forest(XFY, YFY,WFY,clusters = XFY$city_lab)
group_stats <- data.frame(
  fy = datfy$fy,
  effect = f_fy$predictions
) %>%
  group_by(fy) %>%
  summarise(
    mean_effect = mean(effect, na.rm = TRUE),
    se = sd(effect, na.rm = TRUE) / sqrt(n()),
    lower = mean_effect - 1.96 * se,
    upper = mean_effect + 1.96 * se
  )
group_stats

p8 <- ggplot(mapping = aes(x = factor(datfy$fy), y = f_fy$predictions)) +
  geom_boxplot(fill =c("lightblue", "lightgreen"), color = "blue", alpha = 0.7) +
  xlab('方言多样性') +
  ylab('处理效应') +
  scale_x_discrete(labels = c("低", "高")) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 10)
  )
#6.2.2 Confucian Culture
datwh<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_wh.xlsx")
XWH<-datwh[,3:17]
WWH<-datwh$did
YWH<-datwh$gap
f_culture<-causal_forest(XWH, YWH,WWH,clusters = XWH$city_lab)
group_stats2<- data.frame(
  wh= datwh$wh,
  effect = f_wh$predictions
) %>%
  group_by(wh) %>%
  summarise(
    mean_effect = mean(effect, na.rm = TRUE),
    se = sd(effect, na.rm = TRUE) / sqrt(n()),
    lower = mean_effect - 1.96 * se,
    upper = mean_effect + 1.96 * se
  )
p9 <- ggplot(mapping = aes(x = factor(datwh$culture), y = f_culture$predictions)) +
  geom_boxplot(fill = c("lightblue","lightgreen"), color = "blue", alpha = 0.7) +
  xlab('儒家文化') +
  ylab('处理效应') +
  scale_x_discrete(labels = c("弱", "强"))+
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 10)
  )
library(cowplot)
plot_grid(p8,p9,nrow = 1)

#6.3 
datfc<-read_excel("C:/Users/29381/Desktop/DIT/wenjianjia/data_fc.xlsx")
XFC<-datfc[,3:17]
WFC<-datfc$did
YFC<-datfc$gap
f_fc<-causal_forest(XFC, YFC,WFC,clusters = XFC$city_lab)
p10 <- ggplot(mapping = aes(x = factor(datfc$factor), y = f_fc$predictions)) +
  geom_boxplot(fill = c("lightblue","lightgreen"), color = "blue", alpha = 0.7) +
  xlab('数据要素市场') +
  ylab('处理效应') +
  scale_x_discrete(labels = c("完善", "欠完善"))+
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 10)
  )




##


ggplot(vi_df, aes(x = variable, y = importance_scaled, fill = importance_scaled)) +
  geom_col() +
  coord_flip() + # 横向条形图，适配较长的变量名
  scale_fill_gradient(low = "#1f77b4", high = "#ff7f0e") +
  labs(
    title = "变量重要性",
    x = "变量",
    y = "重要性",
    fill = "重要性"
  ) +
  theme_bw()







f5<-causal_forest(X2, Y, W,clusters = X2$city_lab)
test_calibration(f3,"HC0")
X2<-dat[,7:16]
dat2<-cbind(W,ps_values)
ps_forest <- regression_forest(X2, W)
ps_values <- predict(ps_forest)$predictions
hist(ps_values)
ggplot(dat2, aes(x =ps_values, fill = as.factor(W))) +
  geom_density(alpha = 0.4)
glm_ps <- glm(W ~ ., data = dat, family = binomial)
df$estimated_ps <- predict(glm_ps,newdata =dat, type = "response")



