library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)

file <- read_excel("~/Sync/Documents/統計學報告/Bus.xlsx")
# View(file)

n <- nrow(file) # 共有80筆資料

# table(file$`Engine Type`) # Diesel: 53, Gasoline: 27

engine_df <- file %>%
  group_by(`Engine Type`) %>%
  count() %>%
  ungroup() %>%
  mutate(perc = `n` / sum(`n`)) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

brand_df <- file %>%
  group_by(Manufacturer) %>%
  count() %>%
  ungroup() %>%
  mutate(perc = `n` / sum(`n`)) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

# 畫出汽柴油的比例
ggplot(engine_df, aes(x = "", y = perc, fill = `Engine Type`)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
  theme_void()

# 畫出廠牌的比例
ggplot(brand_df, aes(x = "", y = perc, fill = Manufacturer)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
  theme_void()

# 切分柴油和汽油
diesel_df <- filter(file, `Engine Type` == "Diesel")
gasoline_df <- filter(file, `Engine Type` == "Gasoline")

ggplot(file, aes(x=as.factor(`Engine Type`), y=Miles)) +
  geom_boxplot() +
  ylim(10000, 12500) +
  xlab("Engine Type")

# 汽、柴油車車齡boxplot
ggplot(file, aes(x=as.factor(`Engine Type`), y=Age)) +
  geom_boxplot() +
  xlab("Engine Type")
# 汽、柴油車總里程數boxplot
ggplot(file, aes(x=as.factor(`Engine Type`), y=`Odometer Miles`)) +
  geom_boxplot() +
  xlab("Engine Type")

ggplot(file, aes(x=as.factor(`Engine Type`), y=`Maintenance cost`)) +
  geom_boxplot() +
  xlab("Engine Type")

ggplot(file, aes(x=as.factor(`Engine Type`), y=`Maintenance cost`)) +
  geom_violin() +
  xlab("Engine Type")


ggplot(file, aes(x=as.factor(Capacity), y=`Maintenance cost`)) +
  geom_boxplot() +
  xlab("Capacity")

# 各廠牌保養費用比較箱形圖
ggplot(file, aes(x=as.factor(Manufacturer), y=`Maintenance cost`)) +
  geom_boxplot() +
  xlab("Manufacturer")

# ggplot(data = diesel_df, aes(y = Miles, x = seq_along(Miles))) +
#   geom_point() +
#   ylim(5000, 15000) +
#   labs(x = "")
# ggplot(data = gasoline_df, aes(y = Miles, x = seq_along(Miles))) +
#   geom_point() +
#   ylim(5000, 15000) +
#   labs(x= "")

# 切分各品牌
bluebird_df <- filter(file, Manufacturer == "Bluebird")
keiser_df <- filter(file, Manufacturer == "Keiser")
thompson <- filter(file, Manufacturer == "Thompson")

# age & maintenance cost
diesel_age_cost_df <- select(diesel_df, Age, `Maintenance cost`)
gas_age_cost_df <- select(gasoline_df, Age, `Maintenance cost`)

p1 <- ggplot(diesel_age_cost_df, aes(x=Age, y=`Maintenance cost`)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F, color = 'red', formula = 'y ~ x') +
  theme_bw() +
  # 只顯示相關系數，不顯示p-value
  stat_cor(aes(label = ..r.label..), data=diesel_age_cost_df, method = "spearman") +
  labs(x = "Age", y = "Maintenance cost") +
  ggtitle("Diesel")

p2 <- ggplot(gas_age_cost_df, aes(x=Age, y=`Maintenance cost`)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F, color = 'red', formula = 'y ~ x') +
  theme_bw() +
  # 只顯示相關系數，不顯示p-value
  stat_cor(aes(label = ..r.label..), data=gas_age_cost_df, method = "spearman") +
  labs(x = "Age", y = "Maintenance cost") +
  ggtitle("Gasoline")

grid.arrange(p1,p2,nrow=2)

diesel_mile_cost_df <- select(diesel_df, `Odometer Miles`, `Maintenance cost`)
gas_mile_cost_df <- select(gasoline_df, `Odometer Miles`, `Maintenance cost`)

p3 <- ggplot(diesel_mile_cost_df, aes(x=`Odometer Miles`, y=`Maintenance cost`)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F, color = 'red', formula = 'y ~ x') +
  theme_bw() +
  # 只顯示相關系數，不顯示p-value
  stat_cor(aes(label = ..r.label..), data=diesel_mile_cost_df, method = "spearman") +
  labs(x = "Odometer Miles", y = "Maintenance cost") +
  ggtitle("Diesel")

p4 <- ggplot(gas_mile_cost_df, aes(x=`Odometer Miles`, y=`Maintenance cost`)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F, color = 'red', formula = 'y ~ x') +
  theme_bw() +
  # 只顯示相關系數，不顯示p-value
  stat_cor(aes(label = ..r.label..), data=gas_mile_cost_df, method = "spearman") +
  labs(x = "Odometer Miles", y = "Maintenance cost") +
  ggtitle("Gasoline")

grid.arrange(p3,p4,nrow=2)

p5 <- ggplot(diesel_df, aes(x=Miles, y=`Maintenance cost`)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F, color = 'red', formula = 'y ~ x') +
  theme_bw() +
  # 只顯示相關系數，不顯示p-value
  stat_cor(aes(label = ..r.label..), data=diesel_df, method = "spearman") +
  labs(x = "Miles", y = "Maintenance cost") +
  ggtitle("Diesel")

p6 <- ggplot(gasoline_df, aes(x=Miles, y=`Maintenance cost`)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F, color = 'red', formula = 'y ~ x') +
  theme_bw() +
  # 只顯示相關系數，不顯示p-value
  stat_cor(aes(label = ..r.label..), data=gasoline_df, method = "spearman") +
  labs(x = "Miles", y = "Maintenance cost") +
  ggtitle("Gasoline")

p7 <- ggplot(diesel_df, aes(x=Capacity, y=`Maintenance cost`)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F, color = 'red', formula = 'y ~ x') +
  theme_bw() +
  # 只顯示相關系數，不顯示p-value
  stat_cor(aes(label = ..r.label..), data=diesel_df, method = "spearman") +
  labs(x = "Capacity", y = "Maintenance cost") +
  ggtitle("Diesel")

p8 <- ggplot(gasoline_df, aes(x=Capacity, y=`Maintenance cost`)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F, color = 'red', formula = 'y ~ x') +
  theme_bw() +
  # 只顯示相關系數，不顯示p-value
  stat_cor(aes(label = ..r.label..), data=gasoline_df, method = "spearman") +
  labs(x = "Capacity", y = "Maintenance cost") +
  ggtitle("Gasoline")


# 柴油車和年齡
diesel_age_cost_df <- diesel_age_cost_df %>%
  group_by(`Age`) %>%
  summarise(count = n(),
            cost_mean = mean(`Maintenance cost`),
            cost_sd = sd(`Maintenance cost`),
            cost_min = min(`Maintenance cost`),
            cost_max = max(`Maintenance cost`))
# View(diesel_age_cost_df)

# 汽油車和年齡
gas_age_cost_df <- gas_age_cost_df %>%
  group_by(`Age`) %>%
  summarise(count = n(),
            cost_mean = mean(`Maintenance cost`),
            cost_sd = sd(`Maintenance cost`),
            cost_min = min(`Maintenance cost`),
            cost_max = max(`Maintenance cost`))

ggplot(file, aes(x=as.factor(`Engine Type`), y=Capacity)) +
  geom_boxplot() +
  xlab("Engine Type")

# 不同廠牌車輛的保養費用比較
ggplot(file, aes(x=as.factor(Manufacturer), y=`Maintenance cost`)) +
  geom_boxplot() +
  xlab("Manufacturer")

ggplot(file, aes(x=as.factor(Manufacturer), y=`Maintenance cost`)) +
  geom_violin() +
  xlab("Manufacturer")
## 不同廠牌車輛的車齡比較
ggplot(file, aes(x=as.factor(Manufacturer), y=`Age`)) +
  geom_boxplot() +
  xlab("Manufacturer")
## 不同廠牌車輛的總里程數比較
ggplot(file, aes(x=as.factor(Manufacturer), y=`Odometer Miles`)) +
  geom_boxplot() +
  xlab("Manufacturer")

####

hist(diesel_df$Capacity)
hist(gasoline_df$Capacity)

# 汽柴油車的保養費用比較histogram
# 汽、柴油車的數量不一樣，不用histogram!!!!!!!!!!!!!!!
hist(diesel_df$`Maintenance cost`, col=rgb(0,0,1,1/4), breaks = seq(from = 0, to = 13000, by=1000))
hist(gasoline_df$`Maintenance cost`, col=rgb(1,0,0,1/4), add = TRUE, breaks = seq(from = 0, to = 13000, by=1000))

# 車齡和總里程數的關係
ggplot(file, aes(x=Age, y=`Odometer Miles`)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F, color = 'red', formula = 'y ~ x') +
  theme_bw() +
  # 只顯示相關系數，不顯示p-value
  stat_cor(aes(label = ..r.label..), data=file, method = "spearman") +
  labs(x = "Age", y = "Odometer")

# Capacity & Maintainence Cost
ggplot(diesel_df, aes(x=Capacity, y=`Maintenance cost`)) +
  geom_point() +
  labs(x = "Capacity", y = "Maintenance cost") +
  ggtitle("Diesel")
ggplot(gasoline_df, aes(x=Capacity, y=`Maintenance cost`)) +
  geom_point() +
  labs(x = "Capacity", y = "Maintenance cost") +
  ggtitle("Gasoline")

# Cost / Odometer Miles
file <- mutate(file, `Cost per Miles` = `Maintenance cost`/`Odometer Miles`)
ggplot(file, aes(x=as.factor(Manufacturer), y=`Cost per Miles`)) +
  geom_boxplot() +
  xlab("Manufacturer")
# Cost / Age
file <- mutate(file, `Cost per Age` = `Maintenance cost`/Age)
ggplot(file, aes(x=as.factor(Manufacturer), y=`Cost per Age`)) +
  geom_boxplot() +
  xlab("Manufacturer")
