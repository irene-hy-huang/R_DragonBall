####Day1 ####

setwd("C:/Users/2375/Desktop/R_DragonBall")
install.packages(c("tidyverse", "ploty", "zoo", "lubridate", "rmarkdown","data.table", "DT", "kableExtra"), dependencies = TRUE)

#load packages
library(DT)
library(zoo)
library(plotly)
library(lubridate)
library(rmarkdown)
library(data.table)
library(tidyverse)
library(kableExtra)
options(dplyr.print_max=1e9)

#Read and load data
train1<-read.csv("train.csv")  
train2<-fread("train.csv")

train0 <- read.csv("train.csv", stringsAsFactors = FALSE)
test0 <- read.csv("test.csv", stringsAsFactor = FALSE)

#用 dim() 看資料的行列數 (rows x columns)
dim(train0)
dim(test0)

#training 與 testing 資料差別在training data多了房價 (SalesPrice)，這是testing data在建模完畢後最終要去預測的欄位
colnames(train0) [!colnames(train0) %in% colnames(test0)] #在train0裡面，但不在test0裡面的有誰

#3-1 : Subsetting data-資料行列的選取 ####
# select ####
dat <- train0 %>% select(10:15) #選取欄位
dat <- train0 %>% select(MSZoning, Utilities, HouseStyle, Heating, YearBuilt, SalePrice) #根據名稱

#1. 欄位含某個字串:選取欄位名稱含有Lot，Bsmt開頭，或是以Condition結尾
dat <- train0 %>% select(contains("Lot"), starts_with("Bsmt"), ends_with("Condition"))
#2. 欄位符合某種pattern
dat <- train0 %>% select(matches("Yr|Year|year|yr")) #跟年份有關的欄位

dat <- train0 %>% select(-PoolArea, -Fence, -matches("Bsmt|Lot|Garage")) #PoolArea, Fence, 含有Bsmt或Lot或Garage的欄位都刪除

head(dat)

plot_ly(train0, x = ~SalePrice, type = "histogram") #房價簡單的視覺化

range(train0$SalePrice) #所有資料的房價範圍
dat <- train0 %>% filter(SalePrice >= 100000, SalePrice <= 300000) #選取後的房價範圍
range(dat$SalePrice)

scales::percent(nrow(dat) / nrow(train0)) #房價100000~300000佔所有資料的比例達84.3%
table(train0$SaleType) #SaleType當中WD類型最多

dat <- train0 %>% filter(SaleType == "WD") #選SaleTeyp=="WD"的資料(WD:Warranty Deed - Conventional)
table(dat$SaleType)
table(dat$YrSold, dat$SaleType) #售出年份與SaleType的數量

dat <- train0 %>% filter(YrSold < 2008, SaleType == "New") #選2008年以前售出，而且SaleType為New(剛蓋好就賣出)的資料
table(dat$YrSold, dat$SaleType)

dat <- train0 %>% slice(1000:1001) #取出Row位置第1000-1001筆資料
head(dat[, 1:3])

dat <- train0 %>% top_n(5, SalePrice) #選出售價前五名
head(dat %>% select(Id, Neighborhood, SalePrice) %>% arrange(-SalePrice)) #列出Id, 社區名稱並將售價由高至低排序


#3-2 : Grouping and summarizing ####
dat <- train0 %>% group_by(Neighborhood) %>% 
  summarise(low = min(SalePrice),
            high = max(SalePrice),
            average = mean(SalePrice),
            sd = sd(SalePrice)) %>% 
  arrange(-average)

dat <- train0 %>% group_by(Neighborhood, Street) %>%  #亦可增加 grouping 條件 (ex. street)
  summarise(low = min(SalePrice),
            high = max(SalePrice),
            average = mean(SalePrice),
            sd = sd(SalePrice)) %>% 
  arrange(-average)

#簡單地看一下房屋建造年份，外牆材質跟售價的關係
dat <- train0 %>% group_by(YearBuilt, MasVnrType) %>% 
  summarise(average = mean(SalePrice)) %>% 
  arrange(-average)

plot_ly(dat, x = ~YearBuilt, y = ~average, text = ~MasVnrType, type = "scatter",
        size = ~average, color = ~MasVnrType,
        sizes = c(10, 80),
        marker = list(opacity = 0.5, sizemode = "diameter")) %>% 
  layout(title =  "Estate Sale Price by Neighborhood",
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = TRUE
         )

dat <- train0 %>% select(BsmtQual) %>% rename(BsmtHght = BsmtQual) #更改現有欄位名稱 (注意：新名稱放前面)
head(dat)

#3-3 Mutating : 建立新欄位 ####
#關於 basement 的欄位實在有點多，可以做一些合併
colnames(train0 %>% select(contains("Bsmt")))
# case_when ####
#以BsmtFullBath, BsmtHalfBath為例，可以將兩個欄位合併成BsmtBath
Bsmt <- train0 %>% select(Id, matches("Bsmt.*Bath")) %>% 
  mutate(BsmtBath = case_when(BsmtFullBath > 0 | BsmtHalfBath > 0 ~ 1,
                              TRUE ~ 0))
head(Bsmt %>% arrange(-BsmtBath))

#根據房屋整體品質來分組，新增“平均房價”欄位
dat <- train0 %>% group_by(OverallQual) %>% 
  mutate(average_SalePrice = mean(SalePrice)) %>% 
  select(Id, OverallQual, SalePrice)
dat[1:10,]

#3-4 小練習 : 清理 Data description 檔案####
#dcr0 <- read.delim("data_description.txt")

dcr0 <- read.delim("data_description.txt", header = FALSE, stringsAsFactors = FALSE)
datatable(dcr0)

#清理txt檔 ####
#把 data description 做個簡單清理與 reformatting
dcr <- dcr0 %>% 
  #先切開欄位名稱跟該欄位的數值種類(by tab or space)，切完後取第一個位置有文字者
  mutate(feature = sapply(strsplit(V1, '\t | [[:space:]]'), "[", 1)) %>% 
  filter(!is.na(feature)) %>%  #為了下一步的fill, 先把空白的欄位用NA取代
  mutate_all(na_if, "") %>% 
  fill(feature, .direction = "down") %>%  #把feature這個欄位的NA用前一個非NA的值取代
  rename(value = V1, description = V2) %>% #更改較直覺的欄位名稱
  select(feature, value, description) #排序欄位


test <- letters[1:5]
test[1]
`[`(test, 1)

#利用DT, datatable 建立一個方便查詢的表格 (可輸入某欄位名稱，檢視所有數值代表的涵意)
datatable(dcr, options = list(pageLength = 20))

# Part 4 : 進階資料清理及轉換 ####
# 4-1 Reshaping
#BsmtFinType1
fintype1 <- train0 %>% 
  group_by(Id, BsmtFinType1) %>% summarise(count = n()) %>% #計算每個Id在某個BsmtFinType1出現的頻率
  spread(BsmtFinType1, count, fill = 0) #spread 根據BsmtFinType展開欄位
  #fill=0的意思是該Id沒有某種Type時，用0取代，預設值為NA
head(fintype1)

fintype2 <- train0 %>% 
  group_by(Id, BsmtFinType2) %>% summarise(count = n()) %>% 
  spread(BsmtFinType2, count, fill = 0)
head(fintype2)  

#合併以及加總
bsmtfintype <- 
  #bind_rows與rbind的不同是前者會自行比對相同的欄位去合併，後者必須欄位序相同才行
  bind_rows(fintype1, fintype2) %>% group_by(Id) %>% 
  summarise_all(funs(sum(., na.rm = TRUE))) %>% 
  rename_all(function(x) paste0("BsmtFinType_", x)) #標記"BsmtFinType"當作prefix
  
#併回train data
train <- train0 %>% 
  left_join(bsmtfintype, by = c("Id" = "BsmtFinType_Id"))

datatable(train %>% select(Id, contains("BsmtFinType")))



