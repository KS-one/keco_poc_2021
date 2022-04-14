
library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(tmap)
library(tidymodels)
#library(Rfast)
library(geosphere)
library(RANN)
library(ggpubr)

대기오염데이터<- read.csv(".//대기오염데이터.csv")
측정소데이터<- read.csv("대기오염측정소지오코딩전체.csv")

대기오염데이터$측정일시 <- ymd_hms(대기오염데이터$측정일시);
대기오염데이터<- 대기오염데이터 %>% filter(grepl("서울", 지역))

측정소데이터$구분<- if_else((grepl("구", 측정소데이터$측정소명)), "도시대기측정망", "도로변측정망")
대기오염데이터$구분<- if_else((grepl("구", 대기오염데이터$측정소명)), "도시대기측정망", "도로변측정망")

대기오염데이터$구분<- as.factor(대기오염데이터$구분)


summary(aov(PM10 ~ 구분, 대기오염데이터)) #차이가 있음
bartlett.test(PM10 ~ 구분, 대기오염데이터) # 정규성 만족하는 등분산 가정이 만족하지 못함. 아래 lavene 검정함.
var.test(PM10 ~ 구분, 대기오염데이터) #분산비검정

summary(aov(PM10 ~ 측정소명, 대기오염데이터)) #차이가 있음
bartlett.test(PM10 ~ 측정소명, 대기오염데이터) #등분산 가정이 만족하지 못함.

oneway.test(PM10 ~ 측정소명, 대기오염데이터, var.equal = F) #등분산 만족하지 못할때 하는 검정. 차이 있음.

library(lawstat) #welchs anova
levene.test(대기오염데이터$PM10, 대기오염데이터$구분) #구분은 등분산성 가정을 만족함. 측정소명은 만족하지 않았기 때문에 welchs anova 실시
levene.test(대기오염데이터$PM10, 대기오염데이터$구분, location = "mean")

levene.test(대기오염데이터$PM10, 대기오염데이터$측정소명) 
levene.test(대기오염데이터$PM10, 대기오염데이터$측정소명, location = "mean")

levene.test(log1p(대기오염데이터$PM10), 대기오염데이터$측정소명)
levene.test(log1p(대기오염데이터$PM10), 대기오염데이터$측정소명, location = "mean")

levene.test(대기오염데이터$PM25, 대기오염데이터$구분) #등분산 가정 만족하지 못함.
levene.test(대기오염데이터$PM25, 대기오염데이터$구분, location = "mean")

library(rstatix) #Games-Howell 검정
대기오염데이터 %>% group_by(구분) %>% games_howell_test(PM10~측정소명, conf.level = 0.95) %>% head()
대기오염데이터 %>% group_by(구분) %>% games_howell_test(PM10~측정소명, conf.level = 0.95) %>% filter(p.adj.signif == "ns")
대기오염데이터 %>% games_howell_test(PM10~측정소명, conf.level = 0.95) %>% filter(p.adj.signif == "ns") %>% View()

기준데이터 <- 대기오염데이터 %>% filter(측정소명 =="종로구"| 측정소명 =="중구" | 측정소명 == "용산구" | 
                                  측정소명 =="은평구"|측정소명 == "마포구"| 측정소명 == "성동구" | 측정소명 == "광진구"
                                | 측정소명 =="중랑구"  | 측정소명 == "성북구"| 측정소명 =="강북구" | 측정소명 == "노원구"
                                | 측정소명 =="양천구" | 측정소명 == "구로구" | 측정소명 =="금천구" | 측정소명 == "동작구"
                                | 측정소명 =="관악구" | 측정소명 =="서초구" | 측정소명 =="송파구"  | 측정소명 =="강동구")

대체데이터 <- 대기오염데이터 %>% filter(측정소명 =="동대문구"| 측정소명 =="종로구" | 측정소명 == "종로구" | 
                                  측정소명 =="서대문구"|측정소명 == "영등포구"| 측정소명 == "강남구" | 측정소명 == "성동구"
                                | 측정소명 =="광진구"  | 측정소명 == "동대문구"| 측정소명 =="도봉구" | 측정소명 == "도봉구"
                                | 측정소명 =="강서구" | 측정소명 == "영등포구" | 측정소명 =="관악구" | 측정소명 == "서초구"
                                | 측정소명 =="구로구" | 측정소명 =="동작구" | 측정소명 =="강남구"  | 측정소명 =="광진구")

기준데이터$대체측정소명 <- recode(기준데이터$측정소명,
                            "중구" = "종로구",
                            '용산구' = '종로구',
                            '은평구'	= '서대문구',
                            '마포구'	= '영등포구',
                            '성동구'	= '강남구',
                            '광진구'	= '성동구',
                            '종로구' = '동대문구',
                            '중랑구'	= '광진구',
                            '성북구'	= '동대문구',
                            '강북구'	= '도봉구',
                            '노원구'	= '도봉구',
                            '양천구'	= '강서구',
                            '구로구'	= '영등포구',
                            '금천구'	= '관악구',
                            '동작구'	= '서초구',
                            '관악구'	= '구로구',
                            '서초구'	= '동작구',　	　
                            '송파구'	= '강남구',
                            '강동구'	= '광진구'
                            
)


기준대체데이터<- left_join(기준데이터,대체데이터, by = c("대체측정소명" = "측정소명", "측정일시"))
기준대체데이터 %>% head()

기준대체데이터 %>% filter(!is.na(PM10.y)) %>% metrics(PM10.x, PM10.y)

원데이터<- 기준대체데이터 %>% select(1:12)
colnames(원데이터) <- colnames(대기오염데이터)

set.seed(23)



대체데이터_스플릿 <- initial_split(원데이터, prop = 0.80, strata = PM10)
대체데이터_훈련<- training(대체데이터_스플릿)
대체데이터_시험<- testing(대체데이터_스플릿)


breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
labels <- c("Night", "Morning", "Afternoon", "Evening")


대체데이터_훈련<- 대체데이터_훈련 %>% mutate(daynight=cut(x=hour(측정일시), breaks = breaks, labels = labels, include.lowest=TRUE),
                               시간 = hour(측정일시), 측정일시 = as.Date(측정일시))

대체데이터_시험<- 대체데이터_시험 %>% mutate(daynight=cut(x=hour(측정일시), breaks = breaks, labels = labels, include.lowest=TRUE),
                               시간 = hour(측정일시), 측정일시 = as.Date(측정일시))

#base model + feature engineering
대체_recipe<- 대체데이터_훈련 %>% recipe(PM10~.) %>%
  step_date(측정일시, features = c("dow", "month", "year", "semester")) %>%
  step_rm(지역,측정소명,주소, 구분, 측정일시) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_other(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_nzv(all_nominal_predictors())

대체_recipe %>% prep() %>% bake(new_data = 대체데이터_훈련)

# base line 
linear_reg_lm_spec <-
  linear_reg() %>%
  set_engine('lm')



boost_tree_xgboost_spec <-
  boost_tree() %>%
  set_engine('xgboost') %>%
  set_mode('regression')


rand_forest_ranger_spec <-
  rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine('ranger') %>%
  set_mode('regression')



lm_Wf <- workflow() %>% 
  add_model(linear_reg_lm_spec)



xgboost_Wf <- workflow() %>% 
  add_model(boost_tree_xgboost_spec)


rand_Wf <- workflow() %>% 
  add_model(rand_forest_ranger_spec)



lm_Wf <- lm_Wf %>% add_formula(PM10 ~ SO2+CO+O3+NO2+PM25)
lm_fit <- fit(lm_Wf, 대체데이터_훈련)
preds_lm<- predict(lm_fit, 대체데이터_시험)

lm_pred_df <- preds_lm %>%
  bind_cols(대체데이터_시험 %>% select(PM10))


lm_pred_df %>% metrics(PM10, .pred)

tidy(lm_fit)
glance(lm_fit)



xgboost_Wf <- xgboost_Wf %>% add_formula(PM10 ~ SO2+CO+O3+NO2+PM25)
xgboost_fit <- fit(xgboost_Wf, 대체데이터_훈련)
preds_xgboost <- predict(xgboost_fit, 대체데이터_시험)

xgboost_pred_df <- preds_xgboost %>%
  bind_cols(대체데이터_시험 %>% select(PM10))

xgboost_pred_df %>% metrics(PM10, .pred)


rand_Wf <- rand_Wf %>% add_formula(PM10 ~ SO2+CO+O3+NO2+PM25)
rand_fit <- fit(rand_Wf, 대체데이터_훈련)
preds_rand <- predict(rand_fit, 대체데이터_시험)

rand_pred_df <- preds_rand %>%
  bind_cols(대체데이터_시험 %>% select(PM10))


rand_pred_df %>% metrics(PM10, .pred)


write.csv(gdata::combine(lm = lm_pred_df %>% metrics(PM10, .pred),
                         rf =  rand_pred_df %>% metrics(PM10, .pred), xgb = xgboost_pred_df %>% metrics(PM10, .pred)
) %>% arrange(.metric), "baselinemodel.csv", row.names = F)

