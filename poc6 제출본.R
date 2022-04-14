library(tidyverse)
library(data.table)
library(readxl)
library(tidytext)
library(tm)
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_111")
library(KoNLP)

검사항목_raw<- read_csv(".//Poc6/검사항목_원본/검사항목4.csv",locale=locale('ko',encoding='cp949'))
검사항목_raw %>% head()

결과표맵핑_raw <- read_excel(".//Poc6/검사결과표_맵핑/결과표 맵핑.xlsx")
검사항목 <-검사항목_raw
검사항목 <- 검사항목 %>% select('INSP_DIV', 'APP_CONTENT', 'ITEM_NO', 'LINE_NO', 'ROW_NO', 'COLUMN_NO', 'TBL_INOUT', 'IDX_TYPE', 'INSP_STRD')

#검사설치검사 INSP_DIV 2에 해당하는 항목만 추출. 검사항목 <- 검사항목 %>% filter(INSP_DIV == 2) 수행 x
#비고, 약칭, TBL_INOUT O를 제거

검사항목 <- 검사항목 %>% filter(!str_detect(INSP_STRD, '약칭'))
검사항목 <- 검사항목 %>% filter(!str_detect(INSP_STRD, '비고'))
검사항목 <- 검사항목 %>% filter(TBL_INOUT != "O")

검사항목$`1차항목` <- 검사항목$INSP_STRD
검사항목$`2차항목` <- 검사항목$INSP_STRD


#상위항목에 가나다 하위항목에 123을 제거하려는 의도로 추측.
검사항목$`1차항목`<- ifelse(검사항목$IDX_TYPE %in% c('가)', '나)', '다)', '라)', '마)', '바)'), "None", 검사항목$`1차항목`)
검사항목$`2차항목`<- ifelse(검사항목$IDX_TYPE %in% c('1)', '1) ', '10)', '11)', '12)', '2)', '2) ', '3)', '3) ', '4)', '5)', '6)', '7)', '8)', '9)'), 'None', 검사항목$`2차항목`)

검사항목<- 검사항목 %>% mutate(IND = paste0(ITEM_NO, "_", LINE_NO))

검사항목 <- 검사항목 %>%
  group_by(IND) %>%
  mutate(`2차항목` = lead(`2차항목`, order_by=IND)) %>% ungroup()

검사항목$검사항목 <- paste0(검사항목$`1차항목`,"-", 검사항목$`2차항목`)
검사항목$검사항목 <- str_replace(검사항목$검사항목, "-NA", "") 
검사항목$`2차항목YN` <- if_else(is.na(검사항목$`2차항목`), "N", "Y")

검사항목 <- 검사항목 %>% select('INSP_DIV','APP_CONTENT','ITEM_NO', 'LINE_NO', 'ROW_NO', 'COLUMN_NO','1차항목','2차항목','2차항목YN','검사항목')

#장외영향 제거
검사항목 <- 검사항목 %>% filter(!str_detect(검사항목, '장외영향'))
검사항목 <- 검사항목 %>% mutate(검사항목 = str_replace_all(검사항목, "<br/>", ""))

#정밀필터
검사항목$정밀필터 <- 검사항목$검사항목;

검사항목$정밀필터<- ifelse(str_detect(검사항목$정밀필터, "None"), lag(검사항목$`2차항목`), 검사항목$정밀필터)

검사항목<- 검사항목 %>% mutate(정밀필터 = str_remove_all(정밀필터, '\\.|\\ㆍ|\\·|\\「|\\')) 
검사항목<- 검사항목 %>% mutate(정밀필터 = str_remove_all(정밀필터, '유해화학물질취급시설|제조사용시설|실내저장시설|실외저장시설|실외보관시설|사외보관')) 
검사항목<- 검사항목 %>% mutate(정밀필터 = str_remove_all(정밀필터, '운반차량|운송차량|저장시설|보관시설|제조시설|저장설비'))
검사항목<- 검사항목 %>% mutate(정밀필터 = str_remove_all(정밀필터, '유해화학물질|다른법령에따라실시한검사결과합격한경우')) 
검사항목<- 검사항목 %>% mutate(정밀필터 = str_remove_all(정밀필터, '\\(|\\|\\,|\\,|\\)')) 
검사항목<- 검사항목 %>% mutate(정밀필터 = str_remove_all(정밀필터, '다만2014년12월31일이전에\\\\w\\*')) 
검사항목$ITEM_NO <- as.character(검사항목$ITEM_NO)

검사항목<- semi_join(검사항목, 결과표맵핑_raw, by = c("ITEM_NO" = "KEY_ITEM_NO"))

검사항목<- 검사항목 %>% mutate(key = paste0('ITEM_NO','LINE_NO','ROW_NO','COLUMN_NO'))
검사항목<- 검사항목 %>% select('INSP_DIV', 'APP_CONTENT', 'ITEM_NO', 'LINE_NO', 'ROW_NO', 'COLUMN_NO', '1차항목','2차항목','검사항목','정밀필터')

temp<- 검사항목 %>%
  unnest_tokens(명사추출, 정밀필터, token = SimplePos09) %>%
  filter(str_detect(명사추출, '/n'))

temp <- temp %>% mutate(명사추출 = str_remove(명사추출, "/.*$"))
temp %>% head()

temp$LINE_NO <- as.character(temp$LINE_NO)
temp %>% head()
temp %>% unite(NO, APP_CONTENT, LINE_NO) %>% View()
tf_idfv <- temp %>% filter(str_length(명사추출)>=2) %>% unite(NO, APP_CONTENT, LINE_NO) %>% count(NO, 명사추출) %>%
  bind_tf_idf(명사추출, NO, n) %>%
  arrange(desc(tf_idf))

data_dtm <- temp %>% filter(str_length(명사추출)>=2) %>% unite(NO, APP_CONTENT, LINE_NO) %>% count(NO, 명사추출) %>%
  cast_dtm(term = 명사추출, document = NO, value = n)

write.csv(tf_idfv, "tfidf-tidy.csv", row.names = F)

data_dtm_tfidf <- temp %>% filter(str_length(명사추출)>=2) %>% unite(NO, APP_CONTENT, LINE_NO) %>% count(NO, 명사추출) %>%
  cast_dtm(term = 명사추출, document = NO, value = n, weighting = tm::weightTfIdf)
data_matrix <- as.matrix(data_dtm_tfidf)


write.csv(data_matrix, "tfidf_dtm.csv")

#코사인 유사도
library(proxy)
cosine_dist_doc <- as.matrix(dist(as.matrix(data_dtm), method = "cosine"))

n <- ncol(cosine_dist_doc) # number of columns
col_nm <- colnames(cosine_dist_doc) # column names
r <- 1 # row position number
cosine_dist_long <- data.frame() # blank data.frame to store the results

for (i in 1:(n-1)) {
  for (j in (i+1):n){
    cosine_dist_long[r, 1] <- col_nm[i]
    cosine_dist_long[r, 2] <- col_nm[j]
    cosine_dist_long[r, 3] <- cosine_dist_doc[i, j]
    r <- r+1
  }
}

write.csv(cosine_dist_long %>% arrange(V3), "문서별코사인유사도.csv", row.names = F)

#0일수록 유사함.
cosine_dist_long %>% arrange(V3) %>% View()


library(fpc) #dbscan
set.seed(23)
basic_md<- fpc::dbscan(data_matrix, eps = 0.5, MinPts = 5)
basic_md$cluster

library(dbscan)
set.seed(23)

dbscan::kNNdistplot(data_matrix, k=11) #k에 대한 가정들 : 실내저장시설, 실외저장시설, 실내보관시설, 실외보관시설, 사외배관, 운반차량, 운송차량, 저장시설(지하), 보관시설, 제조시설(제조사용), 저장설비
dbscan::kNNdistplot(data_matrix, k=5) #줄여보면 : 저장, 보관, 배관, 차량, 제조시설로 구분했을 때. #큰 차이가 있지는 않음.

#1로 두었을 때 이상값이 너무 많음. 실제로 이상값이 많은 것은 맞지만, 이상값을 줄여주기 위해 두번째 엘보우 포인트인 2값으로 두고 계산하면 다음과 같이 나타남.
set.seed(23)
update_md<- fpc::dbscan(data_matrix, eps = 2, MinPts = 5)
table(basic_md$cluster)
table(update_md$cluster)

basic_md
res <- bind_cols(문서 = row.names(data_matrix), tfidf_기본 = basic_md$cluster, tfidf_tune = update_md$cluster)

data_matrix_basic <- as.matrix(data_dtm)

set.seed(23)
basic_basic_md<- fpc::dbscan(data_matrix_basic, eps = 0.5, MinPts = 5)
basic_basic_md

set.seed(23)

fviz_cluster(basic_basic_md, data_matrix_basic, geom= "point") 
dbscan::kNNdistplot(data_matrix_basic, k=11)
set.seed(23)
dbscan::kNNdistplot(data_matrix_basic, k=5)

set.seed(23)
basic_final_md<- fpc::dbscan(data_matrix_basic, eps = 2, MinPts = 5)
fviz_cluster(basic_basic_md, data_matrix_basic, geom= "point", ellipse.alpha = .2) 

basic_final_md
res_2 <- bind_cols(문서 = row.names(data_matrix_basic), dtm_기본 = basic_basic_md$cluster, dtm_tune = basic_final_md$cluster)
res_2

cluster_res <- inner_join(res, res_2, by = "문서")
cluster_res$동일군집존재여부<- ifelse(cluster_res$dtm_기본 + cluster_res$dtm_tune + cluster_res$tfidf_기본 +cluster_res$tfidf_tune == 0,
                              "N", "Y")
cluster_res %>% filter(동일군집존재여부 == "N")


write.csv(cluster_res, "군집존재여부.csv", row.names = F)
