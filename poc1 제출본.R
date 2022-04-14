library(data.table)
library(tidyverse)
library(tictoc)
library(lubridate)
library(tidytext)
library(tm)
library(RVerbalExpressions)
library(widyr)
library(igraph)
library(ggraph)
library(tidygraph)
library(topicmodels)
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_111")
library(KoNLP)
library(network)





#1월 1일부터 6월 5일
rm(list = ls())


#매체별 키워드 점유율분포 및 네트워크 분석
#인터넷 매체별 수집키워드마다 연결성 높은 키워드 도출
df<- fread(".//소셜데이터2021.csv")

#10883759 -> 2904898
df <- df %>% group_by(출처정보) %>% distinct(검색결과제목, .keep_all = TRUE) 


set.seed(23)
#2904898 매체별 중복하여 100000건씩 뽑은 작업. 100000만건 이하인 항목들은 그대로 추출 나머지는 랜덤하게 랜덤 언더 샘플링.
sample_df <- df %>% group_by(출처정보) %>% slice_sample(n = 100000)
sample_df<- sample_df %>% distinct(검색결과제목, .keep_all = TRUE)
sample_df %>% count(출처정보)

write.csv(sample_df %>% distinct(검색결과제목, .keep_all = TRUE) %>% count(출처정보), "sampletrenddata.csv", row.names = F)
span_rx <- rx() %>%
  rx_find("<") %>%
  rx_anything_but(">") %>%
  rx_find(">")

#특수문자 1차제거(괄호, $, , + 주석, 대괄호, ., 콜론, & )
df <- df %>% mutate(검색결과제목 = str_remove_all(검색결과제목, "\\(|\\$|\\,|\\)|\\||\\/|\\+|\\「|\\」|\\[|\\]|\\.|\\:|\\;|\\&"))

#특수문자 2차제거(html 태그, 유니코드, 하이픈 등)
df$검색결과제목 <- df$검색결과제목 %>%
  str_remove_all(span_rx) %>% # remove html tags
  str_remove_all("\\\\xe2\\\\x80\\\\x94") %>% # remove unicode
  str_replace_all("&#32", " ") %>% # replace html space with space
  str_replace_all("-", " ") %>% # remove hyphens (pros and cons)
  str_remove_all("[^[:alnum:][:space:].]") %>% # remove punctuation except "."
  str_remove("This work is in the.*") 


#토큰화
df_token<- df %>% ungroup() %>%
  select(연계순번, 수집일, 출처정보, 검색어명, 검색결과제목) %>%
  unnest_tokens(pos, 검색결과제목, token = SimplePos09) %>%
  filter(str_detect(pos, '/n'))


#명사태그 추출
df_token <- df_token %>% mutate(pos_done = str_match(pos, '([[:alnum:]]+)/n')[,2])
df_token$pos_done <- str_remove_all(df_token$pos_done, "[[::A-z::]]") #알파벳 삭제
df_token <- df_token %>% filter(str_length(pos_done)>=2)
df_token$pos_done<- str_trim(df_token$pos_done, side = "both") #white space remove
df_token<- df_token %>% filter(str_detect(pos_done, "^\\d+\\d$") == F)


df_token  %>% count(출처정보, pos_done, sort = T) %>% 
  group_by(출처정보) %>% slice_max(n, n = 10, with_ties = FALSE)  %>% ungroup() %>% mutate(pos_done = reorder(pos_done, n)) %>% 
  ggplot(aes(y = pos_done, x = n)) +geom_col() +facet_wrap(~출처정보, scales = "free")


word_cors <- df_token %>% group_by(출처정보) %>%
  add_count(pos_done) %>% 
  filter(n >= 92) %>%  #글자길이 너무 긴거 빼기
  select(-검색어명, -수집일, -n) %>%
  group_modify(~pairwise_cor(.x, item = pos_done, feature = 연계순번, sort = T))


set.seed(23)
#구글
word_cors %>% ungroup() %>% filter(출처정보 == "구글/맞춤검색") %>% select(-출처정보) %>%
  filter(correlation > .2) %>%
  as_tbl_graph() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() + ggtitle("  구글_상관계수기준네트워크그래프") + theme(plot.title = element_text(size = 16, face = "bold"))

#네이버뉴스
word_cors %>% ungroup() %>% filter(출처정보 == "네이버/뉴스") %>% select(-출처정보) %>%
  filter(correlation > .2) %>%
  as_tbl_graph() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() + ggtitle("네이버뉴스_상관계수기준네트워크그래프") + theme(plot.title = element_text(size = 16, face = "bold"))

#네이버블로그
word_cors %>% ungroup() %>% filter(출처정보 == "네이버/블로그") %>% select(-출처정보) %>%
  filter(correlation > .2) %>%
  as_tbl_graph() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() + ggtitle("네이버블로그_상관계수기준네트워크그래프") + theme(plot.title = element_text(size = 16, face = "bold"))

#네이버웹문서
word_cors %>% ungroup() %>% filter(출처정보 == "네이버/웹문서") %>% select(-출처정보) %>%
  filter(correlation > .2) %>%
  as_tbl_graph() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() + ggtitle("네이버웹문서_상관계수기준네트워크그래프") + theme(plot.title = element_text(size = 16, face = "bold"))

#네이버카페
word_cors %>% ungroup() %>% filter(출처정보 == "네이버/카페") %>% select(-출처정보) %>%
  filter(correlation > .2) %>%
  as_tbl_graph() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() + ggtitle("네이버카페_상관계수기준네트워크그래프") + theme(plot.title = element_text(size = 16, face = "bold"))

#다음블로그
word_cors %>% ungroup() %>% filter(출처정보 == "다음/블로그") %>% select(-출처정보) %>%
  filter(correlation > .2) %>%
  as_tbl_graph() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() + ggtitle("다음블로그_상관계수기준네트워크그래프") + theme(plot.title = element_text(size = 16, face = "bold"))

#다음웹문서
word_cors %>% ungroup() %>% filter(출처정보 == "다음/웹문서") %>% select(-출처정보) %>%
  filter(correlation > .2) %>%
  as_tbl_graph() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() + ggtitle("다음웹문서_상관계수기준네트워크그래프") + theme(plot.title = element_text(size = 16, face = "bold"))

#다음카페

word_cors %>% ungroup() %>% filter(출처정보 == "다음/카페") %>% select(-출처정보) %>%
  filter(correlation > .2) %>%
  as_tbl_graph() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() + ggtitle("다음카페_상관계수기준네트워크그래프") + theme(plot.title = element_text(size = 16, face = "bold"))


#lda 군집분석
lda_dtm<- df_token %>% filter(str_length(pos_done) <10) %>%
  group_by(pos_done) %>%
  mutate(word_total = n()) %>% 
  ungroup() %>%
  filter(word_total > 50) %>% unite(document, 검색어명, 출처정보) %>%
  count(document, pos_done) %>%
  cast_dtm(document, pos_done, n)

lda_res <- LDA(lda_dtm, k = 4, control = list(seed = 23))


tidy(lda_res, matrix = "gamma") %>% arrange(desc(gamma)) %>% write.csv("문서감마값.csv", row.names = F)