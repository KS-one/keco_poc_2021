
library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
#활동도, 배출데이터, 배출사업장을 사업장코드와 기간을 중심으로 묶음.
#대기오염물질 배출사업장이 21년 9월 27일기준.

배출<- read_csv(".//Poc4/수도권_배출데이터.csv")
활동<- read_csv(".//Poc4/수도권_활동도.csv")
배출사업장_위경도<- read_csv(".//Poc4/대기오염물질 배출사업장 위경도.csv")
배출사업장정보 <-read_excel(".//Poc4/대기오염물질 배출사업장.xlsx")
배출사업장_위경도 %>% head()
배출사업장정보 %>% head()



배출사업장정보<- 배출사업장정보 %>% filter(권역 == "수도권" | 권역 == "수도권(총량)")
배출사업정보


배출사업장정보<- rename(배출사업장정보, 
                        "사업장코드"= `사업장\r\nID`)

배출사업장<- inner_join(배출사업장정보, 배출사업장_위경도, by =c("사업장코드"))


#****사업장이 누락됨.
setdiff(배출사업장정보$사업장명, 배출사업장$사업장명.x)
setdiff(배출사업장정보$사업장명, 배출사업장$사업장명.x)
배출사업장정보 %>% filter(사업장코드 == 150837)



#대기오염 배출사업장장 업종 데이터에서 시설 분류나 주연료 등록울 둥록해둔 항목이 있는 데이터를 추출함.
배출<- 배출 %>% filter(시설명 %in% 배출사업장$`시설분류 1 (통계용)`)
활동도<- 활동 %>% filter(활동도 %in% 배출사업장$`시설분류 2 (통계용)`|활동도 %in% 배출사업장$`주 사용연료명(통계용)`)
names(배출) <- gsub("\\(|\\)|\\\n","",names(배출))

배출$기간<- ym(배출$기간)
활동도$년월<- ym(활동도$년월)

levels(as.factor(배출$항목명))
배출 %>% head()


배출<- inner_join(배출, 배출사업장 %>% distinct(사업장코드, .keep_all = T) %>% select(사업장코드, 행정구역, 시군구명), by = "사업장코드")
배출 %>% group_by(기간, 항목명) %>% summarise(배출량 = sum(`배출량kg`)) %>% ggplot(aes(기간, 배출량)) + geom_line(aes(colour = 항목명), stat = "identity", size = 1.5) + scale_x_date(date_labels = "%Y-%b") + geom_point(size = 2)
배출 %>% group_by(지역, 기간, 항목명) %>% summarise(배출량 = sum(`배출량kg`)) %>% ggplot(aes(기간, 배출량)) + geom_line(aes(colour = 항목명), stat = "identity") + scale_x_date(date_labels = "%Y-%b") + geom_point(size = 1.5) + facet_wrap(~지역)
배출 %>% group_by(시설명, 기간, 항목명) %>% summarise(배출량 = sum(`배출량kg`)) %>% ggplot(aes(기간, 배출량)) + 
  geom_line(aes(colour = 항목명), stat = "identity") + scale_x_date(date_labels = "%Y-%b") + geom_point(size = 1.5) + facet_wrap(~시설명) +ggtitle("시설별 배출량") +  theme(plot.title = element_text(size = 16))

배출 %>% group_by(기간) %>% summarise(초과건수합계 = sum(실초과건수, na.rm = T)) %>% ggplot(aes(기간, 초과건수합계)) + geom_line(size = 1.5) + scale_x_date(date_labels = "%Y-%b") + geom_point(size = 1.5) +ggtitle("수도권 실초과건수 합계") +  theme(plot.title = element_text(size = 16))
배출 %>% group_by(기간) %>% summarise(초과건수합계 = sum(실초과건수, na.rm = T)) %>% View()
배출 %>% group_by(기간, paste(행정구역, 시군구명)) %>% summarise(초과건수합계 = sum(실초과건수, na.rm = T)) 
배출 %>% ggplot(aes(실초과건수, paste(행정구역, 시군구명), fill = 시설명)) + geom_bar(stat = "identity", position = "stack") +ggtitle("수도권 지역별 실초과건수") + theme(plot.title = element_text(size = 16))
배출 %>% ggplot(aes(실초과건수, 사업장명, fill = paste(행정구역, 시군구명))) + geom_bar(stat = "identity", position = "stack") +ggtitle("수도권 지역별 실초과건수") + theme(plot.title = element_text(size = 16))
배출 %>% ggplot(aes(배출량kg, paste(행정구역, 시군구명), fill = 시설명)) + geom_bar(stat = "identity", position = "stack") +ggtitle("수도권 지역별 배출량") + theme(plot.title = element_text(size = 16))
배출 %>% ggplot(aes(실초과건수, 사업장명)) + geom_bar(stat = "identity", position = "stack") + scale_x_discrete() +ggtitle("수도권 사업장별 실초과건수") + theme(plot.title = element_text(size = 16))

배출 %>% distinct(사업장명, .keep_all = TRUE) %>% count(paste(행정구역, 시군구명), 시설명) %>% ggplot(aes(n, `paste(행정구역, 시군구명)`, fill = 시설명)) + geom_bar(stat = "identity", position = "stack") +ggtitle("수도권 사업장 분포") + theme(plot.title = element_text(size = 16))배출 %>% head()


배출 %>% ggplot(aes(배출량kg, 업종표준산업분류세세분류, fill = 항목명)) + geom_bar(stat = "identity", position = "stack") +ggtitle("업종별 배출량") + theme(plot.title = element_text(size = 16))
배출 %>% ggplot(aes(배출량kg, 업종표준산업분류소분류, fill = 항목명)) + geom_bar(stat = "identity", position = "stack") +ggtitle("업종별 배출량") + theme(plot.title = element_text(size = 16))


활동도 %>% count(활동도, UNIT)
활동_wide<- 활동도 %>% select(-UNIT, -STACK_CODE) %>% pivot_wider(names_from = 활동도, values_from = 양, values_fn = sum)
활동도이름 <- levels(as.factor(활동도$활동도))

data<- full_join(배출, 활동_wide, by = c("기간" = "년월", "사업장코드" = "사업장번호", "사업장명"))
data_2<- full_join(배출, 활동도, by = c("기간" = "년월", "사업장코드" = "사업장번호", "사업장명"))

#업종별 활동도 배출데이터 상관관계
data<- data %>% filter(!is.na(배출량kg))
data_2 %>% head()
library(ggpubr)
data %>% group_by(업종표준산업분류소분류, 항목명) %>% summarise(배출량 = mean(배출량kg)) %>% ggboxplot(x = "업종표준산업분류소분류", y = "배출량",
                                                                                   color = "업종표준산업분류소분류",
                                                                                   add = "jitter", shape = "업종표준산업분류소분류") + theme(axis.text.x = element_text(angle = 90))

?scale
data<- data %>% mutate_at(활동도이름, ~replace(., is.na(.), 0))

data_2<- data_2 %>% filter(!is.na(배출량kg))
data_2<- data_2 %>% filter(!is.na(활동도))
data_2 %>% group_by(업종표준산업분류소분류) %>% summarize(cor = cor(배출량kg, 양, use = "na.or.complete")) %>% write.csv("업종별상관관계분석결과.csv", row.names = F)


상관계수_tsp<- data_2 %>% filter(항목명 == "TSP")%>% group_by(업종표준산업분류소분류) %>% summarize(cor = cor(배출량kg, 양, use = "na.or.complete"))
상관계수_co<- data_2 %>% filter(항목명 == "CO")%>% group_by(업종표준산업분류소분류) %>% summarize(cor = cor(배출량kg, 양, use = "na.or.complete"))
상관계수_sox<- data_2 %>% filter(항목명 == "SOx")%>% group_by(업종표준산업분류소분류) %>% summarize(cor = cor(배출량kg, 양, use = "na.or.complete"))
상관계수_nox<- data_2 %>% filter(항목명 == "NOx")%>% group_by(업종표준산업분류소분류) %>% summarize(cor = cor(배출량kg, 양, use = "na.or.complete"))
상관계수_hcl<- data_2 %>% filter(항목명 == "HCl")%>% group_by(업종표준산업분류소분류) %>% summarize(cor = cor(배출량kg, 양, use = "na.or.complete"))



library(gdata)
cor_data<- gdata::combine(상관계수_tsp, 상관계수_co, 상관계수_sox, 상관계수_nox, 상관계수_hcl)
write.csv(cor_data, "상관res.csv", row.names = F)





