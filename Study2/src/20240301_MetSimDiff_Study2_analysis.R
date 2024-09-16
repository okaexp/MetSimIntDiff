#最終更新：2024年4月21日 22時45分
# /Users/Ryunosuke/Dropbox/比喩関連/基盤数/適切さにおける顕著性_その2/code/実験1/調査結果/AptnessJudgementTask_analysis.Rを参考にした

library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(scales)

# #念の為anovakunも使う
# source("./anovakun_482.txt")

#ローデータの読み込み ----
raw_dat <- read.csv("../data/raw_dat/20240301_MetSimDiff_Study2_20240301_1436.csv", fileEncoding="shift-jis")

#必要データの読み込み ----
dat <- raw_dat %>%
  dplyr::slice(3:nrow(raw_dat)) %>%#データの読み込み
  dplyr::select(-P_ID) %>%
  dplyr::mutate(Sex = as.factor(D_Sex),
                Age = as.integer(D_Age),
                Academic = as.integer(D_AcademicDegree),
                ID = as.factor(ID)) %>%
  dplyr::select(-D_Sex, -D_Age, -D_AcademicDegree) %>%
  dplyr::mutate_at(.vars = vars(starts_with("Q")), .funs = as.integer) %>%#本番の回答はまとめて
  dplyr::mutate_at(.vars = vars(starts_with("P_")), .funs = as.integer)#練習の回答はまとめて

dat_age_all <- dat %>% 
  dplyr::select(Age)  %>% 
  summarise(m_age = mean(Age), sd_age = sd(Age), min_age = min(Age), max_age = max(Age))

dat_sex_all <- dat %>% 
  dplyr::select(Sex) %>% 
  group_by(Sex) %>% 
  summarise(count = n())

#確認質問に正しく回答している人だけを抽出 -> 9名ドロップ----
#P_Pra1_1(犬 - 吠える): 1
#P_Pra1_2(犬 - 忠実だ): 2
#P_Pra2_1(壁 - 高い): 1
#P_Pra2_2(壁 - 乗り越えられない):2
dat_valid_resp <- dat %>%
  dplyr::filter(P_Pra1_1 == 1 & P_Pra1_2 == 2 & P_Pra2_1 == 1 & P_Pra2_2 == 2)

#年齢と性別を確認するためだけのデータの作成 ----
dat_age <- dat_valid_resp %>% 
  dplyr::select(Age)  %>% 
  summarise(m_age = mean(Age), sd_age = sd(Age), min_age = min(Age), max_age = max(Age))

dat_sex <- dat_valid_resp %>% 
  dplyr::select(Sex) %>% 
  group_by(Sex) %>% 
  summarise(count = n())

#分析用のデータをロング型に整形 ----
dat_valid_long <- dat_valid_resp %>%
  dplyr::select(-Sex, -Age, -P_Pra1_1, -P_Pra1_2, -P_Pra2_1, -P_Pra2_2, -Academic, -F_Agre) %>%
  tidyr::gather(key = Questions, value = Value,
                -ID) %>% 
  tidyr::separate(col = Questions, into = c("QID", "Condition"), sep="_") %>%
  tidyr::separate(col = Condition, into = c("MetSim", "FeatID"), sep="P") %>%
  tidyr::drop_na()

######################
#                    #
# 結果をグラフにする #
#                    #
######################

#データの整形：棒グラフ用
#SDは参加者平均から算出
dat_participant_mean <- dat_valid_long %>%
  dplyr::group_by(ID, MetSim) %>%
  dplyr::summarise(Average = mean(Value)) %>%
  dplyr::mutate(MetSim = as.factor(MetSim))

#棒グラフを書く
dat_participant_graph <- dat_participant_mean %>%
  dplyr::group_by(MetSim) %>%
  dplyr::summarise(Mean = mean(Average), SD = sd(Average))

#Mr.Unadonを参考にしながら、棒グラフを書く
#https://mrunadon.github.io/ThesisPlot/
#quartz(type = "pdf", file = "../result/BarPlotConnotationRating_v1.1.pdf")
ggplot()+theme_set(theme_classic(base_size = 12,base_family="Hiragino Mincho Pro W3"))

g<-ggplot(dat_participant_graph, aes(y=Mean, x=MetSim)) #irisOderedのデータフレームを使う。y軸とｘ軸の変数を指定。
g <- g + geom_bar(position = "dodge", stat = "identity")#平均値を表したbarの準備
g <- g + geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = .2, position = position_dodge(.9))#標準誤差
g <- g + scale_y_continuous(name = "Mean connotativeness rating", breaks = seq(0, 5, length=6), limits = c(1, 5), oob=oob_keep)

#2019年6月8日 修正
g <- g + scale_x_discrete(name = "Type of features",
                          labels = c("M" = "Metaphor biased features", "S" = "Simile biased features"),
                          limits=c("M", "S"))

plot(g)
#dev.off()

##################
#                #
# 結果を解析する #
#                #
##################

#ランダム相関を計算しないモデル
#論文で報告するのはこっち
res_lmer_dat <- lmer(formula = Value ~ MetSim + (1 + MetSim|ID) + (1 + MetSim|ID),
                      data = dat_valid_long)
summary(res_lmer_dat)
