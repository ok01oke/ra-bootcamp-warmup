#---一番最初の準備---#
setwd("/Users/okemotoaika/Desktop/ra-bootcamp-warmup")
# サブディレクトリも含めた全ファイル一覧を取得
files <- list.files(path = current_directory, full.names = TRUE, recursive = TRUE)

# ファイル一覧を表示
print(files)
# 必要なパッケージを読み込み

library(tidyverse)
library(readxl)
library(psych)
library(dplyr)
library(ggplot2)

# CSVファイルのパスを指定
file_master <- "/Users/okemotoaika/Desktop/ra-bootcamp-warmup/cleaning/master.csv"

master <- read_csv(file_master)

# 各列に含まれるNAの数を数える
na_counts <- colSums(is.na(master))

# 結果を表示
print(na_counts)

#never switchersとswitchersで分類
master <- master |>
  mutate(swichers = ifelse(is.na(after), 0, 1))

#never switchersとswitcherでそれぞれの統計的特徴量を求める
describeBy(master, master$swichers)
describeBy(master)

summary(master)

#yearごとのaverage graduation rateを求める
average_graduation_rate <- master |>
  group_by(year) |>
  summarise(mean_graduation_rate = mean(gradrate4yr, na.rm = TRUE))

#グラフの描画
ggplot(average_graduation_rate, aes(x = year, y = mean_graduation_rate)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "Average Four-year Graduation Rate by Year",
    x = "Year",
    y = "Four-year Graduation Rate"
  ) +
  theme_minimal()

#yearごとのFraction of schools on semestersを求める
semesters_rate <- master |>
  group_by(year) |>
  summarise(mean_semester_rate = mean(semester, na.rm = TRUE))

#グラフの描画
ggplot(semesters_rate, aes(x = year, y = mean_semester_rate)) +
  geom_line(color = "black") +
  labs(
    title = "Fraction of schools on semesters",
    x = "Year",
    y = "Fraction of schools on semesters"
  ) +
  theme_minimal()

#散布図を作るコードの作成
# 散布図を作成する関数の定義
create_scatter_plot <- function(data, x_var, y_var) {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(color = "blue") + # 散布図のポイント
    labs(
      title = paste("Scatter Plot of", y_var, "vs", x_var),
      x = x_var,
      y = y_var
    ) +
    theme_minimal() # グラフのテーマ
}



# 変数名を指定して散布図を作成
create_scatter_plot(master, "women_ratio", "gradrate4yr")
create_scatter_plot(master, "white_ratio", "gradrate4yr")
create_scatter_plot(master, "costs", "gradrate4yr")

#回帰分析する
model <- lm(gradrate4yr ~ after, data = master)
#回帰分析結果の表示
summary(model)