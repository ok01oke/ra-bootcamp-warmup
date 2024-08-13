#---一番最初の準備---#
getwd()
# 現在の作業ディレクトリ内のフォルダ一覧を取得
folders <- list.dirs(path = ".", full.names = TRUE, recursive = FALSE)

# フォルダ一覧を表示
print(folders)
setwd("./Desktop")
setwd("./ra-bootcamp-warmup")
getwd()

current_directory <- getwd()
# サブディレクトリも含めた全ファイル一覧を取得
files <- list.files(path = current_directory, full.names = TRUE, recursive = TRUE)

# ファイル一覧を表示
print(files)
# 必要なパッケージを読み込み

library(tidyverse)
library(readxl)

#---(a) Semester Dataの整形---#
# CSVファイルのパスを指定
file1 <- "/Users/okemotoaika/Desktop/ra-bootcamp-warmup/warmup training package/01_data/raw/semester_dummy/semester_data_1.csv"
file2 <- "/Users/okemotoaika/Desktop/ra-bootcamp-warmup/warmup training package/01_data/raw/semester_dummy/semester_data_2.csv"

# データを読み込む
data1 <- read_csv(file1)  # 1行目が列名として自動的に読み込まれる
data2 <- read_csv(file2)

# データの構造を確認
str(data1)
str(data2)

# データの最初の数行を表示
head(data1)
head(data2)

colnames(data1) <- as.character(unlist(data1[1, ]))
colnames(data2) <- as.character(unlist(data1[1, ]))
data1 <- data1[-1,]


data1 <- data1 %>%
  mutate(
    unitid = as.numeric(unitid),  
    semester = as.numeric(semester),
    quarter = as.numeric(quarter),
    year = as.numeric(year),
    Y = as.numeric(Y)
  )
data2 <- data2 %>%
  mutate(
    unitid = as.numeric(unitid),  
    semester = as.numeric(semester),
    quarter = as.numeric(quarter),
    year = as.numeric(year),
    Y = as.numeric(Y)
  )

# データを結合
combined_data <- bind_rows(data1, data2)

combined_data <- combined_data %>%
  select(-Y)

# semesterが0から1に変わった年を抽出
change_years <- combined_data |>
  group_by(unitid) |>
  filter(lag(semester) == 0 & semester == 1) |>
  summarise(yearofsem = first(year))

combined_data <- combined_data %>%
  left_join(change_years, by = "unitid")

combined_data <- combined_data |>
  mutate(after = ifelse(year >= yearofsem, 1, 0))

#---(b) Gradrate Dataの整形---
file1991 <- "/Users/okemotoaika/Desktop/ra-bootcamp-warmup/warmup training package/01_data/raw/outcome/1991.xlsx"
grad1991<- read_excel(file1991)
colnames(grad1991) <- as.character(unlist(grad1991[1, ]))
grad1991 <- grad1991[-1,]
grad_combined_data <- bind_rows(grad_combined_data, grad1991)


# 1991年から2016年までのファイル名を作成
years <- years <- c(1991:1993, 1995:2016)
file_names <- paste0(file_path, years, ".xlsx")

# ファイルを読み込み、結合
grad_combined_data <- map(file_names, ~ {
  # ファイルの読み込み
  df <- read_excel(.x)
  
  return(df)
}) %>%
  bind_rows()

#女子学生の４年卒業率を０から１のスケールに変更
grad_combined_data <- grad_combined_data |> 
  mutate(womengradrate4yr = women_gradrate_4yr / 100)
#全学生の４年卒業率を計算
grad_combined_data <- grad_combined_data |> 
  mutate(
    totcohortsize = as.numeric(totcohortsize),
    gradrate4yr = tot4yrgrads / totcohortsize
  )
#男子学生の４年卒業率を０から１のスケールに変更
grad_combined_data <- grad_combined_data |>
  mutate(
    m_4yrgrads = as.numeric(m_4yrgrads),
    mengradrate4yr = m_4yrgrads / m_cohortsize
  )

#To round to the third decimal place
grad_combined_data <- grad_combined_data |>
  mutate(
    gradrate4yr = round(gradrate4yr, 3),
    mengradrate4yr = round(mengradrate4yr, 3)
  )