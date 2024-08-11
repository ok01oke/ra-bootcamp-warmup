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


library(readr)
library(dplyr)

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

data1 <- data1 %>%
  mutate(
    x1 = as.numeric(x1),  # 変換する必要がある場合
    x3 = as.numeric(x3),
    x4 = as.numeric(x4),
    x5 = as.numeric(x5),
    x6 = as.numeric(x6)
  )
data2 <- data2 %>%
  mutate(
    x1 = as.numeric(x1),
    x3 = as.numeric(x3),
    x4 = as.numeric(x4),
    x5 = as.numeric(x5),
    x6 = as.numeric(x6)
  )
# 列名を変更して一致させる
data1 <- data1 %>%
  rename(
    id = x1,
    institution_name = x2,
    semester = x3,
    quarter = x4,
    year = x5,
    value = x6
  )

data2 <- data2 %>%
  rename(
    id = x1,
    institution_name = x2,
    semester = x3,
    quarter = x4,
    year = x5,
    value = x6
  )
# データを結合
combined_data <- bind_rows(data1, data2)
combined_data <- combined_data[-1, ]

