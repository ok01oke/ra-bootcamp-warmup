# 作業ディレクトリを設定
setwd("/path/to/your/project")

# サブディレクトリを含めたファイルのリストを取得
file_list <- list.files(path = ".", recursive = TRUE)

# ファイルリストの表示
print(file_list)

# 必要なパッケージを読み込み
library(readr)

# CSVファイルのパスを指定
file1 <- "./warmup training package/01_data/raw/semester_dummy/semester_data_1.csv"
file2 <- "./warmup training package/01_data/raw/semester_dummy/semester_data_2.csv"

# データを読み込む
data1 <- read_csv(file1)  # 1行目が列名として自動的に読み込まれる
data2 <- read_csv(file2)

# データの構造を確認
str(data1)
str(data2)

# データの最初の数行を表示
head(data1)
head(data2)
