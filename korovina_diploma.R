####### загрузка пакетов #########
{library(tidytext)
  library(dplyr)
  library(stringr)
  library(lightgbm)
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  library(purrr)
  library(ggplot2)
  library(car)
  library(lmtest)
  library(xgboost)
  library(viridis)
  library(randomForestExplainer)
  library(randomForest)
  library(mlbench)
  library(rpart)
  library(rpart.plot)
  library(caret)
  library(ggplot2)
  library(scipub)
  library(ggplot2)
  library(wordcloud)
  library(dplyr)
  library(tidyr)
  library(tidyr)
  library(scales)
  library(corrplot)
  library(Cairo)  
  library(quanteda.textstats)
  library(gt)
  library(gtExtras)
  library(viridis)
  library(AER)
  library(svglite)
  library(gt)
  library(stargazer)
  library(vtable)
  library(corrplot)
  library(outliers)
  library(stargazer)
  library(pdftools)
  library(reshape)
  library(tesseract)
  library(SnowballC)
  library(topicmodels)
  library(tidytext)
  library(tidyr)
  library(gutenbergr)
  library(scales)
  library(plm)
  library(quanteda)
  library(readxl)
  library(ggstatsplot)
  library(devtools)
  library(ggcorrplot)
  library(GGally)
  library(gmm)
  library(xts)
  library(sentopics)
  library(topicmodels)
  library(parallel)
  library(stringr)
  library(qdapRegex)
  library(tidytext)
  library(philentropy)
  library(tm)}

####### обработка текстов годовых отчётов #######
path <- "/Users/anastasiakorovina/Desktop/Diploma"
# списки стоп-слов на русском языке
a <- as.data.frame(quanteda::stopwords("ru", source = "snowball"))
b <- as.data.frame(quanteda::stopwords("ru", source = "stopwords-iso"))
c <- as.data.frame(quanteda::stopwords("ru", source = "marimo"))
d <- as.data.frame(quanteda::stopwords("ru", source = "nltk"))
names(c) <- "word"

# создаем пустой фрейм данных
# df <- data.frame(doc_name = character(), word = character(), frequency = numeric(), total_words = numeric())
# получаем список всех пдф файлов в папке
pdf_files <- list.files(path = path, pattern = "\\.pdf$", full.names = FALSE)

# цикл для обработки каждого пдф файла
for (i in seq_along(pdf_files)) {
  # имя документа
  doc_name <- basename(pdf_files[i])
  # используем pdftools для извлечения текста из пдф файла
  text <- pdf_text(pdf_files[i])
  # извлекаем слова из текста
  words <- text %>% 
    str_to_lower() %>% 
    str_squish() %>% 
    str_replace_all("[^[:alpha:]]", " ") %>% 
    str_split("\\s+") %>% 
    unlist()
  total_word <- length(words)
  # удаляем стоп-слова
  words <- words[!words %in% a$word]
  words <- words[!words %in% b$word]
  words <- words[!words %in% c$word]
  words <- words[!words %in% d$word]
  # стемминг
  words <- wordStem(words, language = 'russian')
  # создаем фрейм данных для текущего документа
  df_temp <- data.frame(doc_name = rep(doc_name, length(words)), word = words, 
                        total_words = rep(total_word, length(words)), stringsAsFactors = FALSE)
  # считаем частоту каждого слова
  df_temp %>% 
    group_by(doc_name, word, total_words) %>% 
    summarise(frequency = n()) %>% 
    ungroup() -> df_temp
  # объединяем фрейм данных для текущего документа с общим фреймом данных
  df <- bind_rows(df, df_temp)
}

####### словарь №1 #######
dictionary <- c("цифровой", "технологии","портал", "виртуальный", 
                "iot","it", "erp", "online", "ai", "artificial", "intelligence", 
                "электронный", "интерфейс", "умный", "информатизация", "трансформация",
                "онлайн", "девайс", "сетевой", "автоматизированный", "киберриски", 
                "компьютеризация", "виртуализация", "компьютеризированный", 
                "архитектура", "распознавание", "алгоритмический", "алгоритмизировать", 
                "беспроводной","виртуализированный", "вычислительный", "дрон", "многоканальный",
                "омниканальный", "передовой", "реструктуризация",
                "роботизация","стандартизация", "cyber", "digitalisation", 
                "кибербезопасность", "киберзащита", "экосистема","аутентификация", 
                "шифрование", "программа","трансформировать", "сайт", "диджитализация", 
                "дигитализация", "гаджет", "цифровизировать","цифровизация", "генерация",
                "кибератака", "intelligence", "software", "киберустойчивость")

dictionary <- wordStem(dictionary, language = 'russian')
pattern <- regex(paste0("\\b(", paste(dictionary, collapse = "|"), ")\\b"), ignore_case = TRUE)
digital <- df[str_detect(df$word, pattern), ]

##### облачко слов #####
# создаем таблицу частот слов
word_freq <- table(digital$word)
word_freq_df <- as.data.frame(word_freq)
colnames(word_freq_df) <- c("Word", "Freq")
# создаем цветовую палитру 
colors <- colorRampPalette(c("#00457E", "#2F70AF", "#B9848C", "#8c67b1", "#7f5f8e"))(60)
wordcloud(words = word_freq_df$Word, freq = word_freq_df$Freq, 
          min.freq = 5, max.words = 40, random.order = FALSE, 
          rot.per = 0.75, colors = colors, scale = c(4, 0.5))


df3 <- digital %>%
  group_by(doc_name) %>%          # группируем данные по столбцу doc_name
  summarise(total_digit = sum(frequency, na.rm = TRUE)) %>%  # суммируем frequency, убирая NA
  ungroup()   
ddd <- digital[, -c(2, 3)]
df4 <- merge(df3, ddd, by = "doc_name")
df4$digital <- df4$total_digit/df4$total_words
df4 <- df4[, -c(2, 3)]
df4 <- df4[ ! duplicated(df4), ]
df4$doc_name <- gsub("\\.pdf", "", df4$doc_name)
names(df4)[1] <- "Ticker_Year"

####### загрузка эксель датасета с КУ #######
corp_gov <- read_excel("~/Documents/КУ.xlsx")
corp_gov_dig <- merge(corp_gov, df4, by = "Ticker_Year")
corp_gov_dig <- as.data.frame(corp_gov_dig)
# сколько всего компаний в выборке?
length(unique(corp_gov_dig$Ticker))

###### словарь №2 - МГУ #####
dictionary <- c("цифровизация","архитектура", "агрегатор",  "алгоритм", "аддитивное", 
                "аутсорсинг", "беспилотный", "беспроводной","блокчейн", "вариативный", 
                "вариативность", "веб", "виртуальный","виртуализация", 
                "дигитализация", "икт", "интернет","квантовый","компьютер", "компьютерный", 
                "кибервалюта", "киберпространство", "кибер", "краудсорсинг", "краудфандинг", 
                "метаданные", "нейросеть", "нейронный", "обезличивание", 
                "онлайн", "онлайновый", "персональный", "оцифровка", "пользовательский", 
                "распределенный", "смарт","робот", "робототехника", "провайдер", "сетевой",   
                "технология", "трансграничный", "цифровой", "хакер","широкополосный",  
                "шеринговый", "умный", "электронный","am", "ai", "it", "iot", "iiot", 
                "ict", "blockchain", "internet","online", "smart","digital","digitalisation")
dictionary <-  wordStem(dictionary, language = 'russian')
pattern <- regex(paste0("\\b(", paste(dictionary, collapse = "|"), ")\\b"), ignore_case = TRUE)
digital_msu <- df[str_detect(df$word, pattern), ]

##### облачко слов номер два #####
# создаем таблицу частот слов
word_freq <- table(digital_msu$word)
word_freq_df <- as.data.frame(word_freq)
colnames(word_freq_df) <- c("Word", "Freq")
# создаем цветовую палитру 
colors <- colorRampPalette(c("#5889b2", "#5c5f9d", "#2d6b22", "#acc864", "#76bcf5"))(60)
wordcloud(words = word_freq_df$Word, freq = word_freq_df$Freq, 
          min.freq = 5, max.words = 40, random.order = FALSE, 
          rot.per = 0.75, colors = colors, scale = c(4, 0.5))

df3 <- digital_msu %>%
  group_by(doc_name) %>%          # группируем данные по столбцу doc_name
  summarise(total_digit = sum(frequency, na.rm = TRUE)) %>%  # суммируем frequency, убирая NA
  ungroup()   
ddd <- digital_msu[, -c(2, 3)]
df4 <- merge(df3, ddd, by = "doc_name")
df4$digital_msu <- df4$total_digit/df4$total_words
df4 <- df4[, -c(2, 3)]
df4 <- df4[ ! duplicated(df4), ]
df4$doc_name <- gsub("\\.pdf", "", df4$doc_name)
names(df4)[1] <- "Ticker_Year"
corp_gov <- read_excel("~/Documents/КУ.xlsx")
corp_gov_dig_ <- merge(corp_gov, df4, by = "Ticker_Year")

#объединяем первый и второй индексы цифровизации
corp_gov_dig1 <-  corp_gov_dig[, c(1, 17)]
corp_gov_dig_1_2 <- merge(corp_gov_dig1, corp_gov_dig_, by = "Ticker_Year", all.x = FALSE)

####### LDA #######
df_td <- df[,-4] %>% 
         cast_dtm(doc_name, word, frequency)
tf_lda_65 <- topicmodels::LDA(df_td, k = 65, 
                              control = list(seed = 255)) #честь и почёт жителям 255

####### репликация статюшки [Fritzsch et al., 2021] #######
# загружаем эталонные документы
refDoc_1 <- pdftools::pdf_text(pdf = "/Users/anastasiakorovina/Desktop/digit_document.pdf") #1
refDoc_2 <- pdftools::pdf_text(pdf = "/Users/anastasiakorovina/Desktop/digital_book.pdf") #2


# Извлекаем слова из текста
words <- refDoc_1 %>% 
  str_to_lower() %>% 
  str_squish() %>% 
  str_replace_all("[^[:alpha:]]", " ") %>% 
  str_split("\\s+") %>% 
  unlist()
total_word <- length(words)
words <- words[!words %in% a$word]
words <- words[!words %in% b$word]
words <- words[!words %in% c$word]
words <- words[!words %in% d$word]
words <- wordStem(words, language = 'russian')
refDoc_1 <- data.frame(doc_name = rep('refDoc_1', length(words)), word = words, 
                     total_words = rep(total_word, length(words)), stringsAsFactors = FALSE)
# считаем частоту каждого слова
refDoc_1 %>% 
  group_by(doc_name, word, total_words) %>% 
  summarise(frequency = n()) %>% 
  ungroup() -> refDoc_1
dtmRefDoc_1 <- refDoc_1 %>%
  cast_dtm(doc_name, word, frequency)

# Извлекаем слова из текста
words <- refDoc_2 %>% 
  str_to_lower() %>% 
  str_squish() %>% 
  str_replace_all("[^[:alpha:]]", " ") %>% 
  str_split("\\s+") %>% 
  unlist()
total_word <- length(words)
words <- words[!words %in% a$word]
words <- words[!words %in% b$word]
words <- words[!words %in% c$word]
words <- words[!words %in% d$word]
words <- wordStem(words, language = 'russian')
refDoc_2 <- data.frame(doc_name = rep('refDoc_2', length(words)), word = words, 
                       total_words = rep(total_word, length(words)), stringsAsFactors = FALSE)
# считаем частоту каждого слова
refDoc_2 %>% 
  group_by(doc_name, word, total_words) %>% 
  summarise(frequency = n()) %>% 
  ungroup() -> refDoc_2
dtmRefDoc_2 <- refDoc_2 %>%
  cast_dtm(doc_name, word, frequency)

#### считаем KL-divergence и индекс цифровизации ####
calculateKlDivergence <- function(lda, dtmDig){
  # lda: fitted lda model 
  # dtmDig - это Document Term Matrix эталонного документа
  digPerDocument<-rep(0,nrow(lda@gamma))
  names(digPerDocument)<-lda@documents
  posteriorDig<-posterior(lda,dtmDig)   
  for (k in 1:nrow(lda@gamma)){
    x<-rbind(posteriorDig$topics[1,],
             lda@gamma[k,])
    digPerDocument[k]=KL(x)
  }
  return(1/digPerDocument) #обратная величина (чем больше - тем больше схожесть)
}
KL_1 <- as.data.frame(calculateKlDivergence(lda=tf_lda, dtmDig=dtmRefDoc_1))
KL_2 <- as.data.frame(calculateKlDivergence(lda=tf_lda, dtmDig=dtmRefDoc_2))
# получаем два новых индекса цифровизации
colnames(KL_1) <- 'digital_LDA_1'
colnames(KL_2) <- 'digital_LDA_2'

mm_1 <- data.frame(Ticker_Year = rownames(KL_1), digital_LDA_1 = KL$digital_LDA_1)
mm_2 <- data.frame(Ticker_Year = rownames(KL_2), digital_LDA_2 = KL$digital_LDA_2)
mm_1$Ticker_Year <- gsub("\\.pdf", "", mm_1$Ticker_Year)
mm_2$Ticker_Year <- gsub("\\.pdf", "", mm_2$Ticker_Year)

corp_gov_dig_1 <- merge(corp_gov_dig, mm_1, by = "Ticker_Year")
corp_gov_dig_2 <- merge(corp_gov_dig, mm_2, by = "Ticker_Year")

corp_gov_dig_1  <-  corp_gov_dig_1[, c(1, 18)]
corp_gov_dig_1_2_3 <- merge(corp_gov_dig_1_2, corp_gov_dig_1, by = "Ticker_Year", all.x = FALSE)

corp_gov_dig_2  <-  corp_gov_dig_2[, c(1, 18)]
corp_gov_dig_all4 <- merge(corp_gov_dig_1_2_3, corp_gov_dig_2, by = "Ticker_Year", all.x = FALSE)

####### итоговый датасет со всеми индексами #######
str(corp_gov_dig_all4)

ggg <- corp_gov_dig_all4[, c(18, 2, 19, 20)]
str(ggg)
ggg_long <- pivot_longer(ggg[, c(1:2)], cols = everything(), 
                         names_to = "variable", 
                         values_to = "value")


#### корреляция ####
forcor <- corp_gov_dig_all4[, c(18, 2, 20, 19,  8:13, 5, 6, 7, 14)]
colnames(forcor) <- c("Индекс цифровизации", "Индекс цифровизации (MSU)", 
                      "Индекс цифровизации LDA №1", "Индекс цифровизации LDA №2", 
                      "Размер СД", "Доля женщин в СД", "Число заседаний СД", 
                      "Средний возраст СД", "Доля независимых директоров", 
                      "Доля иностранных директоров", "Рентабельность активов", 
                      "Активы", "Долг/Активы",  "Государственное участие")
str(forcor)
cor_matrix <- cor(forcor)
p_values <- cor.mtest(forcor)$p
colors <- colorRampPalette(c("#BC2041", "white", "#2F70AF"))(200)
# добавляем звездочки для значимых корреляций
corrplot(cor_matrix, method = "circle", col = colors, 
         type = "upper", tl.col = "black", tl.srt = 45, 
         addgrid.col = "lightgray", number.cex = 0.9, tl.cex = 0.7,
         p.mat = p_values, sig.level = 0.05, insig = "label_sig",  
         pch.cex = 1, pch.col = "black",diag = FALSE)  
correltable(data = forcor[, -c(11, 12, 13, 14)],
  tri = "lower", html = TRUE)
correltable(data = forcor,
  tri = "lower", html = TRUE)

####### график с динамикой всех индексов #######
forgraph <- corp_gov_dig_all4[, c(1, 3, 4, 18, 2, 19, 20)] #для графика
average_pr <- forgraph %>%
  group_by(Number_Year) %>%
  summarise(
    digital = mean(digital, na.rm = TRUE),
    digital_msu = mean(digital_msu, na.rm = TRUE),
    digital_LDA_1 = mean(digital_LDA_1, na.rm = TRUE),
    digital_LDA_2 = mean(digital_LDA_2, na.rm = TRUE)
  )

# преобразуем данные в длинный формат для удобства построения графиков
average_pr_long <- average_pr %>%
  pivot_longer(cols = starts_with("digital"), names_to = "Index", values_to = "Value")
# заменяем названия индексов на русские
average_pr_long <- average_pr_long %>%
  mutate(Index = case_when(
    Index == "digital" ~ "Индекс цифровизации",
    Index == "digital_LDA_1" ~ "Индекс цифровизации LDA №1",
    Index == "digital_msu" ~ "Индекс цифровизации (MSU)",
    Index == "digital_LDA_2" ~ "Индекс цифровизации LDA №2",
    TRUE ~ Index  # На случай, если есть другие значения
  ))

# создаем вектор с подписями для оси X (через один год)
year_breaks <- unique(average_pr_long$Number_Year)[seq(1, length(unique(average_pr_long$Number_Year)), by = 2)]

# строим графики
ggplot(average_pr_long, aes(x = factor(Number_Year), y = Value)) + 
  geom_bar(stat = "identity", aes(fill = Index), alpha = 0.6) + 
  facet_wrap(~ Index, scales = "free_y", ncol = 2) +  # разделяем графики по индексам
  labs(x = NULL, y = NULL, title = NULL) + 
  theme_minimal() + 
  geom_smooth(method = "loess", aes(group = Index, color = Index), 
              se = FALSE, size = 0.5, span = 2.9) + 
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_text(size = 14),
    axis.text.x = element_text(size = 15),  
    axis.text.y = element_text(size = 15),  
    strip.text = element_text(size = 20)  
  ) +
  scale_fill_manual(values = c(
    "Индекс цифровизации" = "cornflowerblue",        
    "Индекс цифровизации (MSU)" = "lightblue",       
    "Индекс цифровизации LDA №1" = "#C0587E",    
    "Индекс цифровизации LDA №2" = "slateblue"    
  )) +
  scale_color_manual(values = c(
    "Индекс цифровизации" = "dodgerblue4",          
    "Индекс цифровизации (MSU)" = "deepskyblue3",   
    "Индекс цифровизации LDA №1" = "#b33572",    
    "Индекс цифровизации LDA №2" = "mediumslateblue"  
  )) +
  scale_x_discrete(breaks = year_breaks)  


####### построение модели КУ #######
filtered_data <- corp_gov_dig_all4 %>%
  group_by(Ticker) %>%               
  ungroup()  

pdata <- pdata.frame(filtered_data, index = c("Ticker", "Number_Year"))

mod_dict_1 <- plm(digital ~ Board_size + Average_age + Board_meetings + 
                    Share_Foreign_Directors  + 
                    Gender_diversity + share_ID
                  + Gov_Part + DebtToAssets + log_Assets + ROA, 
                  model = 'within', effect = 'twoways', 
                  data = pdata %>% mutate(digital = digital*100))
mod_dict_2 <- plm(digital_msu ~ Board_size + Average_age + Board_meetings + 
                    Share_Foreign_Directors  + 
                    Gender_diversity + share_ID
                  + Gov_Part + DebtToAssets + log_Assets + ROA, 
                  model = 'within', effect = 'twoways', 
                  data = pdata %>% mutate(digital_msu = digital_msu*100))
mod_lda_1 <- plm(digital_LDA_1 ~ Board_size + Average_age + Board_meetings + 
                    Share_Foreign_Directors  + 
                    Gender_diversity + share_ID
                  + Gov_Part + DebtToAssets + log_Assets + ROA, 
                  model = 'within', effect = 'twoways', 
                  data = pdata %>% mutate(digital_LDA_1 = digital_LDA_1*100))
mod_lda_2 <- plm(digital_LDA_2 ~ Board_size + Average_age + Board_meetings + 
                   Share_Foreign_Directors  + 
                   Gender_diversity + share_ID
                 + Gov_Part + DebtToAssets + log_Assets + ROA, 
                 model = 'within', effect = 'twoways', 
                 data = pdata %>% mutate(digital_LDA_2 = digital_LDA_2*100))

stargazer(mod_dict_1, mod_dict_1, mod_lda_1, mod_lda_2,  
          type = "html", out = "modelichki_moi_lubimyui.html")

####### построение графика частичных остатков, проверка VIF и тест Рамсея #######
mod <- lm(digital ~ Board_size + Average_age + Board_meetings + 
             Share_Foreign_Directors + Gender_diversity + share_ID
           + Gov_Part + DebtToAssets + log_Assets + ROA + Ticker + as.factor(Number_Year), 
           data = corp_gov_dig %>% mutate(digital = digital*100))
summary(mod)
vif(mod) #нет проблем с мультиколлинеарностью
crPlots(mod, ask = FALSE)  

data_for_plots <- corp_gov_dig %>%
  mutate(
    digital = digital * 100,
    predicted = predict(mod3),
    residuals = residuals(mod3)
  )
plot_partial_residuals <- function(data, model, predictor) {
  data %>%
    mutate(
      partial_resid = residuals + coef(model)[predictor] * .data[[predictor]]
    ) %>%
    ggplot(aes(x = .data[[predictor]], y = partial_resid)) +
    geom_point(alpha = 0.5, color = "#C1DFF9") +  
    geom_smooth(method = "loess", se = FALSE, 
                color = "#1565C0", linewidth = 1, span = 1.5) +  
    geom_line(
      aes(y = coef(model)[predictor] * .data[[predictor]]), 
      color = "black", 
      linetype = "dashed"
    ) + 
    labs(
      title = paste("Component+Residual Plot for", predictor),
      x = predictor,
      y = "Partial Residual"
    ) +
    theme_minimal()
}

pred_labels <- c(
  "Board_size" = "Размер СД",
  "Average_age" = "Средний возраст СД",
  "Board_meetings" = "Количество заседаний СД",
  "Share_Foreign_Directors" = "Доля иностранных директоров",
  "Gender_diversity" = "Гендерное разнообразие",
  "share_ID" = "Доля независимых директоров")

plot_partial_residuals_ru <- function(data, model, predictor) {
  russian_label <- pred_labels[predictor]
  data %>%
    mutate(
      partial_resid = residuals + coef(model)[predictor] * .data[[predictor]]
    ) %>%
    ggplot(aes(x = .data[[predictor]], y = partial_resid)) +
    geom_point(alpha = 0.5, color = "#1E88E5") +
    geom_smooth(
      method = "loess",
      se = FALSE,
      color = "#0D47A1",
      linewidth = 0.5,
      span = 2.5
    ) +
    labs(
      title = russian_label,
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_family = "Arial") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 9))}


plot_partial_residuals_ru(data_for_plots, mod, "Board_size")
# туц туц
predictors <- c("Board_size", "Average_age", "Board_meetings", 
                "Share_Foreign_Directors", "Gender_diversity", "share_ID")

# красивые графики частичных остатков
plots <- map(predictors, ~ plot_partial_residuals_ru(data_for_plots, mod, .x))
wrap_plots(plots, ncol = 3)  

# тест Рамсея для базовой модели самого первого индекса
resettest(mod, power = 2)
#он говорит, что в модели все хорошо, не надо ничего добавлять
#как p-value > 0.05 - нулевая гипотеза принимается

####### проверка - построим GMM для базовой модели с КУ #######
z1 <- pgmm(digital ~ lag(digital, 1) + 
             Board_size + Average_age + Board_meetings + 
             Share_Foreign_Directors + Gender_diversity + share_ID
           + Gov_Part + DebtToAssets + log_Assets + ROA |
             lag(Board_meetings, 3) +  lag(ROA, 2) + lag(Gov_Part, 2:6)
           + lag(Gender_diversity, 1:5) + lag(digital, 2:4)
           + lag(Average_age, 1:8) + lag(share_ID, 2)
           + lag(DebtToAssets, 3),
           collapse = TRUE, 
           data = pdata %>% mutate(digital = digital*100),
           effect = c("twoways"),
           model = c("twosteps"))
summary(z1, robust = T) #даже с добавлением робстаных ошибок переменные КУ значимы

####### создание лагированных переменных КУ #######
pdata_lag <- pdata %>%
  group_by(Ticker) %>%
  arrange(Number_Year) %>%
  mutate(
    digital_lag = ifelse(row_number() == 1, NA, lag(digital, 1L)),
    digital_msu_lag = ifelse(row_number() == 1, NA, lag(digital_msu, 1L)),
    digital_LDA_1_lag = ifelse(row_number() == 1, NA, lag(digital_LDA_1, 1L)),
    digital_LDA_2_lag = ifelse(row_number() == 1, NA, lag(digital_LDA_2, 1L)),
    Board_size_lag = ifelse(row_number() == 1, NA, lag(Board_size, 1L)),
    Average_age_lag = ifelse(row_number() == 1, NA, lag(Average_age, 1L)),
    Board_meetings_lag = ifelse(row_number() == 1, NA, lag(Board_meetings, 1L)),
    Share_Foreign_Directors_lag = ifelse(row_number() == 1, NA, lag(Share_Foreign_Directors, 1L)),
    Gender_diversity_lag = ifelse(row_number() == 1, NA, lag(Gender_diversity, 1L)),
    share_ID_lag = ifelse(row_number() == 1, NA, lag(share_ID, 1L))
  ) %>%
  ungroup()

pdata_lag <- pdata_lag[order(pdata$Ticker, pdata$Number_Year), ]
pdata_lag <- pdata.frame(pdata_lag, index = c("Ticker", "Number_Year"))

dict_1_lag <- plm(digital_lag ~  Board_size_lag + 
                   + Average_age_lag +  Board_meetings_lag + 
                   Share_Foreign_Directors_lag + Gender_diversity_lag + share_ID_lag +
                   + Gov_Part + DebtToAssets + log_Assets + ROA, 
                 model = 'within', effect = 'twoways', 
                 data = pdata_lag %>% mutate(digital_lag = digital_lag*100))

dict_2_lag <- plm(digital_msu_lag ~  Board_size_lag + 
                  + Average_age_lag +  Board_meetings_lag + 
                  Share_Foreign_Directors_lag + Gender_diversity_lag + share_ID_lag +
                  + Gov_Part + DebtToAssets + log_Assets + ROA, 
                model = 'within', effect = 'twoways', 
                data = pdata_lag %>% mutate(digital_msu_lag = digital_msu_lag*100))

lda_1_lag <- plm(digital_LDA_1_lag ~  Board_size_lag + 
                    + Average_age_lag +  Board_meetings_lag + 
                    Share_Foreign_Directors_lag + Gender_diversity_lag + share_ID_lag +
                    + Gov_Part + DebtToAssets + log_Assets + ROA, 
                  model = 'within', effect = 'twoways', 
                  data = pdata_lag %>% mutate(digital_LDA_1_lag = digital_LDA_1_lag*100))

lda_2_lag <- plm(digital_LDA_2_lag ~  Board_size_lag + 
                   + Average_age_lag +  Board_meetings_lag + 
                   Share_Foreign_Directors_lag + Gender_diversity_lag + share_ID_lag +
                   + Gov_Part + DebtToAssets + log_Assets + ROA, 
                 model = 'within', effect = 'twoways', 
                 data = pdata_lag %>% mutate(digital_LDA_2_lag = digital_LDA_2_lag*100))

stargazer(dict_1_lag, dict_2_lag, lda_1_lag, lda_2_lag,  
          type = "html", out = "modelichki_lagggiii.html")


####### построение модели с зависимой ROA #######
mod_1 <- plm(ROA ~ digital + DebtToAssets + log_Assets
              + log(Board_meetings) + Gov_Part + log(Board_size),
              model = 'within', effect = 'twoways',
              data = pdata %>% mutate(digital = digital*100))
mod_2 <- plm(ROA ~ digital_msu + DebtToAssets + log_Assets
             + log(Board_meetings) + Gov_Part + log(Board_size),
             model = 'within', effect = 'twoways',
             data = pdata %>% mutate(digital_msu = digital_msu*100))
mod_3 <- plm(ROA ~ digital_LDA_1 + DebtToAssets + log_Assets
             + Board_meetings + Gov_Part + Board_size,
             model = 'within', effect = 'twoways',
             data = pdata %>% mutate(digital_LDA_1 = digital_LDA_1*100))
mod_4 <- plm(ROA ~ digital_LDA_2 + DebtToAssets + log_Assets
             + Board_meetings + Gov_Part + Board_size,
             model = 'within', effect = 'twoways',
             data = pdata %>% mutate(digital_LDA_2 = digital_LDA_2*100))

stargazer(mod_1, mod_2, mod_3, mod_4, type = "html", out = "modeliki_s_roa.html")


####### GMM - динамическая модель #######
ZZZ1 <- pgmm(ROA ~ lag(ROA, 1) + digital + DebtToAssets + 
               log_Assets + log(Board_meetings) + log(Board_size) + Gov_Part |
               lag(ROA, 2) + lag(Average_age, 1) +
               lag(Board_meetings, 0:2) + lag(Gov_Part, 3)
             + lag(Gender_diversity, 2) + lag(Share_Foreign_Directors, 0:2), 
             data = pdata %>% mutate(digital = digital*100),
             collapse = TRUE,
             effect = c("twoways"),
             model = c("twosteps"))
summary(ZZZ1)

ZZZ2 <- pgmm(ROA ~ lag(ROA, 1) + digital_msu + DebtToAssets + 
               log_Assets + Board_meetings + Board_size + Gov_Part |
               lag(ROA, 2) + lag(Average_age, 1) +
               lag(Board_meetings, 0:2) + lag(Gov_Part, 3)
             + lag(Gender_diversity, 2) + lag(Share_Foreign_Directors, 0:2), 
             data = pdata %>% mutate(digital_msu = digital_msu*100),
             collapse = TRUE,
             effect = c("twoways"),
             model = c("twosteps"))

ZZZ3 <- pgmm(ROA ~ lag(ROA, 1) + digital_LDA_1 + DebtToAssets + 
               log_Assets + Board_meetings + Board_size + Gov_Part |
               lag(ROA, 2) + lag(Average_age, 1) + lag(digital_LDA_1, 3:5) +
               lag(Board_meetings, 0:2) + lag(Gov_Part, 3)
             + lag(Gender_diversity, 2) + lag(Share_Foreign_Directors, 0:2), 
             data = pdata %>% mutate(digital_LDA_1 = digital_LDA_1*100),
             collapse = TRUE,
             effect = c("twoways"),
             model = c("twosteps"))

ZZZ4 <- pgmm(ROA ~ lag(ROA, 1) + digital_LDA_2 + DebtToAssets + 
               log_Assets + Board_meetings + Board_size + Gov_Part |
               lag(ROA, 2) + lag(Average_age, 1) + lag(digital_LDA_2, 3:5) + 
               lag(Board_meetings, 0:2) + lag(Gov_Part, 3)
             + lag(Gender_diversity, 2) + lag(Share_Foreign_Directors, 0:2), 
             data = pdata %>% mutate(digital_LDA_2 = digital_LDA_2*100),
             collapse = TRUE,
             effect = c("twoways"),
             model = c("twosteps"))
stargazer(ZZZ1, ZZZ2, ZZZ3, ZZZ4, type = "html",  out = "gmm.html")





####### модели машинного обучения #######
######### лесок ###########
ml <- corp_gov_dig[, c(17, 4:13)]
str(ml)
#обучим лес
set.seed(5)
mod <- randomForest(digital ~ ROA + log_Assets + DebtToAssets +
                      + Board_size + Gender_diversity + Board_meetings + 
                      + Average_age + share_ID + Share_Foreign_Directors
                    + Gov_Part, 
                    data = ml)
mod
plot(mod, main = "Зависимость ошибки от числа деревьев", lwd = 1) 
plot(mod, main = "Ошибка в зависимости от числа деревьев", col = "#2F70AF", lwd = 1) 
which.min(mod$mse)
#число переменных отбираемых в узле
dim(ml)
t <- tuneRF(ml[,-4], ml[,4], #убираем зависимую переменную (она 4ая по счету)
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 500,
            trace = TRUE)

# Преобразуем результат в датафрейм
t_df <- as.data.frame(t)
t_df$mtry <- as.numeric(rownames(t_df))  # Добавляем столбец с значениями mtry
# Строим цветной график с ggplot2
library(ggplot2)
ggplot(t_df, aes(x = mtry, y = OOBError)) +
  geom_line(color = "#2F70AF", size = 1.2, alpha = 0.7) +  # Линия графика
  geom_point(color = "#2F70AF", size = 3) +   # Точки на графике
  labs(title = "Оптимизация числа переменных, отбираемых в узле",
       x = "Число переменных",
       y = "Ошибка OOB") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
  scale_x_continuous(breaks = t_df$mtry)  # Убедимся, что все значения mtry отображаются на оси X

# итоговая модель
mod <- randomForest(digital ~ ROA + log_Assets + DebtToAssets +
                      + Board_size + Gender_diversity + Board_meetings + 
                      + Average_age + share_ID + Share_Foreign_Directors
                    + Gov_Part, 
                    data = ml, ntree = 493, mtry = 3,localImp = TRUE )

# важность переменных
importance_df <- as.data.frame(importance(mod))
rownames(importance_df) <- c("Рентабельность активов", "Активы", "Долг/Активы", 
                             "Размер СД", "Доля женщин в СД", "Число заседаний СД", 
                             "Средний возраст СД", "Доля независимых директоров", 
                             "Доля иностранных директоров", "Государственное участие")
importance_df$Variable <- rownames(importance_df)
#важность по Джинни 
ggplot(importance_df, aes(x = reorder(Variable, IncNodePurity), y = IncNodePurity, fill = IncNodePurity)) +
  geom_bar(stat = "identity", alpha = 0.8) + 
  scale_fill_gradient(low = "#C1DFF9", high = "#2F70AF") +  
  coord_flip() +
  labs(x = NULL,
       y = "Увеличение ошибки (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.1),  
    axis.text.y = element_text(size = 14), legend.position = "none")  

#stepFactor - шаг поиска
#ntreeTry - число деревьев
#trace - показывать динамику поиска
varImpPlot(mod,sort = T, main = "Top Variable Importance") # по Джинни 

#что важнее всего?
varImpPlot(mod, sort = T, n.var = 10,
           main = "Ранжирование переменных")

colors <- c("#4F2B4C", "#806491", "#B9848C", "#C1DFF9", "#74A9BE",
            "#7990A3", "#748CDB", "#163B88", "#192D45")

colors <- c("#E3F2FD", "#BBDEFB", "#90CAF9", "#64B5F6", "#42A5F5", 
            "#2196F3", "#1E88E5", "#1976D2", "#1565C0", "#0D47A1")


plot_min_depth_distribution(mod) + 
  xlab("Переменные") +
  ylab("Количество деревьев") + 
  scale_fill_manual(values = colors) + 
  ggtitle("Распределение минимальной глубины и среднего сплита") +
  theme_minimal() 
# получаем распределение минимальной глубины
min_depth_frame <- min_depth_distribution(mod)

# создаем именованный вектор с русскими именами
russian_names <- c(
  "ROA" = "Рентабельность активов",
  "log_Assets" = "Активы",
  "DebtToAssets" = "Долг/Активы",
  "Board_size" = "Размер СД",
  "Gender_diversity" = "Доля женщин в СД",
  "Board_meetings" = "Число заседаний СД",
  "Average_age" = "Средний возраст СД",
  "share_ID" = "Доля независимых директоров",
  "Share_Foreign_Directors" = "Доля иностранных директоров",
  "Gov_Part" = "Государственное участие"
)


######### xgboost ###########
# убедимся, что данные не содержат пропущенных значений
ml <- na.omit(ml)
# преобразуем целевую переменную в числовой формат, если это необходимо
ml$digital <- as.numeric(ml$digital)

# создадим матрицу признаков и вектор целевой переменной
X <- model.matrix(digital ~ ROA + log_Assets + DebtToAssets + Board_size + 
                    Gender_diversity + Board_meetings + Average_age + share_ID + 
                    Share_Foreign_Directors + Gov_Part - 1, data = ml)
y <- ml$digital
# преобразуем данные в формат DMatrix, который требуется для XGBoost
dtrain <- xgb.DMatrix(data = X, label = y)

# зададим параметры модели
params <- list(
  objective = "reg:squarederror",  # Для регрессии
  eval_metric = "rmse",            # Метрика оценки
  max_depth = 5,                   # Максимальная глубина дерева
  eta = 0.1,                       # Скорость обучения
  subsample = 0.8,                 # Доля строк данных для каждого дерева
  colsample_bytree = 0.8,          # Доля признаков для каждого дерева
  early_stopping_rounds = 10       # Ранняя остановка
)

# проведём кросс-валидацию
cv_results <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 500,                   # Максимальное количество итераций
  nfold = 5,                       # Количество фолдов для кросс-валидации
  showsd = TRUE,                   # Показывать стандартное отклонение
  stratified = TRUE,               # Стратификация (если целевая переменная категориальная)
  print_every_n = 10,              # Показывать результат каждые 10 итераций
  verbose = 1                      # Включить вывод в консоль
)

# выведем результаты кросс-валидации
print(cv_results)

# найдем оптимальное количество итераций
optimal_nrounds <- which.min(cv_results$evaluation_log$test_rmse_mean)
cat("Оптимальное количество итераций (nrounds):", optimal_nrounds, "\n")

# обучим финальную модель с оптимальным количеством итераций
final_model <- xgboost(
  params = params,
  data = dtrain,
  nrounds = optimal_nrounds,
  verbose = 0
)

# выведем важность переменных
importance_matrix <- xgb.importance(model = final_model)
print(importance_matrix)

# построим график важности переменных
xgb.plot.importance(importance_matrix, rel_to_first = TRUE, 
                    xlab = "Relative Importance", 
                    main = "Relative Importance Ranking based on XGBoost")

# важность переменных
importance_matrix$Feature <- c("Средний возраст СД", "Активы", "Долг/Активы",
                               "Государственное участие", "Рентабельность активов", 
                               "Размер СД", "Число заседаний СД",  "Доля иностранных директоров",
                               "Доля независимых директоров","Доля женщин в СД")


# преобразуем важность переменных в формат для ggplot2
importance_df <- importance_matrix %>%
  as.data.frame() %>%
  mutate(Feature = factor(Feature, levels = Feature[order(Gain)]))

# построим график с градиентной заливкой
ggplot(importance_df, aes(x = Gain, y = reorder(Feature, Gain), fill = Gain)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Gain, 2)), hjust = -0.1, size = 4, color = "black") + 
  scale_fill_gradient(low = "#C1DFF9", high = "#163B88") +  # Градиентная заливка
  labs(x = "Относительная важность", y = NULL, title = "XGBoost") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 14),  # Увеличиваем размер подписей переменных на оси Y
    axis.text.x = element_text(size = 0),
    legend.position = "none"
  )

 
######### lightgbm ###########
ml <- na.omit(ml)
# преобразуем целевую переменную в числовой формат, если это необходимо
ml$digital <- as.numeric(ml$digital)
# создадим матрицу признаков и вектор целевой переменной
X <- as.matrix(ml[, c("ROA", "log_Assets", "DebtToAssets", "Board_size", 
                      "Gender_diversity", "Board_meetings", "Average_age", 
                      "share_ID", "Share_Foreign_Directors", "Gov_Part")])
y <- ml$digital
# преобразуем данные в формат lgb.Dataset
dtrain <- lgb.Dataset(data = X, label = y)

# зададим параметры модели
params <- list(
  objective = "regression",  # Для регрессии
  metric = "rmse",           # Метрика оценки
  boosting_type = "gbdt",    # Тип бустинга
  num_leaves = 31,           # Количество листьев в дереве
  learning_rate = 0.1,       # Скорость обучения
  feature_fraction = 0.9,    # Доля признаков для каждого дерева
  verbose = -1               # Отключим вывод в консоль
)

# проведём кросс-валидацию
cv_results <- lgb.cv(
  params = params,
  data = dtrain,
  nrounds = 500,              # Максимальное количество итераций
  nfold = 5,                  # Количество фолдов для кросс-валидации
  early_stopping_rounds = 10, # Ранняя остановка
  eval_freq = 10,             # Частота вывода результатов
  stratified = TRUE,          # Стратификация (если целевая переменная категориальная)
  verbose = 1                 # Включить вывод в консоль
)

# выведем результаты кросс-валидации
print(cv_results)

# найдем оптимальное количество итераций
optimal_nrounds <- cv_results$best_iter
cat("Оптимальное количество итераций (nrounds):", optimal_nrounds, "\n")

# обучим финальную модель с оптимальным количеством итераций
final_model <- lgb.train(
  params = params,
  data = dtrain,
  nrounds = optimal_nrounds,
  verbose = 1
)

# выведем важность переменных
importance_matrix <- lgb.importance(model = final_model, percentage = TRUE)
print(importance_matrix)
importance_matrix$Feature <- c("Средний возраст СД", "Размер СД",
                               "Долг/Активы", "Государственное участие",
                               "Активы", 
                               "Доля иностранных директоров", "Число заседаний СД",
                               "Рентабельность активов", 
                               "Доля независимых директоров","Доля женщин в СД")


# преобразуем важность переменных в формат для ggplot2
importance_df <- importance_matrix %>%
  as.data.frame() %>%
  mutate(Feature = factor(Feature, levels = Feature[order(Gain)]))

# построим график с градиентной заливкой
ggplot(importance_df, aes(x = Gain, y = reorder(Feature, Gain), fill = Gain)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Gain, 2)), hjust = -0.1, size = 4, color = "black") +  # Добавляем значения
  scale_fill_gradient(low = "#8c67b1", high = "#A7D1D2") +
  labs(x = "Относительная важность", y = NULL, title = "LightGBM") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 14),  # Увеличиваем размер подписей переменных на оси Y
    axis.text.x = element_text(size = 0),
    legend.position = "none"
  )

######### PCA - рейтинг компаний ###########
  pca_data <- corp_gov_dig_all4 %>%
    filter(Number_Year > 2015)  %>%
    group_by(Ticker) %>%
    summarise(across(c(digital_msu, digital, digital_LDA_1, digital_LDA_2),
                     mean, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(complete.cases(.)) %>%
    as.data.frame() %>%
    column_to_rownames("Ticker") %>%
    scale() # Стандартизация после логарифма
  
  pca_data <- as.data.frame(pca_data)
  
  # выполнение PCA
  if(nrow(pca_data) > 3) 
    pca_result <- prcomp(pca_data, scale = TRUE)
  
  # визуализация вклада показателей
  contrib_plot <- fviz_pca_var(pca_result,
                               col.var = "contrib",
                               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                               repel = TRUE) +
    labs(title = "Вклад показателей в главные компоненты")
  contrib_plot
  
  # PCA
  if(nrow(pca_data) > 3) {
    pca_result <- prcomp(pca_data, scale = TRUE)
    
    # Создаем копию rotation с русскими названиями
    rus_rotation <- pca_result$rotation
    rownames(rus_rotation) <- c(
      "Цифровизация (MSU)",
      "Цифровизация (базовый)",
      "LDA (55 тем)", 
      "LDA (65 тем)"
    )
    # Создаем временный объект PCA с переведенными названиями
    temp_pca <- pca_result
    temp_pca$rotation <- rus_rotation
    
    # 3. Визуализация с автоматическим построением
    contrib_plot <- fviz_pca_var(temp_pca,
                                 col.var = "contrib",
                                 gradient.cols = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2"),
                                 repel = T,
                                 labelsize = 2) +
      labs(title = "Вклад показателей в главные компоненты",
           x = "Главная компонента 1",
           y = "Главная компонента 2") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"))
    
    contrib_plot
  }
    
  # подготовка данных для визуализации
  pca_scores <- as.data.frame(pca_result$x) %>%
    rownames_to_column("Ticker") %>%
    mutate(Digital_Score = PC1) %>%
    arrange(Digital_Score) %>%
    mutate(Ticker = factor(Ticker, levels = Ticker))
  
  # Топ-10 компаний 
  top_digital <- pca_scores %>%
    slice_max(Digital_Score, n = 10)
  
  # финальный график 
  library(ggplot2)
  rank_plot <- ggplot(top_digital,
                      aes(x = reorder(Ticker, Digital_Score),   
                          y = Digital_Score,
                          fill = Digital_Score)) +
    geom_col(width = 0.8, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.2f", Digital_Score)),
              hjust = +0.55,  # Сдвиг влево
              vjust = -0.5,   # Центрирование по вертикали
              color = "black", # Цвет текста
              fontface = "bold") +  # Жирный шрифт
    #scale_fill_gradient(low = "#74A9BE", high = "#BBDEFB") + 
    scale_fill_gradient(low = "#BBDEFB", high = "#0D47A1") + 
    labs(title = "Топ-10 цифровых компаний за 2016-2021",
         x = "",                                    # Убираем название оси X
         y = "Интегральный показатель цифровизации (PC1)") +  # Название для оси Y
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      panel.grid.major.x = element_blank(),  # Убираем сетку вдоль оси X
      axis.text.x = element_text(size = 14, face = "bold") # Названия по оси X
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) # Обеспечиваем расширение вертикальной оси
  
  # Отображаем график
  rank_plot
  
  
  pca_data <- corp_gov_dig_all4 %>%
    filter(Number_Year <= 2015)  %>%
    group_by(Ticker) %>%
    summarise(across(c(digital_msu, digital, digital_LDA_1, digital_LDA_2),
                     mean, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(complete.cases(.)) %>%
    as.data.frame() %>%
    column_to_rownames("Ticker") %>%
    scale() # Стандартизация после логарифма
  
  pca_data <- as.data.frame(pca_data)
  
  # выполнение PCA
  if(nrow(pca_data) > 3) 
    pca_result <- prcomp(pca_data, scale = TRUE)
  
  # визуализация вклада показателей
  contrib_plot <- fviz_pca_var(pca_result,
                               col.var = "contrib",
                               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                               repel = TRUE) +
    labs(title = "Вклад показателей в главные компоненты")
  contrib_plot
  
  # PCA
  if(nrow(pca_data) > 3) {
    pca_result <- prcomp(pca_data, scale = TRUE)
    
    # Создаем копию rotation с русскими названиями
    rus_rotation <- pca_result$rotation
    rownames(rus_rotation) <- c(
      "Цифровизация (MSU)",
      "Цифровизация (базовый)",
      "LDA (55 тем)", 
      "LDA (65 тем)"
    )
    # Создаем временный объект PCA с переведенными названиями
    temp_pca <- pca_result
    temp_pca$rotation <- rus_rotation
    
    # 3. Визуализация с автоматическим построением
    contrib_plot <- fviz_pca_var(temp_pca,
                                 col.var = "contrib",
                                 gradient.cols = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2"),
                                 repel = T,
                                 labelsize = 2) +
      labs(title = "Вклад показателей в главные компоненты",
           x = "Главная компонента 1",
           y = "Главная компонента 2") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"))
    
    contrib_plot
  }
  
  # подготовка данных для визуализации
  pca_scores <- as.data.frame(pca_result$x) %>%
    rownames_to_column("Ticker") %>%
    mutate(Digital_Score = PC1) %>%
    arrange(Digital_Score) %>%
    mutate(Ticker = factor(Ticker, levels = Ticker))
  
  # Топ-10 компаний 
  top_digital <- pca_scores %>%
    slice_max(Digital_Score, n = 10)
  
  # финальный график 
  library(ggplot2)
  rank_plot <- ggplot(top_digital,
                      aes(x = reorder(Ticker, Digital_Score),   
                          y = Digital_Score,
                          fill = Digital_Score)) +
    geom_col(width = 0.8, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.2f", Digital_Score)),
              hjust = +0.55,  # Сдвиг влево
              vjust = -0.5,   # Центрирование по вертикали
              color = "black", # Цвет текста
              fontface = "bold") +  # Жирный шрифт
    #scale_fill_gradient(low = "#74A9BE", high = "#BBDEFB") + 
    scale_fill_gradient(low = "#BBDEFB", high = "#0D47A1") + 
    labs(title = "Топ-10 цифровых компаний за 2010-2015",
         x = "",                                    # Убираем название оси X
         y = "Интегральный показатель цифровизации (PC1)") +  # Название для оси Y
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      panel.grid.major.x = element_blank(),  # Убираем сетку вдоль оси X
      axis.text.x = element_text(size = 14, face = "bold") # Названия по оси X
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) # Обеспечиваем расширение вертикальной оси
  
  # Отображаем график
  rank_plot
  
  
  
  

  
  
  
  
  
  