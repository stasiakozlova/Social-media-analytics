library(vkR)
library(tm)
library(stringr)
library(lda)
library(LDAvis)
library(servr)
vkOAuth(7643106)
setAccessToken("a29a977849e2955112e74473e4eaa62a8542f42dba369ff0baa0045ad5eb2572d56f4194c681b89703e97")

members <- getGroupsMembersExecute('supcolorc')
users <- getUsersExecute(members)

wall <- getWallExecute(domain = "supcolorc", progress_bar = TRUE)
comments <- wallGetCommentsList(wall,progress_bar = TRUE)
users_commented <- sapply(comments,
                          function(comment) data.frame(from_id = comment$from_id))
users_commented <- do.call(rbind, users_commented)
likes <- sapply(wall$posts$id,
                function(post_id) likesGetList(type = 'post', owner_id = '-124540357', item_id = post_id))
users_liked <- getUsersExecute(unlist(likes["items",]))
users_posted <- data.frame(from_id = wall$posts$from_id)
active_users <- getUsersExecute(unique(c(users_commented$from_id, users_posted$from_id, users_liked$id)))
nrow(active_users)

active_users <- getUsersExecute(unique(c(users_commented$from_id, users_posted$from_id, users_liked$id)))
nrow(active_users)

population <- unique(rbind(users, active_users))
nrow(population)

clear_population <- population[is.na(population$deactivated), ]
nrow(clear_population)

members_filtered <- intersect(clear_population$id, users$id)
length(members_filtered)

share_active_members <- length(intersect(active_users$id,users$id)) / nrow(users)
share_active_members

domain <- 'supcolorc'
wall <- getWallExecute(domain = domain, count = 0, progress_bar = TRUE)
metrics <- jsonlite::flatten(wall$posts[c("date", "likes", "comments", "reposts")])
metrics$date <- as.POSIXct(metrics$date, origin="1970-01-01", tz='Europe/Moscow')

library(dplyr)
df <- metrics %>% 
  mutate(period = as.Date(cut(date, breaks='month'))) %>% 
  group_by(period) %>%
  summarise(likes = sum(likes.count), comments = sum(comments.count), reposts = sum(reposts.count), n = n())

install.packages("ggplot2")
install.packages("tidyr")
library(ggplot2)
library(tidyr)
ggplot(data=gather(df, 'type', 'count', 2:5), aes(period, count)) + geom_line(aes(colour=type)) +
  labs(x='Date', y='Count')

wall <- getWallExecute(domain = "supcolorc", count = 10000, progress_bar = TRUE)

posts <- c()
posts <- c(wall, as.vector(posts$body))

# Очищаем текст от "мусора"
posts <- str_replace_all(posts, "[ё]", "е")
posts <- str_replace_all(posts, "[[:punct:]]", " ")
posts <- str_replace_all(posts, "[[:digit:]]", " ")
posts <- str_replace_all(posts, "http\\S+\\s*", " ")
posts <- str_replace_all(posts, "[a-zA-Z]", " ")
posts <- str_replace_all(posts, "\\s+", " ")
posts <- tolower(posts)
posts <- str_trim(posts, side = "both")

write(posts, file='posts_processed.txt')

#АЛГОРИТМ K-MEANS
posts_norm <- readLines("posts_processed_norm.txt", encoding="UTF-8")

# Создаем корпус из сообщений
posts.corpus <- Corpus(VectorSource(posts_norm))

# Избавляемся от стоп-слов
stop_words <- read.table('stop_words_russian.txt', encoding="UTF-8")
stop_words <- as.vector(stop_words$V1)

posts.corpus <- tm_map(posts.corpus, removeWords, stop_words)

# Удаляем все лишние пробельные символы
posts.corpus <- tm_map(posts.corpus, stripWhitespace)

posts.tdm <- TermDocumentMatrix(posts.corpus)
posts.tdm
posts.tdm.non.sparse <- removeSparseTerms(posts.tdm, 0.90)

# Кластеризация документов
N <- 5
model <- kmeans(posts.tdm.non.sparse, N)

for (cluster in 1:N) {
  cat("cluster ", cluster, ": ", as.vector(names(model$cluster[model$cluster == cluster])), "\n")
}

# Построение облака слов
library(wordcloud)
posts.matrix <- as.matrix(posts.tdm.non.sparse)
words.freq <- sort(rowSums(posts.matrix), decreasing = TRUE)
wordcloud(words = names(words.freq), freq = words.freq,
          scale = c(5, 1.0), min.freq = 5, random.order = FALSE,
          rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))

#АЛГОРИТМ LDA
posts_norm <- readLines("posts_norm.txt", encoding="UTF-8")

# Построение таблицы частот
doc.list <- strsplit(posts_norm, "[[:space:]]+")
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# Избавляемся от стоп-слов
stop_words <- read.table('stop_words_russian.txt', encoding="UTF-8")
stop_words <- as.vector(stop_words$V1)

del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# Приводим список терминов к формату понятному lda
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Вычисление некоторых статистик
D <- length(documents)
W <- length(vocab)
doc.length <- sapply(documents, function(x) sum(x[2, ])) 
N <- sum(doc.length)
term.frequency <- as.integer(term.table)

# Параметры модели
K <- 5
G <- 1000
alpha <- 0.02
eta <- 0.02

# Построение модели
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)

# Визуализация модели с помощью LDAVis
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

results <- list(phi = phi,
                theta = theta,
                doc.length = doc.length,
                vocab = vocab,
                term.frequency = term.frequency)

json <- createJSON(phi = results$phi, 
                   theta = results$theta, 
                   doc.length = results$doc.length, 
                   vocab = results$vocab, 
                   term.frequency = results$term.frequency)

serVis(json, out.dir = './', open.browser = TRUE)
