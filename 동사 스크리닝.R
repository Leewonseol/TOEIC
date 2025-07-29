library(quanteda)
library(caret)
library(e1071)
library(udpipe)

#— 2. 예제 데이터 준비 —#
df <- data.frame(
  text = c(
    "He reads books.", 
    "To read books is fun.", 
    "All reports will have been reviewed.",
    "She is reading a novel.",
    "They have finished the work.",
    "Reading helps you learn.",
    "John usually arrive early for meetings.",
    "The students in the laboratory have conducted experiments for 6 months.",
    "DR ameliorated depression, suicide, and anger symptoms over time.",
    "These results highlight the role of common cognitive processes, regardless of the specific symptom dimensions, which can be utilized to devise a feasible and effective transdiagnostic psychosocial intervention within, and probably outside, the pandemic."
  ),
  label_position  = c(1, 0, 1, 1, 0, 1, 0, 1, 1, 0),  # 예시값, 필요시 수정
  label_fake      = c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1),  # 예시값, 필요시 수정
  label_agreement = c(1, 0, 1, 1, 0, 1, 0, 1, 1, 0),  # 예시값, 필요시 수정
  label_voice     = c(0, 0, 1, 0, 1, 0, 0, 1, 0, 1),  # 예시값, 필요시 수정
  label_tense     = c(1, 0, 1, 1, 0, 1, 0, 1, 1, 0),  # 예시값, 필요시 수정
  stringsAsFactors = FALSE
)

#— 3. 텍스트 전처리 & DFM → TF-IDF —#
corp    <- corpus(df$text)
toks    <- tokens(corp, remove_punct = TRUE) %>%
           tokens_remove(stopwords("en"))
dfm0    <- dfm(toks)
dfm_tfidf <- dfm_tfidf(dfm0)

# caret 호환을 위해 matrix 변환
xmat <- convert(dfm_tfidf, to = "matrix")

#— 4. 학습/검증 설정 —#
set.seed(123)
train_ctrl <- trainControl(
  method          = "cv",
  number          = 3,
  classProbs      = TRUE,
  summaryFunction = defaultSummary # ROC 대신 Accuracy 등 기본 지표 사용
)

#— 5. 레이블을 factor로 변환 —#
df$label_position  <- factor(df$label_position,  levels = c(0,1), labels = c("no","yes"))
df$label_fake      <- factor(df$label_fake,      levels = c(0,1), labels = c("no","yes"))
df$label_agreement <- factor(df$label_agreement, levels = c(0,1), labels = c("no","yes"))
df$label_voice     <- factor(df$label_voice,     levels = c(0,1), labels = c("no","yes"))
df$label_tense     <- factor(df$label_tense,     levels = c(0,1), labels = c("no","yes"))

#— 6. 단계별 모델 학습 —#
model_position <- train(
  x         = xmat,
  y         = df$label_position,
  method    = "glm",
  family    = "binomial",
  trControl = train_ctrl,
  metric    = "Accuracy"
)
model_fake <- train(
  x         = xmat,
  y         = df$label_fake,
  method    = "svmLinear",
  trControl = train_ctrl,
  metric    = "Accuracy"
)
model_agreement <- train(
  x         = xmat,
  y         = df$label_agreement,
  method    = "glm",
  family    = "binomial",
  trControl = train_ctrl,
  metric    = "Accuracy"
)
model_voice <- train(
  x         = xmat,
  y         = df$label_voice,
  method    = "svmLinear",
  trControl = train_ctrl,
  metric    = "Accuracy"
)
model_tense <- train(
  x         = xmat,
  y         = df$label_tense,
  method    = "glm",
  family    = "binomial",
  trControl = train_ctrl,
  metric    = "Accuracy"
)

#— 7. 평가 예시 —#
prob_position  <- predict(model_position,  xmat, type = "prob")[, "yes"]
prob_fake      <- predict(model_fake,      xmat, type = "prob")[, "yes"]
prob_agreement <- predict(model_agreement, xmat, type = "prob")[, "yes"]
prob_voice     <- predict(model_voice,     xmat, type = "prob")[, "yes"]
prob_tense     <- predict(model_tense,     xmat, type = "prob")[, "yes"]

cm_pos  <- confusionMatrix(predict(model_position, xmat),  df$label_position,  positive = "yes")
cm_fake <- confusionMatrix(predict(model_fake,     xmat),  df$label_fake,      positive = "yes")
cm_ag   <- confusionMatrix(predict(model_agreement, xmat), df$label_agreement, positive = "yes")
cm_vo   <- confusionMatrix(predict(model_voice,     xmat), df$label_voice,     positive = "yes")
cm_te   <- confusionMatrix(predict(model_tense,     xmat), df$label_tense,     positive = "yes")

print(cm_pos)
print(cm_fake)
print(cm_ag)
print(cm_vo)
print(cm_te)
#— 8. 시각화 —#
# 필요 패키지 로드
# install.packages(c("ggplot2","reshape2"))
library(ggplot2)
library(reshape2)

# 8.1 단계별 Confusion Matrix 히트맵
cms <- list(
  Position  = cm_pos$table,
  Fake      = cm_fake$table,
  Agreement = cm_ag$table,
  Voice     = cm_vo$table,
  Tense     = cm_te$table
)

for(stage in names(cms)) {
  # table → long 포맷
  df_cm <- melt(cms[[stage]])
  colnames(df_cm) <- c("Prediction","Reference","Freq")
  
  p <- ggplot(df_cm, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 6) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(
      title = paste0(stage, " Stage\nConfusion Matrix"),
      x     = "Actual",
      y     = "Predicted"
    ) +
    theme_minimal() +
    theme(
      plot.title     = element_text(hjust = 0.5),
      axis.text      = element_text(size = 11),
      axis.title     = element_text(size = 12),
      legend.position = "none"
    )
  print(p)
}

# 8.2 Accuracy / Sensitivity / Specificity 바 차트
metrics <- data.frame(
  Stage       = names(cms),
  Accuracy    = c(cm_pos$overall["Accuracy"],
                  cm_fake$overall["Accuracy"],
                  cm_ag$overall["Accuracy"],
                  cm_vo$overall["Accuracy"],
                  cm_te$overall["Accuracy"]),
  Sensitivity = c(cm_pos$byClass["Sensitivity"],
                  cm_fake$byClass["Sensitivity"],
                  cm_ag$byClass["Sensitivity"],
                  cm_vo$byClass["Sensitivity"],
                  cm_te$byClass["Sensitivity"]),
  Specificity = c(cm_pos$byClass["Specificity"],
                  cm_fake$byClass["Specificity"],
                  cm_ag$byClass["Specificity"],
                  cm_vo$byClass["Specificity"],
                  cm_te$byClass["Specificity"])
)

# long 포맷으로 변환
df_met <- melt(metrics, id.vars = "Stage",
               variable.name = "Metric",
               value.name    = "Value")

ggplot(df_met, aes(x = Stage, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Value, 2)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 4) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Stage-wise Performance Metrics",
    x     = "Screening Stage",
    y     = "Value"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(hjust = 0.5),
    axis.text.x   = element_text(angle = 15, hjust = 1),
    legend.title  = element_blank()
  )
