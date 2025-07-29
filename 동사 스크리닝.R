analyze_sentence_full <- function(sentence) {
  result <- list(sentence = sentence)
  sentence_lower <- tolower(sentence)

  # Step 0: 명령문 판단 + 유효 동사 체크
  result$step0_imperative <- grepl("^please\\s+[a-z]+", sentence_lower)
  if (result$step0_imperative) {
    verb <- strsplit(sentence_lower, "\\s+")[[1]][2]
    result$verb_after_please <- verb
    result$valid_verb_form <- !grepl("s$|ed$", verb)
    if (!result$valid_verb_form) {
      result$final <- "❌ 명령문 구조 오류"
      return(as.data.frame(result))
    }
  }

  # Step 1: 주어 다음 동사 자리 여부 (간단한 위치 기반)
  result$step1_subject_verb_order <- grepl("(he|she|it|they|we|you|mr\\.|ms\\.|the .*?)\\s+[a-z]+", sentence_lower)

  # Step 2: 가짜 동사 제거
  result$step2_fake_verb1 <- !grepl("(who|which|that).*\\s+[a-z]+", sentence_lower)
  result$step2_fake_verb2 <- !grepl("please\\s+[a-z]+|can|may|should|would|will", sentence_lower)

  # Step 3: 수일치
  # ✅ 설명: 불가산 명사 또는 단수 취급해야 하는 주어 패턴
# "uncountable noun"은 설명일 뿐, 실제 매칭에 사용되지 않음
singular_subject_patterns <- c(
  "to ",     # to 부정사
  "ing",     # 동명사
  "that",    # that절
  # 아래는 대표적인 불가산 명사
  "amount", "information", "advice", "news", 
  "equipment", "furniture", "homework", "data"
)

# 주어가 단수로 간주될 수 있는지 확인
must_be_singular <- any(sapply(singular_subject_patterns, grepl, sentence_lower))


  # Step 4: 수동태 패턴 확인
  passive_3_prep <- c("be surprised at", "be satisfied with", "be filled with", 
                      "be equipped with", "be involved with", "be interested in", 
                      "be known for", "be known as", "be based on", "be based in")
  passive_5_inf <- c("asked", "advised", "allowed", "required", "expected", "encouraged", "permitted")
  result$step4_passive_3 <- any(sapply(passive_3_prep, grepl, sentence_lower))
  result$step4_passive_5 <- any(sapply(passive_5_inf, grepl, sentence_lower)) && grepl("to ", sentence_lower)

  # Step 5: 시제 단서
  present <- c("every", "always", "usually", "generally", "frequently", "sometimes")
  present_perfect <- c("since", "for the last", "for the past", "lately", "recently")
  past <- c("yesterday", "ago", "last", "previously", "recently")
  future <- c("next", "tomorrow", "shortly", "soon", "will")
  future_perfect <- c("will have", "by the time", "by")
  past_perf <- grepl("after .* had .*", sentence_lower) || grepl("before .* had .*", sentence_lower)

  result$step5_tense_detected <- any(
    sapply(present, grepl, sentence_lower),
    sapply(present_perfect, grepl, sentence_lower),
    sapply(past, grepl, sentence_lower),
    sapply(future, grepl, sentence_lower),
    sapply(future_perfect, grepl, sentence_lower),
    past_perf
  )

  # Final decision
  step_passes <- unlist(result[c(
    "step1_subject_verb_order",
    "step2_fake_verb1",
    "step2_fake_verb2",
    "step3_agreement",
    "step4_passive_3",
    "step4_passive_5",
    "step5_tense_detected"
  )], use.names = FALSE)
  result$final <- if (any(isFALSE(step_passes))) "❌ 문법 오류 가능성 있음" else "✅ 문법상 가능"

  return(as.data.frame(result))
}
analyze_sentence_full("Please take the survey.")
