analyze_sentence_full <- function(sentence) {
  result <- list(sentence = sentence)
  sentence_lower <- tolower(sentence)

  # Step 0: 명령문 판단 + 유효 동사 체크
  result$step0_imperative <- grepl("^please\\s+[a-z]+|^[a-z]+\\s+", sentence_lower)
  if (result$step0_imperative) {
    verb <- strsplit(sentence_lower, "\\s+")[[1]][ifelse(grepl("^please", sentence_lower), 2, 1)]
    result$verb_after_please <- verb
    result$valid_verb_form <- !grepl("s$|ed$", verb)
    if (!result$valid_verb_form) {
      result$final <- "❌ 명령문 구조 오류"
      return(as.data.frame(result))
    }
  }

  # Step 1: 주어 다음 동사 자리 여부
  result$step1_subject_verb_order <- grepl("(he|she|it|they|we|you|mr\\.|ms\\.|the .*?|[a-z]+)\\s+[a-z]+", sentence_lower)

  # Step 2: 가짜 동사 제거
  result$step2_fake_verb1 <- !grepl("(who|which|that).*\\s+[a-z]+", sentence_lower)
  result$step2_fake_verb2 <- !grepl("can|may|should|would|will", sentence_lower)

  # Step 3: 수일치
  singular_subject_patterns <- c("to ", "ing", "that", "amount", "information", "advice", "news", 
                                "equipment", "furniture", "homework", "data", "team", "group")
  must_be_singular <- any(sapply(singular_subject_patterns, grepl, sentence_lower))
  result$step3_agreement <- if (result$step0_imperative) {
    TRUE  # 명령문은 수일치 검사 제외
  } else if (must_be_singular) {
    grepl("\\s+is\\s+|\\s+has\\s+|\\s+[a-z]+s\\s+", sentence_lower)
  } else {
    grepl("\\s+are\\s+|\\s+have\\s+|\\s+[a-z]+\\s+", sentence_lower)
  }

  # Step 4: 수동태 패턴 확인
  passive_3_prep <- c("be surprised at", "be satisfied with", "be filled with", 
                      "be equipped with", "be involved with", "be interested in", 
                      "be known for", "be known as", "be based on", "be based in")
  passive_4 <- c("shown", "given", "told", "offered", "sent", "invoiced")
  passive_5_inf <- c("asked", "advised", "allowed", "required", "expected", "encouraged", "permitted")
  result$step4_passive_3 <- any(sapply(passive_3_prep, grepl, sentence_lower)) || grepl("be\\s+[a-z]+ed\\s+(at|on|with|for|in)", sentence_lower)
  result$step4_passive_4 <- any(sapply(passive_4, grepl, sentence_lower)) && grepl("to\\s+[a-z]+", sentence_lower)
  result$step4_passive_5 <- any(sapply(passive_5_inf, grepl, sentence_lower)) && grepl("to ", sentence_lower)
  result$step4_passive_general <- grepl("be\\s+[a-z]+ed\\s+|was\\s+[a-z]+ed\\s+", sentence_lower)

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
  ) || result$step0_imperative  # 명령문은 시제 단서 없어도 유효
  result$step5_tense_form <- if (result$step0_imperative) {
    TRUE  # 명령문은 동사 원형 사용
  } else if (any(sapply(present, grepl, sentence_lower))) {
    grepl("\\s+[a-z]+s\\s+|\\s+is\\s+|\\s+are\\s+", sentence_lower)
  } else if (any(sapply(past, grepl, sentence_lower))) {
    grepl("\\s+[a-z]+ed\\s+", sentence_lower)
  } else if (any(sapply(future, grepl, sentence_lower))) {
    grepl("\\s+will\\s+[a-z]+\\s+", sentence_lower)
  } else {
    TRUE
  }

  # Final decision
  step_passes <- unlist(result[c(
    "step1_subject_verb_order",
    "step2_fake_verb1",
    "step2_fake_verb2",
    "step3_agreement",
    "step5_tense_detected",
    "step5_tense_form"
  )], use.names = FALSE)
  result$final <- if (any(isFALSE(step_passes))) "❌ 문법 오류 가능성 있음" else "✅ 문법상 가능"
  result$passive_detected <- result$step4_passive_3 || result$step4_passive_4 || result$step4_passive_5 || result$step4_passive_general

  return(as.data.frame(result))
}

# 문장 입력 예시
analyze_sentence_full("Please take the survey.")
analyze_sentence_full("The team monitors the system.")
analyze_sentence_full("The client was invoiced $500.")
