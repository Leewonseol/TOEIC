# 패키지: dplyr, readr, tibble (한 번만 설치)
# install.packages(c("dplyr", "readr", "tibble"))

library(dplyr)
library(readr)
library(tibble)

# 1) 콜로케이션 데이터 ------------------------------------
collocs <- tribble(
  ~unit, ~en,                          ~ko,
  1, "reserve the right to",           "~할 권리를 보유하다",
  1, "perform a task",                 "업무를 수행하다",
  1, "discontinue production",         "생산을 중단하다",
  1, "provide leadership",             "리더십을 발휘하다",
  1, "acknowledge receipt of",         "수령을 확인하다",
  1, "fill one’s order",               "주문을 이행하다",
  1, "address a problem",              "문제를 처리하다",
  1, "call/hold a meeting",            "회의를 소집하다",
  1, "fill out a questionnaire",       "설문지를 작성하다",
  1, "place an advertisement",         "광고를 내다",
  1, "raise funds",                    "자금을 모으다",
  1, "establish a presence in",        "~에 입지를 구축하다",
  2, "extend one’s gratitude",         "감사를 표하다",
  2, "display initiative",             "진취성을 보이다",
  2, "issue a refund",                 "환불하다",
  2, "obtain approval",                "승인을 받다",
  2, "pursue a degree",                "학위를 취득하다",
  2, "settle a dispute",               "분쟁을 해결하다",
  2, "accept an offer",                "제안을 수락하다",
  2, "pay a visit",                    "방문하다",
  2, "extend an invitation",           "초청하다",
  2, "have reservations about",        "~에 대해 의구심을 갖다",
  2, "accommodate one’s needs",        "필요를 충족하다",
  2, "voice one’s concern",            "우려를 표명하다",
  2, "secure a job",                   "일자리를 얻다"
)

# 2) 히스토리 파일 경로 ------------------------------------
history_file <- "colloc_history.csv"

# 3) 최근 오답 위주로 퀴즈 풀기 -----------------------------
quiz_collocations <- function(
  items = collocs,
  n = 5,
  focus = c("wrong", "all"),
  history_path = history_file
) {
  focus <- match.arg(focus)

  if (file.exists(history_path)) {
    hist <- readr::read_csv(history_path, show_col_types = FALSE)
  } else {
    hist <- tibble(en = character(), correct = logical(),
                   timestamp = as.POSIXct(character()), unit = integer(), ko = character())
  }

  # 각 항목의 '가장 최근 결과' 계산
  last_res <- hist |>
    group_by(en) |>
    slice_max(order_by = timestamp, n = 1, with_ties = FALSE) |>
    ungroup()

  # 오답만 풀기 모드인 경우, 최근에 틀린(en)만 남김
  wrong_pool <- last_res |>
    filter(!correct) |>
    select(en)

  pool <- if (focus == "wrong" && nrow(wrong_pool) > 0) {
    items |> semi_join(wrong_pool, by = "en")
  } else {
    items
  }

  if (nrow(pool) == 0) {
    message("오답풀이 대상이 없습니다. 전체에서 출제합니다.")
    pool <- items
  }

  # n개 샘플
  qs <- pool |>
    slice_sample(n = min(n, nrow(pool)))

  # 콘솔 퀴즈 루프
  results <- vector("list", length = nrow(qs))
  cat("\n=== 콜로케이션 퀴즈 시작 ===\n",
      "모르는 경우 그냥 Enter 누르고 정답 확인 후, 맞았는지 y/n로 체크하세요.\n\n", sep = "")

  for (i in seq_len(nrow(qs))) {
    q <- qs[i, ]
    cat(sprintf("[%d/%d] %s  (단원 %d)\n", i, nrow(qs), q$en, q$unit))
    invisible(readline(prompt = "뜻이 뭐예요? (Enter = 정답 보기) "))
    cat("정답: ", q$ko, "\n", sep = "")
    yn <- readline(prompt = "맞았나요? (y/n): ")
    got <- tolower(trimws(yn)) %in% c("y", "yes")

    results[[i]] <- tibble(
      en = q$en,
      ko = q$ko,
      unit = q$unit,
      correct = got,
      timestamp = Sys.time()
    )
    cat("\n")
  }

  # 히스토리 저장
  hist_new <- bind_rows(hist, bind_rows(results))
  readr::write_csv(hist_new, history_path)
  cat("진행 완료! 기록이 저장되었습니다 -> ", normalizePath(history_path), "\n", sep = "")

  # 요약
  summary <- bind_rows(results) |>
    summarise(total = n(), correct = sum(correct), wrong = sum(!correct))
  print(summary)

  invisible(bind_rows(results))
}

# 4) 오답 복습 리스트 ---------------------------------------
list_recent_wrongs <- function(history_path = history_file, limit = 10) {
  if (!file.exists(history_path)) {
    message("히스토리가 아직 없습니다.")
    return(invisible(NULL))
  }
  hist <- readr::read_csv(history_path, show_col_types = FALSE)

  last_res <- hist |>
    group_by(en, ko, unit) |>
    slice_max(order_by = timestamp, n = 1, with_ties = FALSE) |>
    ungroup()

  last_res |>
    filter(!correct) |>
    arrange(desc(timestamp)) |>
    head(limit) |>
    select(timestamp, unit, en, ko)
}

# 사용 예시:
# quiz_collocations(n = 7, focus = "wrong")   # 최근 오답 위주로 7문제
# quiz_collocations(n = 10, focus = "all")    # 전체에서 랜덤 10문제
# list_recent_wrongs()                         # 최근 오답 목록 보기
