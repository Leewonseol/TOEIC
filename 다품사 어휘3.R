# 0. 필요 패키지 설치·로드 ------------------------------------------------
if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
  install.packages("DiagrammeR")
}
library(DiagrammeR)

# 1. 단어별 품사·동의어 리스트 정의 -------------------------------------------
synonyms <- list(
  Manual    = list(noun      = c("handbook", "guide"),
                   adjective = c("by hand", "hand-operated")),
  Claim     = list(verb1 = c("assert", "maintain"),
                   noun  = c("assertion")),
  Associate = list(noun = c("colleague", "partner"),
                   verb = c("connect", "affiliate")),
  Correct   = list(adjective = c("accurate", "right"),
                   verb      = c("rectify", "amend")),
  Stock     = list(noun1 = c("inventory", "shares"),
                   verb  = c("stockpile", "store")),
  Reserve   = list(verb = c("book", "save", "retain"),
                   noun = c("reservation")),
  Audit     = list(verb = c("inspect", "review"),
                   noun = c("inspection", "examination")),
  Chair     = list(noun1 = c("seat", "chairperson"),
                   verb  = c("preside")),
  Track     = list(noun = c("trail", "path"),
                   verb = c("follow", "trace")),
  Bargain   = list(noun = c("deal", "agreement"),
                   verb = c("negotiate", "haggle")),
  Place     = list(noun  = c("location", "spot"),
                   verb1 = c("put", "situate")),
  Play      = list(verb = c("perform", "act")),
  List      = list(noun = c("directory", "register"),
                   verb = c("enumerate", "catalog")),
  Number    = list(noun = c("digit", "figure"),
                   verb = c("count", "enumerate")),
  Raise     = list(verb = c("lift", "increase", "elevate"),
                   noun = c("increase")),
  Must      = list(verb = c("obligate", "compel"),
                   noun = c("necessity", "obligation")),
  Fine      = list(adjective = c("excellent", "thin"),
                   noun      = c("penalty", "fee"),
                   verb      = c("penalize", "refine")),
  Pending   = list(adjective   = c("awaiting", "undecided"),
                   preposition = c("until", "till")),
  Land      = list(verb = c("obtain", "acquire")),
  Purchase  = list(verb = c("buy", "acquire"),
                   noun = c("acquisition", "procurement")),
  Ship      = list(noun = c("vessel", "boat"),
                   verb = c("transport", "send")),
  Quality   = list(noun      = c("caliber", "standard"),
                   adjective = c("superior", "excellent")),
  Board     = list(noun = c("committee", "plank"),
                   verb = c("embark", "feed")),
  Official  = list(adjective = c("authorized", "formal"),
                   noun      = c("authority", "officer")),
  Leave     = list(verb1 = c("depart", "exit"),
                   verb2 = c("leave behind", "abandon"),
                   noun  = c("vacation", "holiday")),
  Hire      = list(noun = c("recruit", "new employee")),
  Look      = list(noun = c("appearance", "exterior")),
  Clear     = list(verb      = c("remove", "eliminate"),
                   adjective = c("obvious", "transparent"))
)

# 2. DOT 문자열 생성 함수 --------------------------------------------------
#    - root_font, pos_font, leaf_font 으로 깊이별 글씨 크기 조정
create_tree_dot <- function(word, senses,
                            root_font = 16, pos_font = 12, leaf_font = 10) {
  nodes <- edges <- character()
  
  # 2-1. 루트 노드
  nodes <- c(nodes,
    sprintf(
      "\"%s\" [label=\"%s\", shape=box, style=filled, fillcolor=LightBlue, fontsize=%d];",
      word, word, root_font
    )
  )
  
  # 2-2. 품사 → 동의어
  for (pos in names(senses)) {
    pos_id <- paste0(word, "::", pos)
    # 품사 노드
    nodes <- c(nodes,
      sprintf(
        "\"%s\" [label=\"%s\", shape=oval, style=filled, fillcolor=LightGray, fontsize=%d];",
        pos_id, pos, pos_font
      )
    )
    edges <- c(edges,
      sprintf("\"%s\" -> \"%s\";", word, pos_id)
    )
    
    # 동의어 노드
    for (syn in senses[[pos]]) {
      syn_id <- gsub("[^A-Za-z0-9]", "_", paste(word, pos, syn, sep = "_"))
      nodes <- c(nodes,
        sprintf(
          "\"%s\" [label=\"%s\", shape=plaintext, fontsize=%d];",
          syn_id, syn, leaf_font
        )
      )
      edges <- c(edges,
        sprintf("\"%s\" -> \"%s\";", pos_id, syn_id)
      )
    }
  }
  
  # 2-3. 전체 DOT 조립
  dot <- paste(
    "digraph SynTree {",
    "  rankdir = TB;",
    "  node [fontname=\"Helvetica\"];",
    paste(nodes, collapse = "\n  "),
    paste(edges, collapse = "\n  "),
    "}", sep = "\n"
  )
  
  return(dot)
}

# 3. 순차 출력 루프 --------------------------------------------------------
for (w in names(synonyms)) {
  dot <- create_tree_dot(w, synonyms[[w]])
  gr  <- DiagrammeR::grViz(dot)
  
  # 출력
  print(gr)
  
  # Enter 대기
  invisible(readline(prompt = "▶ Press <enter> to view next tree..."))
}
