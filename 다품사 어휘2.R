# 0. 필요 패키지 설치·로드 --------------------------------
for (pkg in c("DiagrammeR")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# 1. 단어별 품사·동의어 리스트 정의 ---------------------------
synonyms <- list(
  Notice      = list(noun = c("notification", "announcement"),
                     verb = c("observe", "note")),
  Name        = list(noun = c("designation", "title"),
                     verb = c("label", "dub")),
  Outline     = list(noun = c("summary", "synopsis"),
                     verb = c("summarize", "draft")),
  Interest    = list(noun = c("curiosity", "stake"),
                     verb = c("intrigue", "engage")),
  Level       = list(noun      = c("tier", "degree"),
                     adjective = c("even", "flat"),
                     verb      = c("flatten", "equalize")),
  Ruin        = list(verb = c("destroy", "spoil"),
                     noun = c("destruction", "collapse")),
  About       = list(preposition = c("regarding", "concerning"),
                     adverb      = c("approximately", "roughly")),
  Grant       = list(verb = c("give", "award"),
                     noun = c("endowment", "allowance")),
  Deposit     = list(noun = c("down payment", "security"),
                     verb = c("place", "store")),
  Mark        = list(noun1 = c("symbol", "spot"),
                     verb1 = c("grade", "evaluate"),
                     verb2 = c("notice", "observe")),
  Rate        = list(noun = c("ratio", "charge"),
                     verb = c("assess", "evaluate")),
  Monitor     = list(noun = c("screen", "display"),
                    (verb = c("supervise"),
  Regular     = list(noun = c("customer", "patron"),
                     verb = c("standardize", "normalize")),
  Charge      = list(noun1 = c("fee", "price"),
                     noun2 = c("attack", "assault"),
                     verb1 = c("bill", "invoice","debit),
                     verb2 = c("rush", "hurry")),
  Professional= list(noun      = c("expert", "specialist"),
                     adjective = c("qualified", "skilled")),
  Form        = list(noun1 = c("shape", "structure"),
                     noun2 = c("application", "document"),
                     verb  = c("create", "formulate")),
  Document    = list(noun = c("paper", "record"),
                     verb = c("record", "log")),
  Commercial  = list(adjective = c("trade", "business-related"),
                     noun      = c("ad", "advertisement")),
  Select      = list(noun      = c("elite", "choice"),
                     adjective = c("chosen", "exclusive")),
  Secure      = list(adjective = c("safe", "protected"),
                     verb      = c("lock", "obtain")),
  View        = list(noun1 = c("sight", "scene"),
                     noun2 = c("opinion", "perspective"),
                     verb1 = c("see", "watch"),
                     verb2 = c("consider", "regard")),
  Complete    = list(adjective = c("finished", "whole"),
                     verb1     = c("finish", "conclude"),
                     verb2     = c("fulfill", "accomplish")),
  Store       = list(noun = c("shop", "repository"),
                     verb = c("save", "stock")),
  Resume      = list(noun = c("CV", "summary"),
                     verb = c("restart", "continue")),
  Last        = list(adjective = c("final", "remaining"),
                     adverb    = c("finally", "ultimately"),
                     verb      = c("persist", "continue")),
  Wear        = list(verb1 = c("don", "put on"),
                     verb2 = c("erode", "abrade"),
                     noun  = c("clothing", "outfit")),
  Lower       = list(adjective = c("inferior", "reduced"),
                     verb      = c("reduce", "decrease")),
  Alert       = list(adjective = c("aware", "vigilant"),
                     noun      = c("warning", "alarm"),
                     verb      = c("notify", "warn")),
  Feature     = list(noun = c("characteristic", "attribute"),
                     verb = c("highlight", "showcase")),
  Estimate    = list(noun = c("approximation", "assessment"),
                     verb = c("calculate", "approximate")),
  Cause       = list(noun = c("reason", "motive"),
                     verb = c("induce", "bring about")),
  Ease        = list(verb = c("alleviate", "reduce"),
                     noun = c("comfort", "relief"))
)

# 2. DOT 문자열 생성 함수 ---------------------------
create_tree_dot <- function(word, senses) {
  nodes <- edges <- character()
  # 루트 노드
  nodes <- c(nodes,
    sprintf("\"%s\" [label=\"%s\", shape=box, style=filled, fillcolor=LightBlue];",
            word, word))
  # 품사·동의어 노드
  for (pos in names(senses)) {
    pos_id <- paste0(word, "::", pos)
    nodes <- c(nodes,
      sprintf("\"%s\" [label=\"%s\", shape=oval, style=filled, fillcolor=LightGray];",
              pos_id, pos))
    edges <- c(edges,
      sprintf("\"%s\" -> \"%s\";", word, pos_id))
    for (syn in senses[[pos]]) {
      syn_id <- gsub("[^A-Za-z0-9]", "_", paste(word, pos, syn, sep="_"))
      nodes <- c(nodes,
        sprintf("\"%s\" [label=\"%s\", shape=plaintext];",
                syn_id, syn))
      edges <- c(edges,
        sprintf("\"%s\" -> \"%s\";", pos_id, syn_id))
    }
  }
  paste(
    "digraph SynTree {",
    "  rankdir = TB;",
    "  node [fontname=\"Helvetica\"];",
    paste(nodes, collapse="\n  "),
    paste(edges, collapse="\n  "),
    "}", sep="\n"
  )
}

# 3. 한 화면에 하나씩 순차 출력 ---------------------------
for (w in names(synonyms)) {
  dot <- create_tree_dot(w, synonyms[[w]])
  gr <- DiagrammeR::grViz(dot)
  
  # 화면에 출력
  print(gr)
  
  # 다음 트리 보기 전까지 대기
  invisible(readline(prompt = "▶ Press <enter> to view next tree..."))
}
