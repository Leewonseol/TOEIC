# 0. 필요 패키지 설치·로드
if (!require("DiagrammeR")) {
  install.packages("DiagrammeR")
  library(DiagrammeR)
} else {
  library(DiagrammeR)
}

# 1. 다의어별 의미(범주)와 동의어 리스트
synonyms <- list(
  Account = list(
    explanation = c("report", "narrative", "description"),
    financial   = c("ledger", "bank account", "balance"),
    consider    = c("consider", "reckon", "regard")
  ),
  Assume = list(
    suppose = c("suppose", "presume", "take for granted"),
    take_on = c("undertake", "shoulder", "accept")
  ),
  Assemble = list(
    gather    = c("gather", "collect", "convene"),
    construct = c("construct", "compile", "build")
  ),
  Change = list(
    alteration = c("alteration", "modification", "adjustment"),
    coins      = c("coins", "small change", "loose change"),
    modify     = c("alter", "convert", "transform"),
    replace    = c("exchange", "swap", "substitute")
  ),
  Contribute = list(
    donate = c("donate", "give", "bestow"),
    assist = c("assist", "help", "aid")
  ),
  Concern = list(
    worry   = c("worry", "anxiety", "apprehension"),
    matter  = c("issue", "matter", "affair"),
    worry_v = c("worry", "bother", "trouble"),
    relate  = c("involve", "pertain", "regard")
  ),
  Drawing = list(
    picture   = c("sketch", "illustration", "drawing"),
    pull      = c("pull", "tug", "haul"),
    selection = c("lottery", "selection", "drawing")
  ),
  Endorse = list(
    support = c("support", "back", "advocate"),
    sign    = c("sign", "approve", "authorize")
  ),
  Easily = list(
    easily = c("easily", "readily", "effortlessly")
  ),
  Foundation = list(
    basis          = c("basis", "groundwork", "foundation"),
    institution    = c("institution", "establishment", "academy"),
    understructure = c("footing", "base", "substructure")
  ),
  Hearing = list(
    listening = c("listening", "auditory perception", "sound"),
    legal     = c("hearing", "trial", "proceeding")
  ),
  Heavy = list(
    weighty = c("weighty", "hefty", "massive"),
    severe  = c("extreme", "intense", "serious")
  ),
  Minute = list(
    time  = c("moment", "instant", "second"),
    small = c("tiny", "minuscule", "microscopic")
  ),
  Original = list(
    first     = c("first", "primary", "initial"),
    novel     = c("novel", "innovative", "fresh"),
    prototype = c("prototype", "exemplar", "original")
  ),
  Outlet = list(
    store      = c("shop", "store", "vendor"),
    electrical = c("socket", "receptacle", "power point"),
    vent       = c("vent", "opening", "passage")
  ),
  Plant = list(
    factory = c("factory", "mill", "industrial plant"),
    flora   = c("flora", "vegetation", "greenery"),
    place   = c("place", "set", "install")
  ),
  Party = list(
    celebration = c("celebration", "festivity", "gathering"),
    group       = c("side", "faction", "group"),
    celebrate   = c("celebrate", "fete", "revel")
  ),
  Performance = list(
    show       = c("show", "recital", "presentation"),
    efficiency = c("efficiency", "output", "effectiveness")
  ),
  Presence = list(
    existence = c("existence", "being", "presence"),
    attendance = c("attendance", "turnout", "presence"),
    aura      = c("aura", "air", "vibe")
  ),
  Practice = list(
    habit     = c("habit", "custom", "routine"),
    rehearsal = c("rehearsal", "drill", "exercise"),
    perform   = c("perform", "execute", "carry out")
  ),
  Receipt = list(
    proof   = c("proof", "voucher", "ticket"),
    revenue = c("revenue", "income", "proceeds")
  ),
  Reception = list(
    welcome = c("welcome", "greeting", "reception"),
    party   = c("celebration", "gathering", "party"),
    signal  = c("signal", "reception", "pickup")
  ),
  Room = list(
    space       = c("space", "area", "chamber"),
    opportunity = c("chance", "opportunity", "scope")
  ),
  Run = list(
    sprint  = c("sprint", "dash", "race"),
    operate = c("operate", "manage", "conduct"),
    period  = c("run", "stint", "stretch"),
    flow    = c("flow", "stream", "current")
  ),
  Story = list(
    tale  = c("tale", "narrative", "account"),
    level = c("floor", "storey", "level"),
    news  = c("news", "report", "article")
  ),
  Sensitive = list(
    delicate   = c("delicate", "vulnerable", "fragile"),
    perceptive = c("perceptive", "insightful", "subtle")
  ),
  Step = list(
    stride  = c("stride", "pace", "step"),
    measure = c("action", "measure", "procedure"),
    move    = c("move", "proceed", "advance")
  ),
  Suspend = list(
    hang  = c("hang", "dangle", "suspend"),
    pause = c("pause", "interrupt", "adjourn"),
    expel = c("expel", "exclude", "bar")
  ),
  Volume = list(
    amount   = c("amount", "quantity", "capacity"),
    book     = c("tome", "publication", "volume"),
    loudness = c("loudness", "amplitude", "intensity")
  )
)

# 2. DOT 문자열 생성 함수
create_tree_dot <- function(word, senses) {
  nodes <- character()
  edges <- character()
  # 루트 노드
  nodes <- c(nodes,
    sprintf("\"%s\" [label=\"%s\", shape=box, style=filled, fillcolor=LightBlue];",
            word, word)
  )
  # 의미(sense)와 동의어 노드
  for (sense in names(senses)) {
    sense_id <- paste0(word, "_", sense)
    nodes <- c(nodes,
      sprintf("\"%s\" [label=\"%s\", shape=oval, style=filled, fillcolor=LightGray];",
              sense_id, sense)
    )
    edges <- c(edges,
      sprintf("\"%s\" -> \"%s\";", word, sense_id)
    )
    for (syn in senses[[sense]]) {
      syn_id <- gsub(" ", "_", paste0(word, "_", sense, "_", syn))
      nodes <- c(nodes,
        sprintf("\"%s\" [label=\"%s\", shape=plaintext];",
                syn_id, syn)
      )
      edges <- c(edges,
        sprintf("\"%s\" -> \"%s\";", sense_id, syn_id)
      )
    }
  }
  dot <- paste(
    "digraph SynonymTree {",
    "  rankdir = TB;",            # Top→Bottom
    "  node [fontname = Helvetica];",
    paste(nodes, collapse = "\n  "),
    paste(edges, collapse = "\n  "),
    "}", sep = "\n"
  )
  return(dot)
}

# 3. 각 단어별로 DiagrammeR 그래프 생성
for (w in names(synonyms)) {
  dot <- create_tree_dot(w, synonyms[[w]])
  print(DiagrammeR::grViz(dot))
}
