# 0. 필요 패키지 설치·로드
if (!require("DiagrammeR")) {
  install.packages("DiagrammeR")
  library(DiagrammeR)
} else {
  library(DiagrammeR)
}

# 1. 다의어별 의미(범주)와 동의어 리스트
synonyms <- list(
  Appointment = list(
    meeting      = c("meeting", "engagement", "rendezvous"),
    designation  = c("designation", "nomination", "posting"),
    arrangement  = c("arrangement", "booking")
  ),
  Allowance = list(
    stipend  = c("stipend", "allotment", "grant"),
    leeway   = c("margin", "leeway", "tolerance")
  ),
  Appreciation = list(
    gratitude        = c("gratitude", "thankfulness", "recognition"),
    value_increase   = c("increase", "rise", "gain")
  ),
  Area = list(
    region      = c("region", "zone", "district"),
    surface     = c("surface", "extent", "expanse"),
    field       = c("field", "domain", "scope")
  ),
  Authority = list(
    power       = c("power", "control", "dominion"),
    agency      = c("agency", "jurisdiction", "administration"),
    expertise   = c("expertise", "knowledge", "proficiency")
  ),
  Capacity = list(
    ability = c("ability", "capability", "competence"),
    volume  = c("volume", "size", "capacity"),
    role    = c("role", "function", "duty")
  ),
  Balance = list(
    equilibrium    = c("equilibrium", "stability", "harmony"),
    account        = c("funds", "amount", "money"),
    counterbalance = c("counterbalance", "offset", "compensate")
  ),
  Commitment = list(
    obligation = c("obligation", "pledge", "responsibility"),
    dedication = c("dedication", "devotion", "loyalty"),
    promise    = c("promise", "vow", "engagement")
  ),
  Competition = list(
    rivalry = c("rivalry", "contest", "competition"),
    contest = c("contest", "tournament", "match")
  ),
  Course = list(
    route      = c("route", "path", "track"),
    curriculum = c("curriculum", "program", "syllabus"),
    dish       = c("dish", "meal", "plate")
  ),
  Coverage = list(
    insurance = c("insurance", "protection", "coverage"),
    reporting = c("reporting", "broadcasting", "news"),
    extent    = c("extent", "range", "scope")
  ),
  Difference = list(
    distinction    = c("distinction", "variation", "disparity"),
    gap            = c("gap", "space", "interval"),
    disagreement  = c("disagreement", "conflict", "divergence")
  ),
  Effective = list(
    efficient = c("efficient", "competent", "effective"),
    valid     = c("operational", "valid", "in effect")
  ),
  Entry = list(
    admission = c("admission", "entrance", "entry"),
    item      = c("item", "record", "entry"),
    input     = c("input", "data entry", "login")
  ),
  Enclosed = list(
    surrounded = c("surrounded", "enclosed", "encircled"),
    attached   = c("attached", "included", "inserted")
  ),
  Figure = list(
    number    = c("number", "digit", "numeral"),
    person    = c("person", "character", "figure"),
    calculate = c("calculate", "compute", "reckon")
  ),
  Good = list(
    virtuous    = c("virtuous", "ethical", "moral"),
    pleasant    = c("pleasant", "nice", "enjoyable"),
    commodity   = c("product", "item", "goods")
  ),
  Overlook = list(
    miss      = c("miss", "neglect", "ignore"),
    supervise = c("supervise", "oversee", "manage"),
    vantage   = c("vantage point", "viewpoint", "lookout")
  ),
  Installment = list(
    payment = c("payment", "installment", "instalment"),
    series  = c("series", "part", "segment")
  ),
  Initiative = list(
    action     = c("action", "measure", "step"),
    enterprise = c("enterprise", "project", "program"),
    beginning  = c("start", "commencement", "inception")
  ),
  Persistent = list(
    enduring = c("enduring", "lasting", "persevering"),
    stubborn = c("stubborn", "tenacious", "determined")
  ),
  Pick_up = list(
    lift    = c("lift", "hoist"),
    learn   = c("learn", "grasp"),
    improve = c("improve", "recover")
  ),
  Resolution = list(
    decision   = c("decision", "determination", "resolution"),
    clarity    = c("clarity", "definition"),
    settlement = c("settlement", "solution")
  ),
  Rest = list(
    remain = c("remain", "leftover"),
    sleep  = c("sleep", "nap"),
    relief = c("relief", "ease")
  ),
  Replacement = list(
    substitute = c("substitute", "proxy"),
    exchange   = c("exchange", "swap"),
    spare      = c("spare", "backup")
  ),
  Reservation = list(
    booking = c("booking", "reservation", "appointment"),
    doubt   = c("doubt", "misgiving", "reservation")
  ),
  Term = list(
    period    = c("period", "duration", "term"),
    word      = c("word", "expression", "term"),
    condition = c("condition", "stipulation"),
    call      = c("call", "denote")
  )
)

# 2. DOT 문자열 생성 함수
create_tree_dot <- function(word, senses) {
  nodes <- c()
  edges <- c()
  # 루트 노드
  nodes <- c(nodes,
    sprintf("\"%s\" [label=\"%s\", shape=box, style=filled, fillcolor=LightBlue];",
            word, word)
  )
  
  # 각 sense(의미)와 하위 동의어 노드
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
  
  # DOT 완성
  dot <- paste(
    "digraph SynonymTree {",
    "rankdir = TB;",                    # Top→Bottom
    "node [fontname = Helvetica];",
    paste(nodes, collapse = "\n"),
    paste(edges, collapse = "\n"),
    "}", sep = "\n"
  )
  return(dot)
}

# 3. 각 단어별로 DiagrammeR 그래프 생성
for (w in names(synonyms)) {
  dot <- create_tree_dot(w, synonyms[[w]])
  print(DiagrammeR::grViz(dot))
}
