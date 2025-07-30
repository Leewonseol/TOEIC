# 1. 필요 패키지 설치 및 로드
if (!require(data.tree)) {
  install.packages("data.tree")
  library(data.tree)
} else {
  library(data.tree)
}

# 2. 단어별 품사·동의어 리스트 정의
synonyms <- list(
  Notice   = list(noun = c("notification", "announcement"),
                  verb = c("observe", "note")),
  Name     = list(noun = c("designation", "title"),
                  verb = c("label", "dub")),
  Outline  = list(noun = c("summary", "synopsis"),
                  verb = c("summarize", "draft")),
  Interest = list(noun = c("curiosity", "stake"),
                  verb = c("intrigue", "engage")),
  Level    = list(noun = c("tier", "degree"),
                  adjective = c("even", "flat"),
                  verb = c("flatten", "equalize")),
  Ruin     = list(verb = c("destroy", "spoil"),
                  noun = c("destruction", "collapse")),
  About    = list(preposition = c("regarding", "concerning"),
                  adverb      = c("approximately", "roughly")),
  Grant    = list(verb = c("give", "award"),
                  noun = c("endowment", "allowance")),
  Deposit  = list(noun = c("down payment", "security"),
                  verb = c("place", "store")),
  Mark     = list(noun  = c("symbol", "spot"),
                  verb1 = c("grade", "evaluate"),
                  verb2 = c("notice", "observe")),
  Rate     = list(noun = c("ratio", "charge"),
                  verb = c("assess", "evaluate")),
  Monitor  = list(noun = c("screen", "display"),
                  verb = c("observe", "track")),
  Regular  = list(noun = c("customer", "patron"),
                  verb = c("standardize", "normalize")),
  Charge   = list(noun1 = c("fee", "price"),
                  noun2 = c("attack", "assault"),
                  verb1 = c("bill", "charge (rush)"),  # “rush” 의미를 함께 표기
                  verb2 = c("rush", "hurry")),
  Professional = list(noun      = c("expert", "specialist"),
                      adjective = c("qualified", "skilled")),
  Form     = list(noun1 = c("shape", "structure"),
                  noun2 = c("application", "document"),
                  verb  = c("create", "formulate")),
  Document = list(noun = c("paper", "record"),
                  verb = c("record", "log")),
  Commercial = list(adjective = c("advertising", "business"),
                    noun      = c("ad", "advertisement")),
  Select   = list(noun      = c("elite", "choice"),
                  adjective = c("chosen", "exclusive")),
  Secure   = list(adjective = c("safe", "protected"),
                  verb      = c("lock", "obtain")),
  View     = list(noun1 = c("sight", "scene"),
                  noun2 = c("opinion", "perspective"),
                  verb1 = c("see", "watch"),
                  verb2 = c("consider", "regard")),
  Complete = list(adjective = c("finished", "whole"),
                  verb1     = c("finish", "conclude"),
                  verb2     = c("fulfill", "accomplish")),
  Store    = list(noun = c("shop", "repository"),
                  verb = c("save", "stock")),
  Resume   = list(noun = c("CV", "summary"),
                  verb = c("restart", "continue")),
  Last     = list(adjective = c("final", "remaining"),
                  adverb    = c("finally", "ultimately"),
                  verb      = c("persist", "continue")),
  Wear     = list(verb1 = c("don", "put on"),
                  verb2 = c("erode", "abrade"),
                  noun  = c("clothing", "outfit")),
  Lower    = list(adjective = c("inferior", "reduced"),
                  verb      = c("reduce", "decrease")),
  Alert    = list(adjective = c("aware", "vigilant"),
                  noun      = c("warning", "alarm"),
                  verb      = c("notify", "warn")),
  Feature  = list(noun = c("characteristic", "attribute"),
                  verb = c("highlight", "showcase")),
  Estimate = list(noun = c("approximation", "assessment"),
                  verb = c("calculate", "approximate")),
  Cause    = list(noun = c("reason", "motive"),
                  verb = c("induce", "bring about")),
  Ease     = list(verb = c("alleviate", "reduce"),
                  noun = c("comfort", "relief"))
)

# 3. 트리 생성
root <- Node$new("Synonym Tree")
for (word in names(synonyms)) {
  # 단어 노드
  wordNode <- root$AddChild(word)
  # 품사별 하위 노드 및 동의어 추가
  for (pos in names(synonyms[[word]])) {
    posNode <- wordNode$AddChild(pos)
    for (syn in synonyms[[word]][[pos]]) {
      posNode$AddChild(syn)
    }
  }
}

# 4. 트리 출력 및 시각화
print(root, "level", "name")   # 터미널에 텍스트 형태로 출력
# plot(root)                  # plotting이 지원되는 환경이라면 시각화도 가능
