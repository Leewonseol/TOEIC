# 0. 필요 패키지 설치·로드
for (pkg in c("data.tree", "igraph", "ggraph")) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 1. 단어별 품사·동의어 리스트 정의
synonyms <- list(
  Manual    = list(noun      = c("handbook", "guide"),
                   adjective = c("written", "hand-operated")),
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

# 2. 상→하 트리 + depth별 폰트 크기 적용 함수 (v5)
plot_syn_tree_v5 <- function(word, syn_list) {
  # 2-1. data.tree 로 노드 생성
  root <- data.tree::Node$new(word)
  for (pos in names(syn_list[[word]])) {
    pos_node <- root$AddChild(pos)
    for (syn in syn_list[[word]][[pos]]) {
      pos_node$AddChild(syn)
    }
  }

  # 2-2. ToDataFrameNetwork → igraph
  edges_df <- data.tree::ToDataFrameNetwork(root)
  g        <- igraph::graph_from_data_frame(edges_df, directed = TRUE)
  V(g)$label <- basename(V(g)$name)

  # 2-3. layout_as_tree 로 좌표 계산 (루트가 위쪽)
  coords_raw <- igraph::layout_as_tree(g,
                                       root     = which(V(g)$label == word),
                                       circular = FALSE)
  colnames(coords_raw) <- c("x", "y")
  # y축 뒤집기: y가 클수록 아래로
  coords_raw[, "y"] <- max(coords_raw[, "y"]) - coords_raw[, "y"]

  # 2-4. 깊이 계산 및 폰트 크기 매핑
  V(g)$depth <- sapply(V(g)$name,
                       function(p) length(strsplit(p, "/")[[1]]) - 1)
  size_map   <- c("0" = 6, "1" = 5, "2" = 4)
  V(g)$font_size <- size_map[as.character(pmin(V(g)$depth, 2))]

  # 2-5. ggraph 로 플로팅
  p <- ggraph::ggraph(g, layout = coords_raw) +
       ggraph::geom_edge_link(color = "gray70", edge_width = 0.8) +
       ggraph::geom_node_point(size = 3, color = "steelblue") +
       ggraph::geom_node_text(aes(label = label, size = font_size),
                              vjust = -0.3, family = "NanumGothic") +
       ggplot2::scale_size_identity() +
       ggplot2::theme_void() +
       ggplot2::ggtitle(paste0("Top–Down Synonym Tree: ", word))

  print(p)
}

# 3. 전체 단어 순차 시각화
for (w in names(synonyms)) {
  plot_syn_tree_v5(w, synonyms)
}
