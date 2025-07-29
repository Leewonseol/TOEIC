# 설치가 필요한 경우
# install.packages("DiagrammeR")

library(DiagrammeR)

grViz("
digraph verb_screening {
  # 왼쪽→오른쪽 방향
  graph [rankdir = LR]

  # 기본 노드 스타일 정의
  node [shape = diamond, 
        style = filled, 
        fillcolor = lightgrey, 
        fontname = Helvetica,
        fontsize = 12]

  # 시작·끝 노드
  Start [shape = oval, label = '시작', fillcolor = paleturquoise]
  End   [shape = oval, label = '정답', fillcolor = palegreen]

  # 의사결정 노드
  Position  [label = '동사 자리?']
  Fake      [label = '가짜 동사?\n(부정사, 동명사, 분사)']
  Agreement [label = '수일치?']
  Voice     [label = '태?\n(능동/수동)']
  Tense     [label = '시제?']

  # 흐름 정의
  Start -> Position -> Fake -> Agreement -> Voice -> Tense -> End
}
")
