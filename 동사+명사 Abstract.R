# R 시각화를 위한 필수 패키지 설치 및 로드
# 필요한 패키지가 없다면 주석을 해제하고 설치하세요.
# install.packages("ggplot2")
# install.packages("dplyr")
library(ggplot2)
library(dplyr)

#--- ggplot2 한글 폰트 설정 ---
# 아래의 모든 시각화 코드에서는 Windows 사용자를 위해 기본 한글 폰트를 "Malgun Gothic"으로 설정했습니다.
# macOS 사용자의 경우, 코드 내의 모든 "Malgun Gothic"을 "AppleGothic"으로 변경하여 사용해주세요.
# 만약 폰트가 여전히 적용되지 않는 경우, 스크립트 실행 전 'extrafont' 패키지를 설치하고
# font_import(); loadfonts(device="win")를 실행하는 방법을 시도해볼 수 있습니다.


#_______________________________________________________________________________
#
# Abstract 1: 윤리 인식 캠페인 효과 평가
#_______________________________________________________________________________

# --- 시각화 1-1: 정책 수신 확인 여부에 따른 팀 구성 비교 (카이제곱 검정) ---
abstract1_data_chi <- data.frame(
  Policy_Receipt = factor(c("수신 확인", "수신 미확인", "수신 확인", "수신 미확인"),
                          levels = c("수신 미확인", "수신 확인")),
  Team_Assembled = factor(c("팀 구성함", "팀 구성함", "팀 구성 안함", "팀 구성 안함")),
  Count = c(15, 8, 5, 10) # N=38에 맞춘 가상 빈도, χ²=6.87 경향 반영
)
ggplot(abstract1_data_chi, aes(x = Policy_Receipt, y = Count, fill = Team_Assembled)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Abstract 1-1: 정책 수신 여부에 따른 연구팀 구성 비교",
    subtitle = "정책을 수신한 연구자가 팀을 구성할 확률이 유의미하게 높음 (χ²(1)=6.87, p=.009)",
    x = "정책 수신 확인 여부", y = "연구자 수", fill = "팀 구성 여부"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16), legend.position = "top") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5)

# --- 시각화 1-2: IRB 계약 여부에 따른 규정 준수율 비교 (t-검정) ---
abstract1_data_ttest <- data.frame(
  Group = factor(c("IRB 계약 부서", "비계약 부서")),
  Mean_Compliance = c(89.4, 77.2),
  SD = c(4.1, 6.8)
)
ggplot(abstract1_data_ttest, aes(x = Group, y = Mean_Compliance, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Compliance - SD, ymax = Mean_Compliance + SD), width = 0.2, linewidth = 0.8) +
  labs(
    title = "Abstract 1-2: IRB 계약 여부에 따른 규정 준수율",
    subtitle = "계약 부서의 준수율이 유의미하게 높음 (t(22)=3.12, p=.005)",
    x = "부서 그룹", y = "평균 규정 준수율 (%)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16), legend.position = "none") +
  ylim(0, 100) +
  geom_text(aes(label = paste0(Mean_Compliance, "%")), vjust = -1.5, size = 5)

#_______________________________________________________________________________
#
# Abstract 2: 연구 워크플로우 효율성 연구
#_______________________________________________________________________________

# --- 시각화 2-1: 조기 지원 요청에 따른 과제 정확도 비교 (t-검정) ---
abstract2_data_ttest <- data.frame(
  Group = factor(c("조기 지원 요청", "지원 요청 안함")),
  Mean_Accuracy = c(94.1, 83.3),
  SD = c(3.2, 5.8)
)
ggplot(abstract2_data_ttest, aes(x = Group, y = Mean_Accuracy, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Accuracy - SD, ymax = Mean_Accuracy + SD), width = 0.2, linewidth = 0.8) +
  labs(
    title = "Abstract 2-1: 지원 요청 여부에 따른 과제 정확도",
    subtitle = "조기 지원 요청 팀의 정확도가 유의미하게 높음 (t(45)=2.57, p=.014)",
    x = "지원 요청 여부", y = "평균 과제 정확도 (%)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16), legend.position = "none") +
  ylim(0, 105) +
  geom_text(aes(label = paste0(Mean_Accuracy, "%")), vjust = -2, size = 5)

# --- 시각화 2-2: 리더십과 과제 완료 시간의 상관관계 (상관분석) ---
set.seed(42)
leadership_score <- rnorm(47, mean = 50, sd = 10)
task_time <- 100 - 0.9 * leadership_score + rnorm(47, mean = 0, sd = 15)
abstract2_data_corr <- data.frame(Leadership = leadership_score, TaskTime = task_time)
ggplot(abstract2_data_corr, aes(x = Leadership, y = TaskTime)) +
  geom_point(alpha = 0.7, color = "navy") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(
    title = "Abstract 2-2: 리더십과 과제 완료 시간의 관계",
    subtitle = "리더십 제공 수준이 높을수록 과제 완료 시간이 단축되는 경향 (r=-0.47, p<.01)",
    x = "리더십 점수 (가상)", y = "과제 완료 시간 (분, 가상)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16))

#_______________________________________________________________________________
#
# Abstract 3: 가시성 및 커뮤니케이션 전략
#_______________________________________________________________________________

# --- 시각화 3-1: 광고 채널에 따른 제출물 증가 효과 (A/B 테스트) ---
abstract3_data_ab <- data.frame(
  Group = factor(c("대조군 (광고 없음)", "실험군 (학술 네트워크 광고)")),
  Submissions = c(100, 147) # 47% 증가를 상대값으로 표현
)
ggplot(abstract3_data_ab, aes(x = Group, y = Submissions, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Abstract 3-1: 광고 채널에 따른 제출물 수 비교",
    subtitle = "학술 네트워크 광고를 통해 제출물이 47% 증가 (p<.001)",
    x = "그룹", y = "제출물 수 (상대값)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16), legend.position = "none") +
  geom_text(aes(label = Submissions), vjust = -0.5, size = 5)

# --- 시각화 3-2: 프로세스 간소화와 리뷰어 만족도의 관계 (회귀분석) ---
set.seed(33)
streamlining_score <- rnorm(100, mean = 10, sd = 2)
reviewer_satisfaction <- 5 + 0.53 * streamlining_score + rnorm(100, mean = 0, sd = 1.5)
abstract3_data_beta <- data.frame(Streamlining = streamlining_score, Satisfaction = reviewer_satisfaction)
ggplot(abstract3_data_beta, aes(x = Streamlining, y = Satisfaction)) +
  geom_point(alpha = 0.6, color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE, color = "#2E8B57", linetype = "solid") +
  labs(
    title = "Abstract 3-2: 프로세스 간소화와 리뷰어 만족도의 관계",
    subtitle = "프로세스 간소화가 리뷰어 만족도를 유의미하게 예측함 (β=0.53, p<.001)",
    x = "프로세스 간소화 점수 (가상)", y = "리뷰어 만족도 (가상)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16))

# --- 시각화 3-3: 개정안에 대한 응답자 만족도 (신뢰구간) ---
abstract3_data_ci <- data.frame(
  label = "개정안 만족도", mean = 87, lower_ci = 81.2, upper_ci = 92.5
)
ggplot(abstract3_data_ci, aes(x = label, y = mean)) +
  geom_point(size = 4, color = "dodgerblue") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1, linewidth = 1, color = "dodgerblue") +
  labs(
    title = "Abstract 3-3: 개정안에 대한 응답자 만족도",
    subtitle = "87%가 '필요를 충족했다'고 응답",
    x = "", y = "동의 비율 (%)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ylim(75, 100) +
  geom_text(aes(label = paste0(mean, "%\n(95% CI: ", lower_ci, "-", upper_ci, ")")), vjust = -2, size = 4.5)

#_______________________________________________________________________________
#
# Abstract 4: 관리자 개입 프로그램 효과
#_______________________________________________________________________________

# --- 시각화 4-1: 개입 전후 직무 만족도 비교 (t-검정) ---
abstract4_data_ttest <- data.frame(
  Time = factor(c("기준 시점", "개입 후"), levels = c("기준 시점", "개입 후")),
  Mean_Satisfaction = c(3.6, 4.3),
  SD = c(0.8, 0.7)
)
ggplot(abstract4_data_ttest, aes(x = Time, y = Mean_Satisfaction, fill = Time)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Satisfaction - SD, ymax = Mean_Satisfaction + SD), width = 0.2, linewidth = 0.8) +
  labs(
    title = "Abstract 4-1: 개입 프로그램 전후 직무 만족도 변화",
    subtitle = "개입 후 직무 만족도가 유의미하게 향상됨 (t(149)=4.12, p<.01)",
    x = "시점", y = "평균 직무 만족도 (5점 척도)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16), legend.position = "none") +
  ylim(0, 5.5) +
  geom_text(aes(label = Mean_Satisfaction), vjust = -2.5, size = 5)

# --- 시각화 4-2: 승진 직원 비율 ---
abstract4_data_promo <- data.frame(
  Category = c("승진", "유지"),
  Percentage = c(12, 88)
)
ggplot(abstract4_data_promo, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "Abstract 4-2: 개입 후 승진 직원 비율",
    subtitle = "전체 직원 중 12%가 더 높은 직책으로 승진함",
    fill = "승진 여부"
  ) +
  theme_void(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16), legend.position = "top") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5))

#_______________________________________________________________________________
#
# Abstract 5: 조직 의사결정과 운영 효율성
#_______________________________________________________________________________

# --- 시각화 5-1: 선임 분석가 유무에 따른 문서화 완료율 비교 (카이제곱 검정) ---
abstract5_data_chi <- data.frame(
  Group = factor(c("선임 분석가 있음", "선임 분석가 없음", "선임 분석가 있음", "선임 분석가 없음")),
  Completion = factor(c("완료", "완료", "미완료", "미완료")),
  Rate = c(65, 50, 35, 50) # 15%p 차이를 보이는 가상 비율
)
ggplot(abstract5_data_chi, aes(x = Group, y = Rate, fill = Completion)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Abstract 5-1: 선임 분석가 유무에 따른 문서화 완료율",
    subtitle = "선임 분석가 있는 팀의 완료율이 15%p 더 높음 (χ²(1)=4.23, p=.040)",
    x = "팀 유형", y = "비율", fill = "완료 여부"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16), legend.position = "top")

# --- 시각화 5-2: 선임 분석가 지원에 따른 프로젝트 성공률 비교 (t-검정) ---
abstract5_data_ttest <- data.frame(
  Group = factor(c("선임 분석가 지원팀", "선임 분석가 미지원팀")),
  Mean_Success = c(78.3, 68.1),
  SD = c(6.2, 8.4)
)
ggplot(abstract5_data_ttest, aes(x = Group, y = Mean_Success, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Success - SD, ymax = Mean_Success + SD), width = 0.2, linewidth = 0.8) +
  labs(
    title = "Abstract 5-2: 선임 분석가 지원에 따른 프로젝트 성공률",
    subtitle = "선임 분석가 지원 팀의 성공률이 10%p 더 높음 (t(42)=2.87, p=.006)",
    x = "팀 유형", y = "평균 프로젝트 성공률 (%)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16), legend.position = "none") +
  ylim(0, 100) +
  geom_text(aes(label = paste0(Mean_Success, "%")), vjust = -1.5, size = 5)

# --- 시각화 5-3: 역할 중복성과 만족도의 관계 (상관분석) ---
set.seed(5)
redundancy <- rnorm(85, mean = 5, sd = 1.5)
satisfaction <- 8 - 0.6 * redundancy + rnorm(85, mean = 0, sd = 1)
abstract5_data_corr <- data.frame(Redundancy = redundancy, Satisfaction = satisfaction)
ggplot(abstract5_data_corr, aes(x = Redundancy, y = Satisfaction)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "orange", linetype = "dashed") +
  labs(
    title = "Abstract 5-3: 역할 중복성과 직무 만족도의 관계",
    subtitle = "역할 중복성이 높을수록 만족도가 감소하는 경향 (r=-0.38, p=.012)",
    x = "역할 중복성 점수 (가상)", y = "직무 만족도 (가상)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16))

#_______________________________________________________________________________
#
# Abstract 6: 파트너십 형성과 전략적 성장
#_______________________________________________________________________________

# --- 시각화 6-1: 교육 워크숍 전후 직원 역량 변화 (t-검정) ---
abstract6_data_ttest <- data.frame(
  Time = factor(c("기준 시점", "워크숍 후"), levels = c("기준 시점", "워크숍 후")),
  Mean_Competency = c(3.8, 4.2),
  SD = c(0.7, 0.6)
)
ggplot(abstract6_data_ttest, aes(x = Time, y = Mean_Competency, fill = Time)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Competency - SD, ymax = Mean_Competency + SD), width = 0.2, linewidth = 0.8) +
  labs(
    title = "Abstract 6-1: 교육 워크숍 전후 직원 역량 변화",
    subtitle = "워크숍 후 역량이 10% 향상됨 (t(66)=3.45, p=.001)",
    x = "시점", y = "평균 역량 점수 (5점 척도)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16), legend.position = "none") +
  ylim(0, 5) +
  geom_text(aes(label = Mean_Competency), vjust = -2.5, size = 5)

# --- 시각화 6-2: 고객 관여 전략에 따른 참여자 수 (ANOVA) ---
abstract6_data_anova <- data.frame(
  Group = factor(c("이전 주기", "표준 관여", "개인화 관여")),
  Participants = c(100, 118, 137) # 37% 증가 경향 반영
)
ggplot(abstract6_data_anova, aes(x = Group, y = Participants, fill = Group)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "Abstract 6-2: 고객 관여 전략에 따른 참여자 수",
    subtitle = "개인화된 관여 활동이 참여자 수를 유의미하게 증가시킴 (F(2,134)=8.91, p<.001)",
    x = "관여 전략", y = "참여자 수 (상대값)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16), legend.position = "none") +
  geom_text(aes(label = Participants), vjust = -0.5, size = 5)

#_______________________________________________________________________________
#
# Abstract 7: 관광 진흥의 전략적 관리
#_______________________________________________________________________________

# --- 시각화 7-1: 이해관계자 참여와 캠페인 실패의 관계 (상관분석) ---
set.seed(7)
involvement <- rnorm(50, mean = 70, sd = 15)
failure_rate <- 100 - 0.95 * involvement + rnorm(50, mean = 0, sd = 10)
abstract7_data_corr <- data.frame(Involvement = involvement, Failure = failure_rate)
ggplot(abstract7_data_corr, aes(x = Involvement, y = Failure)) +
  geom_point(alpha = 0.7, color = "tomato") +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue", linetype = "solid") +
  labs(
    title = "Abstract 7-1: 이해관계자 참여와 캠페인 실패율의 관계",
    subtitle = "이해관계자 참여 부족이 실패의 주요 원인임 (r=-0.73, p<.001)",
    x = "이해관계자 참여도 (가상)", y = "캠페인 실패율 (가상)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16))

# --- 시각화 7-2: 캠페인 전후 관광객 방문 수 비교 (t-검정) ---
abstract7_data_visits <- data.frame(
  Time = factor(c("기준 시점", "캠페인 후"), levels = c("기준 시점", "캠페인 후")),
  Mean_Visits = c(2278, 2847),
  SD = c(289, 312)
)
ggplot(abstract7_data_visits, aes(x = Time, y = Mean_Visits, fill = Time)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Visits - SD, ymax = Mean_Visits + SD), width = 0.2, linewidth = 0.8) +
  labs(
    title = "Abstract 7-2: 캠페인 전후 월 평균 관광객 방문 수",
    subtitle = "캠페인 후 관광객 수가 25% 증가 (t(23)=4.76, p<.001)",
    x = "시점", y = "월 평균 방문객 수"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16), legend.position = "none") +
  geom_text(aes(label = format(Mean_Visits, big.mark = ",")), vjust = -1.5, size = 5)

# --- 시각화 7-3: 캠페인 전후 이해관계자 만족도 비교 (t-검정) ---
abstract7_data_satisfaction <- data.frame(
  Time = factor(c("기준 시점", "캠페인 후"), levels = c("기준 시점", "캠페인 후")),
  Mean_Satisfaction = c(3.1, 4.3),
  SD = c(0.8, 0.6)
)
ggplot(abstract7_data_satisfaction, aes(x = Time, y = Mean_Satisfaction, fill = Time)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Satisfaction - SD, ymax = Mean_Satisfaction + SD), width = 0.2, linewidth = 0.8) +
  labs(
    title = "Abstract 7-3: 캠페인 전후 이해관계자 만족도",
    subtitle = "캠페인 후 만족도가 40% 증가 (t(155)=12.34, p<.001)",
    x = "시점", y = "평균 만족도 (5점 척도)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16), legend.position = "none") +
  ylim(0, 5.5) +
  geom_text(aes(label = Mean_Satisfaction), vjust = -2, size = 5)

#_______________________________________________________________________________
#
# Abstract 8: 학술 기관의 재무 관리 및 자원 배분
#_______________________________________________________________________________

# --- 시각화 8-1: 예산 배분 방식에 따른 자금 활용률 비교 (t-검정) ---
abstract8_data_budget <- data.frame(
  Method = factor(c("전통적 방식", "구조화된 방식")),
  Mean_Utilization = c(70.8, 87.2),
  SD = c(8.1, 5.4)
)
ggplot(abstract8_data_budget, aes(x = Method, y = Mean_Utilization, fill = Method)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Utilization - SD, ymax = Mean_Utilization + SD), width = 0.2, linewidth = 0.8) +
  labs(
    title = "Abstract 8-1: 예산 배분 방식에 따른 자금 활용률",
    subtitle = "구조화된 방식이 23% 더 높은 활용률을 보임 (t(58)=4.67, p<.001)",
    x = "예산 배분 방식", y = "평균 자금 활용률 (%)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16), legend.position = "none") +
  ylim(0, 100) +
  geom_text(aes(label = paste0(Mean_Utilization, "%")), vjust = -1.5, size = 5)

# --- 시각화 8-2: 재정 전환 관리 방식에 따른 예산 불일치 비교 (ANOVA) ---
abstract8_data_anova <- data.frame(
  Management_Style = factor(c("비효율적 관리", "표준 관리", "효율적 관리"), levels=c("비효율적 관리", "표준 관리", "효율적 관리")),
  Discrepancies = c(100, 90, 82) # 18% 감소 경향 반영
)
ggplot(abstract8_data_anova, aes(x = Management_Style, y = Discrepancies, fill = Management_Style)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "Abstract 8-2: 재정 전환 관리 방식에 따른 예산 불일치",
    subtitle = "효율적인 관리가 예산 불일치를 18% 감소시킴 (F(2,89)=12.43, p<.001)",
    x = "전환 관리 방식", y = "예산 불일치 건수 (상대값)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16)) +
  geom_text(aes(label = Discrepancies), vjust = -0.5, size = 5)

# --- 시각화 8-3: 서비스 표준 유지에 따른 성과 등급 비교 (t-검정) ---
abstract8_data_service <- data.frame(
  Group = factor(c("표준 미유지", "표준 유지")),
  Rating = c(3.4, 4.1),
  SD = c(0.9, 0.6)
)
ggplot(abstract8_data_service, aes(x = Group, y = Rating, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Rating - SD, ymax = Rating + SD), width = 0.2, linewidth = 0.8) +
  labs(
    title = "Abstract 8-3: 서비스 표준 유지에 따른 성과 등급",
    subtitle = "서비스 표준을 유지한 부서의 성과 등급이 더 높음 (t(67)=3.28, p=.002)",
    x = "서비스 표준 유지 여부", y = "평균 성과 등급 (5점 척도)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16)) +
  ylim(0, 5) +
  geom_text(aes(label = Rating), vjust = -2, size = 5)

#_______________________________________________________________________________
#
# Abstract 9: 제조업의 운영 관리 및 품질 관리
#_______________________________________________________________________________

# --- 시각화 9-1: 유지보수 전략에 따른 장비 고장률 비교 (t-검정) ---
abstract9_data_maintenance <- data.frame(
  Method = factor(c("사후 대응적", "사전 예방적")),
  Mean_Failures = c(3.6, 2.1),
  SD = c(1.8, 1.2)
)
ggplot(abstract9_data_maintenance, aes(x = Method, y = Mean_Failures, fill = Method)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Failures - SD, ymax = Mean_Failures + SD), width = 0.2, linewidth = 0.8) +
  labs(
    title = "Abstract 9-1: 유지보수 전략에 따른 월 평균 장비 고장 횟수",
    subtitle = "사전 예방적 유지보수가 고장률을 31% 감소시킴 (t(34)=2.89, p=.007)",
    x = "유지보수 전략", y = "월 평균 고장 횟수"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(plot.title = element_text(face = "bold", size = 16), legend.position = "none") +
  geom_text(aes(label = Mean_Failures), vjust = -1.5, size = 5)

# --- 시각화 9-2: 재무 감독 절차에 따른 재정 불일치 감소 (카이제곱 검정) ---
abstract9_data_chi <- data.frame(
  Procedure = factor(c("이전 절차", "개선된 절차", "이전 절차", "개선된 절차"), levels = c("이전 절차", "개선된 절차")),
  Result = factor(c("불일치 발견", "불일치 발견", "정상", "정상")),
  Count = c(25, 20, 23, 28) # N=96, 19% 감소를 반영한 가상 빈도
)
ggplot(abstract9_data_chi, aes(x = Procedure, y = Count, fill = Result)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Abstract 9-2: 재무 감독 절차에 따른 재정 불일치",
    subtitle = "개선된 절차가 재정 불일치 발생률을 19% 감소시킴 (χ²(1)=5.67, p=.017)",
    x = "감독 절차", y = "계정 수", fill = "감사 결과"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(legend.position = "top", plot.title = element_text(face = "bold", size = 16)) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5)

# --- 시각화 9-3: 청구 오류율 감소 효과 (t-검정) ---
abstract9_data_billing <- data.frame(
  Time = factor(c("이전", "이후"), levels = c("이전", "이후")),
  Error_Rate = c(4.2, 1.8)
)
ggplot(abstract9_data_billing, aes(x = Time, y = Error_Rate, fill = Time)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Abstract 9-3: 청구 절차 개선 후 오류율 변화",
    subtitle = "오류율이 4.2%에서 1.8%로 유의미하게 감소 (t(28)=3.45, p=.002)",
    x = "절차 개선 시점", y = "오류율 (%)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16)) +
  geom_text(aes(label = paste0(Error_Rate, "%")), vjust = -0.5, size = 5)

#_______________________________________________________________________________
#
# Abstract 10: 의료 시스템의 기술 도입 및 성과 모니터링
#_______________________________________________________________________________

# --- 시각화 10-1: 전담 모니터링 팀 유무에 따른 시스템 배포 시간 비교 (t-검정) ---
abstract10_data_deployment <- data.frame(
  Group = factor(c("전담팀 없음", "전담팀 있음")),
  Mean_Days = c(68.9, 45.2),
  SD = c(12.4, 8.7)
)
ggplot(abstract10_data_deployment, aes(x = Group, y = Mean_Days, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Days - SD, ymax = Mean_Days + SD), width = 0.2) +
  labs(
    title = "Abstract 10-1: 전담 모니터링 팀 유무에 따른 시스템 배포 시간",
    subtitle = "전담팀이 있을 경우 배포 시간이 34% 더 빠름 (t(78)=5.23, p<.001)",
    x = "전담 모니터링 팀 유무", y = "평균 배포 소요 시간 (일)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") + 
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16)) +
  geom_text(aes(label = Mean_Days), vjust = -1.5, size = 5)

# --- 시각화 10-2: 물류 최적화에 따른 배송 시간 비교 (ANOVA) ---
abstract10_data_anova <- data.frame(
  Group = factor(c("최적화 이전", "부분 최적화", "전면 최적화"), levels=c("최적화 이전", "부분 최적화", "전면 최적화")),
  Delivery_Time = c(100, 85, 71) # 29% 감소를 표현
)
ggplot(abstract10_data_anova, aes(x = Group, y = Delivery_Time, fill = Group)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "Abstract 10-2: 물류 최적화 수준에 따른 배송 시간",
    subtitle = "물류 최적화가 배송 시간을 29% 단축시킴 (F(2,134)=11.67, p<.001)",
    x = "물류 최적화 수준", y = "상대적 배송 시간"
  ) +
  theme_minimal(base_family = "Malgun Gothic") + 
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16)) +
  geom_text(aes(label = Delivery_Time), vjust = -0.5, size = 5)

# --- 시각화 10-3: 기술 전환 관리 전략에 따른 시스템 다운타임 비교 (카이제곱 검정) ---
abstract10_data_chi <- data.frame(
  Management = factor(c("전략적 관리", "일반 관리", "전략적 관리", "일반 관리"), levels = c("일반 관리", "전략적 관리")),
  Outcome = factor(c("다운타임 발생", "다운타임 발생", "정상 운영", "정상 운영")),
  Count = c(35, 45, 55, 45) # N=180, 22% 감소 경향 반영
)
ggplot(abstract10_data_chi, aes(x = Management, y = Count, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Abstract 10-3: 기술 전환 관리 전략에 따른 시스템 다운타임",
    subtitle = "전략적 관리가 다운타임 발생률을 22% 감소시킴 (χ²(1)=9.34, p=.002)",
    x = "전환 관리 전략", y = "시설 수", fill = "운영 상태"
  ) +
  theme_minimal(base_family = "Malgun Gothic") + 
  theme(legend.position = "top", plot.title = element_text(face = "bold", size = 16)) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5)

# --- 시각화 10-4: 기술 도입 전후 환자 만족도 점수 비교 (t-검정) ---
abstract10_data_satisfaction <- data.frame(
  Time = factor(c("도입 전", "도입 후"), levels = c("도입 전", "도입 후")),
  Mean_Score = c(3.7, 4.4),
  SD = c(0.9, 0.6)
)
ggplot(abstract10_data_satisfaction, aes(x = Time, y = Mean_Score, fill = Time)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Score - SD, ymax = Mean_Score + SD), width = 0.2) +
  labs(
    title = "Abstract 10-4: 기술 도입 전후 환자 만족도 점수",
    subtitle = "기술 도입 후 환자 만족도가 유의미하게 향상됨 (t(156)=6.78, p<.001)",
    x = "시점", y = "평균 만족도 점수 (5점 척도)"
  ) +
  theme_minimal(base_family = "Malgun Gothic") + 
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16)) +
  ylim(0, 5.5) +
  geom_text(aes(label = Mean_Score), vjust = -2, size = 5)
