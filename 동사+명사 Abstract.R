# R 시각화를 위한 필수 패키지 설치 및 로드
# install.packages("ggplot2")
# install.packages("dplyr")
library(ggplot2)
library(dplyr)

#_______________________________________________________________________________
#
# Abstract 1: 윤리 인식 캠페인 효과 평가
#_______________________________________________________________________________

# --- 시각화 1: 정책 수신 확인 여부에 따른 팀 구성 비교 (카이제곱 검정 결과) ---
# χ²(1, N = 120) = 6.87, p = .009
# 통계 결과에 부합하는 가상 데이터 생성
# p-value가 유의미하므로, 정책을 수신한 그룹이 팀을 구성할 확률이 더 높다고 설정
abstract1_data_chi <- data.frame(
  Policy_Receipt = factor(c("수신 확인", "수신 미확인", "수신 확인", "수신 미확인"),
                          levels = c("수신 미확인", "수신 확인")),
  Team_Assembled = factor(c("팀 구성함", "팀 구성함", "팀 구성 안함", "팀 구성 안함")),
  Count = c(45, 25, 15, 35) # N=120에 맞춘 가상 빈도
)

# 막대 그래프 생성
ggplot(abstract1_data_chi, aes(x = Policy_Receipt, y = Count, fill = Team_Assembled)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Abstract 1: 정책 수신 여부에 따른 연구팀 구성 비교",
    subtitle = "정책을 수신한 연구자가 팀을 구성할 확률이 유의미하게 높음",
    x = "정책 수신 확인 여부",
    y = "연구자 수",
    fill = "팀 구성 여부"
  ) +
  theme_minimal(base_family = "AppleGothic") + # macOS 사용자의 경우 "AppleGothic"
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "top"
  ) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5)

# --- 시각화 2: IRB 계약 여부에 따른 규정 준수율 비교 (t-검정 결과) ---
# 계약 부서 (M = 89.4, SD = 4.1) vs 비계약 부서 (M = 77.2, SD = 6.8)
abstract1_data_ttest <- data.frame(
  Group = factor(c("IRB 계약 부서", "비계약 부서")),
  Mean_Compliance = c(89.4, 77.2),
  SD = c(4.1, 6.8)
)

# 오차 막대를 포함한 막대 그래프 생성
ggplot(abstract1_data_ttest, aes(x = Group, y = Mean_Compliance, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(
    aes(ymin = Mean_Compliance - SD, ymax = Mean_Compliance + SD),
    width = 0.2,
    linewidth = 0.8
  ) +
  labs(
    title = "Abstract 1: IRB 계약 여부에 따른 규정 준수율",
    subtitle = "계약 부서의 규정 준수율이 유의미하게 높음 (오차막대: ±1 SD)",
    x = "부서 그룹",
    y = "평균 규정 준수율 (%)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  ) +
  ylim(0, 100) +
  geom_text(aes(label = paste0(Mean_Compliance, "%")), vjust = -0.5, size = 5)


#_______________________________________________________________________________
#
# Abstract 2: 연구 워크플로우 효율성 연구
#_______________________________________________________________________________

# --- 시각화 1: 조기 지원 요청에 따른 과제 정확도 비교 (t-검정 결과) ---
# 지원 요청 그룹 (M = 94.1) vs 비요청 그룹 (M = 83.3)
abstract2_data_ttest <- data.frame(
  Group = factor(c("조기 지원 요청", "지원 요청 안함")),
  Mean_Accuracy = c(94.1, 83.3)
)

# 막대 그래프 생성
ggplot(abstract2_data_ttest, aes(x = Group, y = Mean_Accuracy, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Abstract 2: 지원 요청 여부에 따른 과제 정확도",
    subtitle = "조기에 지원을 요청한 팀의 과제 정확도가 더 높음",
    x = "지원 요청 여부",
    y = "평균 과제 정확도 (%)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  ) +
  ylim(0, 100) +
  geom_text(aes(label = paste0(Mean_Accuracy, "%")), vjust = -1.5, size = 5, color = "white")


# --- 시각화 2: 리더십과 과제 완료 시간의 상관관계 (상관분석 결과) ---
# r = -.47, p < .01
# 음의 상관관계를 갖는 가상 데이터 생성 (N=50)
set.seed(42)
leadership_score <- rnorm(50, mean = 50, sd = 10)
task_time <- 100 - 0.8 * leadership_score + rnorm(50, mean = 0, sd = 15)
abstract2_data_corr <- data.frame(Leadership = leadership_score, TaskTime = task_time)

# 회귀선을 포함한 산점도 생성
ggplot(abstract2_data_corr, aes(x = Leadership, y = TaskTime)) +
  geom_point(alpha = 0.7, color = "navy") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(
    title = "Abstract 2: 리더십과 과제 완료 시간의 관계",
    subtitle = "리더십 제공 수준이 높을수록 과제 완료 시간이 단축되는 경향 (r = -0.47)",
    x = "리더십 점수 (가상)",
    y = "과제 완료 시간 (분, 가상)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(plot.title = element_text(face = "bold", size = 16))


#_______________________________________________________________________________
#
# Abstract 3: 디지털 홍보 활동 효과 연구
#_______________________________________________________________________________

# --- 시각화 1: 광고 채널에 따른 제출물 증가 효과 (A/B 테스트 결과) ---
# 광고 그룹이 47% 증가
abstract3_data_ab <- data.frame(
  Group = factor(c("대조군 (광고 없음)", "실험군 (학술 네트워크 광고)")),
  Submissions = c(100, 147) # 기준 100으로 설정 시 47% 증가
)

# 막대 그래프 생성
ggplot(abstract3_data_ab, aes(x = Group, y = Submissions, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Abstract 3: 광고 채널에 따른 제출물 수 비교",
    subtitle = "학술 네트워크 광고를 통해 제출물이 47% 증가",
    x = "그룹",
    y = "제출물 수 (상대값)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  ) +
  geom_text(aes(label = Submissions), vjust = -0.5, size = 5)


# --- 시각화 2: 개정안에 대한 응답자 만족도 (신뢰구간 결과) ---
# 87% 동의 (95% CI [81.2%, 92.5%])
abstract3_data_ci <- data.frame(
  label = "개정안 만족도",
  mean = 87,
  lower_ci = 81.2,
  upper_ci = 92.5
)

# 신뢰구간을 표현하는 포인트 및 오차 막대 그래프
ggplot(abstract3_data_ci, aes(x = label, y = mean)) +
  geom_point(size = 4, color = "dodgerblue") +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    width = 0.1,
    linewidth = 1,
    color = "dodgerblue"
  ) +
  labs(
    title = "Abstract 3: 개정안에 대한 응답자 만족도",
    subtitle = "87%가 '필요를 충족했다'고 응답",
    x = "",
    y = "동의 비율 (%)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  ylim(75, 100) +
  geom_text(
    aes(label = paste0(mean, "%\n(95% CI: ", lower_ci, "-", upper_ci, ")")),
    vjust = -2,
    size = 4.5
  )


#_______________________________________________________________________________
#
# Abstract 4: 관리자 개입 프로그램 효과
#_______________________________________________________________________________

# --- 시각화 1: 개입 전후 직무 만족도 비교 (t-검정 결과) ---
# 개입 후 (M = 4.3, SD = 0.7) vs. 기준 시점 (M = 3.6, SD = 0.8)
abstract4_data_ttest <- data.frame(
  Time = factor(c("기준 시점", "개입 후"), levels = c("기준 시점", "개입 후")),
  Mean_Satisfaction = c(3.6, 4.3),
  SD = c(0.8, 0.7)
)

# 오차 막대를 포함한 막대 그래프 생성
ggplot(abstract4_data_ttest, aes(x = Time, y = Mean_Satisfaction, fill = Time)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(
    aes(ymin = Mean_Satisfaction - SD, ymax = Mean_Satisfaction + SD),
    width = 0.2,
    linewidth = 0.8
  ) +
  labs(
    title = "Abstract 4: 개입 프로그램 전후 직무 만족도 변화",
    subtitle = "개입 후 직무 만족도가 유의미하게 향상됨 (오차막대: ±1 SD)",
    x = "시점",
    y = "평균 직무 만족도 (5점 척도)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  ) +
  ylim(0, 5.5) +
  geom_text(aes(label = Mean_Satisfaction), vjust = -2.5, size = 5)

#_______________________________________________________________________________
#
# Abstract 5: 조직 의사결정과 운영 효율성
#_______________________________________________________________________________

# --- 시각화 1: 선임 분석가 유무에 따른 프로젝트 성공률 비교 (t-검정 결과) ---
# 선임 분석가 지원팀 (M = 78.3, SD = 6.2) vs 미지원팀 (M = 68.1, SD = 8.4)
abstract5_data_ttest <- data.frame(
  Group = factor(c("선임 분석가 지원팀", "선임 분석가 미지원팀")),
  Mean_Success = c(78.3, 68.1),
  SD = c(6.2, 8.4)
)

# 오차 막대를 포함한 막대 그래프 생성
ggplot(abstract5_data_ttest, aes(x = Group, y = Mean_Success, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(
    aes(ymin = Mean_Success - SD, ymax = Mean_Success + SD),
    width = 0.2,
    linewidth = 0.8
  ) +
  labs(
    title = "Abstract 5: 선임 분석가 지원에 따른 프로젝트 성공률",
    subtitle = "선임 분석가가 있는 팀의 성공률이 유의미하게 높음 (오차막대: ±1 SD)",
    x = "팀 유형",
    y = "평균 프로젝트 성공률 (%)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  ) +
  ylim(0, 100) +
  geom_text(aes(label = paste0(Mean_Success, "%")), vjust = -1.5, size = 5)

# --- 시각화 2: 역할 중복성과 만족도의 관계 (상관분석 결과) ---
# r = -.38, p = .012
# 음의 상관관계를 갖는 가상 데이터 생성 (N=60)
set.seed(5)
redundancy <- rnorm(60, mean = 5, sd = 1.5)
satisfaction <- 8 - 0.7 * redundancy + rnorm(60, mean = 0, sd = 1)
abstract5_data_corr <- data.frame(Redundancy = redundancy, Satisfaction = satisfaction)

# 회귀선을 포함한 산점도 생성
ggplot(abstract5_data_corr, aes(x = Redundancy, y = Satisfaction)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "orange", linetype = "dashed") +
  labs(
    title = "Abstract 5: 역할 중복성과 직무 만족도의 관계",
    subtitle = "역할 중복성이 높을수록 만족도가 감소하는 경향 (r = -0.38)",
    x = "역할 중복성 점수 (가상)",
    y = "직무 만족도 (가상)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(plot.title = element_text(face = "bold", size = 16))


#_______________________________________________________________________________
#
# Abstract 6: 파트너십 형성과 전략적 성장
#_______________________________________________________________________________

# --- 시각화 1: 지원 스태프 교육 워크숍의 효과 (t-검정 결과) ---
# 개입 후 (M = 4.2, SD = 0.6) vs 기준 시점 (M = 3.8, SD = 0.7)
abstract6_data_ttest <- data.frame(
  Time = factor(c("기준 시점", "워크숍 후"), levels = c("기준 시점", "워크숍 후")),
  Mean_Competency = c(3.8, 4.2),
  SD = c(0.7, 0.6)
)

# 오차 막대를 포함한 막대 그래프 생성
ggplot(abstract6_data_ttest, aes(x = Time, y = Mean_Competency, fill = Time)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(
    aes(ymin = Mean_Competency - SD, ymax = Mean_Competency + SD),
    width = 0.2,
    linewidth = 0.8
  ) +
  labs(
    title = "Abstract 6: 교육 워크숍 전후 직원 역량 변화",
    subtitle = "워크숍을 통해 직원 역량이 유의미하게 향상됨 (오차막대: ±1 SD)",
    x = "시점",
    y = "평균 역량 점수 (5점 척도)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  ) +
  ylim(0, 5) +
  geom_text(aes(label = Mean_Competency), vjust = -2.5, size = 5)

# --- 시각화 2: 개인화된 고객 관여 활동의 효과 (ANOVA 결과) ---
# F(2, 134) = 8.91, p < .001
# ANOVA 결과에 부합하는 가상 데이터 생성
abstract6_data_anova <- data.frame(
  Group = factor(c("이전 주기", "표준 관여", "개인화 관여")),
  Participants = c(100, 115, 137) # 37% 더 많은 참여자
)

# 막대 그래프 생성
ggplot(abstract6_data_anova, aes(x = Group, y = Participants, fill = Group)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "Abstract 6: 고객 관여 전략에 따른 참여자 수",
    subtitle = "개인화된 관여 활동이 참여자 수를 유의미하게 증가시킴",
    x = "관여 전략",
    y = "참여자 수 (상대값)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  ) +
  geom_text(aes(label = Participants), vjust = -0.5, size = 5)


#_______________________________________________________________________________
#
# Abstract 7: 관광 진흥의 전략적 관리
#_______________________________________________________________________________

# --- 시각화 1: 캠페인 전후 관광객 방문 수 비교 (t-검정 결과) ---
# 캠페인 후 (M = 2847, SD = 312) vs 기준 시점 (M = 2278, SD = 289)
abstract7_data_visits <- data.frame(
  Time = factor(c("기준 시점", "캠페인 후"), levels = c("기준 시점", "캠페인 후")),
  Mean_Visits = c(2278, 2847),
  SD = c(289, 312)
)

# 오차 막대를 포함한 막대 그래프 생성
ggplot(abstract7_data_visits, aes(x = Time, y = Mean_Visits, fill = Time)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(
    aes(ymin = Mean_Visits - SD, ymax = Mean_Visits + SD),
    width = 0.2,
    linewidth = 0.8
  ) +
  labs(
    title = "Abstract 7: 캠페인 전후 월 평균 관광객 방문 수",
    subtitle = "캠페인 후 관광객 수가 25% 증가 (오차막대: ±1 SD)",
    x = "시점",
    y = "월 평균 방문객 수"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  ) +
  geom_text(aes(label = format(Mean_Visits, big.mark = ",")), vjust = -1.5, size = 5)

# --- 시각화 2: 캠페인 전후 이해관계자 만족도 비교 (t-검정 결과) ---
# 캠페인 후 (M = 4.3, SD = 0.6) vs 기준 시점 (M = 3.1, SD = 0.8)
abstract7_data_satisfaction <- data.frame(
  Time = factor(c("기준 시점", "캠페인 후"), levels = c("기준 시점", "캠페인 후")),
  Mean_Satisfaction = c(3.1, 4.3),
  SD = c(0.8, 0.6)
)

# 오차 막대를 포함한 막대 그래프 생성
ggplot(abstract7_data_satisfaction, aes(x = Time, y = Mean_Satisfaction, fill = Time)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(
    aes(ymin = Mean_Satisfaction - SD, ymax = Mean_Satisfaction + SD),
    width = 0.2,
    linewidth = 0.8
  ) +
  labs(
    title = "Abstract 7: 캠페인 전후 이해관계자 만족도",
    subtitle = "캠페인 후 만족도가 40% 증가 (오차막대: ±1 SD)",
    x = "시점",
    y = "평균 만족도 (5점 척도)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  ) +
  ylim(0, 5.5) +
  geom_text(aes(label = Mean_Satisfaction), vjust = -2, size = 5)


#_______________________________________________________________________________
#
# Abstract 8: 학술 기관의 재무 관리 및 자원 배분
#_______________________________________________________________________________

# --- 시각화 1: 예산 배분 방식에 따른 자금 활용률 비교 (t-검정 결과) ---
# 구조화된 방식 (M = 87.2, SD = 5.4) vs 전통적 방식 (M = 70.8, SD = 8.1)
abstract8_data_budget <- data.frame(
  Method = factor(c("전통적 방식", "구조화된 방식")),
  Mean_Utilization = c(70.8, 87.2),
  SD = c(8.1, 5.4)
)

# 오차 막대를 포함한 막대 그래프 생성
ggplot(abstract8_data_budget, aes(x = Method, y = Mean_Utilization, fill = Method)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(
    aes(ymin = Mean_Utilization - SD, ymax = Mean_Utilization + SD),
    width = 0.2,
    linewidth = 0.8
  ) +
  labs(
    title = "Abstract 8: 예산 배분 방식에 따른 자금 활용률",
    subtitle = "구조화된 예산 배분 방식이 23% 더 높은 활용률을 보임 (오차막대: ±1 SD)",
    x = "예산 배분 방식",
    y = "평균 자금 활용률 (%)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  ) +
  ylim(0, 100) +
  geom_text(aes(label = paste0(Mean_Utilization, "%")), vjust = -1.5, size = 5)

# --- 시각화 2: 유연한 일정 관리와 이해관계자 만족도의 관계 (상관분석 결과) ---
# r = .62, p < .001
# 양의 상관관계를 갖는 가상 데이터 생성 (N=70)
set.seed(8)
flexibility <- rnorm(70, mean = 6, sd = 2)
satisfaction_corr <- 2 + 0.8 * flexibility + rnorm(70, mean = 0, sd = 1)
abstract8_data_corr <- data.frame(Flexibility = flexibility, Satisfaction = satisfaction_corr)

# 회귀선을 포함한 산점도 생성
ggplot(abstract8_data_corr, aes(x = Flexibility, y = Satisfaction)) +
  geom_point(alpha = 0.7, color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "cyan", linetype = "solid") +
  labs(
    title = "Abstract 8: 일정 유연성과 이해관계자 만족도의 관계",
    subtitle = "일정 관리 유연성이 높을수록 만족도가 증가하는 경향 (r = 0.62)",
    x = "일정 유연성 점수 (가상)",
    y = "이해관계자 만족도 (가상)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(plot.title = element_text(face = "bold", size = 16))


#_______________________________________________________________________________
#
# Abstract 9: 제조업의 운영 관리 및 품질 관리
#_______________________________________________________________________________

# --- 시각화 1: 유지보수 전략에 따른 장비 고장률 비교 (t-검정 결과) ---
# 사전 예방적 유지보수 (M = 2.1) vs 사후 대응적 유지보수 (M = 3.6)
abstract9_data_maintenance <- data.frame(
  Method = factor(c("사후 대응적", "사전 예방적")),
  Mean_Failures = c(3.6, 2.1),
  SD = c(1.8, 1.2)
)

# 오차 막대를 포함한 막대 그래프 생성
ggplot(abstract9_data_maintenance, aes(x = Method, y = Mean_Failures, fill = Method)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(
    aes(ymin = Mean_Failures - SD, ymax = Mean_Failures + SD),
    width = 0.2,
    linewidth = 0.8
  ) +
  labs(
    title = "Abstract 9: 유지보수 전략에 따른 월 평균 장비 고장 횟수",
    subtitle = "사전 예방적 유지보수가 고장률을 31% 감소시킴 (오차막대: ±1 SD)",
    x = "유지보수 전략",
    y = "월 평균 고장 횟수"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  ) +
  geom_text(aes(label = Mean_Failures), vjust = -1.5, size = 5)

# --- 시각화 2: 디지털 추적 시스템 도입 전후 정시 납품률 비교 (t-검정 결과) ---
# 도입 후 (M = 92.8, SD = 4.1) vs 도입 전 (M = 73.4, SD = 6.2)
abstract9_data_delivery <- data.frame(
  Time = factor(c("도입 전", "도입 후"), levels = c("도입 전", "도입 후")),
  Mean_Rate = c(73.4, 92.8),
  SD = c(6.2, 4.1)
)

# 오차 막대를 포함한 막대 그래프 생성
ggplot(abstract9_data_delivery, aes(x = Time, y = Mean_Rate, fill = Time)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(
    aes(ymin = Mean_Rate - SD, ymax = Mean_Rate + SD),
    width = 0.2,
    linewidth = 0.8
  ) +
  labs(
    title = "Abstract 9: 디지털 추적 시스템 도입 전후 정시 납품률",
    subtitle = "시스템 도입 후 정시 납품률이 27% 향상됨 (오차막대: ±1 SD)",
    x = "시점",
    y = "정시 납품률 (%)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  ) +
  ylim(0, 100) +
  geom_text(aes(label = paste0(Mean_Rate, "%")), vjust = -1.5, size = 5)


#_______________________________________________________________________________
#
# Abstract 10: 의료 시스템의 기술 도입 및 성과 모니터링
#_______________________________________________________________________________

# --- 시각화 1: 전담 모니터링 팀 유무에 따른 시스템 배포 시간 비교 (t-검정 결과) ---
# 전담팀 있음 (M = 45.2일) vs 전담팀 없음 (M = 68.9일)
abstract10_data_deployment <- data.frame(
  Group = factor(c("전담팀 없음", "전담팀 있음")),
  Mean_Days = c(68.9, 45.2),
  SD = c(12.4, 8.7)
)

# 오차 막대를 포함한 막대 그래프 생성
ggplot(abstract10_data_deployment, aes(x = Group, y = Mean_Days, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(
    aes(ymin = Mean_Days - SD, ymax = Mean_Days + SD),
    width = 0.2,
    linewidth = 0.8
  ) +
  labs(
    title = "Abstract 10: 전담 모니터링 팀 유무에 따른 시스템 배포 시간",
    subtitle = "전담팀이 있을 경우 배포 시간이 34% 더 빠름 (오차막대: ±1 SD)",
    x = "전담 모니터링 팀 유무",
    y = "평균 배포 소요 시간 (일)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  ) +
  geom_text(aes(label = Mean_Days), vjust = -1.5, size = 5)

# --- 시각화 2: 기술 도입 전후 환자 만족도 점수 비교 (t-검정 결과) ---
# 도입 후 (M = 4.4, SD = 0.6) vs 도입 전 (M = 3.7, SD = 0.9)
abstract10_data_satisfaction <- data.frame(
  Time = factor(c("도입 전", "도입 후"), levels = c("도입 전", "도입 후")),
  Mean_Score = c(3.7, 4.4),
  SD = c(0.9, 0.6)
)

# 오차 막대를 포함한 막대 그래프 생성
ggplot(abstract10_data_satisfaction, aes(x = Time, y = Mean_Score, fill = Time)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(
    aes(ymin = Mean_Score - SD, ymax = Mean_Score + SD),
    width = 0.2,
    linewidth = 0.8
  ) +
  labs(
    title = "Abstract 10: 기술 도입 전후 환자 만족도 점수",
    subtitle = "기술 도입 후 환자 만족도가 유의미하게 향상됨 (오차막대: ±1 SD)",
    x = "시점",
    y = "평균 만족도 점수 (5점 척도)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  ) +
  ylim(0, 5.5) +
  geom_text(aes(label = Mean_Score), vjust = -2, size = 5)




Abstract 1: Enhancing Research Ethics and Institutional Transparency
This study evaluated the impact of an ethics-awareness campaign on research conduct among early-career scholars (N = 38). Participants were tasked to give a speech on ethical challenges in their field, followed by a panel exercising caution in assessing protocol deviations. The institution reserved the right to suspend studies pending compliance review. Over 63% of researchers placed an order for updated consent materials, with 91% of orders fulfilled within 48 hours. The campaign launch led to awards presented for ethical excellence. Post-intervention, researchers who acknowledged receipt of the new policy were significantly more likely to assemble a team for cross-verification, χ²(1, N = 120) = 6.87, p = .009. The campaign promoted awareness of data transparency, contributing to a 32% increase in efforts to raise funds for the institution. Departments that secured contracts with institutional review boards demonstrated higher compliance rates (M = 89.4%, SD = 4.1) compared to those without contracts (M = 77.2%, SD = 6.8), t(22) = 3.12, p = .005. Faculty who expressed concern about unclear access rights were supported through revised guidelines, which helped institute a policy to meet expectations for transparency. These findings suggest that structured ethics interventions can build a reputation for institutional integrity and enhance compliance, offering implications for fostering ethical research environments.

Abstract 2: Workflow Optimization and Leadership Training
This study investigated the effectiveness of a streamlined research workflow on team performance across four departments. Supervisors were directed to delegate authority, and teams were instructed to draw conclusions from simulated data sets. Although 28% of teams encountered problems during execution, those who sought assistance early achieved higher task accuracy (M = 94.1%) compared to those who did not (M = 83.3%), t(45) = 2.57, p = .014. Leadership provision was negatively correlated with task completion time (r = −.47, p < .01). Teams completing tasks in under 30 minutes were more likely to obtain approval for production continuation, whereas 12 projects were discontinued due to noncompliance. An emergency meeting was called to address issues, raising questions about systemic inefficiencies, leading to three positions filled through external recruitment. A revised policy was instituted, taking effect within 72 hours, with a survey completion rate of 96%. Teams where supervisors assumed a role in oversight demonstrated improved coordination, meeting expectations for performance. These results highlight the importance of early assistance and leadership in optimizing workflows, suggesting that structured interventions can enhance team efficiency and compliance in research settings.

Abstract 3: Visibility and Communication Strategy
This study tested a digital outreach intervention to expand departmental presence using an A/B framework. The group placing ads on scholarly networks experienced a 47% increase in submissions (p < .001). Modifying the protocol to expedite a process reduced processing time by 21% (d = 0.67). Streamlining a process predicted higher reviewer satisfaction (β = .53, p < .001). However, 17% of faculty expressed concern about unclear access rights, prompting efforts to institute a policy ensuring all could gain access to resources. Faculty accepting offers for leadership roles displayed initiative (M = 4.7/5), with several extending invitations to international collaborators. Refund request rates for late-stage dropouts remained below 3%. Post-intervention, 87% of respondents agreed the revisions met their needs (95% CI [81.2%, 92.5%]). These outcomes indicate that targeted digital strategies can build a reputation for departmental visibility and efficiency, with clear access protocols enhancing faculty engagement. The findings underscore the value of streamlined communication and proactive leadership in academic outreach.

Abstract 4: The effects of a managerial intervention program on employee outcomes within a corporate setting
This study explored the effects of a managerial intervention program on employee outcomes within a corporate setting. The intervention trained managers to acknowledge employees’ hard work, extend offers for career development, and extend gratitude to foster morale. Managers were also taught to build relationships with staff, set goals for team performance, and settle disputes to enhance workplace harmony. Despite some managers undergoing reservations about the program's feasibility, the initiative proceeded by lifting a ban on external training, enabling employees to pursue degrees and pursue careers. Data were compiled from employee surveys and performance reviews to assess outcomes. Findings revealed that the intervention posed a risk of initial resistance but ultimately improved job satisfaction (M = 4.3, SD = 0.7) compared to baseline (M = 3.6, SD = 0.8), t(150) = 4.12, p < .01. Employees who secured jobs in higher roles increased by 12%, and managers who paid visits to teams reported stronger engagement. Copies of feedback were retained for analysis. These results suggest that targeted managerial training can enhance organizational effectiveness, though challenges in implementation persist.





