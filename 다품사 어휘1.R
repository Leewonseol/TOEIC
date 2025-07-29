# install.packages(c("DiagrammeR","htmltools"))  # 설치되지 않았다면
library(DiagrammeR)
library(htmltools)

dot_sources <- list(
'
digraph {
  budget [label="budget"];
  budget_noun [label="noun"];
  budget -> budget_noun;
  "budget_noun_financial plan" [label="financial plan"];
  budget_noun -> "budget_noun_financial plan";
  budget_noun_allocation [label="allocation"];
  budget_noun -> budget_noun_allocation;
  budget_verb [label="verb"];
  budget -> budget_verb;
  budget_verb_allocate [label="allocate"];
  budget_verb -> budget_verb_allocate;
  budget_verb_plan [label="plan"];
  budget_verb -> budget_verb_plan;
  budget_adjective [label="adjective"];
  budget -> budget_adjective;
  budget_adjective_economical [label="economical"];
  budget_adjective -> budget_adjective_economical;
  "budget_adjective_low-cost" [label="low-cost"];
  budget_adjective -> "budget_adjective_low-cost";
}
',
'
digraph {
  recruit [label="recruit"];
  recruit_verb [label="verb"];
  recruit -> recruit_verb;
  recruit_verb_enlist [label="enlist"];
  recruit_verb -> recruit_verb_enlist;
  recruit_verb_hire [label="hire"];
  recruit_verb -> recruit_verb_hire;
  recruit_noun [label="noun"];
  recruit -> recruit_noun;
  "recruit_noun_new employee" [label="new employee"];
  recruit_noun -> "recruit_noun_new employee";
  recruit_noun_trainee [label="trainee"];
  recruit_noun -> recruit_noun_trainee;
}
',
'
digraph {
  courtesy [label="courtesy"];
  courtesy_noun [label="noun"];
  courtesy -> courtesy_noun;
  courtesy_noun_politeness [label="politeness"];
  courtesy_noun -> courtesy_noun_politeness;
  courtesy_noun_respect [label="respect"];
  courtesy_noun -> courtesy_noun_respect;
  courtesy_adjective [label="adjective"];
  courtesy -> courtesy_adjective;
  courtesy_adjective_complimentary [label="complimentary"];
  courtesy_adjective -> courtesy_adjective_complimentary;
  courtesy_adjective_free [label="free"];
  courtesy_adjective -> courtesy_adjective_free;
}
',
'
digraph {
  direct [label="direct"];
  direct_verb [label="verb"];
  direct -> direct_verb;
  direct_verb_guide [label="guide"];
  direct_verb -> direct_verb_guide;
  direct_verb_manage [label="manage"];
  direct_verb -> direct_verb_manage;
  direct_adjective [label="adjective"];
  direct -> direct_adjective;
  direct_adjective_straight [label="straight"];
  direct_adjective -> direct_adjective_straight;
  direct_adjective_explicit [label="explicit"];
  direct_adjective -> direct_adjective_explicit;
}
',
'
digraph {
  schedule [label="schedule"];
  schedule_noun [label="noun"];
  schedule -> schedule_noun;
  schedule_noun_timetable [label="timetable"];
  schedule_noun -> schedule_noun_timetable;
  schedule_noun_agenda [label="agenda"];
  schedule_noun -> schedule_noun_agenda;
  schedule_verb [label="verb"];
  schedule -> schedule_verb;
  schedule_verb_plan [label="plan"];
  schedule_verb -> schedule_verb_plan;
  schedule_verb_arrange [label="arrange"];
  schedule_verb -> schedule_verb_arrange;
}
',
'
digraph {
  transition [label="transition"];
  transition_noun [label="noun"];
  transition -> transition_noun;
  transition_noun_change [label="change"];
  transition_noun -> transition_noun_change;
  transition_noun_shift [label="shift"];
  transition_noun -> transition_noun_shift;
  transition_verb [label="verb"];
  transition -> transition_verb;
  transition_verb_move [label="move"];
  transition_verb -> transition_verb_move;
  transition_verb_shift [label="shift"];
  transition_verb -> transition_verb_shift;
}
',
'
digraph {
  caution [label="caution"];
  caution_noun [label="noun"];
  caution -> caution_noun;
  caution_noun_care [label="care"];
  caution_noun -> caution_noun_care;
  caution_noun_warning [label="warning"];
  caution_noun -> caution_noun_warning;
  caution_verb [label="verb"];
  caution -> caution_verb;
  caution_verb_warn [label="warn"];
  caution_verb -> caution_verb_warn;
  caution_verb_alert [label="alert"];
  caution_verb -> caution_verb_alert;
}
',
'
digraph {
  act [label="act"];
  act_verb [label="verb"];
  act -> act_verb;
  act_verb_perform [label="perform"];
  act_verb -> act_verb_perform;
  act_verb_behave [label="behave"];
  act_verb -> act_verb_behave;
  act_noun [label="noun"];
  act -> act_noun;
  act_noun_deed [label="deed"];
  act_noun -> act_noun_deed;
  act_noun_law [label="law"];
  act_noun -> act_noun_law;
}
',
'
digraph {
  present [label="present"];
  present_verb [label="verb"];
  present -> present_verb;
  present_verb_give [label="give"];
  present_verb -> present_verb_give;
  present_verb_offer [label="offer"];
  present_verb -> present_verb_offer;
  present_noun [label="noun"];
  present -> present_noun;
  present_noun_gift [label="gift"];
  present_noun -> present_noun_gift;
  present_noun_time [label="time"];
  present_noun -> present_noun_time;
  present_adjective [label="adjective"];
  present -> present_adjective;
  present_adjective_current [label="current"];
  present_adjective -> present_adjective_current;
  present_adjective_attending [label="attending"];
  present_adjective -> present_adjective_attending;
}
',
'
digraph {
  issue [label="issue"];
  issue_noun [label="noun"];
  issue -> issue_noun;
  issue_noun_problem [label="problem"];
  issue_noun -> issue_noun_problem;
  issue_noun_edition [label="edition"];
  issue_noun -> issue_noun_edition;
  issue_verb [label="verb"];
  issue -> issue_verb;
  issue_verb_distribute [label="distribute"];
  issue_verb -> issue_verb_distribute;
  issue_verb_release [label="release"];
  issue_verb -> issue_verb_release;
}
',
'
digraph {
  following [label="following"];
  following_noun [label="noun"];
  following -> following_noun;
  following_noun_supporters [label="supporters"];
  following_noun -> following_noun_supporters;
  following_noun_fans [label="fans"];
  following_noun -> following_noun_fans;
  following_adjective [label="adjective"];
  following -> following_adjective;
  following_adjective_next [label="next"];
  following_adjective -> following_adjective_next;
  following_adjective_subsequent [label="subsequent"];
  following_adjective -> following_adjective_subsequent;
  following_preposition [label="preposition"];
  following -> following_preposition;
  following_preposition_after [label="after"];
  following_preposition -> following_preposition_after;
}
',
'
digraph {
  award [label="award"];
  award_noun [label="noun"];
  award -> award_noun;
  award_noun_prize [label="prize"];
  award_noun -> award_noun_prize;
  award_noun_honor [label="honor"];
  award_noun -> award_noun_honor;
  award_verb [label="verb"];
  award -> award_verb;
  award_verb_grant [label="grant"];
  award_verb -> award_verb_grant;
  award_verb_give [label="give"];
  award_verb -> award_verb_give;
}
',
'
digraph {
  staff [label="staff"];
  staff_noun [label="noun"];
  staff -> staff_noun;
  staff_noun_employees [label="employees"];
  staff_noun -> staff_noun_employees;
  staff_noun_workers [label="workers"];
  staff_noun -> staff_noun_workers;
  staff_verb [label="verb"];
  staff -> staff_verb;
  staff_verb_man [label="man"];
  staff_verb -> staff_verb_man;
  staff_verb_employ [label="employ"];
  staff_verb -> staff_verb_employ;
}
',
'
digraph {
  line [label="line"];
  line_noun [label="noun"];
  line -> line_noun;
  line_noun_row [label="row"];
  line_noun -> line_noun_row;
  line_noun_string [label="string"];
  line_noun -> line_noun_string;
  line_verb [label="verb"];
  line -> line_verb;
  line_verb_mark [label="mark"];
  line_verb -> line_verb_mark;
  line_verb_border [label="border"];
  line_verb -> line_verb_border;
}
',
'
digraph {
  address [label="address"];
  address_noun [label="noun"];
  address -> address_noun;
  address_noun_location [label="location"];
  address_noun -> address_noun_location;
  address_noun_speech [label="speech"];
  address_noun -> address_noun_speech;
  address_verb [label="verb"];
  address -> address_verb;
  "address_verb_speak to" [label="speak to"];
  address_verb -> "address_verb_speak to";
  "address_verb_deal with" [label="deal with"];
  address_verb -> "address_verb_deal with";
}
',
'
digraph {
  service [label="service"];
  service_noun [label="noun"];
  service -> service_noun;
  service_noun_assistance [label="assistance"];
  service_noun -> service_noun_assistance;
  service_noun_maintenance [label="maintenance"];
  service_noun -> service_noun_maintenance;
  service_verb [label="verb"];
  service -> service_verb;
  service_verb_maintain [label="maintain"];
  service_verb -> service_verb_maintain;
  "service_verb_attend to" [label="attend to"];
  service_verb -> "service_verb_attend to";
}
',
'
digraph {
  long [label="long"];
  long_adjective [label="adjective"];
  long -> long_adjective;
  long_adjective_lengthy [label="lengthy"];
  long_adjective -> long_adjective_lengthy;
  long_adjective_extended [label="extended"];
  long_adjective -> long_adjective_extended;
  long_verb [label="verb"];
  long -> long_verb;
  long_verb_yearn [label="yearn"];
  long_verb -> long_verb_yearn;
  long_verb_desire [label="desire"];
  long_verb -> long_verb_desire;
}
',
'
digraph {
  function [label="function"];
  function_noun [label="noun"];
  function -> function_noun;
  function_noun_role [label="role"];
  function_noun -> function_noun_role;
  function_noun_event [label="event"];
  function_noun -> function_noun_event;
  function_verb [label="verb"];
  function -> function_verb;
  function_verb_work [label="work"];
  function_verb -> function_verb_work;
  function_verb_operate [label="operate"];
  function_verb -> function_verb_operate;
}
',
'
digraph {
  project [label="project"];
  project_noun [label="noun"];
  project -> project_noun;
  project_noun_plan [label="plan"];
  project_noun -> project_noun_plan;
  project_noun_task [label="task"];
  project_noun -> project_noun_task;
  project_verb [label="verb"];
  project -> project_verb;
  project_verb_forecast [label="forecast"];
  project_verb -> project_verb_forecast;
  project_verb_extend [label="extend"];
  project_verb -> project_verb_extend;
}
',
'
digraph {
  position [label="position"];
  position_noun [label="noun"];
  position -> position_noun;
  position_noun_location [label="location"];
  position_noun -> position_noun_location;
  position_noun_job [label="job"];
  position_noun -> position_noun_job;
  position_verb [label="verb"];
  position -> position_verb;
  position_verb_place [label="place"];
  position_verb -> position_verb_place;
  position_verb_arrange [label="arrange"];
  position_verb -> position_verb_arrange;
}
',
'
digraph {
  due [label="due"];
  due_adjective [label="adjective"];
  due -> due_adjective;
  due_adjective_expected [label="expected"];
  due_adjective -> due_adjective_expected;
  due_adjective_owed [label="owed"];
  due_adjective -> due_adjective_owed;
  due_noun [label="noun"];
  due -> due_noun;
  due_noun_fee [label="fee"];
  due_noun -> due_noun_fee;
  due_noun_charge [label="charge"];
  due_noun -> due_noun_charge;
}
',
'
digraph {
  prompt [label="prompt"];
  prompt_verb [label="verb"];
  prompt -> prompt_verb;
  prompt_verb_induce [label="induce"];
  prompt_verb -> prompt_verb_induce;
  prompt_verb_encourage [label="encourage"];
  prompt_verb -> prompt_verb_encourage;
  prompt_adjective [label="adjective"];
  prompt -> prompt_adjective;
  prompt_adjective_immediate [label="immediate"];
  prompt_adjective -> prompt_adjective_immediate;
  prompt_adjective_quick [label="quick"];
  prompt_adjective -> prompt_adjective_quick;
}
',
'
digraph {
  inconvenience [label="inconvenience"];
  inconvenience_noun [label="noun"];
  inconvenience -> inconvenience_noun;
  inconvenience_noun_trouble [label="trouble"];
  inconvenience_noun -> inconvenience_noun_trouble;
  inconvenience_noun_discomfort [label="discomfort"];
  inconvenience_noun -> inconvenience_noun_discomfort;
  inconvenience_verb [label="verb"];
  inconvenience -> inconvenience_verb;
  inconvenience_verb_bother [label="bother"];
  inconvenience_verb -> inconvenience_verb_bother;
  inconvenience_verb_disturb [label="disturb"];
  inconvenience_verb -> inconvenience_verb_disturb;
}
',
'
digraph {
  market [label="market"];
  market_noun [label="noun"];
  market -> market_noun;
  market_noun_bazaar [label="bazaar"];
  market_noun -> market_noun_bazaar;
  market_noun_economy [label="economy"];
  market_noun -> market_noun_economy;
  market_verb [label="verb"];
  market -> market_verb;
  market_verb_promote [label="promote"];
  market_verb -> market_verb_promote;
  market_verb_advertise [label="advertise"];
  market_verb -> market_verb_advertise;
}
',
'
digraph {
  detail [label="detail"];
  detail_noun [label="noun"];
  detail -> detail_noun;
  detail_noun_particular [label="particular"];
  detail_noun -> detail_noun_particular;
  detail_noun_item [label="item"];
  detail_noun -> detail_noun_item;
  detail_verb [label="verb"];
  detail -> detail_verb;
  detail_verb_describe [label="describe"];
  detail_verb -> detail_verb_describe;
  detail_verb_specify [label="specify"];
  detail_verb -> detail_verb_specify;
}
',
'
digraph {
  perfect [label="perfect"];
  perfect_adjective [label="adjective"];
  perfect -> perfect_adjective;
  perfect_adjective_flawless [label="flawless"];
  perfect_adjective -> perfect_adjective_flawless;
  perfect_adjective_ideal [label="ideal"];
  perfect_adjective -> perfect_adjective_ideal;
  perfect_verb [label="verb"];
  perfect -> perfect_verb;
  perfect_verb_improve [label="improve"];
  perfect_verb -> perfect_verb_improve;
  perfect_verb_refine [label="refine"];
  perfect_verb -> perfect_verb_refine;
}
',
'
digraph {
  credit [label="credit"];
  credit_noun [label="noun"];
  credit -> credit_noun;
  credit_noun_loan [label="loan"];
  credit_noun -> credit_noun_loan;
  credit_noun_recognition [label="recognition"];
  credit_noun -> credit_noun_recognition;
  credit_verb [label="verb"];
  credit -> credit_verb;
  credit_verb_assign [label="assign"];
  credit_verb -> credit_verb_assign;
  credit_verb_attribute [label="attribute"];
  credit_verb -> credit_verb_attribute;
}
',
'
digraph {
  local [label="local"];
  local_adjective [label="adjective"];
  local -> local_adjective;
  local_adjective_regional [label="regional"];
  local_adjective -> local_adjective_regional;
  local_adjective_nearby [label="nearby"];
  local_adjective -> local_adjective_nearby;
  local_noun [label="noun"];
  local -> local_noun;
  local_noun_resident [label="resident"];
  local_noun -> local_noun_resident;
}
',
'
digraph {
  expert [label="expert"];
  expert_noun [label="noun"];
  expert -> expert_noun;
  expert_noun_specialist [label="specialist"];
  expert_noun -> expert_noun_specialist;
  expert_noun_professional [label="professional"];
  expert_noun -> expert_noun_professional;
  expert_adjective [label="adjective"];
  expert -> expert_adjective;
  expert_adjective_skilled [label="skilled"];
  expert_adjective -> expert_adjective_skilled;
  expert_adjective_knowledgeable [label="knowledgeable"];
  expert_adjective -> expert_adjective_knowledgeable;
}
',
'
digraph {
  station [label="station"];
  station_noun [label="noun"];
  station -> station_noun;
  station_noun_terminal [label="terminal"];
  station_noun -> station_noun_terminal;
  station_noun_post [label="post"];
  station_noun -> station_noun_post;
  station_verb [label="verb"];
  station -> station_verb;
  station_verb_assign [label="assign"];
  station_verb -> station_verb_assign;
  station_verb_deploy [label="deploy"];
  station_verb -> station_verb_deploy;
}
',
'
digraph {
  representative [label="representative"];
  representative_noun [label="noun"];
  representative -> representative_noun;
  representative_noun_delegate [label="delegate"];
  representative_noun -> representative_noun_delegate;
  representative_noun_agent [label="agent"];
  representative_noun -> representative_noun_agent;
  representative_adjective [label="adjective"];
  representative -> representative_adjective;
  representative_adjective_typical [label="typical"];
  representative_adjective -> representative_adjective_typical;
  representative_adjective_illustrative [label="illustrative"];
  representative_adjective -> representative_adjective_illustrative;
}
',
'
digraph {
  price [label="price"];
  price_noun [label="noun"];
  price -> price_noun;
  price_noun_cost [label="cost"];
  price_noun -> price_noun_cost;
  price_noun_value [label="value"];
  price_noun -> price_noun_value;
  price_verb [label="verb"];
  price -> price_verb;
  price_verb_value [label="value"];
  price_verb -> price_verb_value;
  "price_verb_set price" [label="set price"];
  price_verb -> "price_verb_set price";
}
',
'
digraph {
  finance [label="finance"];
  finance_noun [label="noun"];
  finance -> finance_noun;
  finance_noun_funds [label="funds"];
  finance_noun -> finance_noun_funds;
  finance_noun_economy [label="economy"];
  finance_noun -> finance_noun_economy;
  finance_verb [label="verb"];
  finance -> finance_verb;
  finance_verb_fund [label="fund"];
  finance_verb -> finance_verb_fund;
  finance_verb_support [label="support"];
  finance_verb -> finance_verb_support;
}
'
n <- length(dot_sources)

for (i in seq(1, n, by = 2)) {
  plots <- list()

  # 첫 번째 그래프
  plots[[1]] <- grViz(dot_sources[[i]])

  # 두 번째 그래프(존재할 때만)
  if (i + 1 <= n) {
    plots[[2]] <- grViz(dot_sources[[i + 1]])
  }

  # 한 화면에 2개씩 출력
  print(
    browsable(
      tagList(plots)
    )
  )
}
