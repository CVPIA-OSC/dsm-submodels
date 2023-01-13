juvenile_survival_submodels_UI <- function(id) {
    ns <- NS(id)
    tabsetPanel(
        tabPanel("Rearing Survival",
                  sidebarLayout(
                      sidebarPanel = sidebarPanel("Submodels"), 
                      mainPanel = mainPanel()
                  )),
        tabPanel("Bypass Survival",
                  sidebarLayout(
                      sidebarPanel = sidebarPanel("Submodels"), 
                      mainPanel = mainPanel()
                  )),
    )
}

juvenile_survival_submodels_server <- function(input, output, session) {
    surv_juv_rear <- function(max_temp_thresh, avg_temp_thresh, high_predation,
                              contact_points, prop_diversions, total_diversions,
                              stranded, weeks_flooded,
                              ..surv_juv_rear_int = fallRunDSM::params$..surv_juv_rear_int,
                              .avg_temp_thresh = fallRunDSM::params$.surv_juv_rear_avg_temp_thresh,
                              .high_predation = fallRunDSM::params$.surv_juv_rear_high_predation,
                              .surv_juv_rear_contact_points = fallRunDSM::params$.surv_juv_rear_contact_points,
                              ..surv_juv_rear_contact_points = fallRunDSM::params$..surv_juv_rear_contact_points,
                              .surv_juv_rear_prop_diversions = fallRunDSM::params$.surv_juv_rear_prop_diversions,
                              ..surv_juv_rear_prop_diversions = fallRunDSM::params$..surv_juv_rear_prop_diversions,
                              .surv_juv_rear_total_diversions = fallRunDSM::params$.surv_juv_rear_total_diversions,
                              ..surv_juv_rear_total_diversions = fallRunDSM::params$..surv_juv_rear_total_diversions,
                              .stranded = fallRunDSM::params$.surv_juv_rear_stranded,
                              .medium = fallRunDSM::params$.surv_juv_rear_medium,
                              .large = fallRunDSM::params$.surv_juv_rear_large,
                              .floodplain = fallRunDSM::params$.surv_juv_rear_floodplain,
                              min_survival_rate = fallRunDSM::params$min_survival_rate,
                              stochastic){
        # determine the proportion of weeks when flooded vs not
        prop_ic <-ifelse(weeks_flooded > 0, (4 - weeks_flooded) / 4, 1)
        prop_fp <- 1 - prop_ic
        
        base_score_inchannel <- ..surv_juv_rear_int +
            (.avg_temp_thresh * avg_temp_thresh) +
            (.high_predation * high_predation) +
            (.surv_juv_rear_contact_points * ..surv_juv_rear_contact_points * contact_points * high_predation) +
            (.surv_juv_rear_prop_diversions * ..surv_juv_rear_prop_diversions * prop_diversions) +
            (.surv_juv_rear_total_diversions * ..surv_juv_rear_total_diversions * total_diversions) +
            (.stranded * stranded)
        
        base_score_floodplain <- ..surv_juv_rear_int + .floodplain +
            (.avg_temp_thresh  * avg_temp_thresh) + (.high_predation * high_predation)
        
        if (stochastic) {
            s1 <- ifelse(max_temp_thresh, min_survival_rate, boot::inv.logit(base_score_inchannel))
            m1 <- ifelse(max_temp_thresh, min_survival_rate, boot::inv.logit(base_score_inchannel + .medium))
            l1 <- ifelse(max_temp_thresh, min_survival_rate, boot::inv.logit(base_score_inchannel + .large))
            s2 <- ifelse(max_temp_thresh, min_survival_rate, boot::inv.logit(base_score_floodplain)) ^ prop_fp
            m2 <- ifelse(max_temp_thresh, min_survival_rate, boot::inv.logit(base_score_floodplain + .medium)) ^ prop_fp
            l2 <- ifelse(max_temp_thresh, min_survival_rate, boot::inv.logit(base_score_floodplain + .large)) ^ prop_fp
        } else {
            s1 <- (boot::inv.logit(base_score_inchannel) * (1 - max_temp_thresh)) + (min_survival_rate * max_temp_thresh)
            m1 <- (boot::inv.logit(base_score_inchannel + .medium) * (1 - max_temp_thresh)) + (min_survival_rate * max_temp_thresh)
            l1 <- (boot::inv.logit(base_score_inchannel + .large) * (1 - max_temp_thresh)) + (min_survival_rate * max_temp_thresh)
            s2 <- ((boot::inv.logit(base_score_floodplain) * (1 - max_temp_thresh)) + (min_survival_rate * max_temp_thresh)) ^ prop_fp
            m2 <- ((boot::inv.logit(base_score_floodplain + .medium) * (1 - max_temp_thresh)) + (min_survival_rate * max_temp_thresh)) ^ prop_fp
            l2 <- ((boot::inv.logit(base_score_floodplain + .large) * (1 - max_temp_thresh)) + (min_survival_rate * max_temp_thresh)) ^ prop_fp
        }
        
        list(
            inchannel = cbind(s = s1,
                              m = m1,
                              l = l1,
                              vl = 1),
            floodplain = cbind(s = (s1^prop_ic * s2),
                               m = (m1^prop_ic * m2),
                               l = (l1^prop_ic * l2),
                               vl = 1)
        )
    }
    
    
}
