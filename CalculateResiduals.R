require(dplyr)
require(tidyr)
require(broom)
require(ggplot2)
require(cetcolor)
require(ggpubr)
require(ggpmisc)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/CQResiduals")

monthly_results<-read.csv("WRTDS_Outputs_Clean_01082026.csv")

monthly_results_wide <- monthly_results %>%
  select(Stream_Name, chemical, FNConc_mgL, Month, Year, Discharge_cms) %>%
  pivot_wider(names_from = chemical, values_from = FNConc_mgL, 
              values_fn = function(x) mean(x, na.rm = TRUE))

#### CQ residuals ####
sites<-unique(monthly_results_wide$Stream_Name)

CQ_results<-list()

res_list<-list()

#pdf("DSi_CQ_residuals.pdf", width = 18, height = 6)

for (i in 1:length(sites)) {
  
  print(i)
  
  residuals_df <- monthly_results_wide %>%
    filter(Stream_Name == sites[i]) %>%
    filter(!is.na(DSi)) %>%
    mutate(
      logQ = log(Discharge_cms),
      logC = log(DSi)
    ) %>%
    do({
      model <- lm(logC ~ logQ, data = .)
      augment(model, .)
    }) %>%
    mutate(Month = as.factor(Month)) 
  
  monthly_residual<-residuals_df %>%
    group_by(Month) %>%
    summarise(mean_res=mean(.resid), sd_res=sd(.resid))
  
  monthly_residual$Stream_Name<-sites[i]
  
  res_list[[i]]<-monthly_residual
  
  # p1<-monthly_results_wide %>%
  #   filter(Stream_Name==sites[i]) %>%
  #   ggplot(aes(log(Discharge_cms), log(DSi)))+geom_point(aes(col=as.factor(Month)), size=2)+theme_classic()+
  #   scale_color_manual(values = cet_pal(12, "cbtc1"))+
  #   ggtitle(sites[i])+
  #   labs(x="Log(Q)", y="Log(Si Conc)", col="Month")+
  #   theme(text = element_text(size = 20))+
  #   geom_smooth(method = "lm", se=F, col="black")+
  #   stat_poly_eq(use_label(c("eq", "R2")))
  # 
  # p2 <- ggplot(residuals_df, aes(x = Month, y = .resid, fill = Month)) +
  #   geom_boxplot(aes(group=Month)) +
  #   scale_fill_manual(values = cet_pal(12, "cbtc1")) +
  #   geom_hline(yintercept = 0, linetype = "dashed") +
  #   theme_classic() +
  #   labs(
  #     x = "Month",
  #     y = "Residuals (logC - fitted)",
  #     fill = "Month"
  #   ) +
  #   theme(
  #     text = element_text(size = 20),
  #     legend.position = "none"
  #   )
  # 
  # p3<-ggplot(residuals_df, aes(x = .resid*Discharge_cms, fill = Month)) +
  #   geom_histogram() +
  #   geom_vline(xintercept = 0, lty="dashed")+
  #   scale_fill_manual(values = cet_pal(12, "cbtc1")) +
  #   theme_classic() +
  #   labs(
  #     x = "Residual Flux",
  #     fill = "Month"
  #   ) +
  #   theme(
  #     text = element_text(size = 20),
  #     legend.position = "none"
  #   )
  # p3
  # 
  # p4<-ggarrange(p1, p2, p3, nrow = 1)
  # 
  # print(p4)
  
  lm<-monthly_results_wide %>%
    filter(Stream_Name==sites[i]) %>%
    lm(log(DSi)~log(Discharge_cms), data = .)
  
  lm_results<-monthly_results_wide %>%
    filter(Stream_Name==sites[i]) %>%
    summarise(model = list(lm(log(DSi) ~ log(Discharge_cms), data =. )),
              coef = list(coef(model[[1]])),
              Rsqrd = summary(model[[1]])$r.sq) %>%
    unnest_wider(coef, names_repair = 'unique')
  
  lm_results<-lm_results[,c(3,4)]
  lm_results$Stream_Name<-sites[i]
  
  CQ_results[[i]]<-lm_results
}

#dev.off()

Si_CQ_results_df<-do.call(bind_rows, CQ_results)

Si_res_df<-do.call(bind_rows, res_list)
Si_res_df$solute<-"Si"

#### for N ####
#### CQ residuals ####
sites<-unique(monthly_results_wide$Stream_Name)

CQ_results<-list()

res_list<-list()

pdf("N_CQ_residuals.pdf", width = 18, height = 6)

for (i in 1:length(sites)) {
  
  print(i)
  
  residuals_df <- monthly_results_wide %>%
    filter(Stream_Name == sites[i]) %>%
    filter(!is.na(N)) %>%
    mutate(
      logQ = log(Discharge_cms),
      logC = log(N)
    ) %>%
    do({
      model <- lm(logC ~ logQ, data = .)
      augment(model, .)
    }) %>%
    mutate(Month = as.factor(Month)) 
  
  monthly_residual<-residuals_df %>%
    group_by(Month) %>%
    summarise(mean_res=mean(.resid), sd_res=sd(.resid))
  
  monthly_residual$Stream_Name<-sites[i]
  
  res_list[[i]]<-monthly_residual
  
  p1<-monthly_results_wide %>%
    filter(Stream_Name==sites[i]) %>%
    ggplot(aes(log(Discharge_cms), log(N)))+geom_point(aes(col=as.factor(Month)), size=2)+theme_classic()+
    scale_color_manual(values = cet_pal(12, "cbtc1"))+
    ggtitle(sites[i])+
    labs(x="Log(Q)", y="Log(N Conc)", col="Month")+
    theme(text = element_text(size = 20))+
    geom_smooth(method = "lm", se=F, col="black")+
    stat_poly_eq(use_label(c("eq", "R2")))

  p2 <- ggplot(residuals_df, aes(x = Month, y = .resid, fill = Month)) +
    geom_boxplot(aes(group=Month)) +
    scale_fill_manual(values = cet_pal(12, "cbtc1")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_classic() +
    labs(
      x = "Month",
      y = "Residuals (logC - fitted)",
      fill = "Month"
    ) +
    theme(
      text = element_text(size = 20),
      legend.position = "none"
    )

  p3<-ggplot(residuals_df, aes(x = .resid*Discharge_cms, fill = Month)) +
    geom_histogram() +
    geom_vline(xintercept = 0, lty="dashed")+
    scale_fill_manual(values = cet_pal(12, "cbtc1")) +
    theme_classic() +
    labs(
      x = "Residual Flux",
      fill = "Month"
    ) +
    theme(
      text = element_text(size = 20),
      legend.position = "none"
    )
  p3

  p4<-ggarrange(p1, p2, p3, nrow = 1)

  print(p4)
  
  lm<-monthly_results_wide %>%
    filter(Stream_Name==sites[i]) %>%
    lm(log(N)~log(Discharge_cms), data = .)
  
  lm_results<-monthly_results_wide %>%
    filter(Stream_Name==sites[i]) %>%
    summarise(model = list(lm(log(N) ~ log(Discharge_cms), data =. )),
              coef = list(coef(model[[1]])),
              Rsqrd = summary(model[[1]])$r.sq) %>%
    unnest_wider(coef, names_repair = 'unique')
  
  lm_results<-lm_results[,c(3,4)]
  lm_results$Stream_Name<-sites[i]
  
  CQ_results[[i]]<-lm_results
}

dev.off()

N_CQ_results_df<-do.call(bind_rows, CQ_results)

N_res_df<-do.call(bind_rows, res_list)
N_res_df$solute<-"N"

#### for P ####
#### CQ residuals ####
sites<-unique(monthly_results_wide$Stream_Name)

CQ_results<-list()

res_list<-list()

pdf("P_CQ_residuals.pdf", width = 18, height = 6)

for (i in 1:length(sites)) {
  
  print(i)
  
  residuals_df <- monthly_results_wide %>%
    filter(Stream_Name == sites[i]) %>%
    filter(!is.na(P)) %>%
    mutate(
      logQ = log(Discharge_cms),
      logC = log(P)
    ) %>%
    do({
      model <- lm(logC ~ logQ, data = .)
      augment(model, .)
    }) %>%
    mutate(Month = as.factor(Month)) 
  
  monthly_residual<-residuals_df %>%
    group_by(Month) %>%
    summarise(mean_res=mean(.resid), sd_res=sd(.resid))
  
  monthly_residual$Stream_Name<-sites[i]
  
  res_list[[i]]<-monthly_residual
  
  p1<-monthly_results_wide %>%
    filter(Stream_Name==sites[i]) %>%
    ggplot(aes(log(Discharge_cms), log(P)))+geom_point(aes(col=as.factor(Month)), size=2)+theme_classic()+
    scale_color_manual(values = cet_pal(12, "cbtc1"))+
    ggtitle(sites[i])+
    labs(x="Log(Q)", y="Log(P Conc)", col="Month")+
    theme(text = element_text(size = 20))+
    geom_smooth(method = "lm", se=F, col="black")+
    stat_poly_eq(use_label(c("eq", "R2")))
  
  p2 <- ggplot(residuals_df, aes(x = Month, y = .resid, fill = Month)) +
    geom_boxplot(aes(group=Month)) +
    scale_fill_manual(values = cet_pal(12, "cbtc1")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_classic() +
    labs(
      x = "Month",
      y = "Residuals (logC - fitted)",
      fill = "Month"
    ) +
    theme(
      text = element_text(size = 20),
      legend.position = "none"
    )
  
  p3<-ggplot(residuals_df, aes(x = .resid*Discharge_cms, fill = Month)) +
    geom_histogram() +
    geom_vline(xintercept = 0, lty="dashed")+
    scale_fill_manual(values = cet_pal(12, "cbtc1")) +
    theme_classic() +
    labs(
      x = "Residual Flux",
      fill = "Month"
    ) +
    theme(
      text = element_text(size = 20),
      legend.position = "none"
    )
  p3
  
  p4<-ggarrange(p1, p2, p3, nrow = 1)
  
  print(p4)
  
  lm<-monthly_results_wide %>%
    filter(Stream_Name==sites[i]) %>%
    lm(log(P)~log(Discharge_cms), data = .)
  
  lm_results<-monthly_results_wide %>%
    filter(Stream_Name==sites[i]) %>%
    summarise(model = list(lm(log(P) ~ log(Discharge_cms), data =. )),
              coef = list(coef(model[[1]])),
              Rsqrd = summary(model[[1]])$r.sq) %>%
    unnest_wider(coef, names_repair = 'unique')
  
  lm_results<-lm_results[,c(3,4)]
  lm_results$Stream_Name<-sites[i]
  
  CQ_results[[i]]<-lm_results
}

dev.off()

P_CQ_results_df<-do.call(bind_rows, CQ_results)

P_res_df<-do.call(bind_rows, res_list)
P_res_df$solute<-"P"

all_res_df<-bind_rows(Si_res_df, N_res_df, P_res_df)

all_CQ_df<-bind_rows(Si_CQ_results_df, N_CQ_results_df, P_CQ_results_df)

write.csv(all_res_df, "All_Solutes_Monthly_Residuals.csv")

write.csv(all_CQ_df, "All_Solutes_CQ_slope_R2.csv")
