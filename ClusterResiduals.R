library(dplyr)
library(tidyr)
library(mclust)
library(PCAtools)
library(factoextra)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/CQResiduals")
residual_df<-read.csv("All_Solutes_Monthly_Residuals.csv")

X <- residual_df %>%
  mutate(
    Month = factor(Month, levels = 1:12)
  ) %>%
  pivot_wider(
    id_cols = Stream_Name,
    names_from  = c(solute, Month),
    values_from = mean_res,
    names_sep   = "_"
  )

stream_ids <- X$Stream_Name

X <- X %>%
  filter(!Stream_Name=="Imnavait Weir")

X_mat <- X %>% select(-Stream_Name)

#X_scaled<-X_mat

X_scaled <- scale(X_mat)

X_scaled_t<-t(X_scaled)

pca <- pca(X_scaled_t, metadata=stream_ids)

screeplot(pca)+theme_classic()+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1))

kept_pca<-pca$rotated[,1:6]

# test different numbers of clusters
set.seed(123)

p1<-fviz_nbclust(kept_pca, kmeans, method="wss", k.max = 20)

p1

p2<-fviz_nbclust(kept_pca, kmeans, method="silhouette", k.max = 20)

p2

ggarrange(p1, p2)

#kmeans with 11 clusters
set.seed(123)
kmeans_cluster<-kmeans(kept_pca, iter.max=50, nstart=50, centers = 6)

clusts<-kmeans_cluster$cluster
clusts_df<-data.frame(clusts)
clusts_df$Stream_Name<-stream_ids[-3]

table(clusts_df$clusts)

residual_df<-residual_df %>%
  left_join(clusts_df) %>%
  filter(!is.na(clusts))

residual_df %>%
  mutate(solute=factor(solute, levels=c("Si", "N", "P"))) %>%
  ggplot(aes(Month, mean_res))+geom_smooth(se=F, col="black")+
  facet_grid(rows = vars(solute), cols = vars(clusts))+
  ylim(-1,1)+theme_bw()+theme(text = element_text(size = 20))+
  scale_x_continuous(labels = seq(3,12,3), breaks = seq(3,12,3))+labs(x="Month", y="Mean Monthly Residual")+
  geom_hline(yintercept = 0)+geom_hline(yintercept = 0.2, lty="dashed")+geom_hline(yintercept = -0.2, lty="dashed")
  
spatial_data<-read.csv("Env_Data_AllSites.csv")

clusts_df %>%
  left_join(spatial_data) %>%
  ggplot(aes(x=as.factor(clusts), fill=impacted_class))+geom_bar(stat = "count")+theme_classic()+
  theme(text = element_text(size = 20))+labs(x="Cluster", y="Count")

clusts_df %>%
  left_join(spatial_data) %>%
  ggplot(aes(x=as.factor(clusts), fill=Name))+geom_bar(stat = "count")+theme_classic()+
  theme(text = element_text(size = 20))+labs(x="Cluster", y="Count")

