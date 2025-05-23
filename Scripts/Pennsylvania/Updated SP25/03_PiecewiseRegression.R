#'---
#' title: Seasonal Movements of Wild Turkeys in the Mid-Atlantic Region
#' author: "K. Smelter, F. Buderman"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: df.Winter.Rdata
#'   html_document: 
#'     toc: true
#'---
#'  
#' **Purpose**: This script downloads movement data associated with nesting hens for NSD calculation
#' **Last Updated**: 5/21/25


# package names
packages<-c("tidyverse", "here", "mcp", "lubridate", "knitr", "ezknitr", "loo", "flextable")

# install any packages not previously installed
installed_packages<-packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))


df<-read_csv("nsdtest.csv")
head(df)

m1<-list(displacement~1)
m2<-list(displacement~1,~1)
m3<-list(displacement~1,~1,~1)
m4<-list(displacement~1,~1,~1,~1)
m5<-list(displacement~1,~1,~1,~1,~1)
m6<-list(displacement~1,~1,~1,~1,~1,~1)
m7<-list(displacement~1,~1,~1,~1,~1,~1,~1)

# Fit models
int_mods<-list(m1, m2, m3, m4, m5, m6, m7)
out_mods<-list()

for(i in 1:length(int_mods)){
  out_mods[[i]]<-mcp(model = int_mods[[i]], data = df[,c("days_numeric", "displacement")],
                     par_x = "days_numeric", adapt=5000)
}

for(i in 1:length(out_mods)){
  out_mods[[i]]$loo<-loo(out_mods[[i]])
}

loo_compare(out_mods[[1]]$loo,
            out_mods[[2]]$loo,
            out_mods[[3]]$loo,
            out_mods[[4]]$loo,
            out_mods[[5]]$loo,
            out_mods[[6]]$loo,
            out_mods[[7]]$loo)

loos<-loo_compare(out_mods[[1]]$loo,
                  out_mods[[2]]$loo,
                  out_mods[[3]]$loo,
                  out_mods[[4]]$loo,
                  out_mods[[5]]$loo,
                  out_mods[[6]]$loo,
                  out_mods[[7]]$loo)

loo_df<-as.data.frame(loos[1:7, 1:2])
loo_df[,1:2]<-lapply(loo_df[,1:2], round, 2)
loo_df<-cbind.data.frame('Model Syntax'=c("Six Intercepts", 
                                          "Five Intercepts",
                                          "Seven Intercepts",
                                          "Four Intercepts",
                                          "Two Intercepts",
                                          "Three Intercepts",
                                          "One Intercept"), loo_df)

loo_df %>%  flextable() %>% 
  align(part = "all") %>% # left align
  set_header_labels(values=list("Model Syntax" = "Model Syntax", 
                                "elpd_diff" = "ELPD Difference", 
                                "se_diff" = "SE of Difference")) %>% 
  set_caption(caption = "Leave-one-out Cross Validation was used to compute the Estimated Log Predictive Density (ELPD) for 7 models, each with an increasing number of intercepts. The highest ELPD for the model with 6 intercepts indicates that it has the highest predictive accuracy.") %>% 
  # font(fontname = "Calibri (Body)", part = "all") %>%
  fontsize(size = 12, part = "body") %>% 
  # add footer if you want
  # add_footer_row(values = "* p < 0.05. ** p < 0.01. *** p < 0.001.", 
  #                colwidths = 4) %>% 
  theme_booktabs() %>% # default theme
  border_outer() %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  colformat_num(digits = 2) %>% 
  autofit()


plot(out_mods[[6]], q_predict=T, q_fit=T)+
  xlab("Julian Date")+ylab("Displacement from wintering area (in km)\n")+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5))
