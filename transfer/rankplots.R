library(dplyr)
library(bayesplot)
r2 <- mcmc_intervals_data(hfitex20t, regex_pars="PLY_adj", prob = 0.5, 
                          prob_outer = 0.9, transformations = "exp", point_est = "median")
levels(r2$parameter)<-c('AD Hales', "AJ Finch", "Babar Azam", "BB McCullum", "C Munro", "CH Gayle", "DA Warner",
                        "DJ Malan", "DP Conway", "Morgan", "du Plessis", "Zaman", "Maxwell", "KL Rahul", "KP", "Mohammad Rizwan",
                        "SKY", "Watson",  "Dilshan", "V Kohli")
head(striker)
options(max.print = 100000000)
r2
r2$ll <- round(r2$ll * 6, 3)
r2$l <- round(r2$l * 6, 3)
r2$m <- round(r2$m * 6, 3)
r2$h <- round(r2$h * 6, 3)
r2$hh <- round(r2$hh * 6, 3)
ranks2 <- r2[, c("parameter", "ll", "l", "m","h","hh")]
ranks2<-arrange(ranks2, desc(m))
ranks2<-rename(ranks2, Batter = "parameter", "10th Percentile" = "ll", "25th Percentile" = l, "50th Percentile" = m, "75th Percentile" = h, "90th Percentile" = hh)
ranks2$Modelrank<- c(1:20)
ranks2$ICCrank<- c(2,18,6,14,19,3,9,10,12,1,8,17,11,16,15,4,7,13,5,20)
ranks2$SR <- c(176,156,142,151,141,143,138,139,134,134,136,130,136,138,145,138,127,129,129,121)
ranks2$AVG <- c(46.5,31.3,37.9,28.4,32.9,34.3,31.0,37.8,35.5,37.7,28.6,45.7,35.7,27.9,29.2,52.7,48.1,21.8,42.3,28.2)
ranks2<- ranks2 %>% relocate(ICCrank, .after = Batter)
ranks2<- ranks2 %>% relocate(Modelrank,.after = Batter)
ranks2<- ranks2 %>% relocate(SR,.after = ICCrank)
ranks2<- ranks2 %>% relocate(AVG,.after = ICCrank)
ranks2<-arrange(ranks2, Modelrank)

ranks2
ranks2k<- knitr::kable(ranks2, caption = "Top 20 Batters, Ranked by Median Expected Run Rate", "html") %>% 
  kable_styling(latex_options = c("striped", "scale_down"))

library(kableExtra)
library(webshot)
library(magick)
ranks2k %>% save_kable("test.png")
install.packages("webshot")
install.packages("magick")
