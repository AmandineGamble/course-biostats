# Environment Preparation ##############################################################

setwd("./figures")

library(ggplot2)
library(ggdark)

ggtheme = theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold",  margin=margin(2, 2, 2, 2)), 
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin=margin(0, 0, 0, 0)), 
    plot.margin = margin(5, 5, 5, 5), 
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    axis.title.y.right = element_text(angle = 90),
    axis.title = element_text(size = 12), 
    axis.text = element_text(size = 12, color = "black"), 
    axis.line = element_line(size = 0.25, color = "black"), 
    axis.ticks = element_line(size = 0.25, color = "black"), 
    strip.text = element_text(size = 12, margin=margin(1, 1, 2, 2)), 
    strip.background = element_blank(), 
    legend.margin = margin(2, 2, 2, 2), 
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 12), 
    legend.key = element_rect(fill = NA, color = NA), 
    legend.key.size = unit(0.4, "cm"), 
    legend.background = element_rect(colour = "transparent", fill = ggplot2::alpha("white", 0)), 
    legend.position = "none" 
  )

# Figures ##############################################################

## Cover #############

set.seed(8)

data = round(rgamma(1000, 1, 0.5))
data_a = as.data.frame(data)

data = round(rgamma(1000, 18, 0.5))
data_b = as.data.frame(data)

ggplot() +
  geom_histogram(data = data_a, aes(x = data), 
                 binwidth = 1, fill = "#339966", color = "black") +
  geom_histogram(data = data_b, aes(x = data), 
                 binwidth = 1, fill = "#007BB8", color = "black") +
  dark_theme_classic() +
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.line = element_blank(),
        axis.ticks = element_blank())

ggsave(paste0("export/cover.pdf"),
       width = 34, height = 19, units = "cm",  dpi = 150)

## Garnier et al. 2017 #############

data_Garnier2017 = read.table("datasets/Garnier_et_al_Data_Cutoff_NDV.csv", sep = ",", header = T)
data_Garnier2017 = as.data.frame(data_Garnier2017$PI)
colnames(data_Garnier2017) = "antibodies"

# Plot, seronegative only
ggplot(data = subset(data_Garnier2017, antibodies < 0.4), aes(x = antibodies)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +
  xlab("Antibody level") +
  ylab("Count") +
  coord_fixed(ratio = 1/20) +
  scale_x_continuous(limits = c(-0.3, 1.2)) +
  scale_y_continuous(limits = c(0, 17)) +
  ggtheme

ggsave(paste0("export/Garnier2017neg.pdf"),
       width = 10, height = 6, units = "cm",  dpi = 150)

# Plot, seronegative + seropositive
ggplot(data = data_Garnier2017, aes(x = antibodies)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +
  xlab("Antibody level") +
  ylab("Count") +
  coord_fixed(ratio = 1/20) +
  scale_x_continuous(limits = c(-0.3, 1.2)) +
  scale_y_continuous(limits = c(0, 17)) +
  ggtheme

ggsave(paste0("export/Garnier2017comp.pdf"),
       width = 10, height = 6, units = "cm",  dpi = 150)

## Gamble et al. 2019 #############

data_Wheeler2021 = read.table("datasets/Gamble_et_al_gulls_toxo_elisa_sia.csv", sep = ";", header = T)
data_Wheeler2021 = as.data.frame(data_Wheeler2021$OD_cor)
colnames(data_Wheeler2021) = "antibodies"

ggplot(data = data_Wheeler2021, aes(x = antibodies)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +
  xlab("Antibody level") +
  ylab("Count") +
  ggtheme

ggsave(paste0("export/Gamble2019.pdf"),
       width = 10, height = 6, units = "cm",  dpi = 150)

## Wheeler et al. 2021 #############

data_Wheeler2021 = read.table("datasets/Wheeler_et_al_2021_COVID_vaccine.csv", sep = ";", header = T)
colnames(data_Wheeler2021) = c("vaccine", "antibodies")

ggplot(data = subset(data_Wheeler2021, vaccine == "Pfizer"), aes(x = log10(antibodies))) +
  geom_histogram(binwidth = 0.2, fill = "#6C6AFF", color = "black", alpha = 0.5) +
  xlab("Antibody level") +
  ylab("Count") +
  scale_x_continuous(limits = c(2.5, 4.5), 
                     labels = c(expression(10^2.5), expression(10^3), expression(10^3.5), expression(10^4), expression(10^4.5))) +
  scale_y_continuous(limits = c(0, 6)) +
  ggtheme 

ggsave(paste0("export/Wheeler2021Pfizer.pdf"),
       width = 10, height = 5, units = "cm",  dpi = 150)

ggplot(data = subset(data_Wheeler2021, vaccine == "Moderna"), aes(x = log10(antibodies))) +
  geom_histogram(binwidth = 0.2, fill = "#FF4F4E", color = "black", alpha = 0.5) +
  xlab("Antibody level") +
  ylab("Count") +
  scale_x_continuous(limits = c(2.5, 4.5), 
                     labels = c(expression(10^2.5), expression(10^3), expression(10^3.5), expression(10^4), expression(10^4.5))) +
  scale_y_continuous(limits = c(0, 6)) +
  ggtheme

ggsave(paste0("export/Wheeler2021Moderna.pdf"),
       width = 10, height = 5, units = "cm",  dpi = 150)

## Simulated data #############

set.seed(10)

# Normal distribution

sec_case = round(rnorm(300, 3, 1))
data_norm = as.data.frame(sec_case)

ggplot(data = data_norm, aes(x = sec_case)) +
  geom_histogram(binwidth = 1, fill = "gray", color = "black", alpha = 0.5) +
  xlab(expression("Secondary cases "~italic(R)~"")) +
  ylab("Count") +
  scale_x_continuous(limits = c(-0.5, 30)) +
  scale_y_continuous(limits = c(0, 115)) +
  ggtheme

ggsave(paste0("export/Rnorm.pdf"),
       width = 10, height = 8, units = "cm",  dpi = 150)

mean(data_norm$sec_case)
median(data_norm$sec_case)

# Gamma distribution

sec_case = round(rgamma(300, 0.5, 0.2))
data_gamma = as.data.frame(sec_case)

ggplot(data = data_gamma, aes(x = sec_case)) +
  geom_histogram(binwidth = 1, fill = "gray", color = "black", alpha = 0.5) +
  xlab(expression("Secondary cases "~italic(R)~"")) +
  ylab("Count") +
  scale_x_continuous(limits = c(-0.5, 30)) +
  scale_y_continuous(limits = c(0, 115)) +
  ggtheme

ggsave(paste0("export/Rgamma.pdf"),
       width = 10, height = 8, units = "cm",  dpi = 150)

mean(data_gamma$sec_case)
median(data_gamma$sec_case)