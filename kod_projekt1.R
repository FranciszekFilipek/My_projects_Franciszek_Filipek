dt2 <- read.csv("C:\\Users\\User\\Downloads\\tracks_features.csv")
library("dplyr")
library("ggplot2")

# kod użyty na plakacie

dt2 %>% 
  mutate(valence2 = 400*(valence-0.5)) %>% 
  group_by(year) %>% 
  summarise(median_valence = median(valence2)) %>% 
  mutate(sign = ifelse(median_valence > 0,1,-1)) %>% 
  ggplot(aes(x = year, y = median_valence, fill = as.factor(sign))) +
  geom_col(width=0.6)+
  scale_fill_manual(values = c("#A546AC", "#40E0D0")) + 
  scale_x_continuous(n.breaks = 20, limits = c(1950, 2020),guide = guide_axis(n = 1)) +
  scale_y_continuous(n.breaks = 16, limits = c(-100, 50),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Percentagewise valence of songs across last 70 years",x = "",y = "Negative Valence                                  Positive Valence") +
  theme(
    plot.title = element_text(hjust = 0.5, face="bold", color = "#FFD700", size = 15),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face="bold",color= "#FFD700", size = 10.5),
    axis.text.y = element_text(vjust = 0.5, hjust=1, face="bold", size=12, color= "#FFD700"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#47115A"),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_line(color = "darkslategray2", linewidth = 0.000001, linetype = 3),
    legend.position="none",
    plot.background = element_rect(fill = "#47115A"),
    axis.title = element_text(size = 12, color = "#FFD700", face="bold")) +
  geom_vline(xintercept = 1985.5, linetype="longdash", size = 1, color = "#FFD700") +
  annotate("text", x = 1992, y = -65, label = "Rise of hip-hop", color = "#FFD700", size = 4.5)


# kod wizualizacji na spotkania projektowe:

dt2 %>% 
  group_by(year) %>% 
  summarise(median_energy = median(energy)) %>% 
  ggplot(aes(x = year, y = median_energy)) +
  scale_x_continuous(n.breaks = 14, limits = c(min_year,max_year)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(color='blue', size=1.5) +
  #  geom_point(shape=21, color='black', fill='red', size=2) +
  scale_y_continuous(n.breaks = 12) +
  theme(panel.background = element_rect(fill = 'lightblue', color = 'purple')) +
  labs(title = "Energiczność Piosenek przez ostatnie 70 lat",
       x = "Rok",
       y = "Mediana energiczności")

dt2 %>% 
  filter(year >= min_year, speechiness > 0) %>% 
  group_by(year) %>% 
  summarise(median_energy = median(energy), median_speech = median(speechiness)) %>% 
  ggplot(mapping = aes(x = median_energy, y = median_speech, color = year)) +
  geom_point(size = 2, position=position_jitter(h=0.003,w=0.003)) +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=12,face="bold")) +
  scale_color_gradient(low = "red", high = "green") +
  scale_y_continuous(n.breaks = 15, limits = c(0.035, 0.05)) +
  labs(title = "Zależność energii i ilości tekstu od roku",
       x = "Energiczność",
       y = "ilość tekstu", color = "Rok") +
  theme(legend.title = element_text(color = "blue", size = 15),
        legend.text = element_text(color = "red", face = "bold"))

