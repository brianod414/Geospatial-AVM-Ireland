csdat <- read.csv('./Data/cso_inflation_data.csv')

library(ggplot2)
library(cowplot)




colnames(csdat)
csdat$oVALUE <- csdat$VALUE
csdat$VALUE <- csdat$VALUE/100 + 1

table(csdat$Type)

cs_national <- csdat[csdat$Type == 'National - all residential properties', ]
cs_dublin <- csdat[csdat$Type == 'Dublin - all residential properties', ]
cs_exdublin <- csdat[csdat$Type == 'National excluding Dublin - all residential properties', ]

# create cusum for national 
cs_national$Month <- 1:12
cs_national$C.VALUE <- 1

for(i in 2:12){
  cs_national$C.VALUE[i] <- cs_national$VALUE[i]*cs_national$C.VALUE[i-1]
  
}

# create cusum for dublin 
cs_dublin$Month <- 1:12
cs_dublin$C.VALUE <- 1

for(i in 2:12){
  cs_dublin$C.VALUE[i] <- cs_dublin$VALUE[i]*cs_dublin$C.VALUE[i-1]
  
}


nat_plt <- ggplot(cs_national, aes(x = Month, y = log(C.VALUE))) +
  geom_line() +
  scale_x_continuous(breaks = 1:12) +
  labs(x = "Month", title = "National Property Index", y = "Percentage Change (%)") +
  theme_cowplot(font_size = 12)+theme(plot.title = element_text(hjust = 0.5)) 

dub_plt <- ggplot(cs_dublin, aes(x = Month, y = log(C.VALUE))) +
  geom_line() + 
  scale_x_continuous(breaks = 1:12) +
  labs(x = "Month", title = "Dublin Property Index", y = "Percentage Change (%)") +
  theme_cowplot(font_size = 12)+theme(plot.title = element_text(hjust = 0.5)) 

inf_plots <- plot_grid(nat_plt, dub_plt)
inf_plots
ggsave(filename = 'cso_inflation_plots.png', path = './Figures', width = 30, height = 15, units = 'cm')
