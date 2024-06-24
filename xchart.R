library(ggplot2)
library(lubridate) 
library(zoo)
library(dplyr)
library(readxl) 
#data polls
greatbritain <- read_excel("~/Documents/elections_files/greatbritain.xlsx")
#weighted average
gbmean <- greatbritain %>%
  dplyr::arrange(desc(Party)) %>% 
  dplyr::group_by(Party) %>% 
  dplyr::mutate(pct_02da = zoo::rollmean(Share, k = 4, fill = NA),
                pct_03da = zoo::rollmean(Share, k = 6, fill = NA),
                pct_05da = zoo::rollmean(Share, k = 8, fill = NA),
                pct_21da = zoo::rollmean(Share, k = 10, fill = NA)) %>% 
  dplyr::ungroup() 
#date format
greatbritain$end_date <- as.Date(paste(greatbritain$end_date, 1, sep="-"), format="%Y-%m-%d") 
greatbritain<-as.data.frame(greatbritain)
#plot 
ggplot(gbmean, mapping = aes(x=end_date, y=pct_02da, color=Party))+
  geom_jitter(alpha=0.6)+ 
  geom_smooth(method = 'loess', formula = 'y~x',span=0.5, se=FALSE, lwd=1.5)+
  scale_colour_manual(values = c('#0099FF','green','tomato','orange',"cyan","yellow"))+
  scale_y_continuous(limits = c(min(0),max(50)))+ 
  theme_bw()+
  labs(title = 'Westminster Voting Intention - GE2024',
       caption = 'Sponsors:BestforBritain,ConservativeBritainAlliance,DailyMail,DailyMirror,
             EveningStandard,GBNews,GoodMorningBritain,LadyMcAlpine,
             MailOnSunday,SkyNews,TimesRadio,TheRestIsPolitics,
             TheObserver,THEi,TheTelegraph,TheSun,WPI Strategy\njamesstats.github.io/projectx/britain',x='',y='',color='')+
  theme_minimal() +
  theme(legend.position = "none",plot.title = element_text(family = "Andale Mono"),
        legend.text = element_text(face = 'bold',family = 'Andale Mono'),
        plot.caption = element_text(face = 'italic',hjust = 1, family = "Andale Mono", size = 10),
        axis.text.x = element_text(face = "bold"),axis.text.y = element_text(face = "bold")) +
  facet_wrap(~Pollster,ncol = 4) 