
library(ggplot2)
library(lubridate) 
library(zoo)
library(dplyr)
library(readxl) 

#data
GE <- read_excel("~/Documents/elections_files/greatbritain.xlsx",sheet = 'comp')
#chart
ggplot(GE, mapping = aes(x=Days, y=Share, color=Party))+
  geom_line(lwd=1.5)+ 
  geom_vline(xintercept = -1, linetype=4)+
  geom_vline(xintercept = 0, linetype=4)+
  annotate("text",fontsize=10, fontface="italic", x = -1,y = 25, label = "Election Day", angle=90, vjust=0)+
  annotate("text",fontsize=10, fontface="italic", x = 0,y = 25, label = "Election Results", angle=90, vjust=0)+
  
  scale_colour_manual(values = c('lightblue','#0099ff','lightgreen','green','#ff9980','#ff3300', '#ffc266',"#ff9900",'cyan','yellow','gold'))+ 
  scale_y_continuous(limits = c(min(0),max(50)))+ 
  theme_bw()+
  labs(title = 'Final 30 Days To Election Day - 2019 vs 2024',caption = 'Sources:BMG,DeltaPoll,Redfield&Wilton,PeoplePolling\nTeche,Opinum,Savanta,Survation,WeThink,MoreInCommon\nLord Ashcroft,JL Partners,Panelbase,Focaldata,Ipsos,Kantar,YouGov\nx.com/james_polls',x='',y='',color='')+
  theme_minimal() +
  theme(legend.position = "none",plot.title = element_text(family = "Andale Mono"),
        legend.text = element_text(face = 'bold',family = 'Andale Mono'),
        plot.caption = element_text(face = 'italic',hjust = 1, family = "Andale Mono", size = 10),
        axis.text.x = element_text(face = "bold"),axis.text.y = element_text(face = "bold")) 
ggsave('finalmonth_GB.png', dpi = 1080)

######################## polling firms and media
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
ggplot(gbmean, mapping = aes(x=end_date, y=pct_05da, color=Party))+
  geom_jitter(alpha=0.6)+ 
  geom_smooth(method = 'loess', formula = 'y~x',span=0.5, se=FALSE, lwd=1.5)+
  scale_colour_manual(values = c('#0099FF','green','tomato','orange',"cyan","yellow"))+
  scale_y_continuous(limits = c(min(0),max(50)))+ 
  theme_bw()+
  labs(title = 'Westminster Voting Intention - GE2024',
       caption = 'Sponsors:BestforBritain,ConservativeBritainAlliance,DailyMail,DailyMirror,
             EveningStandard,GBNews,GoodMorningBritain,LadyMcAlpine,
             MailOnSunday,SkyNews,TimesRadio,TheRestIsPolitics,
             TheObserver,THEi,TheTelegraph,TheSun,WPI Strategy\nx.com/james_polls',x='',y='',color='')+
  theme_minimal() +
  theme(legend.position = "none",plot.title = element_text(family = "Andale Mono"),
        legend.text = element_text(face = 'bold',family = 'Andale Mono'),
        plot.caption = element_text(face = 'italic',hjust = 1, family = "Andale Mono", size = 10),
        axis.text.x = element_text(face = "bold", angle = 30),axis.text.y = element_text(face = "bold")) +
  facet_wrap(~Pollster,ncol = 4) 
ggsave('mediapolls.png', dpi = 1080)
#yaml
- section: "MAPS"
contents: 
  - text: "Philadelphia 08 vs 12"
href: philly.qmd
- text: "Las Vegas" 
href: lasvegas.qmd
- section: "Massachusetts"
contents: 
  - text: "Population"
href: mass.qmd
- text: "State House"
href: MAStateHouse.qmd
- text: "Lawrence"
href: lawrence.qmd 