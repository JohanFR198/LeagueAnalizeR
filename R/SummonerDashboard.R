library(ggplot2)
library(plotly)
library(stringr)
library(dplyr)


ggplotly(ggplot(data=champs %>%
                  group_by(name) %>%
                  summarise(n=n(),
                            championPoints=mean(championPoints),
                            championLevel=mean(championLevel)) %>%
                  filter(n!=1),
                aes(x=reorder(name, -n, "desc"),
                    y=n,
                    text=paste("Champion:", name, "\n",
                               "Times Played:", n, "\n",
                               "Level:", championLevel, "\n",
                               "Points:", championPoints)))+
           geom_col(fill="#010A13")+#26919D
           geom_point(aes(y=10*(championPoints-min(championPoints))/(max(championPoints)-min(championPoints))),
                      col = "#956D2F")+
           geom_point(aes(y=10*(championLevel-min(championLevel))/(max(championLevel)-min(championLevel))),
                      col = "#D2A73E")+
           theme_classic()+
           coord_flip()+
           xlab("Champion")+
           ylab("Frequency")+
           theme(
             plot.background = element_rect(fill = "white"),
             panel.background = element_rect(fill = "white"),
             axis.line.x = element_line(color = "White")
           ),
         tooltip = "text")


ggplotly(ggplot(data=champs %>%
                  group_by(lane) %>%
                  summarise(n=n()),
                aes(x=reorder(lane, -n, "desc"),
                    y=n,
                    text=paste("Lane:", lane, "\n",
                               "Times Played:", n, "\n")))+
           geom_col(fill="#010A13")+#26919D
           theme_classic()+
           coord_flip()+
           xlab("Lane")+
           ylab("Frequency")+
           theme(
             plot.background = element_rect(fill = "white"),
             panel.background = element_rect(fill = "white"),
             axis.line.x = element_line(color = "White")
           ),
         tooltip = "text")

