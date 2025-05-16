setwd("C:/Users/memslie/OneDrive - Australian Institute of Marine Science/Desktop/working folder/Post 2024 bleaching/CL manuscript")


library(tidyverse)
library(patchwork)
library(tidybayes)
library(ggdist)
library(ggmap)
library(maps)
library(mapdata)
library(sp)
library(sf)
library(gridExtra)
library(raster)
library(lwgeom)
library(lubridate)
#library(rgeos)
#library(rgdal)
library(ggspatial)
library(oz)
library(rnaturalearth)
library(shapefiles)
library(patchwork)
library(mapping)


##### 
WriteLines("select p_code, a_sector,shelf,aims_reef_name, reef_name, fullreef_id,depth, visit_no,
report_year,sample_date,site_no,transect_no, family,
abundance
from v_rm_sample s, rm_fish05 f
where s.sample_id = f.sample_id
and p_code in ('RAP', 'RM','RMRAP') 
and visit_no is not null
and report_year>1996
and reef_name like 'LIZARD ISLAND'
and transect_no<6","latest.fish.sql")


system("java -jar dbExport.jar latest.fish.sql latest.fish.csv reef reefmon")


###############################################################################
#### dashboard model code




##### note - using latest data following hiccup with dahsboard - modelled dashboard data outputs



load(file="raw data/cl.sector.RData")
load(file="raw data/sec_estimates_CL.RData")

str(sec_estimates)

ann.hcc.post <- sec_estimates[['newdata_year_posteriors']]

cl.long.term.ave <- ann.hcc.post |> 
  group_by(.draw) |> 
  summarise(median=median(value)) |> 
  ungroup() |> 
  summarise(median=median(median))

cl.sector<-sec_estimates[['newdata_year']]
head(cl.sector)
Sector <- c(rep('CL',time=36))
cl.sector <- cbind(cl.sector,Sector)
save(cl.sector,file='cl.sector.RData')
load(file='raw data/cl.sector.RData')


cl_plot <- ggplot(cl.sector,aes(x=REPORT_YEAR,y=median*100))+
  geom_point(stat='identity',size=2)+
  geom_errorbar(stat='identity',width=0,aes(ymax=upper*100,ymin=lower*100))+
  geom_line(stat='identity',group=1)+
  #geom_hline(yintercept = 21.4,linetype='dashed',color='grey40')+
  #scale_x_continuous('Year',breaks=c(1985,1990,1995,2000,2005,2010,2015,2020,2025))+
  scale_x_continuous('Year',breaks=c(1985,1995,2005,2015,2025))+
  scale_y_continuous('Hard coral cover (%)')+
  #facet_wrap(~Sector,ncol=1,labeller = sec_labeller)+
  theme_classic()+
  theme(panel.grid.major.y=element_line(),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14))
cl_plot

save(cl_plot,file='processed/cl_plot.RData')
ggsave(cl_plot,file='outputs/CL_Sector temporal plot.png',height=12,width=8)


cl.sector.comp.rel<-sec_estimates[['newdata_yearcomp']] |> 
  filter(YearComp %in%c('2025-2024','2025-2023'),variable=='frac') |> 
  mutate(rel.decline=100*(median-1))
str(sec_estimates)

save(cl.sector.comp.rel,file='processed/cl.sector.comp.rel.RData')

###### rel change posteriors


cl.sec.comp.post <- sec_estimates[['newdata_yearcomp_posteriors']] |> 
  mutate(Relative_decline=100*(frac-1)) |> 
  filter(YearComp=='2025-2024')

save(cl.sec.comp.post,file='processed/cl.sec.comp.post.RData')


cl.rel.decline.plot<-ggplot(cl.sec.comp.post,aes(y=YearComp,x=Relative_decline))+    #,fill=Sector,color=Sector
  # geom_point(stat='identity',size=2)+
  # geom_errorbar(stat='identity',width=0,aes(ymax=upper*100,ymin=lower*100))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_slabinterval(aes(color = 'red'),
                    position = position_dodge(width = 0.1, preserve = "single"),
                    show.legend = FALSE) +
  stat_slab(aes(fill = 'red',colour='red'),# slab_alpha = after_stat(x) < 100),
            fill_type = "segments",
            height = 1,
            expand = FALSE, trim = TRUE, density = "bounded",
            width = 0.95,
            alpha = 0.5, 
            show.legend = FALSE)+
  scale_y_discrete('Sector',breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
  scale_x_continuous('Coral cover change (%)',limits=c(-60,20))+
  #annotate(geom='text',x=-35,y=2.85,label='p=1.0')+
  #annotate(geom='text',x=-36,y=1.85,label='p=0.999')+
  annotate(geom='text',x=-38.5,y=0.85,label='p=1.0')+
  #facet_wrap(~Sector,ncol=1,labeller = sec_labeller)+
  theme_classic()+
  theme(panel.grid.major.y=element_line(),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        axis.title.y = element_blank())

cl.rel.decline.plot
save(cl.rel.decline.plot,file='processed/cl.rel.decline.plot.RData')
ggsave(cl.rel.decline.plot,file='outputs/cl.rel.decline.plot.png',height=9,width=9)



pl<-'aaaabb'

combined_plot_cl <- cl_plot+cl.rel.decline.plot+
  plot_layout(design=pl)

combined_plot_cl


save(combined_plot_cl,file='processed/combined_plot_cl.RData')
ggsave(combined_plot_cl,file='outputs/combined_plot_cl.png',height=9,width=9)


##################################################################################
####  PLOTS FOR iep aPRIL 2025


# load(file="raw data/cl.sector.RData")
# load(file="raw data/sec_estimates_CL.RData")
# 
# str(sec_estimates)
# 
# ann.hcc.post <- sec_estimates[['newdata_year_posteriors']]
# 
# cl.long.term.ave <- ann.hcc.post |> 
#   group_by(.draw) |> 
#   summarise(median=median(value)) |> 
#   ungroup() |> 
#   summarise(median=median(median))
# 
# cl.sector<-sec_estimates[['newdata_year']]
# head(cl.sector)
# Sector <- c(rep('CL',time=36))
# cl.sector <- cbind(cl.sector,Sector)
# save(cl.sector,file='cl.sector.RData')
# load(file='raw data/cl.sector.RData')
# 
# 
# cl_plot <- ggplot(cl.sector,aes(x=REPORT_YEAR,y=median*100))+
#   geom_point(stat='identity',size=2)+
#   geom_errorbar(stat='identity',width=0,aes(ymax=upper*100,ymin=lower*100))+
#   geom_line(stat='identity',group=1)+
#   annotate(geom='text',label='Cooktown-Lizard Island',x=1985,y=45,size=5,fontface=2,hjust=0)+
#   #geom_hline(yintercept = 21.4,linetype='dashed',color='grey40')+
#   scale_x_continuous('',breaks=c(1985,1990,1995,2000,2005,2010,2015,2020,2025))+
#   scale_y_continuous('Cover (%)',limits=c(0,50),breaks=c(0,20,40))+
#   
#   #facet_wrap(~Sector,ncol=1,labeller = sec_labeller)+
#   theme_classic()+
#   theme(panel.grid.major.y=element_line(),
#         axis.text = element_text(size=11),
#         axis.title = element_text(size=11))
# cl_plot
# 
# save(cl_plot,file='processed/cl_plot.RData')
# ggsave(cl_plot,file='outputs/CL_Sector temporal plot.png',height=12,width=8)
# 
# 
# cl.sector.comp.rel<-sec_estimates[['newdata_yearcomp']] |> 
#   filter(YearComp %in%c('2025-2024','2025-2023'),variable=='frac') |> 
#   mutate(rel.decline=100*(median-1))
# str(sec_estimates)
# 
# save(cl.sector.comp.rel,file='processed/cl.sector.comp.rel.RData')
# 
# ###### rel change posteriors
# 
# 
# cl.sec.comp.post <- sec_estimates[['newdata_yearcomp_posteriors']] |> 
#   mutate(Relative_decline=100*(frac-1)) |> 
#   filter(YearComp=='2025-2024')
# 
# save(cl.sec.comp.post,file='processed/cl.sec.comp.post.RData')
# 
# 
# cl.rel.decline.plot<-ggplot(cl.sec.comp.post,aes(y=YearComp,x=Relative_decline))+    #,fill=Sector,color=Sector
#   # geom_point(stat='identity',size=2)+
#   # geom_errorbar(stat='identity',width=0,aes(ymax=upper*100,ymin=lower*100))+
#   geom_vline(xintercept = 0, linetype = "dashed") +
#   stat_slabinterval(aes(color = 'red'),
#                     position = position_dodge(width = 0.1, preserve = "single"),
#                     show.legend = FALSE) +
#   stat_slab(aes(fill = 'red',colour='red'),# slab_alpha = after_stat(x) < 100),
#             fill_type = "segments",
#             height = 1,
#             expand = FALSE, trim = TRUE, density = "bounded",
#             width = 0.95,
#             alpha = 0.5, 
#             show.legend = FALSE)+
#   scale_y_discrete('Sector',breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
#   scale_x_continuous('Change (%)',limits=c(-60,20))+
#   #annotate(geom='text',x=-35,y=2.85,label='p=1.0')+
#   #annotate(geom='text',x=-36,y=1.85,label='p=0.999')+
#   annotate(geom='text',x=-38.5,y=0.85,label='p=1.0')+
#   #facet_wrap(~Sector,ncol=1,labeller = sec_labeller)+
#   theme_classic()+
#   theme(panel.grid.major.y=element_line(),
#         axis.text = element_text(size=11),
#         axis.title = element_text(size=11),
#         axis.title.y = element_blank())
# 
# cl.rel.decline.plot
# save(cl.rel.decline.plot,file='processed/cl.rel.decline.plot.RData')
# ggsave(cl.rel.decline.plot,file='outputs/cl.rel.decline.plot.png',height=9,width=9)
# 
# 
# 
# pl<-'aaaabb'
# 
# combined_plot_cl <- cl_plot+cl.rel.decline.plot+
#   plot_layout(design=pl)
# 
# combined_plot_cl
# 
# 
# save(combined_plot_cl,file='processed/combined_plot_cl.RData')
# ggsave(combined_plot_cl,file='outputs/combined_plot_cl.png',height=3,width=6)
# 
# 

#############################################################################################################
# sector map --------------------------------------------------------------



gbr.sf <- sf::read_sf('raw data/Great_Barrier_Reef_Marine_Park_Boundary.shp')

qld.sf <- sf::read_sf('raw data/Great_Barrier_Reef_Features.shp') %>%
  filter(FEAT_NAME %in% c("Mainland","Island"))

reefs<-sf::read_sf('raw data/Great_Barrier_Reef_Features.shp') |> 
  filter(FEAT_NAME == "Reef")


towns.sf = rbind(data.frame(Town=c('Cairns'),Latitude=-16.93598,Longitude=145.7402),
                 data.frame(Town=c('Cooktown'),Latitude=-15.4736,Longitude=145.249221),
                 data.frame(Town=c('Innisfail'),Latitude=-17.520843,Longitude=146.030166),
                 data.frame(Town=c('Cardwell'),Latitude=-18.260526,Longitude=146.023342),
                 data.frame(Town=c('Lizard Island'),Latitude=-14.670577,Longitude=145.462032)
                 
)



ltmp.reefs <- read.csv(file='raw data/reef name lookup lat long.csv',strip.white=TRUE) |> 
  filter(REEF_NAME %in% c('CARTER REEF','YONGE REEF','NO NAME REEF','NORTH DIRECTION REEF','SOUTH DIRECTION REEF (NORTH)',
                          'LIZARD ISLAND','MARTIN REEF(14123)','LINNET REEF','MACGILLIVRAY REEF','EYRIE REEF',
                          'EYE REEF'))


reefs <- sf::st_as_sf(reefs, 
                      coords = c(x = "Longitude", y = "Latitude"))
reefs<- st_set_crs(reefs, "+proj=longlat +ellps=WGS84 +datum=WGS84")
reefs<- st_transform(reefs, "+proj=longlat +ellps=WGS84 +datum=WGS84")

sectors<-sf::read_sf('raw data/ltms.shp')

sf_use_s2(FALSE)
reefsec<-reefs %>% st_intersection(sectors) %>%
  mutate(SECTOR_NAM = factor(SECTOR_NAM,levels=c("Cape Grenville","Princess Charlotte Bay","Cooktown / Lizard Island",
                                                 "Cairns","Innisfail","Townsville","Whitsunday","Pompey","Swain","Capricorn Bunker")))


xlim <- c(144.3,149)
ylim <- c(-13.8,-15.87)


cl.sector.map<-ggplot()+
  geom_sf(data=sectors,fill=NA)+
  geom_sf(data=reefsec,aes(fill=SECTOR_NAM),
          show.legend=FALSE)+
  coord_sf(xlim=xlim,ylim=ylim)+
  scale_fill_manual(values=c('grey','grey','#FDBF6F','grey','grey','grey','grey','grey','grey','grey','grey'))+
  annotate(geom='rect',xmin=145.15,xmax=145.8,ymin=-14.35,ymax=-15.1,fill=NA,colour='black')+
  ggspatial::annotation_scale()+
  ggspatial::annotation_north_arrow(height = unit(0.5, "cm"),
                                    width = unit(0.5, "cm"),
                                    pad_y=unit(1,"cm"),
                                    pad_x=unit(2.25,'cm'))+
  ggtitle('a.')+
  scale_x_continuous(breaks=c(145,146))+
  theme_classic()+
  theme(panel.grid.major = element_line(),
        axis.title=element_blank(),
        axis.text=element_text(size=14))

cl.sector.map


#geom_sf(data=reefsec,fill=c('grey','grey','#FDBF6F','grey','grey','grey','grey','grey','grey','grey','grey'))+

aus<-map("worldHires", "Australia", fill=TRUE, xlim=c(110,160),
         ylim=c(-45,-5), mar=c(0,0,0,0))



oz.plot2<-  ggplot(fortify(aus), aes(y=lat, x=long, group=group)) + 
  geom_polygon(colour='grey20',fill='white')+
  geom_sf(data=sectors,inherit.aes=FALSE,fill=c('grey','grey','#FDBF6F','grey','grey','grey','grey','grey','grey','grey','grey'),colour='grey20',linewidth=0.5)+  
  annotate(geom='rect',xmin=143,xmax=147,ymin=-13.5,ymax=-16.5,fill=NA,colour='black')+
  theme_classic()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin = unit(c(0,0,0,0),'cm'))
oz.plot2

load(file='processed/combined_plot_cl.RData')

cl.map.results <- cl.sector.map+
  inset_element(oz.plot2,left=0,bottom=0.16,right=0.25,top=0.44)+
  inset_element(combined_plot_cl,left=0.4,bottom=0.25,right=0.95,top=0.75)
cl.map.results

##########################################################################################################################################

# reef level map ----------------------------------------------------------

carter <- read.csv(file='raw data/manta_reef_Carter Reef_HC_ _ _ _annual_summary.csv',strip.white=TRUE)

carter.comp.post <- read.csv(file='raw data/manta_reef_Carter Reef_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)

carter.comp.post <- carter.comp.post |> 
  filter(YearComp=='2025 - 2024') |> 
  mutate(Relative_decline=(frac-1)*100)

Reef <- c(rep('carter',time=1000))
Sector<- c(rep('Cooktown/Lizard',time=1000))

carter.comp.post <- cbind(carter.comp.post,Reef,Sector)

save(carter,file='processed/carter.RData')
save(carter.comp.post,file='processed/carter.comp.post.RData')


load(file='processed/reef_level_posteriors.RData')


##### carter

load(file='processed/carter.RData')


carter.plot <- ggplot(carter,aes(x=REPORT_YEAR,y=median*100))+
  geom_point(stat='identity',fill='#fdc086',pch=21)+
  geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#fdc086')+
  geom_line(group=1,colour='#fdc086')+
  #scale_y_continuous("Cover (%)",limits=c(0,50),breaks=c(0,20,40))+
  scale_y_continuous("",limits=c(0,61),breaks=c(0,20,40,60))+
  scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
  annotate(geom='text',label='Carter Reef',x=1996,y=59,size=3,fontface=2)+
  theme_classic()+
  theme(#axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size=8),
    axis.title = element_text(size=8),
    axis.text.x = element_blank(),
    strip.text = element_text(size=8),
    plot.margin = unit(c(0,0,0,0),'mm'))

carter.plot


carter_post_plot<-ggplot(reef_level_posteriors|> filter(Reef=='Carter Reef'),aes(y=Reef,x=Relative_decline))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_slabinterval(aes(color = Reef),
                    size=2.5,
                    position = position_dodge(width = 0.1, preserve = "single"),
                    show.legend = FALSE) +
  stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
            fill_type = "segments",
            height = 1,
            expand = FALSE, trim = FALSE, density = "unbounded",
            width = 0.95,
            alpha = 0.5, 
            show.legend = FALSE)+
  scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
  #scale_x_continuous('Change (%)')+
  scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
  scale_fill_manual(values='grey')+
  scale_colour_manual(values='grey50')+
  #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
  #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
  theme_classic()+
  theme(panel.grid.major.y=element_line(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=8),
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        plot.margin = unit(c(0,0,0,0),'mm'))


carter_post_plot


pl<-'aaaabb'

carter.comb.plot<-carter.plot+carter_post_plot+
  plot_layout(design=pl)

carter.comb.plot


save(carter.comb.plot,file='processed/carter.comb.plot.RData')
ggsave(carter.comb.plot,file='outputs/carter.comb.plot.png',height=6,width=9)


##### yonge 

yonge <- read.csv(file='raw data/manta_reef_Yonge Reef_HC_ _ _ _annual_summary.csv',strip.white=TRUE)

yonge.comp.post <- read.csv(file='raw data/manta_reef_Yonge Reef_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)

yonge.comp.post <- yonge.comp.post |> 
  filter(YearComp=='2025 - 2024') |> 
  mutate(Relative_decline=(frac-1)*100)

Reef <- c(rep('Yonge',time=1000))
Sector<- c(rep('Cooktown/Lizard',time=1000))

yonge.comp.post <- cbind(yonge.comp.post,Reef,Sector)

save(yonge,file='processed/yonge.RData')
save(yonge.comp.post,file='processed/yonge.comp.post.RData')




load(file='processed/yonge.RData')

yonge.plot <- ggplot(yonge,aes(x=REPORT_YEAR,y=mean*100))+
  geom_point(stat='identity',fill='#fdc086',pch=21)+
  geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#fdc086')+
  geom_line(group=1,colour='#fdc086')+
  #scale_y_continuous("Cover (%)",limits=c(0,50),breaks=c(0,20,40))+
  scale_y_continuous("",limits=c(0,61),breaks=c(0,20,40,60))+
  scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
  annotate(geom='text',label='Yonge Reef',x=1995.5,y=59,size=3,fontface=2)+
  theme_classic()+
  theme(#axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size=8),
    axis.title = element_text(size=8),
    axis.text.x = element_blank(),
    strip.text = element_text(size=8),
    plot.margin = unit(c(0,0,0,0),'mm'))

yonge.plot


yonge_post_plot<-ggplot(reef_level_posteriors|> filter(Reef=='Yonge Reef'),aes(y=Reef,x=Relative_decline))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_slabinterval(aes(color = Reef),
                    size=2.5,
                    position = position_dodge(width = 0.1, preserve = "single"),
                    show.legend = FALSE) +
  stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
            fill_type = "segments",
            height = 1,
            expand = FALSE, trim = FALSE, density = "unbounded",
            width = 0.95,
            alpha = 0.5, 
            show.legend = FALSE)+
  scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
  #scale_x_continuous('Change (%)')+
  scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
  scale_fill_manual(values='grey')+
  scale_colour_manual(values='grey50')+
  #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
  #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
  theme_classic()+
  theme(panel.grid.major.y=element_line(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=8),
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        plot.margin = unit(c(0,0,0,0),'mm'))


yonge_post_plot


yonge.comb.plot<-yonge.plot+yonge_post_plot+
  plot_layout(design=pl)

yonge.comb.plot


save(yonge.comb.plot,file='processed/yonge.comb.plot.RData')
ggsave(yonge.comb.plot,file='outputs/yonge.comb.plot.png',height=6,width=9)



##### No NAme

No_Name <- read.csv(file='raw data/manta_reef_No Name Reef_HC_ _ _ _annual_summary.csv',strip.white=TRUE)

No_Name.comp.post <- read.csv(file='raw data/manta_reef_Carter Reef_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)

No_Name.comp.post <- No_Name.comp.post |> 
  filter(YearComp=='2025 - 2024') |> 
  mutate(Relative_decline=(frac-1)*100)

Reef <- c(rep('No Name',time=1000))
Sector<- c(rep('Cooktown/Lizard',time=1000))

No_Name.comp.post <- cbind(No_Name.comp.post,Reef,Sector)

save(No_Name,file='processed/No_Name.RData')
save(No_Name.comp.post,file='processed/No_Name.comp.post.RData')



load(file='processed/No_Name.RData')

no_name.plot <- ggplot(No_Name,aes(x=REPORT_YEAR,y=mean*100))+
  geom_point(stat='identity',fill='#fdc086',pch=21)+
  geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#fdc086')+
  geom_line(group=1,colour='#fdc086')+
  #scale_y_continuous("Cover (%)",limits=c(0,50),breaks=c(0,20,40))+
  scale_y_continuous("",limits=c(0,61),breaks=c(0,20,40,60))+
  scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
  annotate(geom='text',label='No Name Reef',x=1998,y=59,size=3,fontface=2)+
  theme_classic()+
  theme(#axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size=8),
    axis.title = element_text(size=8),
    axis.text.x = element_blank(),
    strip.text = element_text(size=8),
    plot.margin = unit(c(0,0,0,0),'mm'))

no_name.plot


no_name_post_plot<-ggplot(reef_level_posteriors|> filter(Reef=='No_Name Reef'),aes(y=Reef,x=Relative_decline))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_slabinterval(aes(color = Reef),
                    size=2.5,
                    position = position_dodge(width = 0.1, preserve = "single"),
                    show.legend = FALSE) +
  stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
            fill_type = "segments",
            height = 1,
            expand = FALSE, trim = FALSE, density = "unbounded",
            width = 0.95,
            alpha = 0.5, 
            show.legend = FALSE)+
  scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
  scale_x_continuous('Change (%)',limits=c(-85,70),breaks=c(-80,-40,0,40))+
  #scale_x_continuous('Change (%)')+
  scale_fill_manual(values='grey')+
  scale_colour_manual(values='grey50')+
  #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
  #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
  theme_classic()+
  theme(panel.grid.major.y=element_line(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=8),
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        plot.margin = unit(c(0,0,0,0),'mm'))


no_name_post_plot


na_name.comb.plot<-no_name.plot+no_name_post_plot+
  plot_layout(design=pl)

na_name.comb.plot

save(na_name.comb.plot,file='processed/na_name.comb.plot.RData')
ggsave(na_name.comb.plot,file='outputs/na_name.comb.plot.png',height=6,width=9)




#####

linnet <- read.csv(file='raw data/manta_reef_Linnet Reef_HC_ _ _ _annual_summary.csv',strip.white=TRUE)

linnet.comp.post <- read.csv(file='raw data/manta_reef_Linnet Reef_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)

linnet.comp.post <- linnet.comp.post |> 
  filter(YearComp=='2025 - 2024') |> 
  mutate(Relative_decline=(frac-1)*100)

Reef <- c(rep('linnet',time=1000))
Sector<- c(rep('Cooktown/Lizard',time=1000))

linnet.comp.post <- cbind(linnet.comp.post,Reef,Sector)

save(linnet,file='processed/linnet.RData')
save(linnet.comp.post,file='processed/linnet.comp.post.RData')




load(file='processed/Linnet.RData')

linnet.plot <- ggplot(linnet,aes(x=REPORT_YEAR,y=mean*100))+
  geom_point(stat='identity',fill='#7fc97f',pch=21)+
  geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#7fc97f')+
  geom_line(group=1,colour='#7fc97f')+
  #scale_y_continuous("Cover (%)",limits=c(0,50),breaks=c(0,20,40))+
  scale_y_continuous("Cover (%)",limits=c(0,61),breaks=c(0,20,40,60))+
  scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
  annotate(geom='text',label='Linnet Reef',x=1994,y=59,size=3,fontface=2)+
  theme_classic()+
  theme(#axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size=8),
    axis.title = element_text(size=8),
    axis.text.x = element_blank(),
    strip.text = element_text(size=8),
    plot.margin = unit(c(0,0,0,0),'mm'))

linnet.plot


linnet_post_plot<-ggplot(reef_level_posteriors|> filter(Reef=='Linnet Reef'),aes(y=Reef,x=Relative_decline))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_slabinterval(aes(color = Reef),
                    size=2.5,
                    position = position_dodge(width = 0.1, preserve = "single"),
                    show.legend = FALSE) +
  stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
            fill_type = "segments",
            height = 1,
            expand = FALSE, trim = FALSE, density = "unbounded",
            width = 0.95,
            alpha = 0.5, 
            show.legend = FALSE)+
  scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
  scale_x_continuous('Change (%)',limits=c(-90,60),breaks=c(-80,-40,0,40))+
  #scale_x_continuous('Change (%)')+
  #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
  #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
  theme_classic()+
  theme(panel.grid.major.y=element_line(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=8),
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        plot.margin = unit(c(0,0,0,0),'mm'))


linnet_post_plot


linnet.comb.plot<-linnet.plot+linnet_post_plot+
  plot_layout(design=pl)

linnet.comb.plot

save(linnet.comb.plot,file='processed/linnet.comb.plot.RData')
ggsave(linnet.comb.plot,file='outputs/linnet.comb.plot.png',height=6,width=9)



#####
martin <- read.csv(file='raw data/manta_reef_Martin Reef_HC_ _ _ _annual_summary.csv',strip.white=TRUE)

martin.comp.post <- read.csv(file='raw data/manta_reef_Martin Reef_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)

martin.comp.post <- martin.comp.post |> 
  filter(YearComp=='2025 - 2024') |> 
  mutate(Relative_decline=(frac-1)*100)

Reef <- c(rep('martin',time=1000))
Sector<- c(rep('Cooktown/Lizard',time=1000))

martin.comp.post <- cbind(martin.comp.post,Reef,Sector)

save(martin,file='processed/martin.RData')
save(martin.comp.post,file='processed/martin.comp.post.RData')




load(file='processed/Martin.RData')

martin.plot <- ggplot(martin,aes(x=REPORT_YEAR,y=mean*100))+
  geom_point(stat='identity',fill='#7fc97f',pch=21)+
  geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#7fc97f')+
  geom_line(group=1,colour='#7fc97f')+
  #scale_y_continuous("Cover (%)",limits=c(0,50),breaks=c(0,20,40))+
  scale_y_continuous("",limits=c(0,61),breaks=c(0,20,40,60))+
  scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
  annotate(geom='text',label='Martin Reef',x=1994,y=59,size=3,fontface=2)+
  theme_classic()+
  theme(#axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size=8),
    axis.title = element_text(size=8),
    axis.text.x = element_blank(),
    strip.text = element_text(size=8),
    plot.margin = unit(c(0,0,0,0),'mm'))

martin.plot


martin_post_plot<-ggplot(reef_level_posteriors|> filter(Reef=='Martin Reef'),aes(y=Reef,x=Relative_decline))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_slabinterval(aes(color = Reef),
                    size=2.5,
                    position = position_dodge(width = 0.1, preserve = "single"),
                    show.legend = FALSE) +
  stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
            fill_type = "segments",
            height = 1,
            expand = FALSE, trim = FALSE, density = "unbounded",
            width = 0.95,
            alpha = 0.5, 
            show.legend = FALSE)+
  scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
  scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
  #scale_x_continuous('Change (%)')+
  #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
  #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
  theme_classic()+
  theme(panel.grid.major.y=element_line(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=8),
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        plot.margin = unit(c(0,0,0,0),'mm'))


martin_post_plot

martin.comb.plot<-martin.plot+martin_post_plot+
  plot_layout(design=pl)

martin.comb.plot

save(martin.comb.plot,file='processed/martin.comb.plot.RData')
ggsave(martin.comb.plot,file='outputs/martin.comb.plot.png',height=6,width=9)


#####

eyrie <- read.csv(file='raw data/manta_reef_Eyrie Reef_HC_ _ _ _annual_summary.csv',strip.white=TRUE)

eyrie.comp.post <- read.csv(file='raw data/manta_reef_Eyrie Reef_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)

eyrie.comp.post <- eyrie.comp.post |> 
  filter(YearComp=='2025 - 2024') |> 
  mutate(Relative_decline=(frac-1)*100)

Reef <- c(rep('eyrie',time=1000))
Sector<- c(rep('Cooktown/Lizard',time=1000))

eyrie.comp.post <- cbind(eyrie.comp.post,Reef,Sector)

save(eyrie,file='processed/eyrie.RData')
save(eyrie.comp.post,file='processed/eyrie.comp.post.RData')




load(file='processed/eyrie.RData')

eyrie.plot <- ggplot(eyrie,aes(x=REPORT_YEAR,y=mean*100))+
  geom_point(stat='identity',fill='#beaed4',pch=21)+
  geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#beaed4')+
  geom_line(group=1,colour='#beaed4')+
  #scale_y_continuous("Cover (%)",limits=c(0,50),breaks=c(0,20,40))+
  scale_y_continuous("",limits=c(0,61),breaks=c(0,20,40,60))+
  scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
  annotate(geom='text',label='Eyrie Reef',x=1993,y=59,size=3,fontface=2)+
  theme_classic()+
  theme(#axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size=8),
    axis.title = element_text(size=8),
    axis.text.x = element_blank(),
    strip.text = element_text(size=8),
    plot.margin = unit(c(0,0,0,0),'mm'))

eyrie.plot


eyrie_post_plot<-ggplot(reef_level_posteriors|> filter(Reef=='Eyrie Reef'),aes(y=Reef,x=Relative_decline))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_slabinterval(aes(color = Reef),
                    size=2.5,
                    position = position_dodge(width = 0.1, preserve = "single"),
                    show.legend = FALSE) +
  stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
            fill_type = "segments",
            height = 1,
            expand = FALSE, trim = FALSE, density = "unbounded",
            width = 0.95,
            alpha = 0.5, 
            show.legend = FALSE)+
  scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
  scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
  #scale_x_continuous('Change (%)')+
  #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
  #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
  theme_classic()+
  theme(panel.grid.major.y=element_line(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=8),
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        plot.margin = unit(c(0,0,0,0),'mm'))


eyrie_post_plot

eyrie.comb.plot<-eyrie.plot+eyrie_post_plot+
  plot_layout(design=pl)

eyrie.comb.plot

save(eyrie.comb.plot,file='processed/eyrie.comb.plot.RData')
ggsave(eyrie.comb.plot,file='outputs/eyrie.comb.plot.png',height=6,width=9)


#####

lizard <- read.csv(file='raw data/manta_reef_Lizard Isles_HC_ _ _ _annual_summary.csv',strip.white=TRUE)

lizard.comp.post <- read.csv(file='raw data/manta_reef_Lizard Isles_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)

lizard.comp.post <- lizard.comp.post |> 
  filter(YearComp=='2025 - 2024') |> 
  mutate(Relative_decline=(frac-1)*100)

Reef <- c(rep('lizard',time=1000))
Sector<- c(rep('Cooktown/Lizard',time=1000))

lizard.comp.post <- cbind(lizard.comp.post,Reef,Sector)

save(lizard,file='processed/lizard.RData')
save(lizard.comp.post,file='processed/lizard.comp.post.RData')



load(file='processed/lizard.RData')

lizard.plot <- ggplot(lizard,aes(x=REPORT_YEAR,y=mean*100))+
  geom_point(stat='identity',fill='#beaed4',pch=21)+
  geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#beaed4')+
  geom_line(group=1,colour='#beaed4')+
  #scale_y_continuous("Cover (%)",limits=c(0,50),breaks=c(0,20,40))+
  scale_y_continuous("Cover (%)",limits=c(0,61),breaks=c(0,20,40,60))+
  scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
  annotate(geom='text',label='Lizard Island',x=1995,y=59,size=3,fontface=2)+
  theme_classic()+
  theme(#axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size=8),
    axis.title = element_text(size=8),
    axis.text.x = element_blank(),
    strip.text = element_text(size=8),
    plot.margin = unit(c(0,0,0,0),'mm'))

lizard.plot


lizard_post_plot<-ggplot(reef_level_posteriors|> filter(Reef=='Lizard Island'),aes(y=Reef,x=Relative_decline))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_slabinterval(aes(color = Reef),
                    size=2.5,
                    position = position_dodge(width = 0.1, preserve = "single"),
                    show.legend = FALSE) +
  stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
            fill_type = "segments",
            height = 1,
            expand = FALSE, trim = FALSE, density = "unbounded",
            width = 0.95,
            alpha = 0.5, 
            show.legend = FALSE)+
  scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
  scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
  #scale_x_continuous('Change (%)')+
  #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
  #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
  theme_classic()+
  theme(panel.grid.major.y=element_line(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=8),
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        plot.margin = unit(c(0,0,0,0),'mm'))


lizard_post_plot

lizard.comb.plot<-lizard.plot+lizard_post_plot+
  plot_layout(design=pl)

lizard.comb.plot

save(lizard.comb.plot,file='processed/lizard.comb.plot.RData')
ggsave(lizard.comb.plot,file='outputs/lizard.comb.plot.png',height=6,width=9)


#####

macs <- read.csv(file='raw data/manta_reef_Macgillivray Reef_HC_ _ _ _annual_summary.csv',strip.white=TRUE)

macs.comp.post <- read.csv(file='raw data/manta_reef_Macgillivray Reef_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)

macs.comp.post <- macs.comp.post |> 
  filter(YearComp=='2025 - 2024') |> 
  mutate(Relative_decline=(frac-1)*100)

Reef <- c(rep('macs',time=1000))
Sector<- c(rep('Cooktown/Lizard',time=1000))

macs.comp.post <- cbind(macs.comp.post,Reef,Sector)

save(macs,file='processed/macs.RData')
save(macs.comp.post,file='processed/macs.comp.post.RData')



load(file='processed/Macgillvray.RData')

macgillvray.plot <- ggplot(macs,aes(x=REPORT_YEAR,y=mean*100))+
  geom_point(stat='identity',fill='#beaed4',pch=21)+
  geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#beaed4')+
  geom_line(group=1,colour='#beaed4')+
  scale_y_continuous("",limits=c(0,61),breaks=c(0,20,40,60))+
  #scale_y_continuous("",limits=c(0,50),breaks=c(0,20,40))+
  scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
  annotate(geom='text',label='Macgillivary Reef',x=1999.5,y=59,size=3,fontface=2)+
  theme_classic()+
  theme(#axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size=8),
    axis.title = element_text(size=8),
    axis.text.x = element_blank(),
    strip.text = element_text(size=8),
    plot.margin = unit(c(0,0,0,0),'mm'))

macgillvray.plot


macgillvray_post_plot<-ggplot(reef_level_posteriors|> filter(Reef=='Macgillvray Reef'),aes(y=Reef,x=Relative_decline))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_slabinterval(aes(color = Reef),
                    size=2.5,
                    position = position_dodge(width = 0.1, preserve = "single"),
                    show.legend = FALSE) +
  stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
            fill_type = "segments",
            height = 1,
            expand = FALSE, trim = FALSE, density = "unbounded",
            width = 0.95,
            alpha = 0.5, 
            show.legend = FALSE)+
  scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
  scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
  #scale_x_continuous('Change (%)')+
  #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
  #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
  theme_classic()+
  theme(panel.grid.major.y=element_line(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=8),
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        plot.margin = unit(c(0,0,0,0),'mm'))


macgillvray_post_plot



macgillvray.comb.plot<-macgillvray.plot+macgillvray_post_plot+
  plot_layout(design=pl)

macgillvray.comb.plot

save(macgillvray.comb.plot,file='processed/macgillvray.comb.plot.RData')
ggsave(macgillvray.comb.plot,file='outputs/macgillvray.comb.plot.png',height=6,width=9)


#####

NDirection <- read.csv(file='raw data/manta_reef_North Direction Island_HC_ _ _ _annual_summary.csv',strip.white=TRUE)

NDirection.comp.post <- read.csv(file='raw data/manta_reef_North Direction Island_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)

NDirection.comp.post <- NDirection.comp.post |> 
  filter(YearComp=='2025 - 2024') |> 
  mutate(Relative_decline=(frac-1)*100)

Reef <- c(rep('NDirection',time=1000))
Sector<- c(rep('Cooktown/Lizard',time=1000))

NDirection.comp.post <- cbind(NDirection.comp.post,Reef,Sector)

save(NDirection,file='processed/NDirection.RData')
save(NDirection.comp.post,file='processed/NDirection.comp.post.RData')


load(file='processed/NDirection.RData')

ndir.plot <- ggplot(NDirection,aes(x=REPORT_YEAR,y=mean*100))+
  geom_point(stat='identity',fill='#beaed4',pch=21)+
  geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#beaed4')+
  geom_line(group=1,colour='#beaed4')+
  scale_y_continuous("",limits=c(0,75),breaks=c(0,20,40,60))+
  #scale_y_continuous("Cover (%)",limits=c(0,61),breaks=c(0,20,40,60))+
  scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
  annotate(geom='text',label='North Direction',x=1998,y=65,size=3,fontface=2)+
  theme_classic()+
  theme(#axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size=8),
    axis.title = element_text(size=8),
    axis.text.x = element_blank(),
    strip.text = element_text(size=8),
    plot.margin = unit(c(0,0,0,0),'mm'))

ndir.plot


ndir_post_plot<-ggplot(reef_level_posteriors|> filter(Reef=='North Direction Island'),aes(y=Reef,x=Relative_decline))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_slabinterval(aes(color = Reef),
                    size=2.5,
                    position = position_dodge(width = 0.1, preserve = "single"),
                    show.legend = FALSE) +
  stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
            fill_type = "segments",
            height = 1,
            expand = FALSE, trim = FALSE, density = "unbounded",
            width = 0.95,
            alpha = 0.5, 
            show.legend = FALSE)+
  scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
  scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
  #scale_x_continuous('Change (%)',limits=c(-85,20),breaks=c(-80,-60,-40,-20,0,20))+
  #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
  #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
  theme_classic()+
  theme(panel.grid.major.y=element_line(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=8),
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        plot.margin = unit(c(0,0,0,0),'mm'))


ndir_post_plot


ndir.comb.plot<-ndir.plot+ndir_post_plot+
  plot_layout(design=pl)

ndir.comb.plot

save(ndir.comb.plot,file='processed/ndir.comb.plot.RData')
ggsave(ndir.comb.plot,file='outputs/ndir.comb.plot.png',height=6,width=9)


#####

load(file='processed/SDirection.RData')

sdir.plot <- ggplot(SDirection,aes(x=REPORT_YEAR,y=mean*100))+
  geom_point(stat='identity',fill='#beaed4',pch=21)+
  geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#beaed4')+
  geom_line(group=1,colour='#beaed4')+
  #scale_y_continuous("Cover (%)",limits=c(0,50),breaks=c(0,20,40))+
  scale_y_continuous("",limits=c(0,61),breaks=c(0,20,40,60))+
  scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
  annotate(geom='text',label='South Direction',x=1998,y=59,size=3,fontface=2)+
  theme_classic()+
  theme(#axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size=8),
    axis.title = element_text(size=8),
    axis.text.x = element_blank(),
    strip.text = element_text(size=8),
    plot.margin = unit(c(0,0,0,0),'mm'))

sdir.plot


sdir_post_plot<-ggplot(reef_level_posteriors|> filter(Reef=='South Direction Island'),aes(y=Reef,x=Relative_decline))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_slabinterval(aes(color = Reef),
                    size=2.5,
                    position = position_dodge(width = 0.1, preserve = "single"),
                    show.legend = FALSE) +
  stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
            fill_type = "segments",
            height = 1,
            expand = FALSE, trim = FALSE, density = "unbounded",
            width = 0.95,
            alpha = 0.5, 
            show.legend = FALSE)+
  scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
  scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
  #scale_x_continuous('Change (%)')+
  #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
  #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
  theme_classic()+
  theme(panel.grid.major.y=element_line(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=8),
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        plot.margin = unit(c(0,0,0,0),'mm'))


sdir_post_plot


sdir.comb.plot<-sdir.plot+sdir_post_plot+
  plot_layout(design=pl)

sdir.comb.plot

save(sdir.comb.plot,file='processed/sdir.comb.plot.RData')
ggsave(sdir.comb.plot,file='outputs/sdir.comb.plot.png',height=6,width=9)



#######  Eye Reef

eye <- read.csv(file='raw data/manta_reef_Eye Reef_HC_ _ _ _annual_summary.csv',strip.white=TRUE)

eye.comp.post <- read.csv(file='raw data/manta_reef_Eye Reef_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)

eye.comp.post <- eye.comp.post |> 
  filter(YearComp=='2025 - 2015') |> 
  mutate(Relative_decline=(frac-1)*100/10)

Reef <- c(rep('eye',time=1000))
Sector<- c(rep('Cooktown/Lizard',time=1000))

eye.comp.post <- cbind(eye.comp.post,Reef,Sector)

save(eye,file='processed/eye.RData')
save(eye.comp.post,file='processed/eyecomp.post.RData')



eye.plot.temporal<-ggplot(eye,aes(x=REPORT_YEAR,y=mean*100))+
  geom_point(stat='identity',fill='#beaed4',pch=21)+
  geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#beaed4')+
  geom_line(group=1,colour='#beaed4')+
  scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
  scale_y_continuous('',limits=c(0,61),breaks=c(0,20,40,60))+
  #facet_wrap(~Sector,ncol=1,labeller = sec_labeller)+
  annotate(geom='text',label='Eye Reef',x=1992,y=49,size=3,fontface=2)+
  theme_classic()+
  theme(#axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size=8),
    axis.title = element_text(size=8),
    axis.text.x = element_blank(),
    strip.text = element_text(size=8),
    plot.margin = unit(c(0,0,0,0),'mm'))
eye.plot.temporal


eye_post_plot<-ggplot(eye.comp.post,aes(y=Reef,x=Relative_decline))+
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_slabinterval(aes(color = Reef),
                    size=2.5,
                    position = position_dodge(width = 0.1, preserve = "single"),
                    show.legend = FALSE) +
  stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
            fill_type = "segments",
            height = 1,
            expand = FALSE, trim = FALSE, density = "unbounded",
            width = 0.95,
            alpha = 0.5, 
            show.legend = FALSE)+
  scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
  scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
  scale_fill_manual(values='seagreen')+
  scale_colour_manual(values='seagreen')+
  #scale_x_continuous('Change (%)')+
  #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
  #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
  theme_classic()+
  theme(panel.grid.major.y=element_line(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=8),
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        plot.margin = unit(c(0,0,0,0),'mm'))



eye_post_plot

pl<-'aaaabb'

eye.comb.plot<-eye.plot.temporal+eye_post_plot+
  plot_layout(design=pl)

eye.comb.plot


save(eye.comb.plot,file='processed/eye.comb.plot.RData')
ggsave(eye.comb.plot,file='outputs/eye.comb.plot.png',height=6,width=9)


#######################################################################################################
#### reef level map

# 
# xlim <- c(144.7,146.2)
# ylim <- c(-14.4,-15.1)
# 
# cl.map<-ggplot()+
#   geom_sf(data=reefs)+
#   coord_sf(xlim=xlim,ylim=ylim)+
#   #annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
#   annotate(geom='segment',x=145.17,xend=145.33,y=-15,yend=-14.8)+       #linnet
#   annotate(geom='segment',x=145.17,xend=145.336,y=-14.88,yend=-14.77)+  #martin
#   annotate(geom='segment',x=145.17,xend=145.34,y=-14.7,yend=-14.71)+    #eyrie
#   annotate(geom='segment',x=145.17,xend=145.415,y=-14.58,yend=-14.665)+ #lizard
#   annotate(geom='segment',x=145.17,xend=145.485,y=-14.42,yend=-14.645)+ #Macs
#   annotate(geom='segment',x=145.61,xend=145.75,y=-14.535,yend=-14.442)+ #carter
#   annotate(geom='segment',x=145.64,xend=145.75,y=-14.59,yend=-14.57)+   #Yonge
#   annotate(geom='segment',x=145.66,xend=145.75,y=-14.65,yend=-14.73)+   #no name  
#   annotate(geom='segment',x=145.52,xend=145.75,y=-14.75,yend=-14.84)+  #N direction
#   annotate(geom='segment',x=145.53,xend=145.75,y=-14.83,yend=-15)+  #s direction
#   ggtitle('b.')+
#   theme_classic()+
#   theme(panel.grid.major = element_line(),
#         axis.title=element_blank(),
#         plot.margin = unit(c(0,0,0,0),'cm'))
# 
# cl.map
# 
# 
# cl.result.map <- cl.map+
#   inset_element(carter.comb.plot,left=0.7,bottom=0.8,right=0.99,top=0.99)+
#   inset_element(yonge.comb.plot,left=0.7,bottom=0.6,right=0.99,top=0.79)+
#   inset_element(na_name.comb.plot,left=0.7,bottom=0.4,right=0.99,top=0.59)+
#   inset_element(linnet.comb.plot,left=0.02,bottom=0.01,right=0.31,top=0.2)+
#   inset_element(martin.comb.plot,left=0.02,bottom=0.21,right=0.31,top=0.4)+
#   inset_element(eyrie.comb.plot,left=0.02,bottom=0.41,right=0.31,top=0.6)+
#   inset_element(lizard.comb.plot,left=0.02,bottom=0.61,right=0.31,top=0.8)+
#   inset_element(macgillvray.comb.plot,left=0.02,bottom=0.81,right=0.31,top=1)+
#   inset_element(ndir.comb.plot,left=0.7,bottom=0.2,right=0.99,top=0.39)+
#   inset_element(sdir.comb.plot,left=0.7,bottom=0.01,right=0.99,top=0.2)
# 
# cl.result.map
# 
# 
# ggsave(cl.result.map,file='outputs/cl.result.map.png',height=9,width=12)
# 
# 
# pl2 <- 'aaaa
#         aaaa
#         bbbb
#         bbbb'
# 
# cl.results.map.manuscript <- cl.map.results+cl.result.map+
#   plot_layout(design=pl2)
# 
# cl.results.map.manuscript
# 
# ggsave(cl.results.map.manuscript,file='outputs/cl.results.map.manuscript.png',height=12,width=12)
# 
# 
# #######################################################################################################
# #### reef level map with Eye reef
# 
# 
# xlim <- c(144.7,146.2)
# ylim <- c(-14.4,-15.1)
# 
# cl.map<-ggplot()+
#   geom_sf(data=reefs)+
#   coord_sf(xlim=xlim,ylim=ylim)+
#   geom_point(data=ltmp.reefs,aes(x=REEF_LONG,y=REEF_LAT,fill=SHELF),pch=21,size=3,show.legend=FALSE)+
#   scale_fill_manual(values = c('#7fc97f','#beaed4','#fdc086'))+
#   #annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
#   annotate(geom='segment',x=145.17,xend=145.33,y=-15,yend=-14.8)+       #linnet
#   annotate(geom='segment',x=145.17,xend=145.336,y=-14.88,yend=-14.77)+  #martin
#   annotate(geom='segment',x=145.17,xend=145.34,y=-14.7,yend=-14.71)+    #eyrie
#   annotate(geom='segment',x=145.17,xend=145.415,y=-14.58,yend=-14.665)+ #lizard
#   annotate(geom='segment',x=145.17,xend=145.485,y=-14.42,yend=-14.645)+ #Macs
#   annotate(geom='segment',x=145.61,xend=145.75,y=-14.535,yend=-14.442)+ #carter
#   annotate(geom='segment',x=145.64,xend=145.75,y=-14.59,yend=-14.57)+   #Yonge
#   annotate(geom='segment',x=145.66,xend=145.75,y=-14.65,yend=-14.73)+   #no name  
#   annotate(geom='segment',x=145.52,xend=145.75,y=-14.75,yend=-14.84)+  #N direction
#   annotate(geom='segment',x=145.53,xend=145.75,y=-14.83,yend=-15)+  #s direction
#   annotate(geom='segment',x=145.48,xend=145.45,y=-14.908,yend=-15.19)+  #eye
#   ggtitle('b.')+
#   theme_classic()+
#   theme(panel.grid.major = element_line(),
#         axis.title=element_blank(),
#         plot.margin = unit(c(0,0,0,0),'cm'))
# 
# cl.map
# 
# 
# cl.result.map <- cl.map+
#   inset_element(carter.comb.plot,left=0.7,bottom=0.8,right=0.99,top=0.99)+
#   inset_element(yonge.comb.plot,left=0.7,bottom=0.6,right=0.99,top=0.79)+
#   inset_element(na_name.comb.plot,left=0.7,bottom=0.4,right=0.99,top=0.59)+
#   inset_element(linnet.comb.plot,left=0.02,bottom=0.01,right=0.31,top=0.2)+
#   inset_element(martin.comb.plot,left=0.02,bottom=0.21,right=0.31,top=0.4)+
#   inset_element(eyrie.comb.plot,left=0.02,bottom=0.41,right=0.31,top=0.6)+
#   inset_element(lizard.comb.plot,left=0.02,bottom=0.61,right=0.31,top=0.8)+
#   inset_element(macgillvray.comb.plot,left=0.02,bottom=0.81,right=0.31,top=1)+
#   inset_element(ndir.comb.plot,left=0.7,bottom=0.2,right=0.99,top=0.39)+
#   inset_element(sdir.comb.plot,left=0.7,bottom=0.01,right=0.99,top=0.2)+
#   inset_element(eye.comb.plot,left=0.35,bottom=0.01,right=0.64,top=0.2)
#   
# cl.result.map
# 
# 
# ggsave(cl.result.map,file='outputs/cl.result.map_with_Eye.png',height=9,width=12)
# 
# 
# pl2 <- 'aaaa
#         aaaa
#         bbbb
#         bbbb'
# 
# cl.results.map.manuscript <- cl.map.results+cl.result.map+
#   plot_layout(design=pl2)
# 
# cl.results.map.manuscript
# 
# ggsave(cl.results.map.manuscript,file='outputs/cl.results.map.manuscript_with_Eye.png',height=12,width=12)

##################################################################################################################
## update Jan 2025 ---------------------------------------------------------

cl <- read.csv(file='raw data/manta_Sectors_CL_HC_ _ _ _annual_summary.csv',strip.white=TRUE)

cl.comp.post <- read.csv(file='raw data/manta_Sectors_CL_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)

cl.comp.post <- cl.comp.post |> 
  filter(YearComp=='2025 - 2024') |> 
  mutate(Relative_decline=(frac-1)*100)

  #Reef <- c(rep('Boulder',time=1000))
  Sector<- c(rep('Cooktown/Lizard',time=1000))

  cl.comp.post <- cbind(cl.comp.post,Sector)
  
  save(cl,file='processed/cl.RData')
  save(cl.comp.post,file='processed/cl.comp.post.RData')
  
 load(file='processed/cl.RData') 
 load(file='processed/cl.comp.post.RData')
 
 median.decline <- cl.comp.post |> 
   summarise(median(Relative_decline)) 

  cl.plot.temporal<-ggplot(cl,aes(x=REPORT_YEAR,y=mean*100))+
    geom_point(stat='identity',fill='black',pch=21)+
    geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='black')+
    geom_line(group=1,colour='black')+
    scale_x_continuous('Year',breaks=c(1985,1990,1995,2000,2005,2010,2015,2020,2025))+
    scale_y_continuous('',limits=c(0,61),breaks=c(0,20,40,60))+
    #facet_wrap(~Sector,ncol=1,labeller = sec_labeller)+
    #annotate(geom='text',label='Boulder Reef',x=2012,y=49,size=3,fontface=2)+
    theme_classic()+
    theme(#axis.line.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size=8),
      axis.title = element_text(size=8),
      strip.text = element_text(size=8),
      plot.margin = unit(c(0,0,0,0),'mm'))
  cl.plot.temporal
  
  
  cl_post_plot<-ggplot(cl.comp.post,aes(y=Sector,x=Relative_decline))+
    geom_vline(xintercept = 0, linetype = "dashed") +
    stat_slabinterval(aes(color = Sector),
                      size=2.5,
                      position = position_dodge(width = 0.1, preserve = "single"),
                      show.legend = FALSE) +
    stat_slab(aes(fill = Sector,colour=Sector),# slab_alpha = after_stat(x) < 100),
              fill_type = "segments",
              height = 1,
              expand = FALSE, trim = FALSE, density = "unbounded",
              width = 0.95,
              alpha = 0.5, 
              show.legend = FALSE)+
    scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
    scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
    #scale_x_continuous('Change (%)')+
    #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
    #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
    theme_classic()+
    theme(panel.grid.major.y=element_line(),
          axis.text = element_text(size=8),
          axis.title = element_text(size=8),
          strip.text = element_text(size=8),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          plot.margin = unit(c(0,0,0,0),'mm'))
  
  
  cl_post_plot
  
  pl<-'aaaabb'
  
  cl.comb.plot<-cl.plot.temporal+cl_post_plot+
    plot_layout(design=pl)
  
  cl.comb.plot
  
  
  save(cl.comb.plot,file='processed/cl.comb.plot.RData')
  ggsave(cl.comb.plot,file='outputs/clr.comb.plot.png',height=6,width=9)
  
###
  
  boulder <- read.csv(file='raw data/manta_reef_Boulder Reef_HC_ _ _ _annual_summary.csv',strip.white=TRUE)
  
  boulder.comp.post <- read.csv(file='raw data/manta_reef_Boulder Reef_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)
  
  boulder.comp.post <- boulder.comp.post |> 
    filter(YearComp=='2025 - 2023') |> 
    mutate(Relative_decline=(frac-1)*100/2)
  
  Reef <- c(rep('Boulder',time=1000))
  Sector<- c(rep('Cooktown/Lizard',time=1000))
  
  boulder.comp.post <- cbind(boulder.comp.post,Reef,Sector)
  
  save(boulder,file='processed/boulder.RData')
  save(boulder.comp.post,file='processed/boulder.comp.post.RData')
  
  
  
  boulder.plot.temporal<-ggplot(boulder,aes(x=REPORT_YEAR,y=mean*100))+
    geom_point(stat='identity',fill='#7fc97f',pch=21)+
    geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#7fc97f')+
    geom_line(group=1,colour='#7fc97f')+
    scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
    scale_y_continuous("",limits=c(0,61),breaks=c(0,20,40,60))+
    #facet_wrap(~Sector,ncol=1,labeller = sec_labeller)+
    annotate(geom='text',label='Boulder Reef',x=1996,y=59,size=3,fontface=2)+
    theme_classic()+
    theme(#axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size=8),
    axis.title = element_text(size=8),
    axis.text.x = element_text(size=8),
    strip.text = element_text(size=8),
    plot.margin = unit(c(0,0,0,0),'mm'))
  boulder.plot.temporal
  
  
  boulder_post_plot<-ggplot(boulder.comp.post,aes(y=Reef,x=Relative_decline))+
    geom_vline(xintercept = 0, linetype = "dashed") +
    stat_slabinterval(aes(color = Reef),
                      size=2.5,
                      position = position_dodge(width = 0.1, preserve = "single"),
                      show.legend = FALSE) +
    stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
              fill_type = "segments",
              height = 1,
              expand = FALSE, trim = FALSE, density = "unbounded",
              width = 0.95,
              alpha = 0.5, 
              show.legend = FALSE)+
    scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
    scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
    #scale_x_continuous('Change (%)')+
    #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
    #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
    theme_classic()+
    theme(panel.grid.major.y=element_line(),
          axis.text = element_text(size=8),
          axis.title = element_text(size=8),
          strip.text = element_text(size=7),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          plot.margin = unit(c(0,0,0,0),'mm'))
  
  
  boulder_post_plot
  
  pl<-'aaaabb'
  
  boulder.comb.plot<-boulder.plot.temporal+boulder_post_plot+
    plot_layout(design=pl)
  
  boulder.comb.plot
  
  
  save(boulder.comb.plot,file='processed/boulder.comb.plot.RData')
  ggsave(boulder.comb.plot,file='outputs/boulder.comb.plot.png',height=6,width=9)
  
  
  ###
  
  egret <- read.csv(file='raw data/manta_reef_Egret Reef_HC_ _ _ _annual_summary.csv',strip.white=TRUE)
  
  egret.comp.post <- read.csv(file='raw data/manta_reef_Egret Reef_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)
  
  egret.comp.post <- egret.comp.post |> 
    filter(YearComp=='2025 - 2023') |> 
    mutate(Relative_decline=(frac-1)*100/2)
  
  Reef <- c(rep('egret',time=1000))
  Sector<- c(rep('Cooktown/Lizard',time=1000))
  
  egret.comp.post <- cbind(egret.comp.post,Reef,Sector)
  
  save(egret,file='processed/egret.RData')
  save(egret.comp.post,file='processed/egret.comp.post.RData')
  
  
  
  egret.plot.temporal<-ggplot(egret,aes(x=REPORT_YEAR,y=mean*100))+
    geom_point(stat='identity',fill='#7fc97f',pch=21)+
    geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#7fc97f')+
    geom_line(group=1,colour='#7fc97f')+
    scale_x_continuous('Year',breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
    scale_y_continuous('Cover (%)',limits=c(0,61),breaks=c(0,20,40,60))+
    #facet_wrap(~Sector,ncol=1,labeller = sec_labeller)+
    annotate(geom='text',label='Egret Reef',x=1994,y=59,size=3,fontface=2)+
    theme_classic()+
    theme(#axis.line.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size=8),
      axis.title = element_text(size=8),
      strip.text = element_text(size=7),
      axis.text.x = element_text(size=8),
      plot.margin = unit(c(0,0,0,0),'mm'))
  egret.plot.temporal
  
  
  egret_post_plot<-ggplot(egret.comp.post,aes(y=Reef,x=Relative_decline))+
    geom_vline(xintercept = 0, linetype = "dashed") +
    stat_slabinterval(aes(color = Reef),
                      size=2.5,
                      position = position_dodge(width = 0.1, preserve = "single"),
                      show.legend = FALSE) +
    stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
              fill_type = "segments",
              height = 1,
              expand = FALSE, trim = TRUE, density = "unbounded",
              width = 0.95,
              alpha = 0.5, 
              show.legend = FALSE)+
    scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
    scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
    scale_fill_manual(values='grey')+
    scale_colour_manual(values='grey50')+
    #scale_x_continuous('Change (%)')+
    #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
    #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
    theme_classic()+
    theme(panel.grid.major.y=element_line(),
          axis.text = element_text(size=8),
          axis.title = element_text(size=8),
          strip.text = element_text(size=7),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          plot.margin = unit(c(0,0,0,0),'mm'))
  
  
  egret_post_plot
  
  pl<-'aaaabb'
  
  egret.comb.plot<-egret.plot.temporal+egret_post_plot+
    plot_layout(design=pl)
  
  egret.comb.plot
  
  
  save(egret.comb.plot,file='processed/egret.comb.plot.RData')
  ggsave(egret.comb.plot,file='outputs/egret.comb.plot.png',height=6,width=9)
  
  
  ###
  
  forrester <- read.csv(file='raw data/manta_reef_Forrester Reef_HC_ _ _ _annual_summary.csv',strip.white=TRUE)
  
  forrester.comp.post <- read.csv(file='raw data/manta_reef_Forrester Reef_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)
  
  forrester.comp.post <- forrester.comp.post |> 
    filter(YearComp=='2025 - 2023') |> 
    mutate(Relative_decline=(frac-1)*100/2)
  
  Reef <- c(rep('forrester',time=1000))
  Sector<- c(rep('Cooktown/Lizard',time=1000))
  
  forrester.comp.post <- cbind(forrester.comp.post,Reef,Sector)
  
  save(forrester,file='processed/forrester.RData')
  save(forrester.comp.post,file='processed/forrester.comp.post.RData')
  
  
  
  forrester.plot.temporal<-ggplot(forrester,aes(x=REPORT_YEAR,y=mean*100))+
    geom_point(stat='identity',fill='#beaed4',pch=21)+
    geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#beaed4')+
    geom_line(group=1,colour='#beaed4')+
    scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
    scale_y_continuous("",limits=c(0,61),breaks=c(0,20,40,60))+
    #facet_wrap(~Sector,ncol=1,labeller = sec_labeller)+
    annotate(geom='text',label='Forrester Reef',x=1997,y=59,size=3,fontface=2)+
    theme_classic()+
    theme(#axis.line.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size=8),
      axis.title = element_text(size=8),
      axis.text.x = element_blank(),
      strip.text = element_text(size=8),
      plot.margin = unit(c(0,0,0,0),'mm'))
  forrester.plot.temporal
  
  
  forrester_post_plot<-ggplot(forrester.comp.post,aes(y=Reef,x=Relative_decline))+
    geom_vline(xintercept = 0, linetype = "dashed") +
    stat_slabinterval(aes(color = Reef),
                      size=2.5,
                      position = position_dodge(width = 0.1, preserve = "single"),
                      show.legend = FALSE) +
    stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
              fill_type = "segments",
              height = 1,
              expand = FALSE, trim = FALSE, density = "unbounded",
              width = 0.95,
              alpha = 0.5, 
              show.legend = FALSE)+
    scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
    scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
    #scale_x_continuous('Change (%)')+
    #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
    #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
    theme_classic()+
    theme(panel.grid.major.y=element_line(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          strip.text = element_text(size=8),
          # axis.title.y=element_blank(),
          # axis.text.y=element_blank(),
          plot.margin = unit(c(0,0,0,0),'mm'))
  
  
  forrester_post_plot
  
  pl<-'aaaabb'
  
  forrester.comb.plot<-forrester.plot.temporal+forrester_post_plot+
    plot_layout(design=pl)
  
  forrester.comb.plot
  
  
  save(forrester.comb.plot,file='processed/forrester.comb.plot.RData')
  ggsave(forrester.comb.plot,file='outputs/forrester.comb.plot.png',height=6,width=9)
  
  ###
  
  helsdon <- read.csv(file='raw data/manta_reef_Helsdon Reef_HC_ _ _ _annual_summary.csv',strip.white=TRUE)
  
  helsdon.comp.post <- read.csv(file='raw data/manta_reef_Helsdon Reef_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)
  
  helsdon.comp.post <- helsdon.comp.post |> 
    filter(YearComp=='2025 - 2021') |> 
    mutate(Relative_decline=(frac-1)*100/4)
  
  Reef <- c(rep('Helsdon',time=1000))
  Sector<- c(rep('Cooktown/Lizard',time=1000))
  
  helsdon.comp.post <- cbind(helsdon.comp.post,Reef,Sector)
  
  save(helsdon,file='processed/helsdon.RData')
  save(helsdon.comp.post,file='processed/helsdon.comp.post.RData')
  
  
  
  helsdon.plot.temporal<-ggplot(helsdon,aes(x=REPORT_YEAR,y=mean*100))+
    geom_point(stat='identity',fill='#beaed4',pch=21)+
    geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#beaed4')+
    geom_line(group=1,colour='#beaed4')+
    scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
    scale_y_continuous("",limits=c(0,61),breaks=c(0,20,40,60))+
    #facet_wrap(~Sector,ncol=1,labeller = sec_labeller)+
    annotate(geom='text',label='Helsdon Reef',x=1995.5,y=59,size=3,fontface=2)+
    theme_classic()+
    theme(#axis.line.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size=8),
      axis.title = element_text(size=8),
      axis.text.x = element_blank(),
      strip.text = element_text(size=8),
      plot.margin = unit(c(0,0,0,0),'mm'))
  helsdon.plot.temporal
  
  
  helsdon_post_plot<-ggplot(helsdon.comp.post,aes(y=Reef,x=Relative_decline))+
    geom_vline(xintercept = 0, linetype = "dashed") +
    stat_slabinterval(aes(color = Reef),
                      size=2.5,
                      position = position_dodge(width = 0.1, preserve = "single"),
                      show.legend = FALSE) +
    stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
              fill_type = "segments",
              height = 1,
              expand = FALSE, trim = FALSE, density = "unbounded",
              width = 0.95,
              alpha = 0.5, 
              show.legend = FALSE)+
    scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
    scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
    #scale_x_continuous('Change (%)')+
    #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
    #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
    theme_classic()+
    theme(panel.grid.major.y=element_line(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          strip.text = element_text(size=8),
          # axis.title.y=element_blank(),
          # axis.text.y=element_blank(),
          plot.margin = unit(c(0,0,0,0),'mm'))
  
  
  helsdon_post_plot
  
  pl<-'aaaabb'
  
  helsdon.comb.plot<-helsdon.plot.temporal+helsdon_post_plot+
    plot_layout(design=pl)
  
  helsdon.comb.plot
  
  
  save(helsdon.comb.plot,file='processed/helsdon.comb.plot.RData')
  ggsave(helsdon.comb.plot,file='outputs/helsdon.comb.plot.png',height=6,width=9)
  
  ###
  
  marx <- read.csv(file='raw data/manta_reef_Marx_HC_ _ _ _annual_summary.csv',strip.white=TRUE)
  
  marx.comp.post <- read.csv(file='raw data/manta_reef_Marx_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)
  
  marx.comp.post <- marx.comp.post |> 
    filter(YearComp=='2025 - 2023') |> 
    mutate(Relative_decline=(frac-1)*100/2)
  
  Reef <- c(rep('marx',time=1000))
  Sector<- c(rep('Cooktown/Lizard',time=1000))
  
  marx.comp.post <- cbind(marx.comp.post,Reef,Sector)
  
  save(marx,file='processed/marx.RData')
  save(marx.comp.post,file='processed/marx.comp.post.RData')
  
  
  
  marx.plot.temporal<-ggplot(marx,aes(x=REPORT_YEAR,y=mean*100))+
    geom_point(stat='identity',fill='#beaed4',pch=21)+
    geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#beaed4')+
    geom_line(group=1,colour='#beaed4')+
    scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
    scale_y_continuous("",limits=c(0,61),breaks=c(0,20,40,60))+
    #facet_wrap(~Sector,ncol=1,labeller = sec_labeller)+
    annotate(geom='text',label='Marx Reef',x=1993,y=59,size=3,fontface=2)+
    theme_classic()+
    theme(#axis.line.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size=8),
      axis.title = element_text(size=8),
      axis.text.x = element_blank(),
      strip.text = element_text(size=8),
      plot.margin = unit(c(0,0,0,0),'mm'))
  marx.plot.temporal
  
  
  marx_post_plot<-ggplot(marx.comp.post,aes(y=Reef,x=Relative_decline))+
    geom_vline(xintercept = 0, linetype = "dashed") +
    stat_slabinterval(aes(color = Reef),
                      size=2.5,
                      position = position_dodge(width = 0.1, preserve = "single"),
                      show.legend = FALSE) +
    stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
              fill_type = "segments",
              height = 1,
              expand = FALSE, trim = FALSE, density = "unbounded",
              width = 0.95,
              alpha = 0.5, 
              show.legend = FALSE)+
    scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
    scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
    #scale_x_continuous('Change (%)')+
    #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
    #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
    theme_classic()+
    theme(panel.grid.major.y=element_line(),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          strip.text = element_text(size=7),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          plot.margin = unit(c(0,0,0,0),'mm'))
  
  
  marx_post_plot
  
  pl<-'aaaabb'
  
  marx.comb.plot<-marx.plot.temporal+marx_post_plot+
    plot_layout(design=pl)
  
  marx.comb.plot
  
  
  save(marx.comb.plot,file='processed/marx.comb.plot.RData')
  ggsave(marx.comb.plot,file='outputs/marx.comb.plot.png',height=6,width=9)
  
  ###
  
  reef14075 <- read.csv(file='raw data/manta_reef_Reef 14-075_HC_ _ _ _annual_summary.csv',strip.white=TRUE)
  
  reef14075.comp.post <- read.csv(file='raw data/manta_reef_Reef 14-075_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)
  
  reef14075.comp.post <- reef14075.comp.post |> 
    filter(YearComp=='2025 - 2021') |> 
    mutate(Relative_decline=(frac-1)*100/4)
  
  Reef <- c(rep('reef14075',time=1000))
  Sector<- c(rep('Cooktown/Lizard',time=1000))
  
  reef14075.comp.post <- cbind(reef14075.comp.post,Reef,Sector)
  
  save(reef14075,file='processed/reef14075.RData')
  save(reef14075.comp.post,file='processed/reef14075.comp.post.RData')
  
  
  
  reef14075.plot.temporal<-ggplot(reef14075,aes(x=REPORT_YEAR,y=mean*100))+
    geom_point(stat='identity',fill='#fdc086',pch=21)+
    geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#fdc086')+
    geom_line(group=1,colour='#fdc086')+
    scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
    scale_y_continuous('Cover (%)',limits=c(0,61),breaks=c(0,20,40,60))+
    #facet_wrap(~Sector,ncol=1,labeller = sec_labeller)+
    annotate(geom='text',label='Reef 14-075',x=1996,y=59,size=3,fontface=2)+
    theme_classic()+
    theme(#axis.line.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size=8),
      axis.title = element_text(size=8),
      axis.text.x = element_blank(),
      strip.text = element_text(size=8),
      plot.margin = unit(c(0,0,0,0),'mm'))
  reef14075.plot.temporal
  
  
  reef14075_post_plot<-ggplot(reef14075.comp.post,aes(y=Reef,x=Relative_decline))+
    geom_vline(xintercept = 0, linetype = "dashed") +
    stat_slabinterval(aes(color = Reef),
                      size=2.5,
                      position = position_dodge(width = 0.1, preserve = "single"),
                      show.legend = FALSE) +
    stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
              fill_type = "segments",
              height = 1,
              expand = FALSE, trim = FALSE, density = "unbounded",
              width = 0.95,
              alpha = 0.5, 
              show.legend = FALSE)+
    scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
    scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
    scale_fill_manual(values='seagreen')+
    scale_colour_manual(values='seagreen')+
    #scale_x_continuous('Change (%)')+
    #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
    #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
    theme_classic()+
    theme(panel.grid.major.y=element_line(),
          axis.text = element_blank(),
          axis.title = element_text(size=7),
          strip.text = element_text(size=8),
          axis.title.x=element_blank(),
          # axis.text.y=element_blank(),
          plot.margin = unit(c(0,0,0,0),'mm'))
  
  
  reef14075_post_plot
  
  pl<-'aaaabb'
  
  reef14075.comb.plot<-reef14075.plot.temporal+reef14075_post_plot+
    plot_layout(design=pl)
  
  reef14075.comb.plot
  
  
  save(reef14075.comb.plot,file='processed/reef14075.comb.plot.RData')
  ggsave(reef14075.comb.plot,file='outputs/reef14075.comb.plot.png',height=6,width=9)
  
  ###
  
  sandbank <- read.csv(file='raw data/manta_reef_Sand bank No.1_HC_ _ _ _annual_summary.csv',strip.white=TRUE)
  
  sandbank.comp.post <- read.csv(file='raw data/manta_reef_Sand bank No.1_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)
  
  sandbank.comp.post <- sandbank.comp.post |> 
    filter(YearComp=='2025 - 2024') |> 
    mutate(Relative_decline=(frac-1)*100)
  
  Reef <- c(rep('sandbank',time=1000))
  Sector<- c(rep('Cooktown/Lizard',time=1000))
  
  sandbank.comp.post <- cbind(sandbank.comp.post,Reef,Sector)
  
  save(sandbank,file='processed/sandbank.RData')
  save(sandbank.comp.post,file='processed/sandbank.comp.post.RData')
  
  
  
  sandbank.plot.temporal<-ggplot(sandbank,aes(x=REPORT_YEAR,y=mean*100))+
    geom_point(stat='identity',fill='#fdc086',pch=21)+
    geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#fdc086')+
    geom_line(group=1,colour='#fdc086')+
    scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
    scale_y_continuous("",limits=c(0,61),breaks=c(0,20,40,60))+
    #facet_wrap(~Sector,ncol=1,labeller = sec_labeller)+
    annotate(geom='text',label='Sand Bank No.1',x=1998,y=59,size=3,fontface=2)+
    theme_classic()+
    theme(#axis.line.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size=8),
      axis.title = element_text(size=8),
      axis.text.x = element_blank(),
      strip.text = element_text(size=8),
      plot.margin = unit(c(0,0,0,0),'mm'))
  sandbank.plot.temporal
  
  
  sandbank_post_plot<-ggplot(sandbank.comp.post,aes(y=Reef,x=Relative_decline))+
    geom_vline(xintercept = 0, linetype = "dashed") +
    stat_slabinterval(aes(color = Reef),
                      size=2.5,
                      position = position_dodge(width = 0.1, preserve = "single"),
                      show.legend = FALSE) +
    stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
              fill_type = "segments",
              height = 1,
              expand = FALSE, trim = FALSE, density = "unbounded",
              width = 0.95,
              alpha = 0.5, 
              show.legend = FALSE)+
    scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
    scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
    # scale_fill_manual(values='seagreen')+
    # scale_colour_manual(values='seagreen')+
    #scale_x_continuous('Change (%)')+
    #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
    #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
    theme_classic()+
    theme(panel.grid.major.y=element_line(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          strip.text = element_text(size=8),
          # axis.title.y=element_blank(),
          # axis.text.y=element_blank(),
          plot.margin = unit(c(0,0,0,0),'mm'))
  
  
  sandbank_post_plot
  
  pl<-'aaaabb'
  
  sandbank.comb.plot<-sandbank.plot.temporal+sandbank_post_plot+
    plot_layout(design=pl)
  
  sandbank.comb.plot
  
  
  save(sandbank.comb.plot,file='processed/sandbank.comb.plot.RData')
  ggsave(sandbank.comb.plot,file='outputs/sandbank.comb.plot.png',height=6,width=9)
  
  
  ###
  
  swinger <- read.csv(file='raw data/manta_reef_Swinger Reef_HC_ _ _ _annual_summary.csv',strip.white=TRUE)
  
  swinger.comp.post <- read.csv(file='raw data/manta_reef_Swinger Reef_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)
  
  swinger.comp.post <- swinger.comp.post |> 
    filter(YearComp=='2025 - 2024') |> 
    mutate(Relative_decline=(frac-1)*100)
  
  Reef <- c(rep('swinger',time=1000))
  Sector<- c(rep('Cooktown/Lizard',time=1000))
  
  swinger.comp.post <- cbind(swinger.comp.post,Reef,Sector)
  
  save(swinger,file='processed/swinger.RData')
  save(swinger.comp.post,file='processed/swinger.comp.post.RData')
  
  
  
  swinger.plot.temporal<-ggplot(swinger,aes(x=REPORT_YEAR,y=mean*100))+
    geom_point(stat='identity',fill='#beaed4',pch=21)+
    geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#beaed4')+
    geom_line(group=1,colour='#beaed4')+
    scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
    scale_y_continuous("",limits=c(0,61),breaks=c(0,20,40,60))+
    #facet_wrap(~Sector,ncol=1,labeller = sec_labeller)+
    annotate(geom='text',label='Swinger Reef',x=1996.5,y=59,size=3,fontface=2)+
    theme_classic()+
    theme(#axis.line.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size=8),
      axis.title = element_text(size=8),
      strip.text = element_text(size=8),
      plot.margin = unit(c(0,0,0,0),'mm'))
  swinger.plot.temporal
  
  
  swinger_post_plot<-ggplot(swinger.comp.post,aes(y=Reef,x=Relative_decline))+
    geom_vline(xintercept = 0, linetype = "dashed") +
    stat_slabinterval(aes(color = Reef),
                      size=2.5,
                      position = position_dodge(width = 0.1, preserve = "single"),
                      show.legend = FALSE) +
    stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
              fill_type = "segments",
              height = 1,
              expand = FALSE, trim = FALSE, density = "unbounded",
              width = 0.95,
              alpha = 0.5, 
              show.legend = FALSE)+
    scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
    scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
    # scale_fill_manual(values='seagreen')+
    # scale_colour_manual(values='seagreen')+
    #scale_x_continuous('Change (%)')+
    #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
    #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
    theme_classic()+
    theme(panel.grid.major.y=element_line(),
          axis.text = element_text(size=8),
          axis.title = element_text(size=8),
          strip.text = element_text(size=7),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          plot.margin = unit(c(0,0,0,0),'mm'))
  
  
  swinger_post_plot
  
  pl<-'aaaabb'
  
  swinger.comb.plot<-swinger.plot.temporal+swinger_post_plot+
    plot_layout(design=pl)
  
  swinger.comb.plot
  
  
  save(swinger.comb.plot,file='processed/swinger.comb.plot.RData')
  ggsave(swinger.comb.plot,file='outputs/swinger.comb.plot.png',height=6,width=9) 

  ###
  
  fly <- read.csv(file='raw data/manta_reef_Fly Reef_HC_ _ _ _annual_summary.csv',strip.white=TRUE)
  
  fly.comp.post <- read.csv(file='raw data/manta_reef_Fly Reef_HC_ _ _ __beta_all_annual_comp_posteriors.csv',strip.white=TRUE)
  
  fly.comp.post <- fly.comp.post |> 
    filter(YearComp=='2025 - 2021') |> 
    mutate(Relative_decline=(frac-1)*100/4)
  
  Reef <- c(rep('Fly',time=1000))
  Sector<- c(rep('Cooktown/Lizard',time=1000))
  
  fly.comp.post <- cbind(fly.comp.post,Reef,Sector)
  
  save(fly,file='processed/fly.RData')
  save(fly.comp.post,file='processed/fly.comp.post.RData')
  
  
  
  fly.plot.temporal<-ggplot(fly,aes(x=REPORT_YEAR,y=mean*100))+
    geom_point(stat='identity',fill='#beaed4',pch=21)+
    geom_errorbar(stat='identity',aes(ymin=lower*100,ymax=upper*100),width=0,colour='#beaed4')+
    geom_line(group=1,colour='#beaed4')+
    scale_x_continuous(breaks=c(1985,1995,2005,2015,2025),limits=c(1985,2026))+
    scale_y_continuous("",limits=c(0,61),breaks=c(0,20,40,60))+
    #facet_wrap(~Sector,ncol=1,labeller = sec_labeller)+
    annotate(geom='text',label='Fly Reef',x=1991,y=59,size=3,fontface=2)+
    theme_classic()+
    theme(#axis.line.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(size=8),
      axis.title = element_text(size=8),
      axis.text.x = element_blank(),
      strip.text = element_text(size=8),
      plot.margin = unit(c(0,0,0,0),'mm'))
  fly.plot.temporal
  
  
  fly_post_plot<-ggplot(fly.comp.post,aes(y=Reef,x=Relative_decline))+
    geom_vline(xintercept = 0, linetype = "dashed") +
    stat_slabinterval(aes(color = Reef),
                      size=2.5,
                      position = position_dodge(width = 0.1, preserve = "single"),
                      show.legend = FALSE) +
    stat_slab(aes(fill = Reef,colour=Reef),# slab_alpha = after_stat(x) < 100),
              fill_type = "segments",
              height = 1,
              expand = FALSE, trim = FALSE, density = "unbounded",
              width = 0.95,
              alpha = 0.5, 
              show.legend = FALSE)+
    scale_y_discrete('Reef')+#,breaks=c('CL','CA','IN'),labels=c("Cooktown/Lizard","Cairns","Innisfail"))+
    scale_x_continuous('Change (%)',limits=c(-85,60),breaks=c(-80,-40,0,40))+
    scale_fill_manual(values='seagreen')+
    scale_colour_manual(values='seagreen')+
    #scale_x_continuous('Change (%)')+
    #annotate(geom = 'text',x=0,y=0.8,label='P = 0.497')+
    #facet_wrap(~Sector,ncol=1)+#,labeller = sec_labeller)+
    theme_classic()+
    theme(panel.grid.major.y=element_line(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          strip.text = element_text(size=8),
          # axis.title.y=element_blank(),
          # axis.text.y=element_blank(),
          plot.margin = unit(c(0,0,0,0),'mm'))
  
  
  fly_post_plot
  
  pl<-'aaaabb'
  
  fly.comb.plot<-fly.plot.temporal+fly_post_plot+
    plot_layout(design=pl)
  
  fly.comb.plot
  
  
  save(fly.comb.plot,file='processed/fly.comb.plot.RData')
  ggsave(fly.comb.plot,file='outputs/fly.comb.plot.png',height=6,width=9) 
  
  
  ##### 
  ## put on map
  
  #############################################################################################################
  # sector map --------------------------------------------------------------
  
  
  
  gbr.sf <- sf::read_sf('raw data/Great_Barrier_Reef_Marine_Park_Boundary.shp')
  
  qld.sf <- sf::read_sf('raw data/Great_Barrier_Reef_Features.shp') %>%
    filter(FEAT_NAME %in% c("Mainland","Island"))
  
  reefs<-sf::read_sf('raw data/Great_Barrier_Reef_Features.shp') |> 
    filter(FEAT_NAME == "Reef")
  
  
  towns.sf = rbind(data.frame(Town=c('Cairns'),Latitude=-16.93598,Longitude=145.7402),
                   data.frame(Town=c('Cooktown'),Latitude=-15.4736,Longitude=145.249221),
                   data.frame(Town=c('Innisfail'),Latitude=-17.520843,Longitude=146.030166),
                   data.frame(Town=c('Cardwell'),Latitude=-18.260526,Longitude=146.023342),
                   data.frame(Town=c('Lizard Island'),Latitude=-14.670577,Longitude=145.462032)
                   
  )
  
  
  
  ltmp.reefs <- read.csv(file='raw data/reef name lookup lat long.csv',strip.white=TRUE) |> 
    filter(REEF_NAME %in% c('CARTER REEF','YONGE REEF','NO NAME REEF','NORTH DIRECTION REEF','SOUTH DIRECTION REEF (NORTH)',
                            'LIZARD ISLAND','MARTIN REEF(14123)','LINNET REEF','MACGILLIVRAY REEF','EYRIE REEF',
                            'EYE REEF','BOULDER REEF','EGRET REEF','FORRESTER REEF','HELSDON REEF','MARX REEF','14075S',
                            'SAND BANK NO 1 REEF','SWINGER REEF','FLY REEF')) |> 
    mutate(REEF_NAME=factor(REEF_NAME))
  
  
  reefs <- sf::st_as_sf(reefs, 
                        coords = c(x = "Longitude", y = "Latitude"))
  reefs<- st_set_crs(reefs, "+proj=longlat +ellps=WGS84 +datum=WGS84")
  reefs<- st_transform(reefs, "+proj=longlat +ellps=WGS84 +datum=WGS84")
  
  sectors<-sf::read_sf('raw data/ltms.shp')
  
  sf_use_s2(FALSE)
  reefsec<-reefs %>% st_intersection(sectors) %>%
    mutate(SECTOR_NAM = factor(SECTOR_NAM,levels=c("Cape Grenville","Princess Charlotte Bay","Cooktown / Lizard Island",
                                                   "Cairns","Innisfail","Townsville","Whitsunday","Pompey","Swain","Capricorn Bunker")))
  
  
  xlim <- c(144.3,149)
  ylim <- c(-13.8,-15.87)
  
  
  cl.sector.map<-ggplot()+
    geom_sf(data=sectors,fill=NA)+
    geom_sf(data=reefsec,aes(fill=SECTOR_NAM),
            show.legend=FALSE)+
    geom_sf(data=sectors,fill=NA)+
    coord_sf(xlim=xlim,ylim=ylim)+
    scale_fill_manual(values=c('grey','grey','#FDBF6F','grey','grey','grey','grey','grey','grey','grey','grey'))+
    #annotate(geom='rect',xmin=145.15,xmax=145.8,ymin=-14.35,ymax=-15.1,fill=NA,colour='black')+
    ggspatial::annotation_scale()+
    ggspatial::annotation_north_arrow(height = unit(0.5, "cm"),
                                      width = unit(0.5, "cm"),
                                      pad_y=unit(1,"cm"),
                                      pad_x=unit(2.25,'cm'))+
    ggtitle('a.')+
    scale_x_continuous(breaks=c(145,146))+
    theme_classic()+
    theme(panel.grid.major = element_line(),
          axis.title=element_blank())
  
  cl.sector.map
  
  
  #geom_sf(data=reefsec,fill=c('grey','grey','#FDBF6F','grey','grey','grey','grey','grey','grey','grey','grey'))+
  
  aus<-map("worldHires", "Australia", fill=TRUE, xlim=c(110,160),
           ylim=c(-45,-5), mar=c(0,0,0,0))
  
  
  
  oz.plot2<-  ggplot(fortify(aus), aes(y=lat, x=long, group=group)) + 
    geom_polygon(colour='grey20',fill='white')+
    geom_sf(data=sectors,inherit.aes=FALSE,fill=c('grey','grey','#FDBF6F','grey','grey','grey','grey','grey','grey','grey','grey'),colour='grey20',linewidth=0.5)+  
    annotate(geom='rect',xmin=143,xmax=147,ymin=-13.5,ymax=-16.5,fill=NA,colour='black')+
    theme_classic()+
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm'))
  oz.plot2
  
  load(file='processed/combined_plot_cl.RData')
  
  cl.map.results <- cl.sector.map+
    inset_element(oz.plot2,left=0,bottom=0.16,right=0.25,top=0.44)+
    inset_element(cl.comb.plot,left=0.4,bottom=0.25,right=0.95,top=0.75)
  cl.map.results
  
  
  ggsave(cl.map.results,file='outputs/cl.sector.map.jan2025.png',height=6.21,width=14.1)
  
  xlim <- c(144.2,146.5)
  ylim <- c(-14.02,-15.8)
  
  cl.map<-ggplot()+
    geom_sf(data=reefs)+
    geom_sf(data=sectors,fill=NA)+
    coord_sf(xlim=xlim,ylim=ylim)+
    geom_point(data=ltmp.reefs,aes(x=REEF_LONG,y=REEF_LAT,fill=SHELF,shape=SHELF,colour=REEF_NAME),size=3,show.legend=FALSE)+
    scale_fill_manual(values = c('#7fc97f','#beaed4','#fdc086'))+
    scale_colour_manual(values=c('black','black','red','black','black','black','black','black','black','red','red','red','red',
                                 'black','red','red','black','black','black','red'))+
    scale_shape_manual(values = c(21,22,23))+
    annotate(geom='rect',xmin=144,xmax=144.87,ymin=-13.8,ymax=-14.5,fill='white',colour='white')+
    #annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotate(geom='segment',x=144.72,xend=145.33,y=-14.87,yend=-14.785,colour='#7fc97f')+       #linnet
    annotate(geom='segment',x=144.72,xend=145.336,y=-14.65,yend=-14.76,colour='#7fc97f')+  #martin
    annotate(geom='segment',x=144.72,xend=145.346,y=-14.4,yend=-14.71,colour='#beaed4')+    #eyrie
    annotate(geom='segment',x=145.87,xend=145.478,y=-14.9,yend=-14.68,colour='#beaed4')+ #lizard
    annotate(geom='segment',x=145.87,xend=145.52,y=-14.675,yend=-14.65,colour='#beaed4')+ #Macs
    annotate(geom='segment',x=145.87,xend=145.605,y=-14.0,yend=-14.525,colour='#fdc086')+ #carter
    annotate(geom='segment',x=145.87,xend=145.63,y=-14.25,yend=-14.59,colour='#fdc086')+   #Yonge
    annotate(geom='segment',x=145.87,xend=145.655,y=-14.47,yend=-14.64,colour='#fdc086')+   #no name  
    annotate(geom='segment',x=145.87,xend=145.525,y=-15.15,yend=-14.742,colour='#beaed4')+  #N direction
    annotate(geom='segment',x=145.87,xend=145.53,y=-15.33,yend=-14.85,colour='#beaed4')+  #s direction
    annotate(geom='segment',x=144.72,xend=145.484,y=-15.15,yend=-14.884,colour='#beaed4')+  #eye
    annotate(geom='segment',x=144.72,xend=145.497,y=-15.35,yend=-14.95,colour='#beaed4')+  #helsdon
    annotate(geom='segment',x=144.72,xend=145.497,y=-15.55,yend=-15.18,colour='#beaed4')+  #forrester
    annotate(geom='segment',x=144.72,xend=145.4,y=-15.7,yend=-15.42,colour='#7fc97f')+  #boulder
    annotate(geom='segment',x=145.25,xend=145.4,y=-15.6,yend=-15.49,colour='#7fc97f')+  #egret
    annotate(geom='segment',x=145.87,xend=145.63,y=-15.52,yend=-15.23,colour='#beaed4')+  #marx
    annotate(geom='segment',x=145.87,xend=145.54,y=-15.7,yend=-15.26,colour='#beaed4')+  #swinger
    annotate(geom='segment',x=144.82,xend=144.91,y=-14.0,yend=-14.18,colour='#fdc086')+  #sandbank
    annotate(geom='segment',x=145.25,xend=145.18,y=-14.01,yend=-14.25,colour='#fdc086')+  #14075
    annotate(geom='segment',x=144.725,xend=145.15,y=-14.2,yend=-14.49,colour='#beaed4')+  #fly
    ggtitle('b.')+
    theme_classic()+
    theme(panel.grid.major = element_line(),
          axis.title=element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm'))
  
  cl.map
  
  
  cl.result.map <- cl.map+
    inset_element(sandbank.comb.plot,left=0.01,bottom=0.90,right=0.31,top=1)+
    inset_element(reef14075.comb.plot,left=0.325,bottom=0.90,right=0.625,top=1)+
    inset_element(carter.comb.plot,left=0.7,bottom=0.90,right=1,top=1)+
    inset_element(yonge.comb.plot,left=0.7,bottom=0.79,right=1,top=0.89)+
    inset_element(fly.comb.plot,left=0.01,bottom=0.79,right=0.31,top=0.89)+
    inset_element(na_name.comb.plot,left=0.7,bottom=0.68,right=0.99,top=0.78)+
    inset_element(eyrie.comb.plot,left=0.01,bottom=0.68,right=0.31,top=0.78)+
    inset_element(martin.comb.plot,left=0.01,bottom=0.57,right=0.31,top=0.67)+
    inset_element(linnet.comb.plot,left=0.01,bottom=0.46,right=0.31,top=0.56)+
    inset_element(macgillvray.comb.plot,left=0.7,bottom=0.57,right=1,top=0.67)+
    inset_element(lizard.comb.plot,left=0.7,bottom=0.46,right=1,top=0.56)+
    inset_element(ndir.comb.plot,left=0.7,bottom=0.35,right=1,top=0.45)+
    inset_element(sdir.comb.plot,left=0.7,bottom=0.24,right=1,top=0.34)+
    inset_element(eye.comb.plot,left=0.01,bottom=0.35,right=0.31,top=0.45)+
    inset_element(helsdon.comb.plot,left=0.01,bottom=0.24,right=0.31,top=0.34)+
    inset_element(forrester.comb.plot,left=0.01,bottom=0.13,right=0.31,top=0.23)+
    inset_element(marx.comb.plot,left=0.7,bottom=0.13,right=1,top=0.23)+
    inset_element(swinger.comb.plot,left=0.7,bottom=0.005,right=1,top=0.12)+
    inset_element(boulder.comb.plot,left=0.01,bottom=0.005,right=0.31,top=0.12)+
    inset_element(egret.comb.plot,left=0.325,bottom=0.005,right=0.625,top=0.12)
  
  cl.result.map
  
  
  #ggsave(cl.result.map,file='outputs/cl.result.map_Jan2025.png',height=9,width=12)
  ggsave(cl.result.map,file='outputs/cl.result.map_Apr2025.png',height=9,width=12)
  
  
  pl2 <-'aaaaaa
         aaaaaa
         aaaaaa
         bbbbbb
         bbbbbb
         bbbbbb
         bbbbbb
         bbbbbb'
  
  cl.results.map.manuscript <- wrap_elements(cl.map.results)+wrap_elements(cl.result.map)+
    plot_layout(design=pl2)
  
  cl.results.map.manuscript
  
  #ggsave(cl.results.map.manuscript,file='outputs/cl.results.map.manuscript_Jan2025.png',height=12,width=12)
  ggsave(cl.results.map.manuscript,file='outputs/cl.results.map.manuscript_Apr2025.png',height=12,width=12)
  

  
 
 
##############################################################################################################
##############################################################################################################
####   DHW bleaching mortality relationships ----------------------------




summary.dat <- read.csv(file='raw data/ltmp_raw_before_after.csv',strip.white=TRUE)

summary.dat.cl<-summary.dat[1:20,]

head(summary.dat.cl)

save(summary.dat.cl,file='summary.dat.cl.RData')


ggplot(summary.dat.cl,aes(x=In.water.DHW,y=Aerial.bleaching))+
  geom_point(stat='identity')+
  geom_smooth(method='lm')+
  scale_y_continuous(limits=c(0,6))

######  dodgy frequentist 
# 
# 
# dhw_bleach.rel <- lm(Aerial.bleaching~Max.DHW,data=summary.dat.cl)
# summary(dhw_bleach.rel)
# 
# 
# 
# plot_a <- ggplot(summary.dat.cl,aes(x=Max.DHW,y=Aerial.bleaching))+
#   geom_point(stat='identity',size=3,aes(shape=Shelf))+
#   scale_y_continuous('Aerial bleaching score')+
#   scale_x_continuous('Maximum DHW',limits = c(0,8))+
#     geom_smooth(method='lm',colour='grey80')+
#   coord_cartesian(ylim = c(0,5))+
#   #geom_text(x=5,y=2.5,label=paste("(r^2)","=",0.68))+
# #  annotate(geom='text',x=5,y=2.5,label=paste("(r^2)","=",0.68),parse=T)+
#   #F=15.82,p=0.007'))+
#   annotate(geom='text',x=5,y=4.8,
#            label=("r^2==0.68"),  
#            parse=TRUE,size=3)+
#   annotate(geom='text',x=5,y=4.3,
#           label=("F==15.82"),  
#           parse=TRUE,size=3)+
#   annotate(geom='text',x=5,y=3.8,
#           label=("p<0.001"),  
#           parse=TRUE,size=3)+
#   theme_classic()+  
#   theme(panel.grid.major = element_line(),
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=12,face='bold'))
# 
# plot_a
# 
# 
# ######
#   ##
# 
# dhw_hcc.change.lm <- lm(Max.DHW~Rel_change_hcc_modelled,data=summary.dat.cl)
# summary(dhw_hcc.change.lm)
# 
# 
# plot_b <- ggplot(summary.dat.cl,aes(y=Rel_change_hcc_modelled,x=Max.DHW))+
#   geom_point(stat='identity',size=3,aes(shape=Shelf))+
#   scale_y_continuous('Coral cover change (%)',limits=c(-80,20))+
#   scale_x_continuous('Maximum DHW',limits = c(0,8))+
#   geom_smooth(method='lm',colour='grey80')+
#   geom_hline(aes(yintercept=0),linetype='dashed',colour='grey')+
#   annotate(geom='text',x=5,y=20,
#            label=("r^2==0.41"),  
#            parse=TRUE,size=3)+
#   annotate(geom='text',x=5,y=10,
#            label=("F==7.24"),  
#            parse=TRUE,size=3)+
#   annotate(geom='text',x=5,y=0,
#            label=("p<0.001"),  
#            parse=TRUE,size=3)+
#   theme_classic()+
#   theme(panel.grid.major = element_line(),
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=12,face='bold'))
# 
# plot_b
# 
# 
# 
# aerial_hcc.change.lm <- lm(Aerial.bleaching~Rel_change_hcc_modelled,data=summary.dat.cl)
# summary(aerial_hcc.change.lm)
# 
# plot_c <- ggplot(summary.dat.cl,aes(y=Rel_change_hcc_modelled,x=Aerial.bleaching))+
#   geom_point(stat='identity',size=3,aes(shape=Shelf))+
#   scale_y_continuous('Coral cover change (%)',limits=c(-80,20))+
#   scale_x_continuous('Aerial bleaching score',limits = c(0,6),breaks=c(0,1,2,3,4,5))+
#   geom_smooth(method='lm',colour='grey80')+
#   geom_hline(aes(yintercept=0),linetype='dashed',colour='grey')+
#   annotate(geom='text',x=5,y=20,
#            label=("r^2==0.54"),  
#            parse=TRUE,size=3)+
#   annotate(geom='text',x=5,y=10,
#            label="F==9.07",  
#            parse=TRUE,size=3)+
#   annotate(geom='text',x=5,y=0,
#            label=("p==0.0045"),  
#            parse=TRUE,size=3)+
#   theme_classic()+
#   theme(panel.grid.major = element_line(),
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=12,face='bold'))
# 
# pl <- 'aaa
#        bbb
#        ccc'
# 
# heat.relationship.plots <- plot_a+plot_b+plot_c+
#   plot_layout(design=pl,guides = 'collect')+
#   plot_annotation(tag_levels='a')
# 
# heat.relationship.plots
# 
# ggsave(heat.relationship.plots,file='heat.relationship.plots.png',height=9,width=9)

##################################################################################
### Bayes ordinal model

library(tidyverse)
library(brms)
library(rstan)
library(tidybayes)
library(emmeans)

load("summary.dat.cl.RData")

head(summary.dat.cl)

## Define an ordinal factor version of the response
summary.dat.cl <- summary.dat.cl |>
  mutate(oAerial.bleaching = factor(Aerial.bleaching, ordered = TRUE)) 


#### NOAA DHW

form <- bf(oAerial.bleaching ~ Max.DHW,
           family = cumulative("logit", threshold = "flexible")) 

mod.1 <- brm(form,
           data = summary.dat.cl,
           chains = 3,
           cores = 3,
           thin = 5,
           iter = 5000,
           warmup = 1000,
           backend = "cmdstanr"    # note Mike, you might have to change this to "rstan" if you don't have cmdstanr installed
)


save(mod.1,file='mod.1.RData')
mod.1 |>
  conditional_effects(categorical = TRUE) |>
  plot(points = TRUE)

mod.1$fit |>
  rstan::stan_trace()
mod.1$fit |>
  rstan::stan_ac()
mod.1$fit |>
  rstan::stan_rhat()
mod.1$fit |>
  rstan::stan_ess()


mod.1 |> pp_check(type = "dens_overlay", nsamples = 100)
mod.1 |> pp_check(type = "intervals", nsamples = 100)

sum_tbl <- mod.1 |>
  as_draws_df() |>
  exp() |> 
  dplyr::select(matches("^b_.*")) |>
  summarise_draws(
    median,
    HDInterval::hdi,
    Pl = ~ mean(.x < 1),
    Pg = ~ mean(.x > 1),
    rhat,
    ess_bulk
  )
sum_tbl
sum_tbl |> filter(variable == "b_Max.DHW") |> pull(median)
## for every one unit increase in Max.DHW, the odds of exceeding the threshold (being in a higher bleaching lass) decreases by a factor of 0.2 (e.g. an 80% reduction in odds).


r2 <- bayes_R2(mod.1)[1]
bayes_p <- sum_tbl |> filter(variable == "b_Max.DHW") |> pull(Pl)

newdata <- with(summary.dat.cl,
                data.frame(Max.DHW = seq(min(Max.DHW, na.rm = TRUE),
                                         max(Max.DHW, na.rm = TRUE),
                                         length.out = 100)))
newdata <- add_epred_draws(mod.1, newdata = newdata, re_formula = NA) |>
  mutate(fit = as.numeric(as.character(.category)) * .epred) |>
  group_by(Max.DHW, .draw) |>
  summarise(fit = sum(fit)) |>
  summarise_draws(
    median,
    HDInterval::hdi
  ) |>
  arrange(Max.DHW) 

plot_a <- newdata |>
  ggplot(aes(x = Max.DHW, y = median)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "orange") +
  geom_line() +
  geom_point(data = summary.dat.cl, aes(y = Aerial.bleaching,shape=Shelf),size=2.7) +
  scale_y_continuous('Aerial bleaching score')+
  scale_x_continuous('Maximum DHW (NOAA)')+
  coord_cartesian(ylim = c(0, 5), xlim = c(0,8)) +
  annotate(geom='text',x=1.5,y=4.8,
           label=paste0("r^2==", round(r2, 3)),  
           parse=TRUE,size=3)+
  annotate(geom='text',x=1.2,y=4,
           label=paste0("P[exceed]==",round(bayes_p, 3)),  
           parse=TRUE,size=3)+
  theme_classic()+  
  theme(panel.grid.major = element_line(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face='bold'))

plot_a

#### eReefs DHW

form.1.1 <- bf(oAerial.bleaching ~ ereefs.DHW,
           family = cumulative("logit", threshold = "flexible")) 

mod.1.1 <- brm(form.1.1,
             data = summary.dat.cl,
             chains = 3,
             cores = 3,
             thin = 5,
             iter = 5000,
             warmup = 1000,
             backend = "cmdstanr"    # note Mike, you might have to change this to "rstan" if you don't have cmdstanr installed
)



mod.1.1|>
  conditional_effects(categorical = TRUE) |>
  plot(points = TRUE)

mod.1.1$fit |>
  rstan::stan_trace()
mod.1.1$fit |>
  rstan::stan_ac()
mod.1.1$fit |>
  rstan::stan_rhat()
mod.1.1$fit |>
  rstan::stan_ess()


mod.1.1 |> pp_check(type = "dens_overlay", nsamples = 100)
mod.1.1 |> pp_check(type = "intervals", nsamples = 100)

sum_tbl <- mod.1.1 |>
  as_draws_df() |>
  exp() |> 
  dplyr::select(matches("^b_.*")) |>
  summarise_draws(
    median,
    HDInterval::hdi,
    Pl = ~ mean(.x < 1),
    Pg = ~ mean(.x > 1),
    rhat,
    ess_bulk
  )
sum_tbl
sum_tbl |> filter(variable == "b_ereefs.DHW") |> pull(median)
## for every one unit increase in Max.DHW, the odds of exceeding the threshold (being in a higher bleaching lass) decreases by a factor of 0.2 (e.g. an 80% reduction in odds).


r2 <- bayes_R2(mod.1)[1]
bayes_p <- sum_tbl |> filter(variable == "b_ereefs.DHW") |> pull(Pl)

newdata <- with(summary.dat.cl,
                data.frame(ereefs.DHW = seq(min(ereefs.DHW, na.rm = TRUE),
                                         max(ereefs.DHW, na.rm = TRUE),
                                         length.out = 100)))
newdata <- add_epred_draws(mod.1.1, newdata = newdata, re_formula = NA) |>
  mutate(fit = as.numeric(as.character(.category)) * .epred) |>
  group_by(ereefs.DHW, .draw) |>
  summarise(fit = sum(fit)) |>
  summarise_draws(
    median,
    HDInterval::hdi
  ) |>
  arrange(ereefs.DHW) 

plot_a.1 <- newdata |>
  ggplot(aes(x = ereefs.DHW, y = median)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "orange") +
  geom_line() +
  geom_point(data = summary.dat.cl, aes(y = Aerial.bleaching,shape=Shelf),size=2.7) +
  scale_y_continuous('Aerial bleaching score')+
  scale_x_continuous('Maximum DHW (eReefs)',breaks=c(0,4,8,12))+
  coord_cartesian(ylim = c(0, 5), xlim = c(0,12)) +
  annotate(geom='text',x=1.8,y=4.8,
           label=paste0("r^2==", round(r2, 3)),  
           parse=TRUE,size=3)+
  annotate(geom='text',x=2,y=4,
           label=paste0("P[exceed]==",round(bayes_p, 3)),  
           parse=TRUE,size=3)+
  theme_classic()+  
  theme(panel.grid.major = element_line(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face='bold'))

plot_a.1


#### in water DHW

# summary.dat.cl <-
#   summary.dat.cl |>
#   filter(!is.na(Aerial.bleaching) & !is.na(In.water.DHW)) |>
#   mutate(oAerial.bleaching2 = factor(Aerial.bleaching, levels = c(1, 2, 3, 4, 5), ordered = TRUE))
# 
# summary.dat.cl
# newdata <- with(summary.dat.cl,
#                 data.frame(In.water.DHW = seq(min(In.water.DHW, na.rm = TRUE),
#                                               max(In.water.DHW, na.rm = TRUE),
#                                               length.out = 100)))
# 
# 
# form.1.2 <- bf(oAerial.bleaching ~ In.water.DHW,
#                family = cumulative("logit", threshold = "flexible"))
# mod.1.2 <- brm(form.1.2,
#                data = summary.dat.cl,
#                chains = 3,
#                cores = 3,
#                thin = 5,
#                iter = 5000,
#                warmup = 1000,
#                backend = "cmdstanr"    # note Mike, you might have to change this to "rstan" if you don't have cmdstanr installed
# )
# 
# sum_tbl <- mod.1.2 |>
#   as_draws_df() |>
#   exp() |> 
#   dplyr::select(matches("^b_.*")) |>
#   summarise_draws(
#     median,
#     HDInterval::hdi,
#     Pl = ~ mean(.x < 1),
#     Pg = ~ mean(.x > 1),
#     rhat,
#     ess_bulk
#   )
# 
# sum_tbl
# sum_tbl |> filter(variable == "b_In.water.DHW") |> pull(median)
# ## for every one unit increase in Max.DHW, the odds of exceeding the threshold (being in a higher bleaching lass) decreases by a factor of 0.2 (e.g. an 80% reduction in odds).
# 
# 
# r2 <- bayes_R2(mod.1.2)[1]
# bayes_p <- sum_tbl |> filter(variable == "b_In.water.DHW") |> pull(Pl)
# 
# 
# newdata <- with(summary.dat.cl,
#                 data.frame(In.water.DHW = seq(min(In.water.DHW, na.rm = TRUE),
#                                               max(In.water.DHW, na.rm = TRUE),
#                                               length.out = 100)))
# pred <- posterior_epred(mod.1.2, newdata = newdata)
# newdata <- mutate(newdata, temp = paste0("V", 1:n()))
# pred1 <- lapply(dimnames(pred)[[3]], function(i) {
#   as_tibble(pred[, , i]) |>
#     mutate(.draw = 1:n()) |>
#     pivot_longer(cols = -.draw, names_to = "temp", values_to = "prob") |>
#     mutate(cat = as.numeric(i)) |>
#     full_join(newdata, by = "temp", relationship = "many-to-many") |>
#     dplyr::select(-temp)
# })
# pred2 <- do.call("rbind", pred1)
# pred3 <- pred2 |>
#   group_by(.draw, In.water.DHW) |>
#   summarise(
#     prob = sum(prob * cat),
#     .groups = "drop"
#   ) |>
#   group_by(In.water.DHW) |>
#   tidybayes::summarise_draws(median, HDInterval::hdi)
# 
# 
# 
# 
# plot_a.2 <- pred3 |> filter(In.water.DHW>5.69) |> 
#   ggplot(aes(x = In.water.DHW, y = median)) +
#   geom_point(data = summary.dat.cl, aes(y = Aerial.bleaching,shape=Shelf),size=2.7) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "orange") +
#   geom_line() +
#   scale_y_continuous('Aerial bleaching score')+
#   scale_x_continuous('Maximum DHW (In water)',breaks=c(0,4,8,12))+
#   coord_cartesian(ylim = c(0, 5), xlim = c(0,12)) +
#   annotate(geom='text',x=1.8,y=4.8,
#            label=paste0("r^2==", round(r2, 3)),
#            parse=TRUE,size=3)+
#   annotate(geom='text',x=2,y=4,
#            label=paste0("P[exceed]==",round(bayes_p, 3)),
#            parse=TRUE,size=3)+
#   theme_classic()+  
#   theme(panel.grid.major = element_line(),
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=12,face='bold'))
# plot_a.2
# 

################################################################################


form.2 <- bf(Rel_change_hcc_modelled~Aerial.bleaching,
           family = 'gaussian') 

mod.2 <- brm(form.2,
             data = summary.dat.cl,
             chains = 3,
             cores = 3,
             thin = 5,
             iter = 5000,
             warmup = 1000,
             backend = "cmdstanr"    # note Mike, you might have to change this to "rstan" if you don't have cmdstanr installed
)



mod.2 |>
  conditional_effects() |>
  plot(points = TRUE)

mod.2$fit |>
  rstan::stan_trace()
mod.2$fit |>
  rstan::stan_ac()
mod.2$fit |>
  rstan::stan_rhat()
mod.2$fit |>
  rstan::stan_ess()


mod.2 |> pp_check(type = "dens_overlay", ndraws = 100)
mod.2 |> pp_check(type = "intervals", ndraws = 100)

sum_tbl <- mod.2 |>
  as_draws_df() |>
  #exp() |> 
  dplyr::select(matches("^b_.*")) |>
  summarise_draws(
    median,
    HDInterval::hdi,
    Pl = ~ mean(.x < 0),
    Pg = ~ mean(.x > 0),
    rhat,
    ess_bulk
  )
sum_tbl
sum_tbl |> filter(variable == "b_Aerial.bleaching") |> pull(median)
## for every one unit increase in Max.DHW, the odds of exceeding the threshold (being in a higher bleaching lass) decreases by a factor of 0.2 (e.g. an 80% reduction in odds).


r2 <- bayes_R2(mod.2)[1]
bayes_p <- sum_tbl |> filter(variable == "b_Aerial.bleaching") |> pull(Pl)   ##### proportion of posteriors (slope estimate) being negative)

newdata <- with(summary.dat.cl,
                data.frame(Aerial.bleaching = seq(min(Aerial.bleaching, na.rm = TRUE),
                                         max(Aerial.bleaching, na.rm = TRUE),
                                         length.out = 100)))

newdata <- emmeans(mod.2,~Aerial.bleaching,at=newdata) |> 
  summary(infer=TRUE) |> 
  as.data.frame() |> 
  dplyr::rename(median=emmean,lower=lower.HPD,upper=upper.HPD)

plot_b <- newdata |>
  ggplot(aes(x = Aerial.bleaching, y = median)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "orange") +
  geom_line() +
  geom_point(data = summary.dat.cl, aes(y = Rel_change_hcc_modelled,shape=Shelf),size=2.7) +
  scale_x_continuous('Aerial bleaching score')+
  scale_y_continuous('Coral cover change (%)')+
  coord_cartesian(xlim = c(0, 5)) +
  annotate(geom='text',x=3,y=35,
           label=paste0("r^2==", round(r2, 3)),  
           parse=TRUE,size=3)+
  annotate(geom='text',x=3.1,y=20,
           label=paste0("P[exceed]==",round(bayes_p, 3)),  
           parse=TRUE,size=3)+
  theme_classic()+  
  theme(panel.grid.major = element_line(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face='bold'))

plot_b

################################################################################
## NOAA DHW

form.3 <- bf(Rel_change_hcc_modelled~Max.DHW,
             family = 'gaussian') 

mod.3 <- brm(form.3,
             data = summary.dat.cl,
             chains = 3,
             cores = 3,
             thin = 5,
             iter = 5000,
             warmup = 1000,
             backend = "cmdstanr"    # note Mike, you might have to change this to "rstan" if you don't have cmdstanr installed
)



mod.3 |>
  conditional_effects() |>
  plot(points = TRUE)

mod.3$fit |>
  rstan::stan_trace()
mod.3$fit |>
  rstan::stan_ac()
mod.3$fit |>
  rstan::stan_rhat()
mod.3$fit |>
  rstan::stan_ess()


mod.3 |> pp_check(type = "dens_overlay", ndraws = 100)
mod.3 |> pp_check(type = "intervals", ndraws = 100)

sum_tbl <- mod.3 |>
  as_draws_df() |>
  #exp() |> 
  dplyr::select(matches("^b_.*")) |>
  summarise_draws(
    median,
    HDInterval::hdi,
    Pl = ~ mean(.x < 0),
    Pg = ~ mean(.x > 0),
    rhat,
    ess_bulk
  )
sum_tbl
sum_tbl |> filter(variable == "b_Max.DHW") |> pull(median)
## for every one unit increase in Max.DHW, the odds of exceeding the threshold (being in a higher bleaching lass) decreases by a factor of 0.2 (e.g. an 80% reduction in odds).


r2 <- bayes_R2(mod.3)[1]
bayes_p <- sum_tbl |> filter(variable == "b_Max.DHW") |> pull(Pg)   ##### proportion of posteriors (slope estimate) being negative)

newdata <- with(summary.dat.cl,
                data.frame(Max.DHW = seq(min(Max.DHW, na.rm = TRUE),
                                                  max(Max.DHW, na.rm = TRUE),
                                                  length.out = 100)))

newdata <- emmeans(mod.3,~Max.DHW,at=newdata) |> 
  summary(infer=TRUE) |> 
  as.data.frame() |> 
  dplyr::rename(median=emmean,lower=lower.HPD,upper=upper.HPD)

plot_c <- newdata |>
  ggplot(aes(x = Max.DHW, y = median)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "orange") +
  geom_line() +
  geom_point(data = summary.dat.cl, aes(y = Rel_change_hcc_modelled,shape=Shelf),size=2.7) +
  scale_x_continuous('Maximum DHW (NOAA)')+
  scale_y_continuous('Coral cover change (%)')+
  coord_cartesian(xlim = c(0,8)) +
  annotate(geom='text',x=1.8,y=25,
           label=paste0("r^2==", round(r2, 3)),  
           parse=TRUE,size=3)+
  annotate(geom='text',x=2,y=10,
           label=paste0("P[exceed]==",round(bayes_p, 3)),  
           parse=TRUE,size=3)+
  theme_classic()+  
  theme(panel.grid.major = element_line(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face='bold'))

plot_c


################################################################################
## ereefs DHW

form.3.1 <- bf(Rel_change_hcc_modelled~ereefs.DHW,
             family = 'gaussian') 

mod.3.1 <- brm(form.3.1,
             data = summary.dat.cl,
             chains = 3,
             cores = 3,
             thin = 5,
             iter = 5000,
             warmup = 1000,
             backend = "cmdstanr"    # note Mike, you might have to change this to "rstan" if you don't have cmdstanr installed
)



mod.3.1 |>
  conditional_effects() |>
  plot(points = TRUE)

mod.3.1$fit |>
  rstan::stan_trace()
mod.3.1$fit |>
  rstan::stan_ac()
mod.3.1$fit |>
  rstan::stan_rhat()
mod.3.1$fit |>
  rstan::stan_ess()


mod.3.1 |> pp_check(type = "dens_overlay", ndraws = 100)
mod.3.1 |> pp_check(type = "intervals", ndraws = 100)

sum_tbl <- mod.3.1 |>
  as_draws_df() |>
  #exp() |> 
  dplyr::select(matches("^b_.*")) |>
  summarise_draws(
    median,
    HDInterval::hdi,
    Pl = ~ mean(.x < 0),
    Pg = ~ mean(.x > 0),
    rhat,
    ess_bulk
  )
sum_tbl
sum_tbl |> filter(variable == "b_ereefs.DHW") |> pull(median)
## for every one unit increase in Max.DHW, the odds of exceeding the threshold (being in a higher bleaching lass) decreases by a factor of 0.2 (e.g. an 80% reduction in odds).


r2 <- bayes_R2(mod.3.1)[1]
bayes_p <- sum_tbl |> filter(variable == "b_ereefs.DHW") |> pull(Pg)   ##### proportion of posteriors (slope estimate) being negative)

newdata <- with(summary.dat.cl,
                data.frame(ereefs.DHW = seq(min(ereefs.DHW, na.rm = TRUE),
                                         max(ereefs.DHW, na.rm = TRUE),
                                         length.out = 100)))

newdata <- emmeans(mod.3.1,~ereefs.DHW,at=newdata) |> 
  summary(infer=TRUE) |> 
  as.data.frame() |> 
  dplyr::rename(median=emmean,lower=lower.HPD,upper=upper.HPD)

plot_c.1 <- newdata |>
  ggplot(aes(x = ereefs.DHW, y = median)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "orange") +
  geom_line() +
  geom_point(data = summary.dat.cl, aes(y = Rel_change_hcc_modelled,shape=Shelf),size=2.7) +
  scale_x_continuous('Maximum DHW (eReefs)',breaks=c(0,4,8,12))+
  scale_y_continuous('Coral cover change (%)')+
  coord_cartesian(xlim = c(0,12)) +
  annotate(geom='text',x=1.8,y=25,
           label=paste0("r^2==", round(r2, 3)),  
           parse=TRUE,size=3)+
  annotate(geom='text',x=2,y=10,
           label=paste0("P[exceed]==",round(bayes_p, 3)),  
           parse=TRUE,size=3)+
  theme_classic()+  
  theme(panel.grid.major = element_line(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face='bold'))

plot_c.1

################################################################################
## in water DHW

# load(file="summary.dat.cl.RData")
# 
# form.3.2 <- bf(Rel_change_hcc_modelled~In.water.DHW,
#                family = 'gaussian') 
# 
# mod.3.2 <- brm(form.3.2,
#                data = summary.dat.cl,
#                chains = 3,
#                cores = 3,
#                thin = 5,
#                iter = 5000,
#                warmup = 1000,
#                backend = "cmdstanr"    # note Mike, you might have to change this to "rstan" if you don't have cmdstanr installed
# )
# 
# 
# 
# mod.3.2 |>
#   conditional_effects() |>
#   plot(points = TRUE)
# 
# mod.3.2$fit |>
#   rstan::stan_trace()
# mod.3.2$fit |>
#   rstan::stan_ac()
# mod.3.2$fit |>
#   rstan::stan_rhat()
# mod.3.2$fit |>
#   rstan::stan_ess()
# 
# 
# mod.3.2 |> pp_check(type = "dens_overlay", ndraws = 100)
# mod.3.2 |> pp_check(type = "intervals", ndraws = 100)
# 
# sum_tbl <- mod.3.2 |>
#   as_draws_df() |>
#   #exp() |> 
#   dplyr::select(matches("^b_.*")) |>
#   summarise_draws(
#     median,
#     HDInterval::hdi,
#     Pl = ~ mean(.x < 0),
#     Pg = ~ mean(.x > 0),
#     rhat,
#     ess_bulk
#   )
# sum_tbl
# sum_tbl |> filter(variable == "b_In.water.DHW") |> pull(median)
# ## for every one unit increase in Max.DHW, the odds of exceeding the threshold (being in a higher bleaching lass) decreases by a factor of 0.2 (e.g. an 80% reduction in odds).
# 
# 
# r2 <- bayes_R2(mod.3.2)[1]
# bayes_p <- sum_tbl |> filter(variable == "b_In.water.DHW") |> pull(Pg)   ##### proportion of posteriors (slope estimate) being negative)
# 
# newdata <- with(summary.dat.cl,
#                 data.frame(In.water.DHW = seq(min(In.water.DHW, na.rm = TRUE),
#                                             max(In.water.DHW, na.rm = TRUE),
#                                             length.out = 100)))
# 
# newdata <- emmeans(mod.3.2,~In.water.DHW,at=newdata) |> 
#   summary(infer=TRUE) |> 
#   as.data.frame() |> 
#   dplyr::rename(median=emmean,lower=lower.HPD,upper=upper.HPD)
# 
# plot_c.2 <- newdata |>
#   ggplot(aes(x = In.water.DHW, y = median)) +
#   geom_point(data = summary.dat.cl, aes(y = Rel_change_hcc_modelled,shape=Shelf),size=2.7) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "orange") +
#   geom_line() +
#   scale_x_continuous('Maximum DHW (In water)',breaks=c(0,4,8,12))+
#   scale_y_continuous('Coral cover change (%)')+
#   coord_cartesian(xlim = c(0,12)) +
#   annotate(geom='text',x=1.8,y=25,
#            label=paste0("r^2==", round(r2, 3)),  
#            parse=TRUE,size=3)+
#   annotate(geom='text',x=2,y=10,
#            label=paste0("P[exceed]==",round(bayes_p, 3)),  
#            parse=TRUE,size=3)+
#   theme_classic()+  
#   theme(panel.grid.major = element_line(),
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=12,face='bold'))
# 
# plot_c.2

###################################################
####  combine plots -ereefs, in-water and NOAA

# pl <- 'aaabbb
#        cccddd
#        #eee#f
#        '
# 
# heat.relationship.plots <- plot_a+plot_a.1+
#   plot_c+plot_c.1+
#   plot_b+guide_area()+
#   plot_layout(design=pl,guides = 'collect')+
#   plot_annotation(tag_levels='a')
# 
# heat.relationship.plots




pl <- 'aaabbb
       cccddd
       eeefff
       '

heat.relationship.plots <- plot_a+plot_a.1+
  plot_c+plot_c.1+
  plot_b+guide_area()+
  plot_layout(design=pl,guides = 'collect')+
  plot_annotation(tag_levels='a')

heat.relationship.plots


ggsave(heat.relationship.plots,file='heat.relationship.plots.png',height=9,width=9)



####  combine plots -ereefs, in-water and NOAA

pl <- 'aaacccddd
       eeefffggg
       ###hhhiii'

heat.relationship.plots <- plot_a+plot_a.1+plot_a.2+
  plot_c+plot_c.1+plot_c.2+
  plot_b+guide_area()+
  plot_layout(design=pl,guides = 'collect')+
  plot_annotation(tag_levels='a')

heat.relationship.plots

ggsave(heat.relationship.plots,file='heat.relationship.plots.png',height=9,width=9)


##### eReefs salinity

# shallow_salinity <- read.csv(file='raw data/ereefs_salinity_CL.csv',strip.white=TRUE)
# 
# shallow_salinity <- shallow_salinity |> 
#   mutate(date=as.Date(date,format='%d/%m/%Y'))
# 
# 
# 
# head(shallow_salinity)
# str(shallow_salinity)
# 
# 
# plot.sal <- ggplot(shallow_salinity,aes(x=date,y=Salinity))+
#   geom_line(aes(x=date,y=Salinity,colour=AIMS_REEF_NAME),alpha=0.5,show.legend = FALSE)+
#   geom_point(aes(x=date,y=Salinity),stat='summary',fun=mean,size=0.7)+
#   geom_line(,stat='summary',fun=mean,group=1,show.legend = FALSE)+
#   annotate(geom='text',x=as.Date('2023-12-01'),y=-5,label='2023',size=2)+
#   scale_y_continuous('Salinity (psu)')+
#   scale_x_date('')+#,date_breaks='1 month',date_labels = "%m %Y",
#                #labels=c('Dec 23','Jan 24','Feb 24','Mar 24','Apr 24','May 24'))+
#   coord_cartesian(ylim=c(30,36))+
#   facet_wrap(~Shelf,ncol=3)+
#   theme_classic()+
#   theme(axis.text.x = element_text(size=10,angle=90,vjust=0.5),
#         panel.grid.major = element_line(),
#         axis.text.y=element_text(size=12),
#         axis.title=element_text(size=12,face='bold'))
# 
# plot.sal
# # scale_x_date('',breaks=c(as.Date('2023-12-01','2024-01-01','2024-02-01','2024-03-01','2024-04-01','2024-05-01')),
# #              labels=c('Dec 23','Jan 24','Feb 24','Mar 24','Apr 24','May 24'))+

###################################################
####  combine plots -ereefs and NOAA

pl <- 'aaaddd
       bbbeee
       cccfff'

heat.relationship.plots <- plot_a+plot_c+plot_b+plot_a.1+plot_c.1+plot.sal+
  plot_layout(design=pl,guides = 'collect')+
  plot_annotation(tag_levels='a')

heat.relationship.plots

ggsave(heat.relationship.plots,file='heat.relationship.plots.png',height=9,width=9)

###################################################
####  combine plots -just NOAA

pl <- 'aaa
       bbb
       ccc'
 

heat.relationship.plots <- plot_a+plot_c+plot_b+
  plot_layout(design=pl,guides = 'collect')+
  plot_annotation(tag_levels='a')

heat.relationship.plots

ggsave(heat.relationship.plots,file='heat.relationship.plots.png',height=9,width=9)


######################################################################################
### coral proportion and bleaching


##  dodgy frequentist

# acr_prop_aerial.lm <- lm(Acr_prop~Aerial.bleaching,data=summary.dat.cl)
# summary(acr_prop_aerial.lm)
# 
# plot_d <- ggplot(summary.dat.cl,aes(y=Acr_prop,x=Aerial.bleaching))+
#   geom_point(stat='identity',size=3,aes(shape=Shelf))+
#   scale_y_continuous('Acropora proporation (%)',limits=c(0,100))+
#   scale_x_continuous('Aerial bleaching score',limits = c(0,6),breaks=c(0,1,2,3,4,5))+
#   geom_smooth(method='lm',colour='grey80')+
#   #geom_hline(aes(yintercept=0),linetype='dashed',colour='grey')+
#   annotate(geom='text',x=5,y=75,
#            label=("r^2==0.37"),  
#            parse=TRUE,size=3)+
#   annotate(geom='text',x=5,y=70,
#            label="F==4.54",  
#            parse=TRUE,size=3)+
#   annotate(geom='text',x=5,y=65,
#            label=("p==0.086"),  
#            parse=TRUE,size=3)+
#   theme_classic()+
#   theme(panel.grid.major = element_line(),
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=12,face='bold'))
# 
# plot_d
# 
# acr_prop_change.lm <- lm(Acr_prop~Rel_change_hcc_modelled,data=summary.dat.cl)
# summary(acr_prop_change.lm)
# 
# 
# plot_e <- ggplot(summary.dat.cl,aes(x=Acr_prop,y=Rel_change_hcc_modelled))+
#   geom_point(stat='identity',size=3,aes(shape=Shelf))+
#   scale_x_continuous('Acropora proporation (%)',limits=c(0,100))+
#   scale_y_continuous('Coral cover change (%)',limits = c(-80,20),breaks=c(-75,-50,-25,0,25))+
#   geom_smooth(method='lm',colour='grey80')+
#   #geom_hline(aes(yintercept=0),linetype='dashed',colour='grey')+
#   annotate(geom='text',x=40,y=20,
#            label=("r^2==0.08"),  
#            parse=TRUE,size=3)+
#   annotate(geom='text',x=40,y=16,
#            label="F==1.57",  
#            parse=TRUE,size=3)+
#   annotate(geom='text',x=40,y=11,
#            label=("p==0.26"),  
#            parse=TRUE,size=3)+
#   theme_classic()+
#   theme(panel.grid.major = element_line(),
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=12,face='bold'))
# plot_e
# 
# 
# 
# 
# poc_prop_aerial.lm <- lm(Poc_prop~Aerial.bleaching,data=summary.dat.cl)
# summary(poc_prop_aerial.lm)
# 
# plot_f <- ggplot(summary.dat.cl,aes(y=Poc_prop,x=Aerial.bleaching))+
#   geom_point(stat='identity',size=3,aes(shape=Shelf))+
#   scale_y_continuous('Pocillopora proporation (%)',limits=c(0,100))+
#   scale_x_continuous('Aerial bleaching score',limits = c(0,6),breaks=c(0,1,2,3,4,5))+
#   geom_smooth(method='lm',colour='grey80')+
#   #geom_hline(aes(yintercept=0),linetype='dashed',colour='grey')+
#   annotate(geom='text',x=5,y=75,
#            label=("r^2==0.76"),  
#            parse=TRUE,size=3)+
#   annotate(geom='text',x=5,y=70,
#            label="F==20.18",  
#            parse=TRUE,size=3)+
#   annotate(geom='text',x=5,y=65,
#            label=("p==0.006"),  
#            parse=TRUE,size=3)+
#   theme_classic()+
#   theme(panel.grid.major = element_line(),
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=12,face='bold'))
# 
# plot_f
# 
# 
# poc_prop_change.lm <- lm(Poc_prop~Rel_change_hcc_modelled,data=summary.dat.cl)
# summary(poc_prop_change.lm)
# 
# 
# plot_g <- ggplot(summary.dat.cl,aes(x=Poc_prop,y=Rel_change_hcc_modelled))+
#   geom_point(stat='identity',size=3,aes(shape=Shelf))+
#   scale_x_continuous('Pocillopora proporation (%)',limits=c(0,100))+
#   scale_y_continuous('Coral cover change (%)',limits = c(-80,25),breaks=c(-75,-50,-25,0,25))+
#   geom_smooth(method='lm',colour='grey80')+
#   #geom_hline(aes(yintercept=0),linetype='dashed',colour='grey')+
#   annotate(geom='text',x=5,y=20,
#            label=("r^2==0.36"),  
#            parse=TRUE,size=3)+
#   annotate(geom='text',x=5,y=16,
#            label="F==4.94",  
#            parse=TRUE,size=3)+
#   annotate(geom='text',x=5,y=11,
#            label=("p==0.068"),  
#            parse=TRUE,size=3)+
#   theme_classic()+
#   theme(panel.grid.major = element_line(),
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=12,face='bold'))
# plot_g
# 
#####coral porporation plots



############## Bayes


################################################################################

## bleaching  vs Acropora

form.4 <- bf(oAerial.bleaching ~ Acr_prop,
           family = cumulative("logit", threshold = "flexible")) 

mod.4 <- brm(form.4,
             data = summary.dat.cl,
             chains = 3,
             cores = 3,
             thin = 5,
             iter = 5000,
             warmup = 1000,
             backend = "cmdstanr"    # note Mike, you might have to change this to "rstan" if you don't have cmdstanr installed
)



mod.4 |>
  conditional_effects(categorical = TRUE) |>
  plot(points = TRUE)

mod.4$fit |>
  rstan::stan_trace()
mod.4$fit |>
  rstan::stan_ac()
mod.4$fit |>
  rstan::stan_rhat()
mod.4$fit |>
  rstan::stan_ess()


mod.4 |> pp_check(type = "dens_overlay", nsamples = 100)
mod.4 |> pp_check(type = "intervals", nsamples = 100)

sum_tbl <- mod.4 |>
  as_draws_df() |>
  exp() |> 
  dplyr::select(matches("^b_.*")) |>
  summarise_draws(
    median,
    HDInterval::hdi,
    Pl = ~ mean(.x < 1),
    Pg = ~ mean(.x > 1),
    rhat,
    ess_bulk
  )
sum_tbl
sum_tbl |> filter(variable == "b_Acr_prop") |> pull(median)
## for every one unit increase in Max.DHW, the odds of exceeding the threshold (being in a higher bleaching lass) decreases by a factor of 0.2 (e.g. an 80% reduction in odds).


r2 <- bayes_R2(mod.4)[1]
bayes_p <- sum_tbl |> filter(variable == "b_Acr_prop") |> pull(Pg)

newdata <- with(summary.dat.cl,
                data.frame(Acr_prop = seq(min(Acr_prop, na.rm = TRUE),
                                         max(Acr_prop, na.rm = TRUE),
                                         length.out = 100)))
newdata <- add_epred_draws(mod.4, newdata = newdata, re_formula = NA) |>
  mutate(fit = as.numeric(as.character(.category)) * .epred) |>
  group_by(Acr_prop, .draw) |>
  summarise(fit = sum(fit)) |>
  summarise_draws(
    median,
    HDInterval::hdi
  ) |>
  arrange(Acr_prop) 

plot_d <- newdata |>
  ggplot(aes(x = Acr_prop, y = median)) +
  geom_point(data = summary.dat.cl, aes(y = Aerial.bleaching,shape=Shelf),size=2.7) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "#1B9E77") +
  geom_line() +
  scale_y_continuous('Aerial bleaching score')+
  scale_x_continuous('Acropora proportion (%)')+
  #coord_cartesian(ylim = c(0, 5), xlim = c(0,8)) +
  annotate(geom='text',x=19.8,y=4.8,
           label=paste0("r^2==", round(r2, 3)),  
           parse=TRUE,size=3)+
  annotate(geom='text',x=20,y=4.4,
           label=paste0("P[exceed]==",round(bayes_p, 3)),  
           parse=TRUE,size=3)+
  ggtitle('b')+
  theme_classic()+  
  theme(panel.grid.major = element_line(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face='bold'))

plot_d

#####
# rel change vs acr proportion


form.5 <- bf(Rel_change_hcc_modelled~Acr_prop,
             family = 'gaussian') 

mod.5 <- brm(form.5,
             data = summary.dat.cl,
             chains = 3,
             cores = 3,
             thin = 5,
             iter = 5000,
             warmup = 1000,
             backend = "cmdstanr"    # note Mike, you might have to change this to "rstan" if you don't have cmdstanr installed
)



mod.5 |>
  conditional_effects() |>
  plot(points = TRUE)

mod.5$fit |>
  rstan::stan_trace()
mod.5$fit |>
  rstan::stan_ac()
mod.5$fit |>
  rstan::stan_rhat()
mod.5$fit |>
  rstan::stan_ess()


mod.5 |> pp_check(type = "dens_overlay", ndraws = 100)
mod.5 |> pp_check(type = "intervals", ndraws = 100)

sum_tbl <- mod.5 |>
  as_draws_df() |>
  #exp() |> 
  dplyr::select(matches("^b_.*")) |>
  summarise_draws(
    median,
    HDInterval::hdi,
    Pl = ~ mean(.x < 0),
    Pg = ~ mean(.x > 0),
    rhat,
    ess_bulk
  )
sum_tbl
sum_tbl |> filter(variable == "b_Acr_prop") |> pull(median)
## for every one unit increase in Max.DHW, the odds of exceeding the threshold (being in a higher bleaching lass) decreases by a factor of 0.2 (e.g. an 80% reduction in odds).


r2 <- bayes_R2(mod.5)[1]
bayes_p <- sum_tbl |> filter(variable == "b_Acr_prop") |> pull(Pl)   ##### proportion of posteriors (slope estimate) being negative)

newdata <- with(summary.dat.cl,
                data.frame(Acr_prop = seq(min(Acr_prop, na.rm = TRUE),
                                                  max(Acr_prop, na.rm = TRUE),
                                                  length.out = 100)))

newdata <- emmeans(mod.5,~Acr_prop,at=newdata) |> 
  summary(infer=TRUE) |> 
  as.data.frame() |> 
  dplyr::rename(median=emmean,lower=lower.HPD,upper=upper.HPD)

plot_e <- newdata |>
  ggplot(aes(x = Acr_prop, y = median)) +
  geom_point(data = summary.dat.cl, aes(y = Rel_change_hcc_modelled,shape=Shelf),size=2.7) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "#1B9E77") +
  geom_line() +
  scale_x_continuous('Acropora proportion (%)')+
  scale_y_continuous('Relative cover change (%)')+
  #coord_cartesian(ylim = c(0, 5), xlim = c(0,8)) +
  annotate(geom='text',x=40,y=18,
           label=paste0("r^2==", round(r2, 3)),  
           parse=TRUE,size=3)+
  annotate(geom='text',x=40,y=7,
           label=paste0("P[exceed]==",round(bayes_p, 3)),  
           parse=TRUE,size=3)+
  ggtitle('c')+
  theme_classic()+  
  theme(panel.grid.major = element_line(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face='bold'))

plot_e


################################################################################

## bleaching  vs Pocillopora

form.6 <- bf(oAerial.bleaching ~ Poc_prop,
             family = cumulative("logit", threshold = "flexible")) 

mod.6 <- brm(form.6,
             data = summary.dat.cl,
             chains = 3,
             cores = 3,
             thin = 5,
             iter = 5000,
             warmup = 1000,
             backend = "cmdstanr"    # note Mike, you might have to change this to "rstan" if you don't have cmdstanr installed
)



mod.6 |>
  conditional_effects(categorical = TRUE) |>
  plot(points = TRUE)

mod.6$fit |>
  rstan::stan_trace()
mod.6$fit |>
  rstan::stan_ac()
mod.6$fit |>
  rstan::stan_rhat()
mod.6$fit |>
  rstan::stan_ess()


mod.6 |> pp_check(type = "dens_overlay", nsamples = 100)
mod.6 |> pp_check(type = "intervals", nsamples = 100)

sum_tbl <- mod.6 |>
  as_draws_df() |>
  exp() |> 
  dplyr::select(matches("^b_.*")) |>
  summarise_draws(
    median,
    HDInterval::hdi,
    Pl = ~ mean(.x < 1),
    Pg = ~ mean(.x > 1),
    rhat,
    ess_bulk
  )
sum_tbl
sum_tbl |> filter(variable == "b_Poc_prop") |> pull(median)
## for every one unit increase in Max.DHW, the odds of exceeding the threshold (being in a higher bleaching lass) decreases by a factor of 0.2 (e.g. an 80% reduction in odds).


r2 <- bayes_R2(mod.6)[1]
bayes_p <- sum_tbl |> filter(variable == "b_Poc_prop") |> pull(Pl)

newdata <- with(summary.dat.cl,
                data.frame(Poc_prop = seq(min(Poc_prop, na.rm = TRUE),
                                          max(Poc_prop, na.rm = TRUE),
                                          length.out = 100)))
newdata <- add_epred_draws(mod.6, newdata = newdata, re_formula = NA) |>
  mutate(fit = as.numeric(as.character(.category)) * .epred) |>
  group_by(Poc_prop, .draw) |>
  summarise(fit = sum(fit)) |>
  summarise_draws(
    median,
    HDInterval::hdi
  ) |>
  arrange(Poc_prop) 

plot_f <- newdata |>
  ggplot(aes(x = Poc_prop, y = median)) +
  geom_point(data = summary.dat.cl, aes(y = Aerial.bleaching,shape=Shelf),size=2.7) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "#E6AB02") +
  geom_line() +
  scale_y_continuous('Aerial bleaching score')+
  scale_x_continuous('Pocillopora proportion')+
  #coord_cartesian(ylim = c(0, 5), xlim = c(0,8)) +
  annotate(geom='text',x=39.8,y=4.8,
           label=paste0("r^2==", round(r2, 3)),  
           parse=TRUE,size=3)+
  annotate(geom='text',x=40,y=4.4,
           label=paste0("P[exceed]==",round(bayes_p, 3)),  
           parse=TRUE,size=3)+
  ggtitle('d')+
  theme_classic()+  
  theme(panel.grid.major = element_line(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face='bold'))

plot_f

#####
# rel change vs poc proportion


form.7 <- bf(Rel_change_hcc_modelled~Poc_prop,
             family = 'gaussian') 

mod.7 <- brm(form.7,
             data = summary.dat.cl,
             chains = 3,
             cores = 3,
             thin = 5,
             iter = 5000,
             warmup = 1000,
             backend = "cmdstanr"    # note Mike, you might have to change this to "rstan" if you don't have cmdstanr installed
)



mod.7 |>
  conditional_effects() |>
  plot(points = TRUE)

mod.7$fit |>
  rstan::stan_trace()
mod.7$fit |>
  rstan::stan_ac()
mod.7$fit |>
  rstan::stan_rhat()
mod.7$fit |>
  rstan::stan_ess()


mod.7 |> pp_check(type = "dens_overlay", ndraws = 100)
mod.7 |> pp_check(type = "intervals", ndraws = 100)

sum_tbl <- mod.7 |>
  as_draws_df() |>
  #exp() |> 
  dplyr::select(matches("^b_.*")) |>
  summarise_draws(
    median,
    HDInterval::hdi,
    Pl = ~ mean(.x < 0),
    Pg = ~ mean(.x > 0),
    rhat,
    ess_bulk
  )
sum_tbl
sum_tbl |> filter(variable == "b_Poc_prop") |> pull(median)
## for every one unit increase in Max.DHW, the odds of exceeding the threshold (being in a higher bleaching lass) decreases by a factor of 0.2 (e.g. an 80% reduction in odds).


r2 <- bayes_R2(mod.7)[1]
bayes_p <- sum_tbl |> filter(variable == "b_Poc_prop") |> pull(Pg)   ##### proportion of posteriors (slope estimate) being negative)

newdata <- with(summary.dat.cl,
                data.frame(Poc_prop = seq(min(Poc_prop, na.rm = TRUE),
                                          max(Poc_prop, na.rm = TRUE),
                                          length.out = 100)))
# newdata <- add_epred_draws(mod.7, newdata = newdata, re_formula = NA) |>
#   mutate(fit = .epred) |>
#   group_by(Aerial.bleaching, .draw) |>
#   summarise_draws(
#     median,
#     HDInterval::hdi
#   ) |>
#   arrange(Aerial.bleaching)

newdata <- emmeans(mod.7,~Poc_prop,at=newdata) |> 
  summary(infer=TRUE) |> 
  as.data.frame() |> 
  dplyr::rename(median=emmean,lower=lower.HPD,upper=upper.HPD)

plot_g <- newdata |>
  ggplot(aes(x = Poc_prop, y = median)) +
  geom_point(data = summary.dat.cl, aes(y = Rel_change_hcc_modelled,shape=Shelf),size=2.7) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "#E6AB02") +
  geom_line() +
  scale_x_continuous('Pocillopora proportion (%)')+
  scale_y_continuous('Relative cover change (%)')+
  #coord_cartesian(ylim = c(0, 5), xlim = c(0,8)) +
  annotate(geom='text',x=29.5,y=36,
           label=paste0("r^2==", round(r2, 3)),  
           parse=TRUE,size=3)+
  annotate(geom='text',x=30,y=24,
           label=paste0("P[exceed]==",round(bayes_p, 3)),  
           parse=TRUE,size=3)+
  ggtitle('e')+
  theme_classic()+  
  theme(panel.grid.major = element_line(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face='bold'))

plot_g

##########################################################
### AIMS in-water

head(iw.data.ts.filtered)

iw.data.ts.filtered.sum <- iw.data.ts.filtered |> 
  group_by(Reef) |> 
  summarise(DHW_inwater=max(IW_DHW,na.rm=TRUE))

###############################################
### combine plots


pl3 <- 'aaabbb
       cccddd'


prop.relationship.plots <- plot_d+plot_e+plot_f+plot_g+
  plot_layout(design=pl3,guides = 'collect')+
  theme(legend.position='left')#,
        legend.direction='horizontal')
  #plot_annotation(tag_levels='a')

prop.relationship.plots

ggsave(prop.relationship.plots,file='prop.relationship.plots.png',height=9,width=9)


#########################################################################################################
########################################################################################################

# how much acr ------------------------------------------------------------

load(file='raw data/comp2021_2024.RData')

str(comp2021_2024)

#load(file='raw data/all_benthic_data.RData')
head(comp2021_2024)
levels(comp2021_2024$cREPORT_YEAR)

cl.2024 <-comp2021_2024 |> 
  filter(cREPORT_YEAR==2024,A_SECTOR=='CL')


ggplot(comp2021_2024 |> filter(COMP_2021=='ACTO'),aes(x=cREPORT_YEAR,y=COVER,fill=REEF_NAME))+
  geom_point(stat='summary',fun.y='mean',show.legend = F)+
  geom_errorbar(stat='summary',fun.data=mean_se)+
  facet_grid(A_SECTOR~SHELF)
  




levels(how_much_acr$REEF)

how_much_acr<-cl.2024 |> #filter(A_SECTOR=="CL",REPORT_YEAR %in% c(2023,2024)) |> 
  #droplevels() |> 
  dplyr::select(-POINTS,-total.points,-VISIT_NO) |> 
  pivot_wider(names_from=COMP_2021,values_from = COVER) |> 
  #group_by(P_CODE,A_SECTOR,SHELF,REEF,SITE_DEPTH,REPORT_YEAR,SITE_NO,SITE_LAT,SITE_LONG,TRANSECT_NO,cREPORT_YEAR,Decade) |> 
  #summarise(across(ACBX:S_POR_RUS),~sum(.x)) |> 
  dplyr::select(-AB,-CA,-F_SC,-F_SC_NEP,-F_SC_SAR,-F_SC_SCl_LOB,-F_SC_XEN,-SC_GORG_LIKE,-SC_OTH,-SP,-TA,-MA_OTH,-MA_OTH,-MA_RED,
                -MA_BROWN,-ZOA,-SG,-OT,-MA_GREEN,-SC_OTH_E,-G_MIL) |> 
  mutate(tot_hc=rowSums(across(ACD:COR_CL))) |> 
  mutate(acr=ACBX+ACD+ACSE+ACTO) |> 
  mutate(acr_prop=ACBX+ACD+ACSE+ACTO/tot_hc*100) |> 
  mutate(poc=G_POC+G_STY+G_SER) |> 
  mutate(poc_prop=poc/tot_hc*100) |> 
  mutate(por=G_POR_B+G_POR_CECS+G_POR_M) |> 
  mutate(por_prop=por/tot_hc*100) |>
  group_by(A_SECTOR,SHELF,REEF_NAME) |> 
  summarise(tot_hc=mean(tot_hc),
            acr_prop=mean(acr_prop),
            poc_prop=mean(poc_prop),
            por_pro=mean(por_prop)) |> 
  as.data.frame()
  #dplyr::select(A_SECTOR,SHELF,REEF_NAME,REPORT_YEAR,SITE_NO,TRANSECT_NO,cREPORT_YEAR,tot_hc,acr,acr_prop,poc,poc_prop,por,por_prop)


#how_much_acr[c(4,57,59,61,63)] 
names(how_much_acr)

save(how_much_acr,file='how_much_acr.RData')


head(how_much_acr)

comp_prop <- how_much_acr |> group_by(A_SECTOR,SHELF,REEF_NAME) |> 
  summarise(acr_prop_ave=mean(acr_prop),poc_prop_ave=mean(poc_prop),por_prop_ave=mean(por_prop),
            acr_prop_sd=sd(acr_prop),poc_prop_sd=sd(poc_prop),por_prop_sd=sd(por_prop),
            acr_prop_se=sqrt(acr_prop_sd)/15,poc_prop_se=sqrt(poc_prop_sd)/15,por_prop_se=sqrt(por_prop_sd)/15) |> 
  mutate(REEF_NAME=factor(REEF_NAME,levels=c('LINNET REEF','MARTIN REEF(14123)','LIZARD ISLAND','MACGILLIVRAY REEF',
                                            'NORTH DIRECTION REEF','CARTER REEF','NO NAME REEF','YONGE REEF')))


ggplot(comp_prop,aes(x=REEF_NAME,y=acr_prop_ave))+
  geom_point(stat='identity',colour='black',position = position_dodge(width=0.5),size=2.5)+
  geom_errorbar(stat='identity',aes(ymin = acr_prop_ave-(2*acr_prop_se),ymax = acr_prop_ave+(2*acr_prop_se)),width=0,,colour='black',
                position = position_dodge(width=0.5))+
  geom_point(stat='identity',aes(y=poc_prop_ave),colour='blue',position = position_dodge(width=1.5),size=2.5)+
  geom_errorbar(stat='identity',aes(ymin = poc_prop_ave-(2*poc_prop_se),ymax = poc_prop_ave+(2*poc_prop_se)),width=0,colour='blue',
                position = position_dodge(width=1.5))+
  geom_point(stat='identity',aes(y=por_prop_ave),colour='red',position = position_dodge(width=2.5),size=2.5)+
  geom_errorbar(stat='identity',aes(ymin = por_prop_ave-(2*por_prop_se),ymax = por_prop_ave+(2*por_prop_se)),width=0,colour='red',
                position = position_dodge(width=1.5))
  

###############################################################################################

# ## 2024 composition plots -----------------------------------------------



head(comp2021_2024)

reef_comp_2024_plot <- ggplot(comp2021_2024 |> filter(A_SECTOR=='CL',
                               !COMP_2021 %in% c('AB','CA','MA_BROWN','MA_GREEN','MA_OTH','MA_RED',
                                                 'OT','SC_GORG_LIKE','SC_OTH','SC_OTH_E','SG','SP',
                                                 'TA','ZOA')),
       aes(x=COMP_2021,y=COVER,fill=COMP_2021))+
  geom_bar(stat='summary',fun.y='mean',show.legend = T,position=position_dodge(width=0.1))+
  geom_errorbar(stat='summary',fun.data=mean_se,position=position_dodge(width=0.1))+
  facet_wrap(~REEF_NAME)+
  theme(legend.text = element_text(size=7),
        legend.position = 'bottom',
        legend.title=element_blank(),
        axis.text.x = element_blank())+
  guides(fill = guide_legend(nrow = 6))

reef_comp_2024_plot

ggsave(reef_comp_2024_plot,file='reef_comp_2024_plot.png', height=9,width=9)





###########################################################################
## plot broad categories of hc



# dplyr::select(-AB,-CA,-F_SC,-F_SC_NEP,-F_SC_SAR,-F_SC_SCl_LOB,-F_SC_XEN,-SC_GORG_LIKE,-SC_OTH,-SP,-TA,-MA_OTH,-MA_OTH,-MA_RED,
#               -MA_BROWN,-ZOA,-SG,-OT,-MA_GREEN,-SC_OTH_E,-G_MIL) 


broad.props <- cl.2024 |> 
  dplyr::select(-POINTS,-total.points,-VISIT_NO) |> 
  pivot_wider(names_from=COMP_2021,values_from = COVER) |>
  mutate(other_Acropora=ACD+ACBX+ACSE,
         Porites=G_POR_B+G_POR_CECS+G_POR_M,
         other_coral=COR_CBCF+COR_CE+COR_CL+COR_CMCS+F_AGA_CEMS+F_AGA_CF+F_EUPH_PLU+F_FUN_CECF+F_FUN_F_C+F_FUN_F_S+F_MER_CEMS+
         F_MER_PLE_PLOC+F_SID_COS_PSA+G_ACA_MIC_HOM+G_AST+G_DIP+G_ECH_CB+G_ECH_OTH+G_GAL+G_GON_ALV_BER+G_HEL+G_HYD_CB+G_HYD_OTH+
         G_ISO_B+G_ISO_CSE+G_LEP+G_LOB_AUST+G_MER+G_MON+G_MOS+G_MYC+G_OXY_ECL_ECY+G_PAC+G_PEC+G_SER+G_STY+G_TUB_HET+G_TUR_DUN,
         soft_coral=SC_OTH_E+F_SC+F_SC_CLA+F_SC_NEP+F_SC_SAR+F_SC_SCl_LOB+F_SC_XEN,
         Pocilloporidae=G_POC+G_STY+G_SER) |> 
  mutate(REEF_NAME=factor(REEF_NAME,levels=c('LINNET REEF','MARTIN REEF(14123)','LIZARD ISLAND','MACGILLIVRAY REEF','NORTH DIRECTION REEF',
                                      'CARTER REEF','NO NAME REEF','YONGE REEF'),
                   labels=c('Linnet','Martin','Lizard Island','Macgillivray','North Direction/n Island','Carter','No Name','Yonge')))
  # mutate(Reef=ifelse(REEF_NAME=='LINNET REEF',Linnet,
  #                    ifelse(REEF_NAME=='MARTIN REEF',Martin,
  #                           ifelse(REEF_NAME=='LIZARD ISLAND',Lizard,
  #                                  ifelse(REEF_NAME=='NORTH DIRECTION REEF',Nth_Direction,
  #                                         ifelse(REEF_NAME=='MACGILLIVRAY REEF',Macgillivray,
  #                                                ifelse(REEF_NAME=='CARTER REEF',Carter,
  #                                                       ifelse(REEF_NAME=='NO NAME REEF',No_Name,
  #                                                              ifelse(REEF_NAME=='YONGE REEF',Yonge)))))))))

broad.props


broad.props.l <- broad.props |> 
  dplyr::select(A_SECTOR,SHELF,REEF_NAME,REPORT_YEAR,SITE_NO,TRANSECT_NO,cREPORT_YEAR,ACTO,other_Acropora,Pocilloporidae,Porites,other_coral,soft_coral) |> 
  rename(table_Acropora=ACTO) |> 
  pivot_longer(cols=table_Acropora:soft_coral,names_to = 'Taxa') |> 
  mutate(Taxa=factor(Taxa,levels=c('table_Acropora','other_Acropora','Porites','other_coral','soft_coral','Pocilloporidae')))


broad.comp.plot <- ggplot(broad.props.l,aes(x=REEF_NAME,y=value,fill=Taxa))+
  geom_bar(stat='summary',fun.y='mean',show.legend = T,position=position_dodge(width=0.6),width=0.5,colour='black',linewidth=0.1)+
  geom_errorbar(stat='summary',fun.data=mean_se,position=position_dodge(width=0.6),width=0)+
  scale_fill_brewer(type="qual",palette=2,labels=c('table Acropora','other Acropora','Porites','other corals','soft corals','Pocilloporidae'))+
  scale_y_continuous('Cover (%)',limits=c(0,61),breaks=c(0,20,40,60))+
  ggtitle('a')+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.title.x=element_blank())

broad.comp.plot

ggsave(broad.comp.plot,file='outputs/broad.comp.plot.png',height=5,width=7)

################################################################################################################################
#### plot with coral taxa as proportion of total HC

names(broad.props)

broad.props_prop <- broad.props |> 
  mutate(total_hc=ACTO+other_Acropora+G_POC+Porites+other_coral,
         table_Acropora_prop=ACTO/total_hc*100,
         other_Acropora_prop=other_Acropora/total_hc*100,
         Porites_prop=Porites/total_hc*100,
         other_coral_prop=other_coral/total_hc*100,
         Pocilloporidae_prop=Pocilloporidae/total_hc*100) |> 
  as.data.frame()



head(broad.props_prop)

# mutate(REEF_NAME=factor(REEF_NAME,levels=c('LINNET REEF','MARTIN REEF(14123)','LIZARD ISLAND','MACGILLIVRAY REEF','NORTH DIRECTION REEF',
#                                            'CARTER REEF','NO NAME REEF','YONGE REEF'),
#                         labels=c('Linnet','Martin','Lizard Island','Macgillivray','North Direction/n Island','Carter','No Name','Yonge')))

levels(broad.props_prop.l$REEF_NAME)

broad.props_prop.l <- broad.props_prop |> 
  dplyr::select(A_SECTOR,SHELF,REEF_NAME,REPORT_YEAR,SITE_NO,TRANSECT_NO,cREPORT_YEAR,table_Acropora_prop,other_Acropora_prop,
                Pocilloporidae_prop,Porites_prop,other_coral_prop,soft_coral) |> 
  #rename(table_Acropora=ACTO) |> 
  pivot_longer(cols=c(table_Acropora_prop:soft_coral,soft_coral),names_to = 'Taxa') |> 
  mutate(Taxa=factor(Taxa,levels=c('table_Acropora_prop','other_Acropora_prop','Porites_prop','other_coral_prop','soft_coral','Pocilloporidae_prop')),
         REEF_NAME=factor(REEF_NAME,levels=c('Linnet','Martin','Lizard Island','Macgillivray','North Direction Island',
                                                                                        'Carter','No Name','Yonge'),
                                    labels=c('Linnet','Martin','Lizard Island','Macgillivray','North Direction\n Island','Carter','No Name','Yonge')))


broad.comp.plot <- ggplot(broad.props_prop.l,aes(x=REEF_NAME,y=value,fill=Taxa))+
  geom_bar(stat='summary',fun.y='mean',show.legend = T,position=position_dodge(width=0.6),width=0.5,colour='black',linewidth=0.1)+
  geom_errorbar(stat='summary',fun.data=mean_se,position=position_dodge(width=0.6),width=0)+
  geom_vline(xintercept = 2.5,linetype='dashed')+
  geom_vline(xintercept = 5.5,linetype='dashed')+
  scale_fill_brewer(type="qual",palette=2,labels=c('table Acropora','other Acropora','Porites','other corals','soft corals','Pocilloporidae'))+
  scale_y_continuous('Proportion of total cover (%)')+
  annotate(geom='text',x=1.5,y=55,label='Inner shelf')+
  annotate(geom='text',x=4,y=55,label='Mid-shelf')+
  annotate(geom='text',x=7,y=55,label='Outer shelf')+
  ggtitle('a')+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45,hjust=1,size=12),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=12,face='bold'),
        legend.position='top',
        legend.title=element_blank(),
        legend.key.size = unit(0.2, "cm"),
        plot.margin = margin(0,0,1,0,"cm"))+
  guides(fill = guide_legend(nrow = 1))

broad.comp.plot

ggsave(broad.comp.plot,file='outputs/broad.comp.plot.png',height=5,width=7)






# library(RColorBrewer)
# brewer.pal(n=6,'Dark2')

########

pl4 <- 'aaa
        bbb
        bbb'


acr_poc_plot <- broad.comp.plot+prop.relationship.plots+
  plot_layout(design=pl4)#+
  #plot_annotation(tag_levels='a')

acr_poc_plot

ggsave(acr_poc_plot,file='outputs/acr_poc_plot.png',height=9,width=9)
ggsave(acr_poc_plot,file='outputs/acr_poc_plot.pdf',height=9,width=9)


############################################################################
###3  water quality

ereefs <- read.csv(file='raw data/ereefs_salinity_CL.csv',strip.white=TRUE)
head(ereefs)

ereefs <- ereefs |> 
  mutate(AIMS_REEF_NAME=factor(AIMS_REEF_NAME),
         Shelf=factor(Shelf),
         date=format(as.Date(date),"%d/%m/%Y"))

str(ereefs)


ggplot(ereefs |> filter (AIMS_REEF_NAME=='Helsdon Reef'),
       aes(x=date,y=Salinity,fill=AIMS_REEF_NAME,colour=AIMS_REEF_NAME))+
  geom_point(stat='identity')+
  geom_line(stat='identity',group=1)+
  facet_wrap(~Shelf)+
  theme(axis.text.x = element_text(angle=90,size=6))

# ereefs <- ereefs |> 
#   mutate(Site.Name=factor(Site.Name),
#          cDepth=factor(Depth),
#          Date=as.Date(Aggregated.Date.Time))
# 
# ggplot(ereefs |> filter(Variable=='salt'),
#        aes(x=Date,y=median,fill=cDepth,group=cDepth))+
#   geom_point(stat='identity',pch=21)+
#   geom_line(stat='identity')+
#   facet_wrap(~Site.Name)
# 
# ggplot(ereefs |> filter(Variable=='temp'),
#        aes(x=Date,y=median,fill=cDepth,group=cDepth))+
#   geom_point(stat='identity',pch=21)+
#   geom_line(stat='identity')+
#   facet_wrap(~Site.Name)


#######################################################
######  COTS data

load(file='manta_cots_hcc_for_DW_Oct24_update.RData')

head(manta_cots_hcc_for_DW_Oct24_update)

cots.summary<-manta_cots_hcc_for_DW_Oct24_update |> 
  group_by(A_SECTOR,SHELF,REEF_ID,REEF_NAME,REEF_LAT,REEF_LONG,REPORT_YEAR) |> 
  summarise(tows=max(TOW_SEQ_NO),
            cots=sum(COT_COUNT),
            cots_per_tow=cots/tows,
            scar.low=sum(SCAR=='P'),
            prop.scars.low=scar.low/tows,
            scar.high=sum(SCAR=='C'),
            prop.scars.high=scar.high/tows) |> 
  mutate(SHELF=factor(SHELF,levels=c('I','M','O')),
         REEF_NAME=factor(REEF_NAME,levels=c('LINNET REEF','MARTIN REEF(14123)','EYRIE REEF','EYE REEF','LIZARD ISLAND','MACGILLIVRAY REEF','NORTH DIRECTION REEF',
                                             'SOUTH DIRECTION REEF (NORTH)','CARTER REEF','NO NAME REEF','YONGE REEF','HASTINGS REEF','MACKAY REEF','THETFORD REEF',
                                             'AGINCOURT REEFS (NO 1)','ST CRISPIN REEF','FEATHER REEF','FARQUHARSON REEF (NO 1)','PEART REEF','TAYLOR REEF'),
                          labels=c('Linnet','Martin','Eyrie','Eye','Lizard Island','Macgillivray','North Direction\n Island','South Direction\n Island',
                                   'Carter','No Name','Yonge','Hastings','Mackay','Thetford','Agincourt','St Crispin','Feather','Farquharson',
                                   'Peart','Taylor')),
         cots_per_tow=cots/tows)

cots.summary

save(cots.summary,file='cots.summary.RData')

cots.summary.cl <- cots.summary |> 
  filter(A_SECTOR=='CL')

cots.summary.cl

cots.summary.cl.l <- cots.summary.cl |> 
  dplyr::select(SHELF,REEF_NAME,prop.scars.low,prop.scars.high,cots_per_tow) |> 
  pivot_longer(cols=prop.scars.low:cots_per_tow,names_to = 'data_type',values_to = 'scars_cots')

cots.scar.plot<-ggplot(cots.summary.cl |> 
                         filter(A_SECTOR=='CL'),       #Cooktown/Lizard
                       aes(x=REEF_NAME,y=prop.scars.low))+
  geom_bar(stat='identity',fill='grey70',just=1,width=0.4,color='black')+
  geom_bar(stat='identity',fill='black',aes(y=prop.scars.high),just=0,width=0.4,color='black')+
  scale_y_continuous('Propotion of tows with COTS scars',limits=c(0,1))+
  scale_x_discrete('')+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45,vjust=0.5,hjust=0.5),
        panel.grid.major.y = element_blank())
cots.scar.plot

ggsave(cots.scar.plot,file='cots.scar.plot.png',height=7,width=9)



cots.plot.cl<-ggplot(cots.summary.cl,aes(x=REEF_NAME,y=cots_per_tow))+
  
  geom_hline(yintercept=1,colour='red',linetype='dashed')+
  geom_hline(yintercept=0.22,colour='orange',linetype='dashed')+
  geom_hline(yintercept=0.1,colour='darkorchid',linetype='dashed')+
  geom_bar(stat='identity',fill='grey40',color='black',just=1,width=0.4)+
  #geom_bar(stat='identity',fill='black',aes(y=prop.scars.high),just=0,width=0.4)+
  scale_y_continuous('Number of COTS per tow',limits=c(0,1))+
  scale_x_discrete('')+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45,vjust=0.5,hjust=0.5),
        panel.grid.major.y = element_blank())
cots.plot.cl


cots.plot.comb <- cots.plot.cl+cots.scar.plot2+
  plot_annotation(tag_level = 'a')
cots.plot.comb

ggsave(cots.plot.comb,file='outputs/cots.plot.comb.png',height=6,width=10)

##################################################################################################

cots.summary.cl.l <- cots.summary.cl |> 
  dplyr::select(SHELF,REEF_NAME,prop.scars.low,prop.scars.high,cots_per_tow) |> 
  pivot_longer(cols=prop.scars.low:cots_per_tow,names_to = 'data_type',values_to = 'scars_cots')


cots.scar.plot2 <- ggplot(cots.summary.cl.l |> filter(data_type!='cots_per_tow'),aes(x=REEF_NAME,y=scars_cots,fill=data_type))+
  geom_bar(stat='identity',position=position_dodge(width=0.5),just=1,width=0.4,color='black')+
  scale_y_continuous('Propotion of tows with COTS scars',limits=c(0,1))+
  scale_x_discrete('')+
  scale_fill_manual(values=c('grey70','black'),labels=c('>10 scars','<10 scars'))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45,vjust=0.5,hjust=0.5),
        panel.grid.major.y = element_blank(),
        legend.position = 'inside',
        legend.justification = c(1,0.8),
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_blank())


########################################################################################
### in water temp logger ----------------------------------------------------


iw.data.ts <- read.csv(file = "NOAA_CRW_SST_DHW_LizardSector_long.csv", strip.white = TRUE)

#sets DateTime as a dttm POSIXct class using Lubridate

iw.data.ts$Date = dmy(iw.data.ts$Date, tz="Australia/Brisbane")

iw.data.ts$ReefName <- as.factor(iw.data.ts$ReefName)
iw.data.ts$Slope_location <- as.factor(iw.data.ts$Slope_location)


#Filter dataset by date range
iw.data.ts.filtered <- iw.data.ts %>%
  filter(Date >= "2023-10-31" & Date <= "2024-04-30")
iw.data.ts.filtered <- iw.data.ts.filtered %>%
  mutate(date = as.Date(Date, format="%Y-%m-%d"))

levels(iw.data.ts$ReefName)


iw.data.ts.filtered <- iw.data.ts.filtered |> 
  mutate(Reef=ifelse(ReefName=='Lizard Island_FL1','Lizard Island',
                 ifelse(ReefName=='Lizard Island_SL1','Lizard Island',
                     ifelse(ReefName=='MacGillvray_SL1','Macgillivray Reef',
                            ifelse(ReefName=='Martin_SL1','Martin Reef','Yonge Reef'))))) |> 
  mutate(Shelf=ifelse(ReefName=='Lizard Island_FL1','Mid-shelf',
                  ifelse(ReefName=='Lizard Island_SL1','Mid-shelf',
                      ifelse(ReefName=='MacGillvray_SL1','Mid-shelf',
                             ifelse(ReefName=='Martin_SL1','Inner shelf','Outer shelf')))))

head(iw.data.ts.filtered)


p.WTemp <- ggplot(iw.data.ts.filtered,aes(x=date,y=IW_Max, linetype = Slope_location, colour = Reef)) + 
  geom_line(linewidth = 0.8) + 
  scale_color_manual(values=c("purple","#004488", "#994455", "#117733")) +
  #scale_y_continuous(name="In Water Temperature (AIMS Logger Daily Maximum (\u00b0C))", limits = c(26, 31.5), breaks = seq(26,31.5,0.5)) + 
  geom_hline(aes(yintercept=29.16), linewidth = 0.5,colour='black') +
  geom_hline(aes(yintercept=30.16), linewidth = 0.5,colour='red') +
  scale_y_continuous(name="Water Temperature (Nightly Maximum (\u00b0C))", limits = c(26, 31.5), breaks = seq(26,31.5,1)) + 
  scale_x_date('',date_breaks = 'month', date_labels = "%b %Y", label="Date (Month-Year)") +
  scale_linetype_manual('Location',values=c(1,2),breaks=c('FLAT','SLOPE'),labels=c('Flat','Slope'))+
  facet_wrap(~Shelf,ncol=3)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size=11),
        axis.title.y = element_text(size=12),
        strip.text = element_text(size=12))

p.WTemp


p.SST <- ggplot(iw.data.ts.filtered,aes(x=date,y=SST_NOAA, linetype = Slope_location, colour = Reef)) + 
  geom_line(linewidth = 0.8) + 
  scale_color_manual(values=c("purple","#004488", "#994455", "#117733")) +
  #scale_y_continuous(name="In Water Temperature (AIMS Logger Daily Maximum (\u00b0C))", limits = c(26, 31.5), breaks = seq(26,31.5,0.5)) + 
  geom_hline(aes(yintercept=28.6), linewidth = 0.5,colour='black') +
  geom_hline(aes(yintercept=29.6), linewidth = 0.5,colour='red') +
  scale_y_continuous(name="Sea Surface Temperature (\u00b0C)", limits = c(26, 31.5), breaks = seq(26,31.5,1)) + 
  scale_x_date('',date_breaks = 'month', date_labels = "%b %Y", label="Date (Month-Year)") +
  scale_linetype_manual('Location',values=c(1,2),breaks=c('FLAT','SLOPE'),labels=c('Flat','Slope'))+
  facet_wrap(~Shelf,ncol=3)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size=11),
        axis.title.y = element_text(size=12),
        strip.text = element_text(size=12))

p.SST

#####  DHW plots

#plot in-water DHW Time Series
p.iw.DHW <- ggplot(iw.data.ts.filtered,aes(x=date,y=IW_DHW, linetype = Slope_location, colour = Reef)) + 
  geom_line(linewidth = 0.8) + 
  scale_color_manual(values=c("purple","#004488", "#994455", "#117733")) +
  #scale_y_continuous(name="In Water Temperature (AIMS Logger Daily Maximum (\u00b0C))", limits = c(26, 31.5), breaks = seq(26,31.5,0.5)) + 
  geom_hline(aes(yintercept=4), linewidth = 0.5,colour='black') +
  geom_hline(aes(yintercept=8), linewidth = 0.5,colour='red') +
  scale_y_continuous(name="Degree Heating Weeks (\u00b0C-weeks)", limits = c(0, 12), breaks = seq(0,12,1)) + 
  scale_x_date('',date_breaks = 'month', date_labels = "%b %Y", label="Date (Month-Year)") +
  scale_linetype_manual('Location',values=c(1,2),breaks=c('FLAT','SLOPE'),labels=c('Flat','Slope'))+
  facet_wrap(~Shelf,ncol=3)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45,hjust=1,size=11),
        axis.text.y = element_text(size=11),
        axis.title.y = element_text(size=12),
        strip.text = element_text(size=12))

p.iw.DHW

#plot NOAA DHW Time Series

p.noaa.DHW <- ggplot(iw.data.ts.filtered,aes(x=date,y=DHW_NOAA, linetype = Slope_location, colour = Reef)) + 
  geom_line(linewidth = 0.8) + 
  scale_color_manual(values=c("purple","#004488", "#994455", "#117733")) +
  #scale_y_continuous(name="In Water Temperature (AIMS Logger Daily Maximum (\u00b0C))", limits = c(26, 31.5), breaks = seq(26,31.5,0.5)) + 
  geom_hline(aes(yintercept=4), linewidth = 0.5,colour='black') +
  geom_hline(aes(yintercept=8), linewidth = 0.5,colour='red') +
  scale_y_continuous(name="Degree Heating Weeks (\u00b0C-weeks)", limits = c(0, 12), breaks = seq(0,12,1)) + 
  scale_x_date('',date_breaks = 'month', date_labels = "%b %Y", label="Date (Month-Year)") +
  scale_linetype_manual('Location',values=c(1,2),breaks=c('FLAT','SLOPE'),labels=c('Flat','Slope'))+
  facet_wrap(~Shelf,ncol=3)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45,hjust=1,size=11),
        axis.text.y = element_text(size=11),
        axis.title.y = element_text(size=12),
        strip.text = element_text(size=12))

p.noaa.DHW

pl.temp='AABB
         CCDD'

temp.plots <- p.WTemp+p.SST+p.noaa.DHW+p.iw.DHW+
  plot_annotation(tag_level = 'a')+
  plot_layout(design=pl.temp,guides = 'collect') &
  theme(legend.position='bottom')#,


temp.plots

ggsave(temp.plots,file='temp.plots.png',height=9,width=12)

########################################################################################
### Salinity from eReefs ----------------------------------------------------


shallow_salinity <- read.csv(file='raw data/GBR4_H4p0_reefsID_dailymean_2023-12_2024-04.csv',strip.white=TRUE)
head(shallow_salinity)


shallow_salinity <- shallow_salinity |> 
  dplyr::select(c(date,X14045S_salt:X15030S_temp))

names(shallow_salinity)

ggplot(shallow_salinity,aes(x=date,y=Salinity))+
  geom_point()+
  theme(axis.text.x = element_text(angle=90,size=7))

ggplot(shallow_salinity,aes(x=date,y=X14135S_salt))+
  geom_point()+
  theme(axis.text.x = element_text(angle=90,size=7))

ggplot(shallow_salinity,aes(x=date,y=X15012S_salt))+
  geom_point()+
  theme(axis.text.x = element_text(angle=90,size=7))
