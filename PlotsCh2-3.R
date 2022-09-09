## Encoding: Iso 8859-1

### Generate plots  2.1, 2.2, 2.3 and 3.1
library("PracTools")

library(readr)
library(sae)
library(sampling)
library(magrittr)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(furrr)
library(purrr)
library(data.table)

SB11_20202=read_delim("./SB11_20202/SB11_20202.txt",delim="¬")
SB11_20212=read_delim("./SB11_20212/SB11_20212.txt",delim="¬")

SB11_20212 %<>% filter(!is.na(ESTU_GENERO)) 
SB11_20212 %<>% filter(!is.na(PUNT_GLOBAL),!is.na(ESTU_INSE_INDIVIDUAL))
SB11_20212$ESTU_COD_RESIDE_DEPTO %<>% as.factor()
SB11_20212$ESTU_COD_RESIDE_MCPIO %<>% as.factor()
SB11_20212$DESEMP_INGLES2=SB11_20212$DESEMP_INGLES=="B+"

var_text_name="PUNT_GLOBAL"

SB11_20212$FAMI_ESTRATOVIVIENDA %<>% replace_na("NA")
SB11_20212$FAMI_NUMLIBROS %<>% replace_na("NA")
SB11_20212$FAMI_CUARTOSHOGAR %<>% replace_na("NA")
SB11_20212$ESTU_TIENEETNIA %<>% replace_na("NA")
SB11_20212$ESTU_GENERO %<>% replace_na("NA")


# SB11_20212 %<>% group_by(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO) %>% 
#   mutate(ESTU_INSE_INDIVIDUAL=replace_na(median(ESTU_INSE_INDIVIDUAL,na.rm=T)))


SB11_20212$FAMI_ESTRATOVIVIENDA %<>% factor(levels=
                                              c("NA","Sin Estrato","Estrato 1","Estrato 2","Estrato 3","Estrato 4",
                                                "Estrato 5","Estrato 6"),ordered = T)

SB11_20212$FAMI_EDUCACIONMADRE %<>% replace_na("NS/NR")
SB11_20212$FAMI_EDUCACIONMADRE %>% unique

SB11_20212$FAMI_EDUCACIONMADRE = case_when(
  SB11_20212$FAMI_EDUCACIONMADRE %in% c("NS/NR","No sabe","No Aplica")~"NS/NR",
  SB11_20212$FAMI_EDUCACIONMADRE %in% c("Primaria incompleta","Ninguno")~"Primaria incompleta",
  SB11_20212$FAMI_EDUCACIONMADRE %in% c("Primaria completa","Secundaria (Bachillerato) incompleta")~"Primaria completa",
  SB11_20212$FAMI_EDUCACIONMADRE %in% c("Secundaria (Bachillerato) completa","Técnica o tecnológica incompleta","Educación profesional incompleta")~"Secundaria (Bachillerato) completa",
  TRUE~SB11_20212$FAMI_EDUCACIONMADRE
) 

SB11_20212$FAMI_EDUCACIONMADRE %<>% factor(
  levels = c("NS/NR","Primaria incompleta","Primaria completa","Secundaria (Bachillerato) completa",
             "Técnica o tecnológica completa","Educación profesional completa","Postgrado"),ordered = T )

SB11_20212$FAMI_EDUCACIONPADRE %<>% replace_na("NS/NR")
SB11_20212$FAMI_EDUCACIONPADRE %>% unique

SB11_20212$FAMI_EDUCACIONPADRE = case_when(
  SB11_20212$FAMI_EDUCACIONPADRE %in% c("NS/NR","No sabe","No Aplica")~"NS/NR",
  SB11_20212$FAMI_EDUCACIONPADRE %in% c("Primaria incompleta","Ninguno")~"Primaria incompleta",
  SB11_20212$FAMI_EDUCACIONPADRE %in% c("Primaria completa","Secundaria (Bachillerato) incompleta")~"Primaria completa",
  SB11_20212$FAMI_EDUCACIONPADRE %in% c("Secundaria (Bachillerato) completa","Técnica o tecnológica incompleta","Educación profesional incompleta")~"Secundaria (Bachillerato) completa",
  TRUE~SB11_20212$FAMI_EDUCACIONPADRE
) 

SB11_20212$FAMI_EDUCACIONPADRE %<>% factor(
  levels = c("NS/NR","Primaria incompleta","Primaria completa","Secundaria (Bachillerato) completa",
             "Técnica o tecnológica completa","Educación profesional completa","Postgrado"),ordered = T )



SB11_20212$FAMI_TIENEINTERNET %<>% replace_na("NA") 
SB11_20212 %<>% ungroup

SB11_20212 %<>% filter(!is.na(ESTU_COD_RESIDE_MCPIO))  



Determine_sample_sizes=function(var_text_name){
  ### Variance last year of valid  scores 
  SB11_20202_sumar_mpio=SB11_20202 %>%filter(!is.na(.data[[var_text_name]])) %>% 
    group_by(ESTU_COD_RESIDE_MCPIO,ESTU_COD_RESIDE_DEPTO) %>% 
    summarise(Sh=sd(.data[[ var_text_name ]],na.rm=T),tot=n()) 
  #Remove non localizable students
  SB11_20202_sumar_mpio %<>% 
    filter(!is.na(ESTU_COD_RESIDE_MCPIO),!is.na(Sh))
  # On new frame count valid studients in each municipio
  SB11_20212_sumar_mpio=SB11_20212 %>% 
    filter(!is.na(.data[[var_text_name]])) %>% 
    group_by(ESTU_COD_RESIDE_MCPIO,ESTU_COD_RESIDE_DEPTO) %>% 
    summarise(Nh=n()) %>% mutate(ch=1) 
  #Join old and new
  SB11_20212_sumar_mpio=left_join(SB11_20212_sumar_mpio,SB11_20202_sumar_mpio)
  ### Sh is variance of data, if level is in group but no var is available be conservative and use the max
  SB11_20212_sumar_mpio %<>% ungroup %>% 
    mutate(Sh=ifelse(is.na(Sh),max(Sh,na.rm = T),Sh)) 
  ### Number of unit  Neyman, variance-constrained allocations in a stratified simple random sample
  ### Fized V0 0,06
  alloc=strAlloc(Nh=SB11_20212_sumar_mpio$Nh,Sh=SB11_20212_sumar_mpio$Sh,
                 alloc="totvar",
                 V0 = 0.05,ch = SB11_20212_sumar_mpio$ch)
  ## Fizes, Round up sample size
  SB11_20212_sumar_mpio$size=alloc$nh %>% ceiling()
  # Fizex make sample size at least 5 in each municipio, but not larger than population size
  SB11_20212_sumar_mpio %<>% mutate(size=pmin(pmax(size,5),Nh))
  SB11_20212_sumar_mpio
  
}


Sample_d=Determine_sample_sizes(var_text_name)

SB11_20212 %<>%  arrange(ESTU_COD_RESIDE_MCPIO)

Create_random_sample=function(Sample_s,population_=SB11_20212){
  ## Only sample levels with known si<e
  
  s22 =population_%>% filter(!is.na(ESTU_COD_RESIDE_MCPIO),
                             population_$ESTU_COD_RESIDE_MCPIO %in% 
                               Sample_s$ESTU_COD_RESIDE_MCPIO) 
  
  #s22=s22 %>% arrange(ESTU_COD_RESIDE_MCPIO)  
  Sample_s %<>% arrange(ESTU_COD_RESIDE_MCPIO) 
  Sample2=strata(data=s22,stratanames = "ESTU_COD_RESIDE_MCPIO",
                 size=Sample_s$size,method="srswor")
  s22$id=seq_along(s22$ESTU_COD_RESIDE_MCPIO)
  
  s222=left_join(Sample2 ,s22 %>% select(-ESTU_COD_RESIDE_MCPIO),by=c("ID_unit"="id"))
  s222
}


# make reproducible
set.seed(5734)
sample1=Create_random_sample(Sample_d)




Totpop=SB11_20212 %>%  group_by(ESTU_COD_RESIDE_MCPIO) %>%
  summarise(totPop=n()) 


error=sample1 %>%  group_by(ESTU_COD_RESIDE_MCPIO) %>%
  summarise(tot=n(),mean=mean(PUNT_GLOBAL),err=sd(PUNT_GLOBAL)/sqrt(tot)) 

error2=left_join(error,Totpop)

error2 %<>% mutate(err=err*sqrt((totPop-tot)/(totPop-1)))


library(ggplot2)

ggplot(data=error,aes(x=err))+geom_histogram()  


ggplot(data=error,aes(y=err,x=log(tot)))+geom_point() + 
  labs(x="Logarithm or Area Size",y="Design Based Sample Error") 


error %<>% mutate(interv=cut(tot,breaks=c(1,10,100,10000),right=T))

levels(error$interv)=c("Between 1 and 10","Between 11 and 100","More than 100")  

e2=error %>% select(ESTU_COD_RESIDE_MCPIO,interv)

sample1=left_join(sample1,e2)

error %>% group_by(interv) %>% summarise(tot=n())

ggplot(data=error,aes(x=interv,fill=interv))+geom_bar() +
  theme(legend.position = "none")+labs(x="",y="Quantity")+
  geom_text(stat="count",aes(x=interv, y=..count../2,label=..count..))

bars=sample1 %>% group_by(interv,COLE_AREA_UBICACION) %>% summarise(tot=n()) %>% 
  ungroup() %>% group_by(interv) %>% mutate(percen=tot/sum(tot))

bars$COLE_AREA_UBICACION %<>% str_replace_all("URBANO", "URBAN") 

bars$text=(bars$percen*100) %>% str_trunc(5,ellipsis = "") %>% paste0("%")

ggplot(data=bars,aes(x=interv,fill=COLE_AREA_UBICACION,y=percen))+
  labs(x="Area Size",y="Percentage",fill="School")+geom_col()+
  scale_y_continuous(labels = scales::percent)+geom_text(aes(label=text),
    position=position_stack(vjust=0.5))

ggplot(data=sample1,aes(color=interv,x=ESTU_INSE_INDIVIDUAL))+
  labs(x="INSE",y="Density",color="Area Size")+geom_density(bw=0.8)

