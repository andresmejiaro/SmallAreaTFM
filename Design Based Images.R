library("PracTools")
library(readr)
library(sae)
library(sampling)
library(magrittr)
library(stringr)
library(tidyr)

library(ggplot2)
library(furrr)
library(purrr)
library(data.table)
library(dplyr)

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


a=list.files() %>% str_detect("simul_todo junto version con mas filtros")

bootread_list=list.files()[a]

bootread=read_csv(bootread_list,id="arc")

boot_result=bootread%>% 
  select(ESTU_COD_RESIDE_DEPTO=ESTU_COD_RESIDE_DEPTO.x,
         ESTU_COD_RESIDE_MCPIO,MB_Dept_Mpio,MB_Mpio,MB_Dept,HT_mean) %>% 
  pivot_longer(cols = MB_Dept_Mpio:HT_mean) 


real=SB11_20212 %>% group_by(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO) %>% 
  summarise(Real=mean(PUNT_GLOBAL))


sample_sizes=sample1 %>% group_by(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO) %>% summarise(tot=n())

boot_result %<>% left_join(sample_sizes) %>%  left_join(real)

boot_result %<>% ungroup() %>% group_by(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO,name) %>%
  summarise(estimator=mean(value),Bias=mean(value)-mean(Real),MSE=mean((value-Real)^2),
              tot=mean(tot))

library(ggplot2)

boot_result$name %<>% str_replace_all("MB_Dept_Mpio","Model Based Department\nand Municipality") 
boot_result$name %<>% str_replace_all("MB_Dept","Model Based Department")
boot_result$name %<>% str_replace_all("MB_Mpio","Model Based Municipality")
boot_result$name %<>% str_replace_all("HT_mean","Direct Estimator") 


ggplot(data=boot_result,aes(x=log(tot),y=abs(Bias),color=name))+geom_point()+
  facet_wrap(~name)+theme(legend.position = "none")+
  labs(x="Logarithm of Area Sample Size",y="Estimated Bias (Absolute Value)")


ggplot(data=boot_result,aes(x=Bias,fill=name))+geom_boxplot()+labs(fill=NULL)+
  theme(legend.position = c(0.75,0.2),legend.key.size = unit(0.4,"cm"),
        axis.text.y = element_blank(),axis.ticks.y=element_blank())


ggplot(data=boot_result,aes(x=MSE,fill=name))+geom_boxplot()+labs(fill=NULL,x="Mean Squared Error")+
  theme(legend.position = c(0.75,0.2),legend.key.size = unit(0.4,"cm"),
        axis.text.y = element_blank(),axis.ticks.y=element_blank())

ggplot(data=boot_result,aes(x=log(tot),y=MSE,color=name))+geom_point()+facet_wrap(~name)+
  facet_wrap(~name)+theme(legend.position = "none")+
  labs(x="Logarithm of Area Sample Size",y="Mean Squared Error")

