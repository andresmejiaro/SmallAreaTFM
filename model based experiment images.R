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

a=list.files() %>% str_detect("model_based_exp")

bootread_list=list.files()[a]

bootread=read_csv(bootread_list,id="arc")

bootread %<>% rename(SE_dpto_mpio=SE_1)

bootread %<>% mutate(SE_dpto=(MeanPred_dpto-Real_synthetic)^2,SE_mpio=(MeanPred_mpio-Real_synthetic)^2)

boot_result=bootread %>% group_by(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO) %>% 
  summarise(SE_dpto_mpio=mean(SE_dpto_mpio),
            SE_dpto=mean(SE_dpto),
            SE_mpio=mean(SE_mpio),
            SE_HT=mean(SE_HT),counts=n(),
            bias_dpto_mpio=mean(MeanPred-Real_synthetic),
            bias_dpto=mean(MeanPred_dpto-Real_synthetic),
            bias_mpio=mean(MeanPred_mpio-Real_synthetic),
            bias_HT=mean(HT-Real_synthetic))


boot_result %<>% select(-counts) %>% pivot_longer(cols=SE_dpto_mpio:bias_HT,
                                                 names_to = "Names") %>% 
  mutate(Type=str_split_fixed(Names,"_",2)[,1],model=str_split_fixed(Names,"_",2)[,2]) %>% 
  select(-Names) %>% pivot_wider(names_from = Type,values_from = value)


library(ggplot2)



boot_result$model %<>% str_replace_all("dpto_mpio","Model Based Department\nand Municipality") 
boot_result$model %<>% str_replace_all("dpto","Model Based Department") 
boot_result$model %<>% str_replace_all("mpio","Model Based Municipality") 
boot_result$model %<>% str_replace_all("HT","Direct Estimator") 


ggplot(data=boot_result,aes(x=SE,fill=model,y=model))+geom_boxplot()+
  labs(fill=NULL,x="Mean Squared Error",y=NULL)+
  theme(legend.position = "none",legend.key.size = unit(0.4,"cm"),
        axis.ticks.y=element_blank())




ggplot(data=boot_result,aes(x=SE,fill=model))+geom_boxplot()


ggplot(data=boot_result %>% filter(model!="Direct Estimator"),aes(x=SE,fill=model))+geom_boxplot()+
labs(fill=NULL,x="Mean Squared Error",y=NULL)+
  theme(legend.position = c(0.8,0.8),legend.key.size = unit(0.4,"cm"),
        axis.text = element_blank(),axis.ticks.y=element_blank())


ggplot(data=boot_result,aes(x=bias,fill=model))+geom_boxplot()+
  labs(fill=NULL,x="Bias",y=NULL)+
  theme(legend.position = c(0.8,0.8),legend.key.size = unit(0.4,"cm"),
        axis.text = element_blank(),axis.ticks.y=element_blank())


ggplot(data=boot_result %>% filter(model!="Direct Estimator"),
       aes(x=bias,fill=model))+geom_boxplot()+
  labs(fill=NULL,x="Bias",y=NULL)+
  theme(legend.position = c(0.8,0.8),legend.key.size = unit(0.4,"cm"),
        axis.text = element_blank(),axis.ticks.y=element_blank())

### get sample sizes from generating file


ssize=sample1 %>% group_by(ESTU_COD_RESIDE_MCPIO) %>% summarise(samplesize=n())


boot_result=left_join(boot_result,ssize)


ggplot(data=boot_result%>% filter(model=="Direct Estimator"),aes(y=abs(bias) ,x=log(samplesize),color=model))+geom_point()+
  facet_wrap(~model)+
  labs(fill=NULL,x="Logarithm of Area Sample Size",y="Bias (Absolute Value)")+
  theme(legend.position = "none",legend.key.size = unit(0.4,"cm"),)
  

ggplot(data=boot_result %>% filter(model!="Direct Estimator"),aes(y=abs(bias),x=log(samplesize),color=model))+geom_point()+
  facet_wrap(~model)+labs(fill=NULL,x="Logarithm of Area Sample Size",y="Bias (Absolute Value)")+
  theme(legend.position = "none",legend.key.size = unit(0.4,"cm"),)



ggplot(data=boot_result%>% filter(model=="Direct Estimator"),aes(y=SE ,x=log(samplesize),color=model))+geom_point()+
  facet_wrap(~model)+labs(fill=NULL,x="Logarithm of Area Sample Size",y="Mean Squared Error")+
  theme(legend.position = "none",legend.key.size = unit(0.4,"cm"),)


ggplot(data=boot_result %>% filter(model!="Direct Estimator"),aes(y=SE,x=log(samplesize),color=model))+geom_point()+
  facet_wrap(~model)+labs(fill=NULL,x="Logarithm of Area Sample Size",y="Mean Squared Error")+
  theme(legend.position = "none",legend.key.size = unit(0.4,"cm"),)


