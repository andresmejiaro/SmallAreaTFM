## Encoding: Iso 8859-1
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

#otras_var=""




#otras_var=c(".")


create_model=function(sample,var_text_name,otras_var){
  otras_var=paste(otras_var,collapse = "+")
  if(str_length(otras_var)==0){
    formula_e=paste0(var_text_name,"~"," (1|ESTU_COD_RESIDE_DEPTO/ESTU_COD_RESIDE_MCPIO)")
  }else{
    formula_e=paste0(var_text_name,"~",otras_var,"+ (1|ESTU_COD_RESIDE_DEPTO/ESTU_COD_RESIDE_MCPIO)")
  }
  
  
  model1=lmer(formula_e,data=sample)
  
}


# model1=create_model(sample = sample1,otras_var = otras_var,var_text_name = "PUNT_INGLES")
# 
# 
# plot(residuals(model1),predict(model1))

create_estimates_level=function(mmodel,r_sample,var_text_name,orsample,
                                population_=SB11_20212){
  Datanew=population_
  Datanew %<>%  
    select(mmodel@frame %>% names,ESTU_CONSECUTIVO) 
  
  Datanew$prediction=predict(mmodel,newdata=Datanew ,allow.new.levels=TRUE)
  Datanew$in_sample=Datanew$ESTU_CONSECUTIVO %in% r_sample$ESTU_CONSECUTIVO
  Datanew$final_pred=Datanew[,var_text_name][[1]]
  Datanew$final_pred=if_else(!Datanew$in_sample,Datanew$prediction,Datanew$final_pred)
  
  s1=Datanew %>% group_by(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO) %>% 
    summarise(MeanPred=mean(final_pred,na.rm=T),N=n(),N_sample=sum(in_sample))
  
  
  HT=r_sample %>% group_by(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO) %>% 
    summarise(HT=mean(.data[[var_text_name]],na.rm=T))
  
  
  Real_mean=population_ %>% select(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO,
                                   all_of(var_text_name)) %>% 
    group_by(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO) %>% 
    summarise(Real=mean(.data[[var_text_name]],na.rm=T))
  
  
  
  s1=left_join(s1,Real_mean,by=c("ESTU_COD_RESIDE_DEPTO","ESTU_COD_RESIDE_MCPIO"))
  s1=left_join(s1,HT,by=c("ESTU_COD_RESIDE_DEPTO","ESTU_COD_RESIDE_MCPIO"))
}

#debug(create_estimates_level)
otras_var=c("FAMI_EDUCACIONMADRE",
            "FAMI_EDUCACIONPADRE",
            "COLE_NATURALEZA",
            "FAMI_ESTRATOVIVIENDA","ESTU_GENERO",
            "COLE_AREA_UBICACION","FAMI_NUMLIBROS",
            "ESTU_TIENEETNIA","FAMI_CUARTOSHOGAR",
            "FAMI_TIENEINTERNET",
            "COLE_CALENDARIO",
            "COLE_JORNADA")#,
#            "ESTU_INSE_INDIVIDUAL")

model1=create_model(sample = sample1,otras_var = otras_var,var_text_name = var_text_name)
plot(model1)
summary(model1)
# 
error=predict(model1,newdata=sample1)-sample1$PUNT_GLOBAL
# 
# qplot(x=error,y=sample1$ESTU_INSE_INDIVIDUAL )+theme(legend.position = "none")

create_new_syntetic_population=function(model_=model1,
                                        population_=SB11_20212){
  
  
  
  
  population_$y_syntethic=predict(model_,
                                  re.form=~0,newdata=population_)
  
  
  
  vcov_df=VarCorr(model_)
  model_@optinfo
  
  MCIPIO_=population_ %>% select(ESTU_COD_RESIDE_MCPIO) %>% distinct()
  DEPTO_=population_ %>% select(ESTU_COD_RESIDE_DEPTO) %>% distinct()
  
  MCIPIO_$Random_e_Mcipio=rnorm(dim(MCIPIO_)[1],sd=sqrt(vcov_df$`ESTU_COD_RESIDE_MCPIO:ESTU_COD_RESIDE_DEPTO`[1,1]))
  DEPTO_$Random_e_Depto=rnorm(dim(DEPTO_)[1],sd=sqrt(vcov_df$ESTU_COD_RESIDE_DEPTO[1,1]))
  population_$Random_e=rnorm(dim(population_)[1],sd=sqrt(attr(vcov_df,"sc")))
  
  
  
  population_ %<>% left_join(MCIPIO_,by=c("ESTU_COD_RESIDE_MCPIO")) 
  population_ %<>% left_join(DEPTO_,by=c("ESTU_COD_RESIDE_DEPTO"))
  
  population_ %<>% mutate(y_syntethic=y_syntethic+Random_e_Depto+Random_e_Mcipio+
                            Random_e) 
  population_  
}


HT=sample1 %>% group_by(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO) %>% 
  summarise(HT=mean(PUNT_GLOBAL,na.rm=T),sdHT=sd(PUNT_GLOBAL))

sample_s_=sample1

one_boot2=function(model_=model1,population_=SB11_20212,sample_=sample1,
                   otras_var=otras_var,
                   rec=1){
  
  
  population_s_=create_new_syntetic_population(model_ = model_,population_ = population_)
  sample_s_=semi_join(population_s_,sample_,by=c("ESTU_CONSECUTIVO"))
  model_s_=create_model(sample = sample_s_,
                        otras_var = otras_var,
                        var_text_name = "y_syntethic")
  
  
  sample_s_$Prob=sample_$Prob
  
  HT=sample_s_ %>% group_by(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO) %>% 
    summarise(HT=sum(y_syntethic/Prob))
  
  pop_count=population_s_ %>% 
    group_by(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO) %>% 
    summarise(N=n())
  
  HT %<>%  left_join(pop_count,by=c("ESTU_COD_RESIDE_DEPTO","ESTU_COD_RESIDE_MCPIO"))
  HT %<>% mutate(HT=HT/N)
  
  ab=create_estimates_level(mmodel = model_s_,
                            r_sample = sample_s_,
                            var_text_name = "y_syntethic",
                            orsample = sample_s_,
                            population_=population_s_)
  ab %<>% ungroup %>% select(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO,
                             MeanPred)
  ab$rec=rec
  ab %<>% left_join(HT,by=c("ESTU_COD_RESIDE_DEPTO","ESTU_COD_RESIDE_MCPIO")) 
  
  Real=population_s_ %>% group_by(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO) %>% 
    summarise(Real_synthetic=mean(y_syntethic,na.rm=T))
  ab %<>% left_join(Real,by=c("ESTU_COD_RESIDE_DEPTO","ESTU_COD_RESIDE_MCPIO"))
  ab
}




one_boot=function(Sample_s,otras_var=otras_var,var_text_name,rec=1,population_){
  sample1=Create_random_sample(Sample_s,population_)
  model1=create_model(sample = sample1,otras_var = otras_var,var_text_name = var_text_name)
  ab=create_estimates_level(mmodel = model1,
                            r_sample = sample1,
                            var_text_name = var_text_name,
                            orsample = sample1,population_ = population_)
  
  
  HT=sample1 %>% group_by(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO) %>% 
    summarise(HT_sum=sum(.data[[var_text_name]]/Prob),HT_mean=mean(.data[[var_text_name]]))
  
  ab %<>% ungroup %>% select(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO,MeanPred)
  ab$rec=rec
  ab
  left_join(ab,HT)
}





plan(multisession, workers = 12)

population2=SB11_20212 %>% select(all_of(model1@frame %>% names),ESTU_CONSECUTIVO)



rm(SB11_20212)
rm(SB11_20202)
gc()



#debug(one_boot2)
#options(dplyr.summarise.inform = FALSE)

for(i in 1:32){
  tic=Sys.time()
  bootstrap_=future_map_dfr(1:1000, ~one_boot2(
    model_ = model1,sample_ = sample1,
    otras_var = otras_var,rec = .x,
    population_ = population2),.options = furrr_options(seed=1234566+i,stdout = FALSE),.progress = T)
  
  toc=Sys.time()
  toc-tic
  ### 10000 toma 11 horas
  
  bootstrap_ %<>% mutate(SE_1=(MeanPred-Real_synthetic)^2,SE_HT=(HT-Real_synthetic)^2)
  
  results=bootstrap_ %>% group_by(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO) %>% 
    summarise(MRSE_1=sqrt(mean(SE_1)),MRSE_HT=sqrt(mean(SE_HT)))
  
  fwrite(bootstrap_,paste0("bootstrap_",i,".csv"))
  fwrite(results,paste0("results model based",i,".csv"))
  
  rm(bootstrap_)
  rm(results)
}
