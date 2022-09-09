library(dplyr)
library(stringr)
library(readr)
library(magrittr)

a=list.files() %>% str_detect("bootstrap_")

bootread_list=list.files()[a]

bootread=read_csv(bootread_list,id="arc")

boot_result=bootread %>% group_by(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO) %>% 
  summarise(SE_1=mean(SE_1),SE_HT=mean(SE_HT),counts=n())

sample_sizes=sample1 %>% group_by(ESTU_COD_RESIDE_DEPTO,ESTU_COD_RESIDE_MCPIO) %>% summarise(tot=n())

boot_result %<>% left_join(sample_sizes) 

library(ggplot2)

ggplot(data=boot_result,aes(x=log(tot),y=sqrt(SE_1)))+geom_point() + 
  labs(x="Logarithm or Area Size",y="Bootstrap Model Based Mean Squared Error") 

