
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Joice-Lindsay

### Carregando Pacotes

``` r
library(tidyverse)
```

### Carregando as bases de dados individuas

``` r
casa <- readr::read_rds("data/casa.rds")
citomeria <- readr::read_rds("data/citomeria.rds")
morf_mot_vig <- readr::read_rds("data/morf_mot_vig.rds")
```

### Juntando as bases de dados

``` r
my_data_set <- left_join(citomeria,casa %>% 
                           select(-codigo,-nascim,-datacoleta,-salto),
                         by=c("grupo","touro","coleta"))
my_data_set <- left_join(my_data_set, morf_mot_vig %>% 
                           select(-codigo,-nascim,-datacoleta,-salto),
                         by=c("grupo","touro","coleta"))
```

### Gráficos ao longo do tempo

``` r
my_variables <- names(my_data_set[8:length(my_data_set)])
for(i in seq_along(my_variables)){
  my_vari <- my_variables[i]
  da <- my_data_set %>% 
    select(grupo, coleta, touro, my_vari) %>% 
    rename("Y" = my_vari)

  my_plot <- da %>%
    group_by(coleta,grupo) %>% 
    summarise(y = mean(Y,na.rm=TRUE),
              n=n(),
              stderr = sd(Y,na.rm=TRUE)/sqrt(n)) %>% 
    ggplot(aes(x=coleta, y=y, fill=grupo)) +
    geom_col(position = "dodge",color="black") +
    theme_bw() +
    labs(x="Coleta", y=my_vari) +
    scale_fill_manual(values = c("lightgray","coral") ) +
    geom_errorbar(aes(ymin=y-stderr, ymax=y+stderr), width=.2,
                 position=position_dodge(.9))
  print(my_plot)
}
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-6.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-7.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-8.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-9.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-10.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-11.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-12.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-13.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-14.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-15.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-16.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-17.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-18.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-19.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-20.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-21.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-22.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-23.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-24.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-25.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-26.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-27.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-28.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-29.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-30.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-31.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-32.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-33.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-34.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-35.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-36.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-37.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-38.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-39.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-40.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-41.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-42.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-43.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-44.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-45.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-46.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-47.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-48.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-49.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-50.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-51.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-52.png)<!-- -->

### Correlação CASA

``` r
my_data_set %>% 
  filter(grupo=="C") %>% 
  drop_na() %>% 
  select(concdose:alh, -peso.x, -peso.y) %>% 
  cor() %>% 
  corrplot::corrplot()
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
my_data_set %>% 
  filter(grupo=="C") %>% 
  drop_na() %>% 
  select(concdose:alh, -peso.x, -peso.y) %>% 
  cor() %>% 
  corrplot::corrplot()
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Correlação citomeria

``` r
my_data_set %>% 
  filter(grupo=="C") %>% 
  drop_na() %>% 
  select(fitc_a_median_t0:o2_pi_a_m , -peso.x, -peso.y) %>% 
  select(ends_with("m")) %>% 
  cor() %>% 
  corrplot::corrplot()
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
my_data_set %>% 
  filter(grupo=="T") %>% 
  drop_na() %>% 
  select(fitc_a_median_t0:o2_pi_a_m , -peso.x, -peso.y) %>% 
  select(ends_with("m")) %>% 
  cor() %>% 
  corrplot::corrplot()
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Correlação morfologia motilidadade e vigor

``` r
my_data_set %>% 
  filter(grupo=="C") %>% 
  drop_na() %>% 
  select(conc:totais , -peso.x, -peso.y) %>% 
  cor() %>% 
  corrplot::corrplot()
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
my_data_set %>% 
  filter(grupo=="T") %>% 
  drop_na() %>% 
  select(conc:totais , -peso.x, -peso.y) %>% 
  cor() %>% 
  corrplot::corrplot()
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
touros <- my_data_set %>% pull(touro) %>% unique()
for( i in 1:length(touros)){
  
  dsa <- my_data_set %>%  filter(touro == touros[i]) %>% 
    select(fitc_a_median_t0:totais) 
  
  if(i == 1 ) {df <- apply(dsa, 2, sum,na.rm=TRUE)
  }else{
    dfa <- apply(dsa, 2, sum,na.rm=TRUE)
    df <- rbind(df,dfa)
  }
}
row.names(df) <- touros
```

### Análise de Agrupamento Hierárquico CASA

``` r
    da_pad<-vegan::decostand( df %>% as.data.frame() %>% 
                               select(concdose:alh), 
                      method = "standardize",
                      na.rm=TRUE)
    da_pad_euc<-vegan::vegdist(da_pad,"euclidean") 
    da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
    plot(da_pad_euc_ward, 
         ylab="Distância Euclidiana",
         xlab="Acessos", hang=-1,
         col="blue", las=1,
         cex=.6,lwd=1.5);box()
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
    grupo<-cutree(da_pad_euc_ward,3)
```

### Análise de Agrupamento Hierárquico CITOMERIA

``` r
    da_pad<-vegan::decostand( df %>% as.data.frame() %>% 
                               select(fitc_a_median_t0:o2_pi_a_m) %>% 
                                select(ends_with("m")), 
                      method = "standardize",
                      na.rm=TRUE)
    da_pad_euc<-vegan::vegdist(da_pad,"euclidean") 
    da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
    plot(da_pad_euc_ward, 
         ylab="Distância Euclidiana",
         xlab="Acessos", hang=-1,
         col="red", las=1,
         cex=.6,lwd=1.5);box()
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
    grupo<-cutree(da_pad_euc_ward,3)
```

### Análise de Agrupamento Hierárquico MORFOLOGIA MOTILIDADE E VIGOR

``` r
    da_pad<-vegan::decostand( df %>% as.data.frame() %>% 
                               select(conc:totais), 
                      method = "standardize",
                      na.rm=TRUE)
    da_pad_euc<-vegan::vegdist(da_pad,"euclidean") 
    da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
    plot(da_pad_euc_ward, 
         ylab="Distância Euclidiana",
         xlab="Acessos", hang=-1,
         col="purple", las=1,
         cex=.6,lwd=1.5);box()
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
    grupo<-cutree(da_pad_euc_ward,3)
```
