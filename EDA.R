library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(gridExtra)

load("county_data.RData")

create_plot = function(input, name){
    if(is.factor(input)){
        p = data_frame(
            price = county_data$price,
            input = input
        ) %>% 
            ggplot(aes(x = input, y = price)) + 
            geom_boxplot() + 
            labs(x = name)
    }else{
        p = data_frame(
            price = county_data$price,
            input = input
        ) %>% 
            ggplot(aes(x = input, y = price)) + 
            geom_point() + 
            labs(x = name)
    }
    return(p)
}

subplots = function(df, cols){
    eda_plots = map(cols, function(i){create_plot(df[, i], names(df)[i])})
    grid.arrange(grobs = eda_plots, nrow = 3, ncol = 3)
}

df = county_data %>% 
    select(- County, - State, - price) %>%
    mutate(hilton_count = as.factor(hilton_count))
### change df's certain column into factor before inputting into `subplots` 
### if boxplot is preferred for this column's predictor

walk(1:(ncol(df) %/% 9 + 1), function(group_num){
    col_index = (group_num * 9  - 8):min(group_num * 9, ncol(df))
    subplots(df, col_index)
})