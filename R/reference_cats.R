

cats_ref <- function(data){
  data %>%
    mutate_if(is.character, as.factor) %>%
    select_if(is.factor) %>%
    map_chr(~levels(.x)[1]) %>%
    enframe(name = 'variable', value = 'cat_ref')
}
