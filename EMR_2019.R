library(rvest)
library(tidyverse)

# Dados ---------------------------------------------------
u <- "http://www.emr2019.com.br/evento/escoladeregressao2019/programacao/gradeatividades/4"
r <- httr::GET(u)
u_base <- "http://www.emr2019.com.br"

links <- r %>%
  xml2::read_html() %>%
  xml2::xml_find_all("//a[@data-toggle='modal']") %>%
  xml2::xml_attr("href") %>%
  paste0(u_base, .)

nomes <- purrr::map(links, ~{ 
  .x %>% 
    xml2::read_html() %>% 
    xml2::xml_find_all("//li//div//p") %>% 
    rvest::html_text() %>% 
    str_squish()
})


res <- nomes %>% 
  keep(~length(.x) > 0) %>% 
  unlist() %>% 
  str_to_title() %>% 
  str_split(pattern = "\\(Confirmado\\)") %>% 
  purrr::map(as.data.frame(t)) %>% 
  map_dfr(data.frame) %>% 
  setNames(c("nome", "classe"))  %>% 
  mutate(classe = ifelse(str_detect(classe, "Coord"), 
                         "Coordenador(a)", classe)) %>% 
  mutate(sexo = c("M", "M", "F", "M", "M", "F", "M", "M",
                  "F", "M", "F", "M", "F", "M", "M", "M", 
                  "M", "F", "M", "M"))



write.table(res, "data/EMR_2019.txt")

# Gráficos --------------------------------------------
res %>% 
  group_by(classe, sexo) %>% 
  count() %>% 
  group_by(classe) %>% 
  mutate(porcentagem = n/sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(y = porcentagem, x = sexo)) +
  geom_histogram(stat = "identity", aes(fill = sexo)) +
  scale_fill_manual(values = c('#f5c04b', 'rosybrown')) +
  facet_wrap(~classe) +
  labs(title = "Distribuição de sexos na atual 
programação do EMR 2019", 
       caption = "Fonte: http://www.emr2019.com.br") +
  theme_bw()

ggsave("img/emr_2019.pdf", 
       plot = last_plot(), 
       width = 5, height = 3, 
       units = "in", dpi = 300)



