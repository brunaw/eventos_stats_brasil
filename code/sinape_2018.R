library(rvest)
library(tidyverse)

url <- "http://www.sinape2018.com.br/evento/23sinape/programacao/gradeatividades"

# Dados ---------------------------------------------------
r <- httr::GET(url)
u_base <- "http://www.sinape2018.com.br/"
res <- r %>%
  xml2::read_html() %>% 
  xml2::xml_find_all("//td//p") %>% 
  rvest::html_text() %>% 
  str_squish() %>% 
  str_split(pattern = ":") %>% 
  purrr::map(as.data.frame(t)) %>% 
  map_dfr(data.frame) %>% 
  setNames(c("classe", "nome")) %>% 
  mutate(sexo = c("M", "M", "M", "M", "M", "M", "M", "M",
                  "F", "M", "F", "M", "F", "M", "F", "F", 
                  "F", "M", "M", "M", "M", "F", "F", "F",
                  "M",  "F", "F", "F", "F", "F", "M", "M",
                  "M", "M", "M", "M", "M", "M", "M", "M",
                  "M", "F", "M", "M", "M", "M", "F", "F",
                  "M", "M", "M", "F", "F", "F", "F", "M",
                  "M", "M", "M", "M",  "M",  "M", "F", "M",
                  "M", "M", "F", "M")) %>% 
  mutate(classe = ifelse(str_detect(classe, "Coord"), 
                         "Coordenador(a)", classe)) %>% 
  mutate(classe = ifelse(str_detect(classe, "Minist"), 
                         "Palestrante", classe))


write.table(res, "data/sinape_2018.txt")

# Gráficos --------------------------------------------
res = read.table("data/sinape_2018.txt")

res %>% 
  group_by(classe, sexo) %>% 
  count() %>% 
  group_by(classe) %>% 
  mutate(porcentagem = n/sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(y = porcentagem, x = sexo)) +
  geom_histogram(stat = "identity", aes(fill = sexo)) +
  scale_y_continuous(labels = 
                       scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c('#f5c04b', 'rosybrown')) +
  labs(title = "Distribuição de sexos na 
programação do Sinape 2018 - por participação", 
       caption = "Fonte: http://www.sinape2018.com.br/") +
  facet_wrap(~classe) +
  theme_bw()

ggsave("img/sinape_2018_part.pdf", 
       plot = last_plot(), 
       width = 5, height = 3, 
       units = "in", dpi = 300)

res %>% 
  group_by(sexo) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(porcentagem = n/sum(n)) %>% 
  ggplot(aes(y = porcentagem, x = sexo)) +
  geom_histogram(stat = "identity", aes(fill = sexo)) +
  scale_fill_manual(values = c('#f5c04b', 'rosybrown')) +
  #facet_wrap(~classe) +
  scale_y_continuous(labels = 
                       scales::percent_format(accuracy = 1)) +
  labs(title = "Distribuição de sexos na 
programação do Sinape 2018 - geral", 
       caption = "Fonte: http://www.sinape2018.com.br/") +
  theme_bw()


ggsave("img/sinape_2018.pdf", 
       plot = last_plot(), 
       width = 5, height = 4, 
       units = "in", dpi = 300)
