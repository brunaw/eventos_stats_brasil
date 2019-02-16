# Sinape
library(rvest)
library(magrittr)

url <- "http://www.sinape2018.com.br/evento/23sinape/programacao/gradeatividades"


r <- httr::GET(url)
u_base <- "http://www.sinape2018.com.br/"
conferencistas <- r %>%
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
                         "Coordenador(a)", classe))

conferencistas %>% 
  group_by(classe, sexo) %>% 
  count() %>% 
  group_by(classe) %>% 
  mutate(porcentagem = n/sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(y = porcentagem, x = sexo)) +
  geom_histogram(stat = "identity", aes(fill = sexo)) +
  facet_wrap(~classe) +
  theme_bw()
  
#-------------
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

nomes <- p
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


res %>% 
  group_by(classe, sexo) %>% 
  count() %>% 
  group_by(classe) %>% 
  mutate(porcentagem = n/sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(y = porcentagem, x = sexo)) +
  geom_histogram(stat = "identity", aes(fill = sexo)) +
  facet_wrap(~classe) +
  theme_bw()
