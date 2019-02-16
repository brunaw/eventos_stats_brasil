#' Author: Julio Trecenti
#' Subject:

# library(tidyverse)
library(magrittr)

# Import -----------------------------------------------------------------------
u_pdf <- "www.leg.ufpr.br/~walmes/rbras63/livreto-rbras63.pdf"
r <- httr::GET(u_pdf)
rbras <- pdftools::pdf_text(r$content)

# Tidy -------------------------------------------------------------------------

# obtem linhas separadas
linhas <- rbras %>% 
  magrittr::extract(-c(1:11, 62:length(.))) %>% 
  stringr::str_c(collapse = "\n") %>% 
  stringr::str_split("\n") %>% 
  dplyr::first()

# apenas linhas que tem 2 pontos
linhas_2p <- linhas %>% 
  magrittr::extract(stringr::str_detect(., ":")) %>% 
  stringr::str_match("^([A-Z][^:]+): ?(.+)") %>% 
  tibble::as_tibble(.name_repair = "unique") %>% 
  purrr::set_names(c("a", "titulo", "nome")) %>% 
  dplyr::filter(
    !is.na(titulo), 
    !stringr::str_detect(titulo, "feira|Gaussiana")
  ) %>% 
  dplyr::select(titulo, nome)

# todos os autores (o que vem depois de xxx:)
autores <- linhas_2p %>% 
  dplyr::mutate(tipo = stringr::str_detect(titulo, "[0-9]"),
                grupo = cumsum(tipo)) %>% 
  dplyr::group_by(grupo) %>% 
  dplyr::summarise(
    tipo_participacao = titulo[2],
    nome = stringr::str_c(nome[-1], collapse = ", "),
  ) %>% 
  dplyr::select(-grupo) %>% 
  dplyr::mutate(
    nome = stringr::str_extract(nome, "[^,]+"),
    nome = stringr::str_squish(nome),
    tipo_participacao = stringr::str_remove(tipo_participacao, "es$")
  ) %>% 
  head(102)

# todos os palestrantes (que vem depois dos titulos com pts)
palestrantes <- linhas %>% 
  stringr::str_subset(" {6,7}(?=[A-Z])") %>% 
  stringr::str_squish() %>% 
  stringr::str_extract("[^,]+") %>% 
  purrr::set_names(rep("Palestrante", length(.))) %>% 
  tibble::enframe("tipo_participacao", "nome")

# bind
da_rbras <- dplyr::bind_rows(autores, palestrantes)

# sexo colocado na mão
da_rbras <- tibble::tribble(
  ~tipo_participacao, ~nome, ~sexo,
  "Autor", "Luiz Ricardo Nakamura - UFSC", "M",
  "Autor", "Samuel Macêdo", "M",
  "Autor", "Cristian Villegas - ESALQ/USP", "M",
  "Autor", "Paulo Henrique Sales Guimarães - UFLA", "M",
  "Autor", "Agatha S. Rodrigues - USP", "F",
  "Autor", "Eduardo S. B. de Oliveira - UFSCar", "M",
  "Conferencista", "Louise Ryan", "F",
  "Conferencista", "Bárbara Henning", "F",
  "Conferencista", "Jacob Hjelmborg", "M",
  "Conferencista", "Rafael Izbiski", "M",
  "Conferencista", "David Bickel", "M",
  "Conferencista", "Guilherme J. M. Rosa", "M",
  "Coordenador", "Paulo Canas Rodrigues", "M",
  "Coordenador", "Francisco Louzada", "M",
  "Coordenador", "Ana Paula Corte", "F",
  "Coordenador", "Isolde Previdelli", "F",
  "Coordenador", "Giovana Oliveira Silva", "F",
  "Coordenador", "Isolde Previdelli", "F",
  "Coordenador", "Alfredo José Barreto Luiz", "M",
  "Coordenador", "Carlos Alberto de Bragança Pereira", "M",
  "Autor", "Felipe Barletta", "M",
  "Autor", "Cayan Atreio Portela Bárcena Saavedra", "M",
  "Autor", "Augusto Felix Marcolin", "M",
  "Autor", "Carla Regina Guimarães Brighenti", "F",
  "Autor", "Matheus Saraiva Alcino", "M",
  "Autor", "Vanessa Ferreira Sehaber", "F",
  "Autor", "Peter de Matos Campos", "M",
  "Palestrante", "Paulo Canas Rodrigues", "M",
  "Palestrante", "Carlos Tadeu dos Santos Dias", "M",
  "Palestrante", "Gleici da Silva Castro Perdoná", "F",
  "Palestrante", "Anderson Ara", "M",
  "Palestrante", "Francisco Louzada", "M",
  "Palestrante", "Razer Anthom Nizer Rojas Montaño", "M",
  "Palestrante", "Jaime Wojciechowski", "M",
  "Palestrante", "Ana Beatriz Schikowski", "F",
  "Palestrante", "Deivison Venicio Souza", "M",
  "Palestrante", "Mariana Ragassi Urbano", "F",
  "Palestrante", "Eniuce Menezes de Souza,", "F",
  "Palestrante", "Arthur Cesar de Moura Rocha", "M",
  "Palestrante", "Suely Ruiz Giolo", "F",
  "Palestrante", "Jackson Santos da Conceiçava", "M",
  "Palestrante", "Giovana Oliveira Silva", "F",
  "Palestrante", "Jairo Santos Lordelo", "M",
  "Palestrante", "Miguel Angel Uribe-Opazo", "M",
  "Palestrante", "Diogo Francisco Rossoni", "M",
  "Palestrante", "Ana Julia Righetto", "F",
  "Palestrante", "Erica Luciene Alves de Lima", "F",
  "Palestrante", "Alfredo José Barreto Luiz", "M",
  "Palestrante", "Natalia L. Oliveira", "F",
  "Palestrante", "Carlos A. de B. Pereira", "M",
  "Palestrante", "Alexandre Galvão Patriota", "M"
)

# Visualize --------------------------------------------------------------------

# Model ------------------------------------------------------------------------

# Export -----------------------------------------------------------------------

# readr::write_rds(d, "")
