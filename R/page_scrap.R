#' Scraping d'une page google Scholar
#' Prend une url en enter et scrape le contenu de la page
#'
#' @param scholarURL
#'
#' @return La fonction renvois en data.frame de 6  colonne
#' @import xml2 rvest stringr
#' page_scrape('exemple.com')
#' @export


page_scrape <- function(scholarURL) {
  ## We give this function a copy url from google scholar and it will scrape the information from the page.
  ## cela retourne un data frame

  ## TEST URL
  # scholarURL <- "https://scholar.google.fr/scholar?hl=fr&as_sdt=0%2C5&q=A+role-playing+game+in+irrigated+system+negotiation%3A+between+play+and+reality&btnG="

  ## Each object of the class "gs_ri" contains a reference to a document found by google scholar
  ## it creates a list of several objects
  p1 <- read_html(scholarURL)%>%
    xml_nodes(xpath = '//*[@class="gs_ri"]')

  ### extract title and apply the cleanning action
  title <- html_node(p1, "h3") %>%
    xml_text(trim=T)

  ### extract URL for the block resource
  my.url <- html_node(p1, "a") %>%
    html_attr('href')

  ## extract autors and journal information
  ### If we whan to clip those inforamtion we can use str_locate(autors, "-") #TODO
  autors <- html_nodes(p1, xpath = '//*[@class="gs_a"]') %>%
    xml_text(trim=T)

  ## extract accessory information (as ciation numbre)
  infos <- html_nodes(p1, xpath = '//*[@class="gs_fl"]') %>%
    xml_text(trim=T)

  nb_citations <- as.numeric(str_extract(infos, "\\d+"))

  ## find URL for the search page with all reference citing this object
  url_tps1 <- p1 %>%
    html_nodes("a")
  text_tps2 <- url_tps1 %>%
    html_attr("href")

  ref.link <- text_tps2[6]

  ## Find if any "next page" link are in this page ?
  my.next.l <- NULL
  nextlink <- read_html(scholarURL)%>%
    html_node(xpath = '//*[@id="gs_n"]')
  if(length(nextlink) > 0){
    l.size <- length(nextlink %>% html_nodes("a")) # array size in porder to find the "next" link
    links.v <- nextlink %>%
      html_nodes("a") %>%
      html_attr("href")
    my.next.l <- links.v[l.size]
  }
  my.next.l <- paste0("https://scholar.google.fr",my.next.l)
  ## reconstruire le tableau de donnee
  return(data.frame(autors, title, my.url, nb_citations, ref.link, my.next.l))
}
