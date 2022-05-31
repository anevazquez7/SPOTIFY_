library(plumber)
library(spotidy)
library(dplyr)
library(sf)

#*@apiTitle Examen Data Science
#*@apiDescription Libreria "spotidy"

#* Funcion creada en el apartado 2 y 4
#* @param artista Introduce el nombre del artista
#* @get /funcion

funcion<-function(artista){
  df_artista<-search_artists(artista)
  df_artista<-df_artista%>%
    select(artist, artist_id)%>%
    head(1)
  
  artists_rel <-get_artist_related_artists(
    df_artista$artist_id,
    output = "tidy",
    limit = 20,
    offset = 0,
    token = my_token
  )
  
  artists_rel<-artists_rel%>%
    arrange(desc(popularity))%>%
    head(10)
  resultados<-data.frame()
  for (i in (1:nrow(artists_rel))){
    resultados[i,1:7]<-get_artist_top_tracks(artists_rel$related_artist_id[i], country = "US")%>%
      filter(artist_id==artists_rel$related_artist_id[i])%>%
      arrange(desc(release_date))%>%
      head(1)
  }
  resultado_final<-resultados%>%
    arrange(desc(release_date))%>%
    head(1)
  return(paste("La cancion", resultado_final$track, "del cantante", resultado_final$artist_name, "tiene una popularidad de", 
               resultado_final$popularity, ". Este artista se hizo muy famoso por colaborar con", artista))
}
