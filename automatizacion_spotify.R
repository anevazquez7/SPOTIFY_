library(spotidy)
library(dplyr)
SPOTIFY_CLIENT_ID <-"79ba4f46f96048509889804d6ae01f8c"
SPOTIFY_CLIENT_SECRET <- "840aa37dd39a4b01a3d388f4d61ec931"
my_token <- get_spotify_api_token(SPOTIFY_CLIENT_ID, SPOTIFY_CLIENT_SECRET)

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

funcion("Rosalia") #ejemplo
funcion("amaia romero") #para ver que funciona en otros casos


