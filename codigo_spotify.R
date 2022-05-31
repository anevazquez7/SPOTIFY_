install.packages("spotifyr")
library(spotidy)
library(dplyr)

SPOTIFY_CLIENT_ID <-"79ba4f46f96048509889804d6ae01f8c"
SPOTIFY_CLIENT_SECRET <- "840aa37dd39a4b01a3d388f4d61ec931"
my_token <- get_spotify_api_token(SPOTIFY_CLIENT_ID, SPOTIFY_CLIENT_SECRET)

rosalia<-search_artists("Rosalia")
rosalia<-rosalia%>%
          select(artist, artist_id)%>%
          head(1)
artists_rel <-get_artist_related_artists(
  rosalia$artist_id,
  output = "tidy",
  limit = 20,
  offset = 0,
  token = my_token
)
artists_rel<-artists_rel%>%
  arrange(desc(popularity))%>%
  head(10)
resultados<-data.frame()
for (i in (1:10)){
  resultados[i,1:7]<-get_artist_top_tracks(artists_rel$related_artist_id[i], country = "US")%>%
    filter(artist_id==artists_rel$related_artist_id[i])%>%
    arrange(desc(release_date))%>%
    head(1)
}
resultado_final<-resultados%>%
  arrange(desc(release_date))%>%
  head(1)
  
View(resultado_final)

