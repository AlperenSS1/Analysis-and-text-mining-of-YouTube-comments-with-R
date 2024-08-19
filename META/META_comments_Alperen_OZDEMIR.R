# Kutuphanelerin yuklenmesi
install.packages("tuber")
install.packages("httpuv")
install.packages("dplyr")
install.packages("tidytext")
install.packages(c("tm", "SnowballC"))
install.packages("stringr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("textdata")
install.packages("ggthemes")
install.packages("sentimentr")
install.packages("pander")
install.packages("pastecs")
install.packages("syuzhet")

#Kutuphanelerin cagirilmasi
library(syuzhet) # Metinlerden duygu ve duygu tabanlD1 olay orgusu cikarmak icin bir arac saglar
library(tuber) # YouTube API'ye erisim saglar
library(httpuv) # HTTP ve WebSocket isteklerini dogrudan R icinden islemek icin dusuk seviye soket ve protokol destegi saglar
library(dplyr) # Veri manipulasyonu icin bir dil saglar
library(tidytext) # Tidy veri ilkelerini kullanarak bircok metin madenciligi gorevini daha kolay ve etkili araC'larla tutarlD1 hale getirir.
library(tm) #  R icinde metin madenciligi uygulamalarD1 icin bir cerceve saglar
library(SnowballC) #  Porter'in kelime kok bulma algoritmasini uygulayan C 'libstemmer' kutuphanesine bir R arayuz saglar
library(stringr) # Karakter dizileri uzerinde calismayi kolaylastiran fonksiyonlar saglar
library(ggplot2) # Grafikleri deklaratif bir sekilde olusturmak icin bir sistem saglar
library(wordcloud) #  GC<zel kelime bulutlari cizmek icin bir arac saglar
library(wordcloud2) # Kelime bulutlari cizmek icin bir arac saglar
library(textdata) # Metinle ilgili veri setlerine kolay erisim saglar
library(ggthemes) # 'ggplot2' icin ek temalar, geoms ve olcekler saglar
library(sentimentr) #  ingilizce'deki metin polaritesi duygusunu hizli bir sekilde hesaplar
library(pander) # R nesnelerini Pandoc'un markdown formatina donusturmek icin minimal ve kolay bir arac saglar
library(pastecs) # Uzay-zaman serilerinin duzenlenmesi, ayristirilmasi ve analizi

# Youtube uzerinden yorum cekme ve csv olarak kaydetme
app_id <- ""
app_secret <- ""
yt_oauth(app_id, app_secret, token ="")
get_all_comments(video_id = "gElfIo6uw4g")
comments1 <- get_all_comments(video_id ="gElfIo6uw4g")
write.csv(comments1, file = "META_comments_Alperen.csv")

# Yorum verimi okuyorum
yorumlar <- read.csv("C:/Users/Asus/Veri_Proje/META_comments_Alperen")

# Yorum veri cercevesini secme
yorumlar <- yorumlar %>%
  dplyr::select(X,textOriginal)

# Cikarilacak kelime listem
TheStopList <- readLines("C:/Users/Asus/Veri_Proje/stopwords_Alperen.csv")
TheStopList <- data.frame(word = TheStopList)

# Kucuk harfe cevirme
yorumlar$textOriginal <- tolower(yorumlar$textOriginal)

# Linkleri ve etiketleri  kaldirma
yorumlar$textOriginal <- gsub("http[^ ]*", "", yorumlar$textOriginal)
yorumlar$textOriginal <- gsub("@[^ ]*", "", yorumlar$textOriginal)

# Emojileri kaldirma
yorumlar$textOriginal <- gsub("<.*?>", "", yorumlar$textOriginal)

# Sayilari kaldirma
yorumlar$textOriginal <- removeNumbers(yorumlar$textOriginal)

# Noktalama isaretlerini kaldirma
yorumlar$textOriginal <- str_replace_all(yorumlar$textOriginal, "[[:punct:]]", "")

# Cikarilacak kelime listemdeki kelimeleri kaldirma
yorumlar <- yorumlar %>% filter(!textOriginal %in% TheStopList)

# Ingilizce alfabeye uygun olmayan kelimeleri kaldirdim
for(i in seq_along(yorumlar)) {
  if(is.character(yorumlar[[i]])) {
    yorumlar[[i]] <- stringr::str_replace_all(yorumlar[[i]], "[^a-zA-Z ]", "")
  }
}

# Cikarilan yerlere NA degeri atama
yorumlar$textOriginal[yorumlar$textOriginal == ""] <- NA

# NA degerlerini kaldirma
yorumlar <- na.omit(yorumlar)


# Yorumlari kelimelere ayirip cikarilacak kelimeleri cikarma
yorumlar <- yorumlar %>% 
  unnest_tokens(word, textOriginal) %>% 
  anti_join(TheStopList)


# Tekrar cumle haline getirme
cumle <- yorumlar %>%
  group_by(X) %>%
  summarise(word = paste(word, collapse = " "))

# Kelimelerin tekrar sayisina gore siralama
kelime_tekrar <- yorumlar %>% count(word, sort = TRUE)


# Kelime bulutu olusturma
wordcloud2(kelime_tekrar, size = 1, minSize = 0, gridSize = 0,
           fontFamily = 'Times New Roman', fontWeight = 'bold',
           color = 'random-dark', backgroundColor = "white", shuffle = TRUE,
           rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65,
           widgetsize = NULL, figPath = NULL, hoverFunction = NULL)




# En cok tekrar eden 30 kelimeyi secme
kelime_tekrar_top30 <- kelime_tekrar %>% 
  top_n(30, n)

# Histogram olusturma
ggplot(kelime_tekrar_top30, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "#3b5998") +
  coord_flip() +
  labs(title = "En Cok Tekrar Eden 30 Kelime",
       x = "Kelime",
       y = "Tekrar Sayisi")



#-----------------------


# 'sentiment' fonksiyonu, 'Tekrar_yorum$word' veri cercevesindeki her kelimenin duygu degerini hesaplar.
polarite <- sentiment(cumle$word)

# 'cbind' fonksiyonu, 'Tekrar_yorum$word' ve 'polarite' veri cercevelerini birlestirir ve 'tablo' adli yeni bir veri cercevesi olusturur.
tablo <- cbind(cumle$word, polarite[,c(3,4)])

# istatistikleri hesapla
stat.desc(polarite$sentiment, basic=T) %>% pander()



# Polarite gorseli
ggplot(tablo, aes(word_count, sentiment))+
  geom_point(color="#3b5998")+
  geom_hline(yintercept = mean(tablo$sentiment), color="red", size=1)+
  labs(y = "Skor", x = "Kelimelerin Frekansi") +
  theme_gray()+
  ylim(-3,3)+
  labs(caption = "Veri Kaynagi: Youtube'da Everything Facebook revealed about the Metaverse in 11 minutes adli videodur.")+
  theme(plot.caption = element_text(hjust = 0, face = "italic"))







# yorumlar icerisindeki veri cercevesini Bing ile birlestir.
Duygu <- yorumlar %>%
  inner_join(get_sentiments("bing")) %>%
  
  # Her bir kelimenin ve duyarlilik puaninin frekansini hesapla
  count(word,sentiment)

# Duygu verisini duyarliliga gore grupla ve en cok tekrar eden 20 kelimeyi goster
Duygu %>% 
  group_by(sentiment) %>% 
  top_n(20) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>%
  
  # Bar grafigi olustur
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +  # Barlari ciz ve efsaneyi gizle
  facet_wrap(~sentiment, scales = "free_y") +
  
  # Eksenleri etiketle
  labs(y = " ",
       x = NULL) +
  
  # Grafikte x ve y eksenlerini cevir
  coord_flip()




#Yorumlarin yogunluklari
bar_grafik <- tablo$sentiment
bar_grafik <- as.data.frame(bar_grafik)

# Notr, negatif ve pozitif bar gorselleri
bar_grafik$bar_grafik[bar_grafik$bar_grafik > 0] <- 'Pozitif'
bar_grafik$bar_grafik[bar_grafik$bar_grafik == 0] <- 'Notr'
bar_grafik$bar_grafik[bar_grafik$bar_grafik < 0] <- 'Negatif'

# 'bar_grafik' sutununu bir faktor olarak donusturun
bar_grafik$bar_grafik <- as.factor(bar_grafik$bar_grafik)

# Kategorilere gore sayilari hesaplayin
a_sayilari <- table(bar_grafik$bar_grafik)


# Renklerin bir vektorunu olusturun
renkler <- c("red", "grey", "green")

# Bar grafigini olusturun ve her bir bar icin farkli bir renk belirleyin
barplot(a_sayilari, main='Duygu Dagilimi', xlab='Kategori', ylab='Sayi', col=renkler)




# yorumlar icerisindeki her bir kelimeyi frekansa gore sirala
afins <- yorumlar %>%
  count(word, sort = TRUE) %>%
  
  # AFINN ile birlestir.
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  
  # Her bir kelimenin toplam katkisini hesapla.
  summarize(contribution = sum(n * value)) %>% 
  
  # En yuksek 12 kelimeyi al
  top_n(10, abs(contribution)) %>%
  
  # Kelimeleri katkiya gore yeniden sirala.
  mutate(word = reorder(word, contribution))


# 10 farkli renk iceren bir vektor olusturun
renklera <- c("brown", "black", "grey", "red", "turquoise", "orange", "yellow", "darkgreen", "green", "#3b5998")

# Bar grafigini olusturun ve her bir bar icin farkli bir renk belirleyin
ggplot(data = afins, aes(word, contribution, fill = word)) +
  geom_col() +  # Barlari ciz
  scale_fill_manual(values = renklera) +  # Belirli renkleri kullan
  coord_flip() +  # Grafikte x ve y eksenlerini cevir
  labs(y = "Kelime sikligi * AFINN puani", x = "Kelimeler")  # y eksenini etiketle


