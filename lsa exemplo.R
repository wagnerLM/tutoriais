# AVQ - Latent Semantic Analysis
# instalando os pacotes
install.packages("tm")
install.packages("ggplot2")
install.packages("lsa")

# abrindo os pacotes
library(tm)
library(ggplot2)
library(lsa)

# preparando os dados:
# 9 frases
text <- c("transporting food by cars will cause global warming. so we should go local.",
          "we should try to convince our parents to stop using cars because it will cause global warming.",
          "some food, such as mango, requires a warm weather to grow. so they have to be transported to canada.",
          "a typical electronic circuit can be built with a battery, a bulb, and a switch.",
          "electricity flows from batteries to the bulb, just like water flows through a tube.",
          "batteries have chemical energe in it. then electrons flow through a bulb to light it up.",
          "birds can fly because they have feather and they are light.",
          "why some birds like pigeon can fly while some others like chicken cannot?",
          "feather is important for birds' fly. if feather on a bird's wings is removed, this bird cannot fly.")

#"transportar comida por carros causará o aquecimento global. por isso devemos comprar alimentos locais."
#"devemos tentar convencer nossos pais a pararem de usar carros porque isso causará o aquecimento global."
#"alguns alimentos, como manga, exigem um clima quente para crescer. então eles têm que ser transportados para o Canadá.",
#"um circuito eletrônico típico pode ser construído com uma bateria, uma lâmpada e um interruptor",
#"a eletricidade flui das baterias para o bulbo, assim como a água flui através de um tubo",
#"as baterias têm uma substância química. então os elétrons fluem através de uma lâmpada para acendê-la",
#"as aves podem voar porque têm penas e são leves."
#"porque alguns pássaros como pombo podem voar enquanto outros como galinha não podem?",
#"pena é importante para o voo das aves. se as penas nas asas de um pássaro forem removidas, esta ave não pode voar."

# criando uma coluna com as classes reconhecidas
view <- factor(rep(c("view 1", "view 2", "view 3"), each=3))
# criando o banco de dados
df <- data.frame(text, view, stringsAsFactors=FALSE)
# observando o banco de dados
View(df)

# preparando o corpus
# sepanrando apenas o texto
corpus <- Corpus(VectorSource(df$text))
# apenas minúsculas
corpus <- tm_map(corpus, tolower)
# removendo pontuações
corpus <- tm_map(corpus, removePunctuation)
# removendo "stopwords"
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
# stemização (manter a raiz)
corpus <- tm_map(corpus, stemDocument, language = "english")
# verificando o corpus
corpus 

# para comparar as técnicas, primeiro uma análise de escalonamento multidimensional
# com a matriz bruta dos dados
td.mat <- as.matrix(TermDocumentMatrix(corpus))
View(td.mat)
# a distância é calculada com a matriz transposta, aquele "t" antes da matriz anterior
View(t(as.matrix(td.mat)))
dist.mat <- dist(t(as.matrix(td.mat)))
# verificando a matriz
dist.mat
View(as.matrix(dist.mat))

# Escalonamento Multidimensional, em 2 dimensões (k=2)
fit <- cmdscale(dist.mat, eig=TRUE, k=2)
# criando um gráfico com ggplot2
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot(points, aes(x=x,y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=df$view)) + 
  geom_text(data=points,aes(x=x, y=y-0.2, label=row.names(df)))
# salve o gráfico! Export -> Save as PDF... -> Escolha destino e nome

# Escalonamento Multidimensional com LSA
# Ponderando os termos em peso local (lw) e peso global (gw)
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat) 
View(td.mat.lsa)
# criando o espaço latente (M = T S t(D); a matriz M é o produto das matrizes de termos "T", documentos "D" e a diagonal "S"
# de valores singulares
lsaSpace <- lsa(td.mat.lsa) 
View(lsaSpace)
lsaSpace$tk
lsaSpace$dk
lsaSpace$sk

as.textmatrix(lsaSpace)

# calculando as distâncias
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) 
dist.mat.lsa

# Escalonamento Multidimensional com LSA, duas dimensões (k=2)
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)
# gerando o gráfico
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=df$view)) + 
  geom_text(data=points,aes(x=x, y=y-0.2, label=row.names(df)))
# salve o gráfico e compare! 