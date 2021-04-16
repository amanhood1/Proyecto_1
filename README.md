Proyecto\_1\_def
================

## ALumnos: Alejandro Manhood y Estivaliz Rosales

## Objetivos del trabajo:

En este proyecto se tiene por objetivo encontrar una combinaon ideal
para un sandwich y de esta manera que sea añadido a la carta de un
restoran. Lo que se hará en nuestro proyecto es centrarnos
especificamente en los sandwiches que hayan tenido calificacion 5.

Esto es porque bajo nuestro punto de vista estos son los que mejor han
sido evaluados y por ende, esto debe traducirse a la preferencia por
parte de los clientes por estos, lo cual se puede deber a los
ingredientes que incluyen. Lo que se hará es tener estos ingredientes
para luego limpiar estos ingredientes, sacando conectores y palabras
redundantes para finalmente calcular las frecuencias de estos y asi
formar el sandwich perfecto.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.1.0     v stringr 1.4.0
    ## v tidyr   1.1.3     v forcats 0.5.1
    ## v readr   1.4.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
library(stringr)
library(qdap)
```

    ## Loading required package: qdapDictionaries

    ## Loading required package: qdapRegex

    ## 
    ## Attaching package: 'qdapRegex'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     %+%

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     explain

    ## Loading required package: qdapTools

    ## 
    ## Attaching package: 'qdapTools'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     id

    ## Loading required package: RColorBrewer

    ## 
    ## Attaching package: 'qdap'

    ## The following objects are masked from 'package:base':
    ## 
    ##     Filter, proportions

``` r
library(tm)
```

    ## Loading required package: NLP

    ## 
    ## Attaching package: 'NLP'

    ## The following object is masked from 'package:qdap':
    ## 
    ##     ngrams

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     annotate

    ## 
    ## Attaching package: 'tm'

    ## The following objects are masked from 'package:qdap':
    ## 
    ##     as.DocumentTermMatrix, as.TermDocumentMatrix

``` r
library(utf8)
setwd("C:/Users/amanh/OneDrive/Documentos/GitHub/Proyecto-1")
datos <- read.csv("sanguchez.csv",header= TRUE, sep=";")
```

## Analisis de los datos:

Primero analizamos la data para de esta manera poder tener una idea como
es y asi hacer que sea mas facil ver los tipos de datos.

``` r
head(datos)
```

    ##                                                               url
    ## 1          https://365sanguchez.com/abocado-cantina-buenos-aires/
    ## 2                   https://365sanguchez.com/alba-hotel-matanzas/
    ## 3   https://365sanguchez.com/albedrio-restaurant-santiago-centro/
    ## 4 https://365sanguchez.com/albedrio-restaurant-santiago-centro-2/
    ## 5              https://365sanguchez.com/aldea-nativa-providencia/
    ## 6            https://365sanguchez.com/aleman-experto-providencia/
    ##                 Local                                         Direccion  Precio
    ## 1     Abocado Cantina   C1125AAE, French 2316, C1125AAF CABA, Argentina $5.210.
    ## 2          Alba Hotel   Carlos Ibañez del Campo s/n – Matanzas, Navidad  $7.000
    ## 3 Albedrio Restaurant     Huérfanos 640, Santiago, Región Metropolitana  $7.290
    ## 4 Albedrío Restaurant Pasaje Huerfanos 640 edificio B local 5, Santiago  $8.690
    ## 5        Aldea Nativa  Tobalaba 1799, Providencia, Región Metropolitana  $4.900
    ## 6      Alemán Experto Av. Pedro de Valdivia 1683, Providencia, Santiago  $6.500
    ##                                                                                                                  Ingredientes
    ## 1                                               Suprema de pollo dulce, espinaca, crema ácida, repollo avinagrado y guacamole
    ## 2                     Carne mechada en reducción de vino tinto, champiñones salteados, cebolla caramelizada y queso derretido
    ## 3                          Mayonesa al olivo, champiñones salteados, jalapeños, queso Mozzarella, papas hilo y cebolla morada
    ## 4                          Queso Mozzarella, Rúcula, Champiñon portobello relleno de cheddar y luego apanado en panko y frito
    ## 5 Tofu asado no transgénico, palta, tomate, champiñones, mix de hojas verdes orgánicas,  mayonesa de zanahoria vegana casera,
    ## 6                           Hamburguesa, queso Cheddar, cebolla caramelizada, berros, pepinillos y salsa Jack Daniel’s Honey.
    ##   nota
    ## 1    3
    ## 2    3
    ## 3    4
    ## 4    4
    ## 5    4
    ## 6    3
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               texto
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Ojo acá! En la sanguchería “Abocado” (@AbocadoCantina) de Recoleta, más que un sándwich exquisito (que igual estaba bueno), descubrí una maravilla para copiar: acá el apanado, el frito del pollo, era dulce. Y bien crocante. Exquisito. Les juro que es el mejor apanado que he probado en mi vida. A esta suprema de pollo dulce, la acompañaban con espinaca (yo la hubiese puesto a la crema), crema ácida, repollo avinagrado y guacamole. Lamentablemente, la palta acá en Argentina no es como la chilena. Es más aguachenta. Y el pan, nuevamente sigue la línea que me ha tocado en este país, que no logra ser del nivel que tenemos en Chile. Pero insisto: ese batido hay que exportarlo. Estaba exquisito. Y sigo pensando en él.
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Aprovechando que me escapé a Matanzas con @catabarra_ a canjear mi regalo de cumpleaños (clases de surf), quise probar algunos sanguchitos de la zona. Y como hace un año me quedé a alojar en @albahotelboutique y tuve una muy buena experiencia, hoy quise darle una oportunidad a su carta de comida. Y a pesar de que en general nos fue bastante mal (3 de los platos andaban muuuy bajos), mi sanguchito salvó muy bien. Y es que la mezcla de carne mechada en reducción de vino tinto, champiñones salteados, cebolla caramelizada en marraqueta (y le sumé queso derretido), es demasiado buena. No falla. Así que de 1 a 5, este se lleva 3 narices de chancho. Es decir, es un buen sándwich. Vaya a probarlo con confianza. Una marrquetita crujiente y de poca miga, una mechada muy suave y harto queso son sus puntas de lanzas. Sí, hay cosas por mejorar. Por ejemplo, las “mechas” de la carne como que se pegaban unas a otras, entonces a veces de un mordisco te llevabas casi toda la carne. O el caldo lo haría más intenso. Porque lo que chorreaba aquí eran los champiñones más que la carne. Pero apaña harto, además que estás comiendo EN la playa misma.
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Sin duda, uno de los lugares que me ENCANTA visitar. Lejos la mejor hamburguesa que tienen es la Portobello (con un champiñón frito relleno de Cheddar y todo), pero como no estamos en temporada de hongos, no había ahora. Esa, sin duda, se lleva cinco narices. Hoy vine a @RestoAlbedrio con@MaxKbzon y nos dimos la torta comiendo. Él fue por un sándwich de prieta con manzana verde, papas hilo y mayo de ají verde. Yo, una burger “Picante”, con mayonesa al olivo, champiñones salteados, jalapeños, queso Mozzarella, papas hilo y cebolla morada. Solo les adelanto una cosa: tienen una salsa de reducción de cerveza con jugo de piña y azúcar rubia, que debiesen venderla en bidones!! Es EXQUISITA!
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Con @nitanzorron fuimos a probar esta maravilla de @albedrio_resto. Anoten: hamburguesa casera, queso mozzarella, rúcula y champiñon portobello relleno de cheddar y luego apanado en panko y frito . Una maravilla! Es que los champiñones rellenos ya son atómicos… Pero ahora que vienen fritos, tienes un sabor estratosférico. La mejor idea del mundo. Es una verdura muy “carnosa” y rica, realzada por el queso y el apanado. El toque amargo de la rúcula viene bien, y la hamburguesa en sí, creo que es la más jugosa que he probado. Me recordó a la de Ciudad Vieja. Anda perfecta. El pan Brioche, bien dulce, y de miga consistente. No tan aireada. Mi único punto a mejorar es que sentí que era muy “aguado” (los champiñones tienen alto porcentaje de agua), por lo que me faltó malicia. Un picante, o una salsa de ajo… No sé. Algo que te vuele la cabeza. De hecho, Albedrío tiene dos salsas que creo que pondrían a esta hamburguesa en el top chileno: la de la casa, que es una reducción de cerveza, pulpa de piña y azúcar rubia, y una mayonesa con cilantro y ajo que es perfecta. Con @nitanzorron conversamos que agregando esa salsa, el sandwich sube de nivel a SS3. Muy buena. Vayan a ver nuestra visita a mi canal de YouTube (link en mi perfil) para todos los detalles y comenten si les tinca porque encuentro que es mega creativa y muuuuy rica.
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Ojo los vegetarianos!! Porque gracias a@genoveva_tenaillon (síganla si quieren ver unas recetas exquisitas pero saludables al mismo tiempo) que me pasó el dato, encontré el templo de los sándwiches vegetarianos y jugos naturales wenos wenos wenos. Es Aldea Nativa, en Tobalaba, y a pesar de que es 99% más probable que prefiera un sándwich carnívoro, creo que los que probé acá son de los mejorcitos que me han tocado (hasta ahora, en La Tegualda están los mejores). El Barros Luco de la Geno estaba bien bueno (con carne de libre pastoreo, sin hormonas ni antibióticos… Y no, claramente este no era veggie jaja), pero me gustó más el mío: tofu asado no transgénico, palta, tomate, champiñones, mix de hojas verdes orgánicas, y le sumé una mayonesa de zanahoria vegana casera, que viene con todos los sándwiches (échensela entera). A ver. Era rico, pero la nota se la lleva principalmente porque es el mejor tofu que he probado en Chile. En general lo cocinan muy fome, pero este estaba marinado en soya y asado a la plancha, así que tenía un gustito distinto al típico “quesillo sin sabor” jajaj. Además, venía como con un cevichito de champiñones que también se lo puse adentro  y agarró una jugosidad que el pan agradeció. Con estos dos ingredientes que le puse, las verduras agarraron un aliño exquisito. De los vegetarianos que he probado, es de los más ricos. Pero si no te gusta el Tofu, también puedes probar alguna de las hamburguesas vegetarianas que tienen. Me gustó harto el lugar, además de que también es un mercado donde venden miles de productos orgánicos, vegetarianos y de esa onda.
    ## 6 Salsa de bourbon: checkAlemán ExpertoCómo llegué a Alemán ExpertoYa había venido un par de veces al Alemán Experto. Tanto al local de Santa Magdalena, como a este de Pedro de Valdivia. En todas las visitas tuve suerte dispar. En algunos me gustó harto, otras no tanto.La cosa es que hoy tuve que hacer trámites cerca, y como tenía poco tiempo para buscar una sanguchería, preferí ir al Alemán Experto, que aún no lo sumaba a 365 Sánguchez.Fotos tomadas con mi celular Sony Xperia XRestaurante y sanguchería Alemán ExpertoAlemán Experto es una sanguchería que cuenta con dos locales. El primero está en Santa Magdalena, y el otro en Pedro de Valdivia, en la esquina con Francisco Bilbao. Ojo, que también están abriendo uno en La Dehesa.Este restaurante es, tal como lo dice su nombre, bien alemán. Es decir, abundan los sánguches grandes y la cerveza.Hablando del local de Pedro de Valdivia, siento que hicieron un gran trabajo con su fachada exterior. Si no me creen, miren la foto de más arriba. Y es que la terraza que sacaron está increíble. Además, por su ubicación, siempre hay gente, por lo que me tinca que se arma buen ambiente para los after office.Les dejo su pagina web. Carta de sándwiches Alemán ExpertoLa carta de sándwiches del Alemán Experto es amplia, tanto en sus bases, como también en sus combinaciones gourmet y clásicas.Por el lado más jugado, la sanguchería Alemán Experto cuenta con hamburguesas y mechadas BBQ. De las primeras, destacan una que tiene camarones y queso azul ($6.400), y la que pedí yo. Se llama Jack Daniel’s Honey, y tiene una salsa basada en ese licor, además de queso Cheddar, berros, cebolla caramelizada y pepinillos.En las mechadas BBQ, hay dos opciones. una con tocino crispy, y la otra con queso Azul y aros de cebolla.Luego de esta sección más “gourmet”, Alemán Experto también cuenta con hamburguesas, churrascos, lomitos, aves, salchichas y mechadas para poder armarlas como italianos, lucos y chacareros.Para terminar, hay una sección de sándwiches vegetarianos. Son hamburguesas de quinoa, y tiene cuatro combinaciones distintas. Hamburguesa Jack Daniel’s Honey en Alemán ExpertoA pesar de no ser un fanático del bourbon, admito que sí me gusta esta variante con toques de miel. Y en una salsa, mejor aún.Tengo que decir que es un sándwich correcto. La salsa no se roba el protagonismo, y aporta un toque justo de dulzor y también de “malicia”.La cebolla caramelizada estaba suave, y los berros perfectos para contrastar el frescor con lo dulce de la cebolla y la salsa.Lo que no me gustó tanto, es que la hamburguesa estaba un poco seca. Tuvo suerte, eso sí, de que venía acompañada con harta salsa, por lo que lograba pasar piola. Pero si nos quedamos en la carne, le falta.Y el otro punto negativo, y esto ya parece que es una maldición que me persigue, fue el queso Cheddar. Primero, porque no estaba derretido. Cueck. Y segundo, porque su sabor era demasiado plástico. Les prometo que tengo ganas de hacer una cata de quesos Cheddar, quizás con Daniel Greve, para poderles recomendar cuáles son buenos. Pero este, no.Maridaje con Cerveza Austral LagerEn resumen: Alemán Experto puede ser experto en otras cosas, pero no en hamburguesasRecién ahora, que estoy escribiendo estas líneas, me doy cuenta que quizás hice una movida tonta. Si voy a un lugar que se llama Alemán Experto, lo normal sería haber pedido un lomito. Con chucrut, con pepinillos… algo por ahí.Se supone que los alemanes también le pegan a las fricandelas, pero este no fue el caso. De hecho, la carne no era tan especiada como suele serlo en ese país. Pero aún así, me tinca que el lomito aquí puede ser un gran acierto.Quedé con ganas de volver. Volver y probar otra proteína, como el lomito o la mechada. Así que nos vemos pronto, Alemán Experto.

``` r
str(datos)
```

    ## 'data.frame':    410 obs. of  7 variables:
    ##  $ url         : chr  "https://365sanguchez.com/abocado-cantina-buenos-aires/" "https://365sanguchez.com/alba-hotel-matanzas/" "https://365sanguchez.com/albedrio-restaurant-santiago-centro/" "https://365sanguchez.com/albedrio-restaurant-santiago-centro-2/" ...
    ##  $ Local       : chr  "Abocado Cantina" "Alba Hotel" "Albedrio Restaurant" "Albedrío Restaurant" ...
    ##  $ Direccion   : chr  "C1125AAE, French 2316, C1125AAF CABA, Argentina" "Carlos Ibañez del Campo s/n – Matanzas, Navidad" "Huérfanos 640, Santiago, Región Metropolitana" "Pasaje Huerfanos 640 edificio B local 5, Santiago" ...
    ##  $ Precio      : chr  "$5.210." " $7.000" "$7.290" "$8.690" ...
    ##  $ Ingredientes: chr  "Suprema de pollo dulce, espinaca, crema ácida, repollo avinagrado y guacamole" " Carne mechada en reducción de vino tinto, champiñones salteados, cebolla caramelizada y queso derretido" "Mayonesa al olivo, champiñones salteados, jalapeños, queso Mozzarella, papas hilo y cebolla morada" "Queso Mozzarella, Rúcula, Champiñon portobello relleno de cheddar y luego apanado en panko y frito" ...
    ##  $ nota        : int  3 3 4 4 4 3 3 3 3 3 ...
    ##  $ texto       : chr  "Ojo acá! En la sanguchería “Abocado” (@AbocadoCantina) de Recoleta, más que un sándwich exquisito (que igual es"| __truncated__ "Aprovechando que me escapé a Matanzas con @catabarra_ a canjear mi regalo de cumpleaños (clases de surf), quise"| __truncated__ "Sin duda, uno de los lugares que me ENCANTA visitar. Lejos la mejor hamburguesa que tienen es la Portobello (co"| __truncated__ "Con @nitanzorron fuimos a probar esta maravilla de @albedrio_resto. Anoten: hamburguesa casera, queso mozzarell"| __truncated__ ...

Luego se saco un resumen de los datos para poder identificar algun dato
que tenga valores Na y de esta manera sacarlos para tener mayor orden.
Como tambien el numero de filas y columnas.

``` r
summary(datos)
```

    ##      url               Local            Direccion            Precio         
    ##  Length:410         Length:410         Length:410         Length:410        
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  Ingredientes            nota          texto          
    ##  Length:410         Min.   :1.000   Length:410        
    ##  Class :character   1st Qu.:3.000   Class :character  
    ##  Mode  :character   Median :3.000   Mode  :character  
    ##                     Mean   :3.167                     
    ##                     3rd Qu.:4.000                     
    ##                     Max.   :5.000                     
    ##                     NA's   :8

``` r
dim(datos)
```

    ## [1] 410   7

## Pre procesamiento de datos

Esto se realiza para dejar variables que sean de interes, mediante
inspeccion visual de estas misnas. En este caso se sacaran las variables
“url”, “local”, “direccion”, “precio” y “texto”. Ya que estas no aportan
informacion para el analisis que nosotros queremos hacer. Dejando
unicamente tanto las notas como los ingredientes que contienen los
sandwiches.

``` r
sandw <- datos[,! (colnames(datos) %in% c("url","Local","Direccion","texto", "Precio"))]
#datos
```

Luego se procede a retirar todos los sandwich que tengan nota inferior a
5, para de esta manera quedarnos con los que han recibido la mejor
calificacion y asi tener a los mejor valorados. Además se hace un
resumen para verificar que no esxistan datos con valores faltantes. Al
saber que no existen se prosigue con el trabajo de limpieza de la data.

``` r
sandw <- filter(sandw, nota == 5)
summary(sandw)
```

    ##  Ingredientes            nota  
    ##  Length:56          Min.   :5  
    ##  Class :character   1st Qu.:5  
    ##  Mode  :character   Median :5  
    ##                     Mean   :5  
    ##                     3rd Qu.:5  
    ##                     Max.   :5

``` r
dim(sandw)
```

    ## [1] 56  2

## Limpieza de Ingredientes para dejar palabras necesarias:

Lo siguiente a realizar es crear una nueva variable para almacenar los
ingredientes de los sandwiches con mejores notas, (nos costo poder sacar
las notas del original, asi que era mas facil crear una nueva variable
con los ingredientes). Esta variable tiene por nombre sanwi y contendrá
los ingredientes de las 56 entidades.

Junto con lo anterior se procede a comenzar con la limpieza de
caracteres, puntuacion y conectores que presente nuesta data. Para asi
tenerla lo mas limpia posible para el procesamiento de texto. Se revisa
cada vez con las lineas sanwi\[i\] para corroborar como se va limpiando
nuestra variable.

``` r
sanwi <- sandw$Ingredientes
sanwi[1]
```

    ## [1] "Pato, queso Pepper Jack, jalapeños, cebolla morada, espinaca, tomate y mayonesa Habanero, en pan Brioche."

``` r
sanwi <- tolower(sanwi)
sanwi <- removePunctuation(sanwi)
sanwi <- removeNumbers(sanwi)
sanwi <- rm_stopwords(sanwi, tm::stopwords("spanish"))
sanwi[1]
```

    ## [[1]]
    ##  [1] "pato"      "queso"     "pepper"    "jack"      "jalapeños" "cebolla"  
    ##  [7] "morada"    "espinaca"  "tomate"    "mayonesa"  "habanero"  "pan"      
    ## [13] "brioche"

``` r
sanwi[4]
```

    ## [[1]]
    ##  [1] "filete"      "res"         "relish"      "betarraga"   "raíz"       
    ##  [6] "picante"     "rábano"      "aros"        "cebolla"     "champiñones"
    ## [11] "portobellos"

Al ejecutar tenemos individualizados los ingredientes, pero tenemos
ahora el problema de que necesitamos tenerlos todos en una sola linea,
para de esta manera, poder generar una limpieza y asi quedarnos con el
texto que nos servira. Para esto creamos un vector con la libreria qdap,
para que de esta manera se puedan contar los ingredientes.

``` r
sani2 <- VectorSource(sanwi)
sani3 <- VCorpus(sani2)
```

Una vez obtenido el corpus de texto, se procede a converitr este corpus
en un data frame, y que de esta manera se pueda graficar para ver las
frecuencias de los ingredientes. Para esto se hizo el grafico con 20
ingredinetes, ya que bajo nuestro punto de vista es una cantidad
razonable.

``` r
#
final <- as.data.frame(sani3)
(frequent_terms <- freq_terms(final,top = 20))
```

    ##    WORD         FREQ
    ## 1  queso          32
    ## 2  cebolla        22
    ## 3  mayonesa       19
    ## 4  tomate         18
    ## 5  salsa          17
    ## 6  pan            14
    ## 7  lechuga        12
    ## 8  tocino         12
    ## 9  hamburguesa    11
    ## 10 palta           9
    ## 11 ají             7
    ## 12 carne           7
    ## 13 morada          7
    ## 14 ajo             6
    ## 15 caramelizada    6
    ## 16 casera          6
    ## 17 crema           6
    ## 18 mozzarella      6
    ## 19 verdes          6
    ## 20 champiñones     5
    ## 21 cheddar         5
    ## 22 frito           5
    ## 23 pepinillos      5
    ## 24 rúcula          5
    ## 25 verde           5

``` r
plot(frequent_terms)
```

![](Proyecto_1_def_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Gráfico de distribución de ingredientes:

Se puede apreciar que existen ingredientes que tienen una gran cantidad
de ocurrencias, sin embargo para poder tener una mejor visualización se
procede con filtrar a los 7 ingredientes mas repetidos.

``` r
(ingre_final <- freq_terms(final,9))
```

    ##   WORD        FREQ
    ## 1 queso         32
    ## 2 cebolla       22
    ## 3 mayonesa      19
    ## 4 tomate        18
    ## 5 salsa         17
    ## 6 pan           14
    ## 7 lechuga       12
    ## 8 tocino        12
    ## 9 hamburguesa   11

## Conclusiones:

Ahora se puede apreciar mas claramente que ingredientes se repiten con
mayor frecuencia en los sandwiches. Estos son queso (32), cebolla (22),
mayonesa (19), tomate (18), lechuga (12), tocino (12) y hamburguesa
(11). El pan no lo contamos ya que este es algo que todo sanwich lleva,
por ende se puede obviar que estara incluido. Luego el tema de la salsa
se puede dejar a criterio del cliente, si es que desea incluirla en su
orden.

Finalmente, dejamos estos 7 ingredientes, ya que nos parece una cantidad
razonable para un sandwich que no es de una cadena de comida rápida (Mc
Donalds, Burger King, etc), si no que esta orientado a algo mas
especifico como otras cadenas que uno puede encontrar en su ciudad.
Además tomamos como punto para escoger que tuviese una repeticion igual
o mayor a 10, de esta manera nos quedamos con los ingredientes
escogidos.
