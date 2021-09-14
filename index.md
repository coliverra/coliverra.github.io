## TRI

O texto introdutório - Fonte: <https://guiadoestudante.abril.com.br/>
<p align="justify">
Acertar muitas questões no Enem não é garantia de uma super nota, assim
como não ir tão bem de acordo com o gabarito não significa que a nota
vai ser baixa. Isso tudo porque a composição da nota leva em
consideração a Teoria de Resposta ao Item (TRI), um complexo cálculo
matemático que leva em conta, antes de tudo, a coerência dos acertos.
</p>
<p align="justify">
Segundo a TRI, um candidato que acerta mais questões difíceis do que
fáceis é considerado incoerente. Isso porque a probabilidade de isso
acontecer é muito baixa e dai conclui-se que ele chutou, o que diminui
sua nota. Já uma prova considerada coerente, dentro do esperado, é
aquela em que o candidato acerta muitas fáceis, algumas médias e poucas
difíceis. Isso explica porque alguns candidatos acertam o mesmo número
de questões e têm notas diferentes, ou porque alguns candidatos acertam
menos questões do que outros e obtêm notas maiores.
</p>
<p align="justify">
O gráfico apresenta a relação entre o número de acertos no gabarito e a
real nota do candidato no Enem de 2013 da Cidade de Santarém-PA. Os
gráficos são resultado de uma pesquisa feita com 14.953 candidatos que
participaram dos dois dias de prova e representa apenas a prova de
Ciencias da Natureza e suas Tecnologias. Em breve postaremos os gráficos
da relação Acertos vs Notas de todos os anos posteriores das provas de
Matemática e Ciências da Natureza.
</p>
<p align="justify">
Iniciamos com o filtro para que a leitura seja apenas das variáveis que
desejamos. Nesse caso utizamos o dicionário dos microdados para checar
os respectivos nomes.
</p>

``` r
colsToKeep <- c("COD_MUNICIPIO_RESIDENCIA","NO_MUNICIPIO_RESIDENCIA",
  "COD_UF_RESIDENCIA","UF_RESIDENCIA", "IN_PRESENCA_CN", "IN_PRESENCA_CH",
  "IN_PRESENCA_LC", "IN_PRESENCA_MT", "ID_PROVA_CN", "GABARITO_CN", "NOTA_CN", "TX_RESPOSTAS_CN")
```

<p align="justify">
Agora usamos a função fread no pacote data.table. Com ele o carregamento
de grandes conjuntos de dados precisa de apenas alguns segundos.
</p>

``` r
setwd("/mnt/10efef75-8353-46be-b050-2cb93c385aee/Microdados/Microdados_enem_2013/DADOS")
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df2 <- fread('MICRODADOS_ENEM_2013.csv', header=TRUE, select=colsToKeep, sep = ';', verbose=F)
# Usando o fread, dispensa o argumento sep, nrosw

## Retirando as linhas vazias. O canditato comparaceu mais nao respondeu (0 ou NA)
# APLICADO A TABELA
get.data <- df2[!is.na(df2$'NOTA_CN') & (df2$'NOTA_CN' != 0.0) & (df2$'IN_PRESENCA_CN' == 1)
  & (df2$'IN_PRESENCA_CH' == 1) & (df2$'IN_PRESENCA_LC' == 1) & (df2$'IN_PRESENCA_MT' == 1) & 
    (df2$'NO_MUNICIPIO_RESIDENCIA'=="SANTAREM"),]

#Capacidade de gravação rápida paralelizada. Use isso caso deseje guardar em arquivo
fwrite(get.data, "dataEnem2013.csv")

# Dispensa a leitura. Comente caso use a variável  "get.data"
get.data.filt <- fread("dataEnem2013.csv")
```

## Filtrando as respostas e gabarito

<p align="justify">
Usamos o data.table() por ser mais rápido do que “dplyr” e “plyr”. Aqui
filtramos apenas o gabarito da prova azul (COR = 171). Ela vai ser a
prova default. Isso porque as questões nas demais provas estão em ordem
diferentes. Aqui vamos organizar todas em uma mesma ordem. Se você não
achar necessário, adote a metodologia mais adequada.
</p>

``` r
# Respostas
set_all<-get.data.filt[,.(NO_MUNICIPIO_RESIDENCIA,COD_UF_RESIDENCIA, UF_RESIDENCIA, ID_PROVA_CN, 
  NOTA_CN, TX_RESPOSTAS_CN)] 

# Gabarito da prova azul.
set_gabarito <- get.data.filt[ID_PROVA_CN ==171, .(GABARITO_CN)]
```

<p align="justify">
Observem que as respostas dos candidatos estão nesse formato “ABDACE…”.
Iremos agora separar as respostas e gabaritos por colunas. Usamos a
função “separate()” do pacote “dplyr” (tidyverse). Você pode usar
“colsplit” também. Outra opção também você fazer um filtro apenas das
respostas e gabarito para um arquivo e usar o
read.fwf(‘respostas.csv’,w=c(rep(1,45))). Esta opção pode ser mais
demorada.
</p>

``` r
my_into<-c(paste('Q',46:90,sep=""))

#### Split: Separa as respostas e gabaritos por colunas. Pacote "tidyr" ####
get.split.all<-set_all %>% separate(TX_RESPOSTAS_CN, into=as.character(my_into), sep=1:length(my_into)) 
get.split.gab.cn<-set_gabarito %>% separate(GABARITO_CN, into=as.character(my_into), sep= 1:length(my_into)) 
```

Procure o help da função separate para entender cada argumento.

## Selecionando as questões correspondentes

<p align="justify">
Vamos selecionar a cor da prova e ordenar-las de acordo com a prova
Azul. Exemplo: A questão 58 da prova amarela é a questão 46 da prova
azul, etc. Essa reordenação é feita a partir dos arquivos disponíveis na
pasta PLANILHAS.
</p>

``` r
options(width = 200)
set_cor_azul<-get.split.all[(ID_PROVA_CN ==171), .(NO_MUNICIPIO_RESIDENCIA,COD_UF_RESIDENCIA,UF_RESIDENCIA, NOTA_CN,Q46, Q47, Q48, Q49, Q50, Q51,Q52, Q53, Q54, Q55, Q56, Q57, Q58, Q59, Q60,Q61,Q62,Q63, Q64, Q65, Q66, Q67, Q68, Q69, Q70, Q71, Q72, Q73, Q74, Q75,
  Q76, Q77, Q78, Q79, Q80, Q81, Q82, Q83, Q84, Q85, Q86, Q87, Q88, Q89, Q90)]
```

``` r
set_cor_amarela<-get.split.all[(ID_PROVA_CN ==172), .(NO_MUNICIPIO_RESIDENCIA,COD_UF_RESIDENCIA,UF_RESIDENCIA, NOTA_CN, Q58,Q59,Q60,Q61,Q62,Q54,Q55,Q56,Q57,Q46,Q47,Q48,Q49,Q63,Q64,Q65,Q50,Q51,Q52,Q53,
  Q69,Q70,Q71,Q72,Q73,Q74,Q66,Q78,Q80,Q81,Q82,Q83,Q86, Q87,Q88,Q89,Q90,Q84,Q85,Q67,Q68,Q79,Q75,Q76,Q77)]
```

### Ordenar as provas

<p align="justify">
Agora vamos usar o “rbindlist” para unir todas as provas em sua devida
ordem. Para isso, reorganizamos os heads das colunas. Usamos novamente a
prova azul como referência. Aqui você pode adotar outras maneiras para
organizar. Pra mim essa foi mais fácil.
</p>
<p align="justify">
Usamos o “rbindlist” por ser mais eficiente, considerando que estamos
usando data.table(). Experimente usar o rbind (nativo) ou bind\_rows,
filter(), select() do dplyr. Chamo atenção aqui com os conflitos do
pacote “tidyverse” e “MASS”. Veja sobre tidyverse\_conflicts() em
<https://tidyverse.tidyverse.org/>.
</p>

``` r
azul_amarela <-rbindlist(list(set_cor_azul, set_cor_amarela), use.names=TRUE)
azul_amarela_branca <-rbindlist(list(azul_amarela, set_cor_branca), use.names=TRUE)
RespostasEnem2013<-rbindlist(list(azul_amarela_branca, set_cor_rosa), use.names=TRUE)
```

## Conferindo o gabarito e respostas

Aqui nos capturamos apenas a primeira linha do gabarito da prova azul,
já que todas as provas agora estão em ordem e o gabarito é único.

``` r
# Guardando os dados de resposta em arquivo
fwrite(RespostasEnem2013, "RespostasEnem2013.csv")
# Guardando os dados de gabarito em arquivo
fwrite(get.split.gab.cn, "GabaritoEnem2013.csv")

#ignore a 
get.respostas_all= fread("RespostasEnem2013.csv") # 
get.gabarito_all =fread("GabaritoEnem2013.csv") # 

set.only.line<-get.gabarito_all[1,]
```

Transformando a linha do gabarito em caracter e as respostas em matriz
pois usaremos a função “mult.choice” do pacote “ltm”.

``` r
key_azul<-as.character(set.only.line)

# Set matrix de resposta
set.respost<-as.matrix(get.respostas_all[,5:49]) # Set coluna onde estao as respostas
```

Agora checamos as respostas. Teremos 1 para acertos e 0 para erros.

``` r
# Compara o gabarito
as.binary<-mult.choice(set.respost, key_azul)
```

e somaremos quantos acertos cada candidato obteve na prova. Em seguida
adicionamos o numero de acertos no arquivo de respostas.

``` r
n.acert.by.partip <-data.frame(rowSums(as.binary))

setnames(n.acert.by.partip, "Acertos")

RespostasEnem2013Enotas<-bind_cols(list(get.respostas_all, n.acert.by.partip))
```

## Gerando os gráficos

Inseri aqui uma espécie de categoria das notas. Você pode adequar como
achar melhor. Exemplo:

``` r
#df$Category[df$NOTA_CN >= 300 & df$NOTA_CN <= 420] = "Baixa"
```

Enfim, o gráfico da relação Nota vs Acertos da prova de Ciências da
Natureza.

``` r
#my_y<-df$NOTA_CN  # 2013

#df$Category = factor(df$Category) 
#p0 <- ggplot(df, aes(x=Acertos, y=my_y,  colour=Category)) + geom_point(size=0.5) + theme_bw() +
#  scale_x_continuous(name="Numero de Acertos", breaks = seq(0,45,2)) + 
#  scale_y_continuous(name="NOTA", breaks = seq(0,1000,200), limits = c(300,950))+ 
#  theme(panel.grid = element_blank(), legend.position = "none")  #+ scale_colour_manual(values=cbPalette) # +
#  #geom_smooth(method="lm")
#p0
```

    ## Warning in (function (kind = NULL, normal.kind = NULL, sample.kind = NULL) : non-uniform 'Rounding' sampler used

<img src="Publish_files/figure-gfm/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

O gráfico a seguir corresponde ao número de acertos dos candidatos.

``` r
g <- ggplot(df, aes(x=Acertos)) + geom_histogram(binwidth = 1, colour="grey51", fill="darkcyan") +
  scale_x_continuous(name="N. Acertos", breaks = seq(0,45,3), limits = c(0,45)) +
  scale_y_continuous(name="Densidade")  + theme(panel.border = element_blank(), panel.grid = element_blank(), panel.background = element_blank())
# + theme_bw()
g
```

    ## Warning: Removed 2 rows containing missing values (geom_bar).

<img src="Publish_files/figure-gfm/pressure11-1.png" style="display: block; margin: auto;" />
<p align="justify">

É um material técnico. Espero que ainda assim ajude os iniciantes.

Nesse <a href="https://rpubs.com/coliveira/803625" target="_blank"> link
</a> há um breve estudo sobre as notas de corte para acesso em cursos na
Universidade Federal do Oeste do Pará por meio do ENEM.
</p>
