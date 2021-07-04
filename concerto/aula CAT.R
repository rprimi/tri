# aula CAT USF 2021

# prepara ambiente para análise
install.packages("catIrt")

library("catIrt")

#########################
# Binary Response Model #
#########################
set.seed(888)

# gera um banco com 100 itens no modelo 2PL dicotômico:
b.params <- cbind(pos = 1:100, a = runif(100, .5, 1.5), b = rnorm(100, 0, 2), c = 0) # incluí a coluna posição


# importar base de itens
# ajustar o working directoty para onde estiver a sintaxe
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documentos icloud/UFSC/Pos-graduacao/Disciplinas/Aula CAT USF 2021")

library(readr)
banco_itens <- read_csv("concerto/base_inicial_opcoes.csv", 
                        col_types = cols(pos = col_integer()))

b.params<-as.matrix(banco_itens[,c(1,2,3,4)]) # extrai apenas os parametros dos itens

# valores iniciais

theta<- runif(1,-1,1) # theta inicial
finaliza<-0
nitens<-0
resp<-NULL
select<-NULL

while (finaliza==0) { 

nitens<-nitens+1

if(nitens==1) parametros<-b.params else parametros<-b.params[-select,]  # elimina itens já selecionados


item_select <- itChoose(left_par = parametros, mod = "brm",
                            numb = 1, n.select = 5,
                            cat_theta = theta,
                            select = "UW-FI",
                            at = "theta")

it_select<-item_select$params[[1]] # seleciona apenas a posição do item

select<-cbind(select,it_select)

print(paste("item", it_select, banco_itens$enunciado[it_select], "; b=",b.params[it_select,3]))

# importa dados do teclado

resposta<-as.numeric(readline(prompt = "Acerto ou erro? "))
resp<-cbind(resp,resposta)


# select<-c(60,  42,  28,  75,  86) # itens difíceis
# select<-c(21,  98,  18,  53,  58) # itens fáceis
# resp<- c(1, 1, 0, 1, 1) # padrão de respostas

theta_prov<-bmeEst(resp=resp,params=b.params[select,c(2,3,4)],mod="brm") 

theta<-theta_prov$theta

print(paste("Theta provisório=",theta_prov$theta))

if (nitens==6) finaliza<-1
if (theta_prov$sem<.05) finaliza<-1

}

theta_prov


