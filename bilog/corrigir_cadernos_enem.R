itens = read.table ('D:/Microdados/2019/ITENS_PROVA_2019.csv', sep = ";", header = T)

banco.novo = data.frame()

# corrigir a prova de acordo com o caderno
for (caderno in 515:518)
{
  
  # selecionar os respondentes do caderno
  resp.cad = filter (amostra, CO_PROVA_MT == caderno)
  
  # selecionar o gabarito
  gabarito = resp.cad [1, paste0 ('G', 1:45)] %>%
    t()
  
  # pegar os códigos dos itens
  cod.itens = filter (itens, CO_PROVA == caderno)$CO_ITEM
  
  # corrigir (trnsformar em 0 e 1)
  correcao = key2binary (resp.cad [,paste ("I", 1:45, sep = "")], gabarito)
  
  # juntar o corrigido com o objeto resp.cad, mas sem as marcações e os gabaritos
  banco.novo. = cbind (resp.cad[, -c(23:112)], correcao)
  
  # nomear os itens com seus respectivos códigos. Isso é importante para o passo seguinte, 
  # pois o empilhamento (rbind) será feito com base no nome das variáveis. Como cada caderno 
  # possui uma ordem diferente dos itens, o I1 do caderno 459 não é o mesmo I1 do caderno 460. 
  # Mas os códigos dos itens se mantêm. Se este procedimento de renomear não for feito, o 
  # que acontece é que o empilhamento será feito de maneira errada. Vai juntar o I1 do 459 
  # com o I1 do 460. Com os novos nomes, o item 82123 será o mesmo em todos os cadernos.
  names (banco.novo.)[23:67] = cod.itens
  
  # agora sim o empilhamento
  banco.novo = rbind (banco.novo, banco.novo.)
  
}
