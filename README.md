# GExpDes - Interface Gráfica para o Pacote ExpDes

![](logo.png)

Os dados gerados a partir de um experimento são analisados segundo metodologias de Estatística Experimental. Para realizar essas análises, destacam-se a utilização da linguagem R, particularmente com o pacote **ExpDes**. Para quem não quer se preocupar com a linguagem R, esse trabalho apresenta uma aplicação em **Shiny** que possibilita o uso dos pacotes **ExpDes** e **labestData**. A interface reduz a complexidade da análise, sendo um auxiliar para a montagem dos comandos da linguagem R para realizá-la. Ao final, os comandos que foram necessários também são disponibilizados ao usuário.

Artigo do pacote publicado na Sigmae - [link](https://publicacoes.unifal-mg.edu.br/revistas/index.php/sigmae/article/view/951)

**Lançamento na 64ª Reunião Anual da Região Brasileira da Sociedade Internacional de Biometria (RBras) e 18º Simpósio de Estatística Aplicada à Experimentação Agronômica (SEAGRO)** - [link](https://www.rbras64.com.br/)

## Equipe de Desenvolvimento

**Universidade Federa de Santa Catarina - UFSC**
- Rogério Kormann (Agronomia)
- Eduardo Nunes Rosa (Agronomia)
- Crysttian Arantes Paixão
- Bruna Juliana Américo Machado (Medicina Veterinária)
- Felipe Weber Ferrarez (Agronomia)
- Amanda Mor (Medicina Veterinária)
- Douglas Rufino Vaz (Engenharia Florestal)

> Agradecimento especial João Vitor Berner Pereira (Agronomia)

**Universidade Federal de Alfenas**
- Eric Batista Ferreira
- Denismar Alves Nogueira

# Instalação
Para realizar a instalação do pacote gExpDes execute os passos 1 e 2 na sequência.


## **Passo 1 - Instale os pacotes:**

O GExpDes utiliza os seguintes pacotes do R: shiny, shinythemes, ExpDes.pt, rmarkdown, plotly, xtable, nortest, car, pracma, stringr e ds.

Para instalar os pacotes descritos, copie os códigos abaixo e cole no terminal do R.

**install.packages("shiny",dependencies=TRUE)**

**install.packages("shinythemes",dependencies=TRUE)**

**install.packages("ExpDes.pt",dependencies=TRUE)**

**install.packages("rmarkdown",dependencies=TRUE)**

**install.packages("plotly",dependencies=TRUE)**

**install.packages("xtable",dependencies=TRUE)**

**install.packages("nortest",dependencies=TRUE)**

**install.packages("car",dependencies=TRUE)**

**install.packages("pracma",dependencies=TRUE)**

**install.packages("stringr",dependencies=TRUE)**

**install.packages("ds",dependencies=TRUE)**

## **Passo 2 - Instale o pacote GExpDes e o labestData de acordo com o seu sistema operacional:**

### Opção A - Instalação direta do pacote 

### Para Linux

#### gExpDes

install.packages("https://github.com/gExpDes/gexpdes/raw/master/GExpDes_1.0.tar.gz", repos  =  NULL)

#### labestData

install.packages("http://leg.ufpr.br/~walmes/pacotes/labestData_0.1-17.458.tar.gz", repos = NULL)

### Para Windows

#### gExpDes

install.packages("https://github.com/gExpDes/gexpdes/raw/master/GExpDes_1.0.zip", repos  =  NULL)

#### labestData

install.packages("http://leg.ufpr.br/~walmes/pacotes/labestData_0.1.17.458.zip", repos = NULL)

### Opção B - Instalação direta do git

#### gExpDes

require(devtools)

install_git(url = "https://github.com/gExpDes/gexpdes.git", branch = "master",build_vignettes=TRUE)

#### labestData

install.packages("http://leg.ufpr.br/~walmes/pacotes/labestData_0.1.17.458.zip", repos = NULL)


Sobre o labestData, consultar [https://gitlab.c3sl.ufpr.br/pet-estatistica/labestData](https://gitlab.c3sl.ufpr.br/pet-estatistica/labestData)

# Executando

Após instalar o pacote GExpDes, digite os comandos no R:

**require(GExpDEs)**

**GExpDesView()**

> Nota: A interface aparecerá após a execução do comando **GExpDesView()**

## Sugestões e erros

Caso tenha alguma sugestão, pedimos que nos envie um e-mail (gexpdes@gmail.com) ou abra uma *issue* no git.

## Colaborações

Caso queira colaborar com o desenvolvimento do pacote, por favor, nos comunique via e-mail. (gexpdes@gmail.com), pois será um prazer contar com a sua ajuda.

## Notas da versão

A versão atual do pacote é a 1.0, lançada na 54ª RBRAS e 18º SEAGRO.
