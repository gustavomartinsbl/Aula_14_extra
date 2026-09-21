##### Atividade aula 14 - extra - banco 2 - equivalente ao SINASC ######
##### Na branch main inserir os comandos e salvar o script com o nome script_aula_14_extra#####

# Tarefa 1: Leitura do banco de dados banco 2 = SINASC.csv com o nome de dados_aula14
# Ler o arquivo, verificar estrutura dos dados e dar uma olhada nos dados

dados_aula14 <- read.csv("banco 2 = SINASC.csv",
                         sep = ";",
                         header = TRUE,
                         stringsAsFactors = FALSE)

str(dados_aula14)
head(dados_aula14)
names(dados_aula14)

# Ao terminar a Tarefa 1 commit com a mensagem " script - tarefa 1" e envie para o repositório Aula_14_Extra


# Tarefa 2: Manipulação dos dados
# Padronizar as categorias SEXO_PROPRIETARIO para Masculino e Feminino
# Atribuir legendas para a variável TIPO_VEICULO, sendo 1: Carro e 2: Moto
# Criar uma nova variável em dados_aula14 F_IDADE categorizando as idades em: 22 a 34, 35 a 45

dados_aula14$SEXO_PROPRIETARIO <- ifelse(
  tolower(dados_aula14$SEXO_PROPRIETARIO) == "feminino",
  "Feminino",
  "Masculino"
)

# Atribuir legendas para TIPO_VEICULO
dados_aula14$TIPO_VEICULO <- factor(
  dados_aula14$TIPO_VEICULO,
  levels = c(1, 2),
  labels = c("Carro", "Moto")
)

# Criar F_IDADE
dados_aula14$F_IDADE <- ifelse(
  dados_aula14$IDADE_PROPRIETARIO >= 22 &
    dados_aula14$IDADE_PROPRIETARIO <= 34,
  "22 a 34",
  ifelse(
    dados_aula14$IDADE_PROPRIETARIO >= 35 &
      dados_aula14$IDADE_PROPRIETARIO <= 45,
    "35 a 45",
    NA
  )
)

# Ao terminar a Tarefa 2 commit com a mensagem " script - tarefa 1 a 2" e envie para o repositório Aula_14_Extra


# Tarefa 3: Leitura do banco de dados Tabela_PAM.csv (com o nome tabela_pam) e:
# agregar ao banco dados_aula14 as informações de VALOR_P10 e VALOR_P90
# criar a variável PAM (somente quando TIPO_VEICULO = "Carro"), de acordo com IDADE_PROPRIETARIO e SEXO_PROPRIETARIO, com as seguintes categorias:
# PAM = "PIC", se VALOR_VEICULO < VALOR_P10; "AIC", se VALOR_P10 <= VALOR_VEICULO <= VALOR_P90; "GIC", se VALOR_VEICULO > VALOR_P90

# Leitura da Tabela_PAM
tabela_pam <- read.csv("Tabela_PAM ..csv",
                       sep = ";",
                       header = TRUE,
                       stringsAsFactors = FALSE)

# Agregar VALOR_P10 e VALOR_P90 ao banco principal
dados_aula14 <- merge(
  dados_aula14,
  tabela_pam,
  by = c("IDADE_PROPRIETARIO", "SEXO_PROPRIETARIO"),
  all.x = TRUE
)

# Criar PAM somente para Carro
dados_aula14$PAM <- NA

dados_aula14$PAM[
  dados_aula14$TIPO_VEICULO == "Carro" &
    dados_aula14$VALOR_VEICULO < dados_aula14$VALOR_P10
] <- "PIC"

dados_aula14$PAM[
  dados_aula14$TIPO_VEICULO == "Carro" &
    dados_aula14$VALOR_VEICULO >= dados_aula14$VALOR_P10 &
    dados_aula14$VALOR_VEICULO <= dados_aula14$VALOR_P90
] <- "AIC"

dados_aula14$PAM[
  dados_aula14$TIPO_VEICULO == "Carro" &
    dados_aula14$VALOR_VEICULO > dados_aula14$VALOR_P90
] <- "GIC"

# Conferir
table(dados_aula14$PAM, useNA = "ifany")

# Ao terminar a Tarefa 3 commit com a mensagem " script - tarefa 1 a 3" e envie para o repositório Aula_14_Extra

 
# Tarefa 4: Criar o banco de dados BACO_AULA14_RJ, POR MUNICÍPIO, com as seguintes variáveis listadas abaixo. 
# Variáveis que se referem a medidas de posição e de dispersão devem ser calculadas sem considerar NAs

# Atenção: a 1a linha do banco deve ser da UF 33
# ANO: 2025
# NIVEL: UF ou MUNICIPIO
# CODIGO: código do municipio (ou da UF)
# TVV: total de veiculos vendidos
# TVRC: total de vendas com registros completos nas 5 variáveis originais de banco 2 = SINASC
# TVVF: total de veículos vendidos para mulher
# TVVM: total de veículos vendidos para homem
# TVCF: total de carros vendidos para mulheres
# TVCM: total de carros vendidos para homens
# TVMF: total de motos vendidas para mulheres
# TVMM: total de motos vendidas para homens
# TVC_22_34: total de carros vendidos para pessoas na faixa etária de 22 a 34 anos
# TVC_35_45: total de carros vendidos para pessoas na faixa etária de 35 a 45 anos
# IMVCF: idade média das mulheres proprietárias de veículo carro 
# DPVCF: desvio-padrão das idades das mulheres proprietárias de veículo carro
# IVCF_P25: percentil 25 das idades das mulheres proprietárias de veículo carro
# IVCF_P50: percentil 50 das idades das mulheres proprietárias de veículo carro
# IVCF_P75: percentil 75 das idades das mulheres proprietárias de veículo carro
# IMVMM: idade média dos homens proprietários de veículo moto 
# DPVMM: desvio-padrão das idades dos homens proprietários de veículo moto
# IVMM_P25: percentil 25 das idades dos homens proprietários de veículo moto
# IVMM_P50: percentil 50 das idades dos homens proprietários de veículo moto
# IVMM_P75: percentil 75 das idades dos homens proprietários de veículo moto
# TPIC: total de compradores com perfil PIC
# TAIC: total de compradores com perfil AIC
# TGIC: total de compradores com perfil GIC


media <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  mean(x, na.rm = TRUE)
}


desvio <- function(x) {
  if (sum(!is.na(x)) <= 1) {
    return(NA)
  }
  sd(x, na.rm = TRUE)
}


percentil <- function(x, p) {
  if (all(is.na(x))) {
    return(NA)
  }
  as.numeric(quantile(x, probs = p, na.rm = TRUE))
}



gera_resumo <- function(dados, nivel, codigo) {
  

  mulheres_carro <- dados[
    dados$SEXO_PROPRIETARIO == "Feminino" &
      dados$TIPO_VEICULO == "Carro",
  ]

  homens_moto <- dados[
    dados$SEXO_PROPRIETARIO == "Masculino" &
      dados$TIPO_VEICULO == "Moto",
  ]
  
  data.frame(
    ANO = 2025,
    NIVEL = nivel,
    CODIGO = codigo,
    
    TVV = nrow(dados),
    
    TVRC = sum(complete.cases(
      dados[, c("MUNICIPIO",
                "SEXO_PROPRIETARIO",
                "IDADE_PROPRIETARIO",
                "TIPO_VEICULO",
                "VALOR_VEICULO")]
    )),
    
    TVVF = sum(dados$SEXO_PROPRIETARIO == "Feminino", na.rm = TRUE),
    
    TVVM = sum(dados$SEXO_PROPRIETARIO == "Masculino", na.rm = TRUE),
    
    TVCF = sum(
      dados$SEXO_PROPRIETARIO == "Feminino" &
        dados$TIPO_VEICULO == "Carro",
      na.rm = TRUE
    ),
    
    TVCM = sum(
      dados$SEXO_PROPRIETARIO == "Masculino" &
        dados$TIPO_VEICULO == "Carro",
      na.rm = TRUE
    ),
    
    TVMF = sum(
      dados$SEXO_PROPRIETARIO == "Feminino" &
        dados$TIPO_VEICULO == "Moto",
      na.rm = TRUE
    ),
    
    TVMM = sum(
      dados$SEXO_PROPRIETARIO == "Masculino" &
        dados$TIPO_VEICULO == "Moto",
      na.rm = TRUE
    ),
    
    TVC_22_34 = sum(
      dados$TIPO_VEICULO == "Carro" &
        dados$F_IDADE == "22 a 34",
      na.rm = TRUE
    ),
    
    TVC_35_45 = sum(
      dados$TIPO_VEICULO == "Carro" &
        dados$F_IDADE == "35 a 45",
      na.rm = TRUE
    ),
    
    IMVCF = media(mulheres_carro$IDADE_PROPRIETARIO),
    
    DPVCF = desvio(mulheres_carro$IDADE_PROPRIETARIO),
    
    IVCF_P25 = percentil(
      mulheres_carro$IDADE_PROPRIETARIO, 0.25
    ),
    
    IVCF_P50 = percentil(
      mulheres_carro$IDADE_PROPRIETARIO, 0.50
    ),
    
    IVCF_P75 = percentil(
      mulheres_carro$IDADE_PROPRIETARIO, 0.75
    ),
    
    IMVMM = media(homens_moto$IDADE_PROPRIETARIO),
    
    DPVMM = desvio(homens_moto$IDADE_PROPRIETARIO),
    
    IVMM_P25 = percentil(
      homens_moto$IDADE_PROPRIETARIO, 0.25
    ),
    
    IVMM_P50 = percentil(
      homens_moto$IDADE_PROPRIETARIO, 0.50
    ),
    
    IVMM_P75 = percentil(
      homens_moto$IDADE_PROPRIETARIO, 0.75
    ),
    
    TPIC = sum(dados$PAM == "PIC", na.rm = TRUE),
    
    TAIC = sum(dados$PAM == "AIC", na.rm = TRUE),
    
    TGIC = sum(dados$PAM == "GIC", na.rm = TRUE)
  )
}


linha_uf <- gera_resumo(
  dados_aula14,
  "UF",
  33
)


municipios <- sort(unique(dados_aula14$MUNICIPIO))

linhas_municipios <- do.call(
  rbind,
  lapply(municipios, function(m) {
    
    dados_municipio <- dados_aula14[
      dados_aula14$MUNICIPIO == m,
    ]
    
    gera_resumo(
      dados_municipio,
      "MUNICIPIO",
      m
    )
  })
)


BANCO_AULA14_RJ <- rbind(
  linha_uf,
  linhas_municipios
)


str(BANCO_AULA14_RJ)
head(BANCO_AULA14_RJ)

# Ao terminar a Tarefa 4 commit com a mensagem " script - tarefa 1 a 4" e envie para o repositório Aula_14_Extra


# Tarefa 5: Exportar o banco de dados BANCO_AULA14_RJ com o nome BANCO_AULA14_RJ.csv

write.csv(
  BANCO_AULA14_RJ,
  "BANCO_AULA14_RJ.csv",
  row.names = FALSE
)

# Ao terminar a Tarefa 5 commit com a mensagem "dados e script - Etapa 2" e envie para o repositório Aula_14_Extra
