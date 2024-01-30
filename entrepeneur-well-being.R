library(openxlsx)
library(readxl)
library(dplyr)
library(broom)
library(tidyr)

setwd("C:/Users/natalr2/OneDrive - University of Illinois - Urbana/Desktop/Projects/anova_renda")

# Specify the file path
df <- read_excel("base_nova_sem missing_grouped.xls.xlsx", sheet = "Normal")


# Testando a normalidade dos dados
# Shapiro-Wilk test pra cada variavel
numerical_vars <- df[, c("renda", "tx_desemprego", "pib", "propriedade", "integridade", "sofrer_empregado", "sofrer_empreendedor")]
shapiro_results <- lapply(numerical_vars, shapiro.test)

# Display the results
for (i in seq_along(shapiro_results)) {
  cat("Variable:", names(shapiro_results)[i], "\n")
  cat("p-value:", shapiro_results[[i]]$p.value, "\n")
  cat("Test Statistic:", shapiro_results[[i]]$statistic, "\n")
  cat("\n")
}

# Concluindo que nenhuma das variaveis seguem uma normal
# Violando a normalidade para ANOVA, aplicando o teste de Kruskal-Wallis

########################### kruskal wallis #########################################
# Funcao que define a aplicacao e exibicao dos resultados do kruskal wallis
kruskal_wallis_test <- function(df, target_variable, numerical_vars) {
  # target variable
  target <- df[[target_variable]]
  
  # extraindo variaveis numericas de df
  vars <- df[, numerical_vars, drop = FALSE]
  
  # Rodando Kruskal-Wallis para cada numerical var, usando renda como target
  kruskal_results <- lapply(names(vars), function(var) {
    kruskal.test(vars[[var]], g = target)
  })
  
  # Resultados
  for (i in seq_along(kruskal_results)) {
    cat("Variable:", names(kruskal_results)[i], "\n")
    cat("Chi-squared:", kruskal_results[[i]]$statistic, "\n")
    cat("Degrees of Freedom:", kruskal_results[[i]]$parameter, "\n")
    cat("p-value:", kruskal_results[[i]]$p.value, "\n")
    cat("\n")
  }
}

# Aplicando a funcao usando renda como target sobre todas as variaveis numericas
kruskal_wallis_test(df, "renda", c("tx_desemprego", "pib", "propriedade", "integridade", "sofrer_empregado", "sofrer_empreendedor"))

########################### Testes de post hoc #########################
# No caso do KW, rankeia o valor e analisa os rankings
# Hipotese por tras considera que tem distribuicoes diferentes
# grupos com formatos iguais, compara medianas, no nosso caso, formatos diff
# compara-se as distribuicoes
# a fim de simplicacoes, consideramos que as H0, a mediana dos grupos sao iguais
# representa que, quando o p-value < 0.05 representa que pelo menos uma das medianas
# tem diff significativa

# a h0 sendo aceita, significa que tem diferenca, mas precisamos entender qual a diff
# por isso fazer teste de post hoc
install.packages("rstatix")
library(rstatix)

##### Teste de Dunn com ajuste do valor de p
#dunn_test(renda ~ paises, data = df, p.adjust.method = "bonferroni") # igual spss
dunn_test(tx_desemprego ~ renda, data = df, p.adjust.method = "bonferroni")
dunn_test(pib ~ renda, data = df, p.adjust.method = "bonferroni")
dunn_test(propriedade ~ renda, data = df, p.adjust.method = "bonferroni")
dunn_test(integridade ~ renda, data = df, p.adjust.method = "bonferroni")
dunn_test(sofrer_empregado ~ renda, data = df, p.adjust.method = "bonferroni")
dunn_test(sofrer_empreendedor ~ renda, data = df, p.adjust.method = "bonferroni")



