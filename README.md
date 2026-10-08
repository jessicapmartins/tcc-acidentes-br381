# Previsão de acidentes graves na BR-381 (MG)

Modelo para estimar a probabilidade de um acidente na BR-381, em Minas Gerais, ter **feridos graves ou mortos**. O projeto compara estatística tradicional, Machine Learning e Deep Learning. Foi o trabalho de conclusão da Pós-graduação em Analytics e Inteligência Artificial – Data Science da FIA Business School (2024).

**Ferramentas:** R · Python · pandas · scikit-learn · feature-engine · scorecardpy · CatBoost · XGBoost · LightGBM · TensorFlow/Keras

---

## Estrutura do projeto (CRISP-DM)

### 1. Entendimento do negócio
Minas Gerais lidera os acidentes de trânsito em rodovias federais, e a BR-381 concentra boa parte deles. O cenário de negócio, construído para o trabalho acadêmico, é o seguinte: ajudar a Polícia Rodoviária Federal a planejar a fiscalização e o acionamento de equipes de apoio (SAMU, Bombeiros), antecipando quais ocorrências têm mais risco de ser graves.

**Pergunta:** dada uma nova ocorrência na BR-381/MG, qual a probabilidade de ela ter feridos graves ou mortos?

**Prioridade do modelo:** sensibilidade. Deixar de prever um acidente grave (falso negativo) custa mais do que mobilizar equipes sem necessidade (falso positivo).

### 2. Entendimento dos dados
- **Fonte:** [Dados Abertos da PRF – Acidentes](https://www.gov.br/prf/pt-br/acesso-a-informacao/dados-abertos/dados-abertos-da-prf).
- **Período:** jan/2022 a out/2023 para o histórico e nov/2023 para o teste fora do tempo (out-of-time) da regressão tradicional.
- **Volume:** 125.563 ocorrências no Brasil. Depois do filtro MG + BR-381, restaram **4.605 ocorrências**.
- **Variável resposta:** `houve_mortos_ou_feridos_graves`. Ocorrências com feridos graves ou mortos são **24%** da base.

### 3. Preparação dos dados
Feita em R (`r/01_higienizacao_eda_regressao_logistica.R`):
- **Variáveis criadas:** ano/mês, período do dia, região, causa do acidente agrupada, tipo de acidente categorizado e zona.
- **Variáveis removidas:** as que vazariam a resposta (mortos, feridos etc.) e as de granularidade muito alta (km, município).
- **Análise exploratória:** univariada e bivariada.
- **Seleção de variáveis:** por Information Value (IV) e significância estatística.

**Variáveis finais do modelo escolhido:** pessoas envolvidas, veículos envolvidos, causa do acidente (agrupada), tipo de pista e tipo de acidente (categorizado).

### 4. Modelagem
| Abordagem | Modelos testados |
|---|---|
| Estatística tradicional | Regressão logística (R) |
| Machine Learning | Regressão logística, Árvore de Decisão, Random Forest, CatBoost, XGBoost, LightGBM, com validação cruzada estratificada (5 folds), ajuste de ponto de corte e RandomizedSearch |
| Deep Learning | Redes densas (com e sem regularização), CNN 1D, LSTM |

### 5. Avaliação
Base dividida em 70% treino (3.223) e 30% teste (1.382), estratificada pela variável resposta.

**Modelo escolhido: regressão logística (Machine Learning)**, com ponto de corte ajustado e hiperparâmetros otimizados:
- **Sensibilidade de 70%** e **AUC de 68%**;
- desempenho equilibrado entre treino e teste, sem sinais de overfitting;
- especificidade e acurácia aceitáveis para o problema.

Na comparação, a CNN chegou a uma sensibilidade maior, mas perdeu especificidade e teve o maior custo operacional entre os modelos.

**Custo operacional.** As premissas de custo (por ocorrência) foram definidas para o estudo:

| Situação | Custo |
|---|---|
| Acidente grave | R$ 2.600 |
| Acidente não previsto (falso negativo) | R$ 3.000 |
| Mobilização preventiva sem acidente (falso positivo) | R$ 400 |

Com essas premissas, o modelo **eleva** o custo total na base de teste. Isso acontece porque há mais falsos positivos. Foi uma escolha consciente: priorizar a detecção de ocorrências graves, que envolvem vidas. A função de custo está no notebook (`calcular_custo_prevencao`).

### 6. Próximos passos
- Novas fontes de dados: perfil das vítimas e dados meteorológicos detalhados.
- Técnicas de balanceamento, como SMOTE.
- Otimizar o equilíbrio entre sensibilidade e especificidade, porque a precisão ainda é baixa.

---

## Como reproduzir
```
├── data/base_acidente_transito_MG_381_v2.csv   # base tratada (MG, BR-381), gerada pelo script R
├── r/01_higienizacao_eda_regressao_logistica.R  # preparação, EDA e regressão logística tradicional
├── notebooks/02_modelagem_ml_deep_learning.ipynb # Machine Learning, Deep Learning e custo
└── docs/apresentacao_final_tcc.pdf               # apresentação final com todos os resultados
```
1. Para rodar o script R desde o início, baixe os arquivos `datatran` de 2022 e 2023 no portal da PRF e salve em `data/raw/`.
2. O notebook usa direto a base tratada em `data/`. Ele foi desenvolvido no Google Colab. Instale as dependências com `pip install -r requirements.txt`.

---

**Autora:** Jéssica Priscila de Oliveira Martins · [LinkedIn](https://www.linkedin.com/in/jessica-oliveira-martins/)
