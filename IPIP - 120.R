# Load some libraries
if (!require(tidyverse)) {install.packages("tidyverse")
  library(tidyverse)}
if (!require(lubridate)) {install.packages("lubridate")
  library(lubridate)}
if (!require(psych)) {install.packages("psych")
  library(psych)}
if (!require(dlypr)) {install.packages("dlypr")
  library(dlypr)}
if (!require(formr)) {install.packages("formr")
  library(formr)}
if (!require(sm)) {install.packages("sm")
  library(sm)}
if (!require(hrbrthemes)) {install.packages("hrbrthemes")
  library(hrbrthemes)}
if (!require(reshape)) {install.packages("reshape")
  library(reshape)}

# Source the DOD_Library.R script
#source("libraries/DOD_Library.R")

# Import data

data <- subset(read.csv("File.csv"),
              select = -c(id, age, city, youthgroup_time))

data <- data[complete.cases(data),]

# Create keys for scoring

key.list <- list(anxiety=c(1, 31, 61, 91),
                 anger=c(6, 36, 66, -96),
                 depression=c(11, 41, 71, -101),
                 selfconsciousness=c(11, 41, 71, -101),
                 immoderation=c(21, -51, -81, -111),
                 vulnerability=c(26, 56, 86, -116),
                 friendliness=c(2, 32, 62, -92),
                 gregariousness=c(7, 37, -67, -97),
                 assertiveness=c(12, 42, 72, -102),
                 activitylevel=c(17, 47, 77, -107),
                 excitementseeking=c(22, 52, 82, 112),
                 cheerfulness=c(27, 57, 87, 117),
                 imagination=c(3, 33, 63, 93),
                 artisticinterest=c(8, 38, -68, -98),
                 emotionality=c(13, 43, -73, -103),
                 adventurousness=c(18, -48, -78, -108),
                 intellect=c(23, -53, -83, -113),
                 liberalism=c(28, 58, -88, -118),
                 trust=c(4, 34, 64, -94),
                 morality=c(-9, -39, -69, -99),
                 altruism=c(44, 14, -74, -104),
                 cooperation=c(-19, -49, -79, -109),
                 modesty=c(-24, -54, -84, -114),
                 sympathy=c(29, 59, -89, -119),
                 selfefficacy=c(5, 35, 65, 95),
                 orderliness=c(10, -40, -70, -100),
                 dutifulness=c(15, 45, -75, -105),
                 achievementestriving=c(50, 20, -110, -80),
                 selfdiscipline=c(25, 55, -85, -115),
                 cautiousness=c(-30, -60, 90, -120))

keys <- make.keys(data, keys.list = key.list)

# Score the results and obtain a data frame

data_scores <- scoreVeryFast(keys, data, min = 1, max = 5, totals = TRUE)

data_scores_df <- as.data.frame(data_scores)

### Add qualitative data

ql_data <- subset(read.csv("File.csv"),
               select = c(id, age, city, youthgroup_time))

data_scores_df <- cbind(ql_data, data_scores_df)


# Getting scores for the scales

attach(data_scores_df) #make variables available


## Neuroticism 

neuroticism <- data_scores_df[c("anxiety", 
                                "anger", 
                                "depression", 
                                "selfconsciousness", 
                                "immoderation")]

neuro_m <- round(rowMeans(neuroticism),2)
t_score_neuro <- round((10*((rowSums(neuroticism)-66.67)/16.59)+50),2)

neuroticism <- cbind(id,age, city, neuroticism, as.numeric(neuro_m), t_score_neuro)

## Extraversion

extraversion <- data_scores_df[c("friendliness", 
                                 "gregariousness",
                                 "assertiveness",
                                 "activitylevel",
                                 "excitementseeking",
                                 "cheerfulness")]

extra_m <- round(rowMeans(extraversion),2)
t_score_extra <- round((10*((rowSums(extraversion)-79.19)/15.55)+50),2)

extraversion <- cbind(id,age, city, extraversion, extra_m, t_score_extra)

## Openness

openness <- data_scores_df[c("imagination",
                             "artisticinterest",
                             "emotionality",
                             "adventurousness",
                             "intellect",
                             "liberalism")]

open_m <- round(rowMeans(openness),2)
t_score_open <- round((10*((rowSums(openness)-86.88)/12.75)+50),2)

openness <- cbind(id,age, city, openness, open_m, t_score_open)

## Agreeableness

agreeableness <- data_scores_df[c("trust",
                                  "morality",
                                  "altruism",
                                  "cooperation",
                                  "modesty",
                                  "sympathy")]

agreeab_m <- round(rowMeans(agreeableness),2)
t_score_agreeab <- round((10*((rowSums(agreeableness)-84.21)/13.55)+50),2)

agreeableness <- cbind(id,age, city, agreeableness, agreeab_m, t_score_agreeab)

## Conscientiousness
conscientiousness <- data_scores_df[c("selfefficacy",
                                      "orderliness",
                                      "dutifulness",
                                      "achievementestriving",
                                      "selfdiscipline",
                                      "cautiousness")]

consc_m <- round(rowMeans(conscientiousness),2)
t_score_consc <- round((10*((rowSums(conscientiousness)-85.76)/14.27)+50),2)

conscientiousness <- cbind(id,age, city, conscientiousness, consc_m, t_score_consc)

# Data frame of means

data_means <- as.data.frame(cbind(id, 
                                  city, 
                                  neuro_m, 
                                  extra_m, 
                                  open_m, 
                                  agreeab_m, 
                                  consc_m))

data_means$neuro_m <- as.numeric(data_means$neuro_m) 
data_means$extra_m <- as.numeric(data_means$extra_m) 
data_means$open_m <- as.numeric(data_means$open_m) 
data_means$agreeab_m <- as.numeric(data_means$agreeab_m) 
data_means$consc_m <- as.numeric(data_means$consc_m) 

# Get the results for means

Barcelona_means <- as.data.frame(subset(data_means, city == "Barcelona"))
Madrid_means <- as.data.frame(subset(data_means, city == "Madrid"))
Valencia_means <- as.data.frame(subset(data_means, city == "Valencia"))
Málaga_means <- as.data.frame(subset(data_means, city == "Málaga"))

# Data frame of t scores

data_t_scores <- as.data.frame(cbind(id, 
                                     city, 
                                     t_score_neuro, 
                                     t_score_extra, 
                                     t_score_open, 
                                     t_score_agreeab, 
                                     t_score_consc))

data_t_scores$t_score_neuro <- as.numeric(data_t_scores$t_score_neuro) 
data_t_scores$t_score_extra <- as.numeric(data_t_scores$t_score_extra) 
data_t_scores$t_score_open <- as.numeric(data_t_scores$t_score_open) 
data_t_scores$t_score_agreeab <- as.numeric(data_t_scores$t_score_agreeab) 
data_t_scores$t_score_consc <- as.numeric(data_t_scores$t_score_consc) 

Barcelona_t <- as.data.frame(subset(data_t_scores, city == "Barcelona"))
Madrid_t <- as.data.frame(subset(data_t_scores, city == "Madrid"))
Valencia_t <- as.data.frame(subset(data_t_scores, city == "Valencia"))
Málaga_t <- as.data.frame(subset(data_t_scores, city == "Málaga"))


# Boxplots

## Neuroticism

data_t_scores %>%
  ggplot( aes(x=city, y=t_score_neuro, fill=city)) +
  geom_boxplot() +
  geom_jitter(color="blue", size=2, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Neuroticism") +
  xlab("") +
  ylab("Neuroticism")

ggstatsplot::ggbetweenstats(data = data_t_scores,
                            x = "city",
                            y = "t_score_neuro")

## Extraversion

data_t_scores %>%
  ggplot( aes(x=city, y=t_score_extra, fill=city)) +
  geom_boxplot() +
  geom_jitter(color="blue", size=2, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Extraversion") +
  xlab("") +
  ylab("Extraversion")

ggstatsplot::ggbetweenstats(data = data_t_scores,
                            x = "city",
                            y = "t_score_extra")

## Openness

data_t_scores %>%
  ggplot( aes(x=city, y=t_score_open, fill=city)) +
  geom_boxplot() +
  geom_jitter(color="blue", size=2, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Openness") +
  xlab("") +
  ylab("Openness")

ggstatsplot::ggbetweenstats(data = data_t_scores,
                            x = "city",
                            y = "t_score_open")

## Agreeableness

data_t_scores %>%
  ggplot( aes(x=city, y=t_score_agreeab, fill=city)) +
  geom_boxplot() +
  geom_jitter(color="blue", size=2, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Agreeableness") +
  xlab("") +
  ylab("Agreeableness")

ggstatsplot::ggbetweenstats(data = data_t_scores,
                            x = "city",
                            y = "t_score_agreeab")

## Conscientiousness

data_t_scores %>%
  ggplot( aes(x=city, y=t_score_consc, fill=city)) +
  geom_boxplot() +
  geom_jitter(color="blue", size=2, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Conscientiousness") +
  xlab("") +
  ylab("Conscientiousness")

ggstatsplot::ggbetweenstats(data = data_t_scores,
                            x = "city",
                            y = "t_score_consc")

## t-scores for the Big Five

BF_t_scores <- melt(data_t_scores)
boxplot(data=BF_t_scores, value~variable)
  
ggplot(BF_t_scores, aes(x = variable, y = value, color = city)) +  # ggplot function
  geom_boxplot() +
  ggtitle("Big Five t scores") +
  xlab("Big Five") +
  ylab("Scores")

## Neuroticism subscales

n1 <- melt(neuroticism, id = "city")
n1 <- filter(n1, variable != "id" & 
               variable != "as.numeric(neuro_m)" & 
               variable != "age")
n1$value <- as.numeric(n1$value)

ggplot(n1, aes(x = variable, y = value, color = city)) +  # ggplot function
  geom_boxplot() +
  ggtitle("Neuroticism") +
  xlab("Subscales") +
  ylab("Neuroticism")

## Extraversion subcales

ex1 <- melt(extraversion, id = "city")
ex1 <- filter(ex1, variable != "id" & 
                   variable != "extra_m" & 
                   variable != "age")
ex1$value <- as.numeric(ex1$value)

ggplot(ex1, aes(x = variable, y = value, color = city)) +  # ggplot function
  geom_boxplot() +
  ggtitle("Extraversion") +
  xlab("Subscales") +
  ylab("Extraversion")


## Openness subcales

op1 <- melt(openness, id = "city")
op1 <- filter(op1, variable != "id" & 
                variable != "open_m" & 
                variable != "age")
op1$value <- as.numeric(op1$value)

ggplot(op1, aes(x = variable, y = value, color = city)) +  # ggplot function
  geom_boxplot() +
  ggtitle("Openness") +
  xlab("Subscales") +
  ylab("Openness")

## Agreeableness subcales

ag1 <- melt(agreeableness, id = "city")
ag1 <- filter(ag1, variable != "id" & 
                variable != "agreeab_m" & 
                variable != "age")
ag1$value <- as.numeric(ag1$value)

ggplot(ag1, aes(x = variable, y = value, color = city)) +  # ggplot function
  geom_boxplot() +
  ggtitle("Agreeableness") +
  xlab("Subscales") +
  ylab("Agreeableness")

## Conscientiousness subcales

c1 <- melt(conscientiousness, id = "city")
c1 <- filter(c1, variable != "id" & 
               variable != "consc_m" & 
               variable != "age")
c1$value <- as.numeric(c1$value)

ggplot(c1, aes(x = variable, y = value, color = city)) +  # ggplot function
  geom_boxplot() +
  ggtitle("Conscientiousness") +
  xlab("Subscales") +
  ylab("Conscientiousness")



# Additional code
bcn_sco_df %>% 
  ggplot(aes (x = anxiety)) +
  geom_histogram()


bcn_sco_df %>% 
  ggplot(aes(x = morality, y = liberalism)) +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(x = intellect, y = friendliness)) +
  geom_point() +
  geom_smooth(method = "lm")


cor(bcn_sco_df$morality, bcn_sco_df$liberalism)
cor(bcn_sco_df$intellect, bcn_sco_df$friendliness)




bcn_means %>% 
  ggplot(aes(y = neuro_m, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = anxiety, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = anger, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = depression, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = selfconsciousness, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = immoderation, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = vulnerability, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = friendliness, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = gregariousness, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = assertiveness, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = activitylevel, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = excitementseeking, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = cheerfulness, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = imagination, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = artisticinterest, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = emotionality, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = adventurousness, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = intellect, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = liberalism, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = trust, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = morality, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = altruism, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = cooperation, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = modesty, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = sympathy, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = selfefficacy, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = orderliness, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = dutifulness, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = achievementestriving, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = selfdiscipline, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

bcn_sco_df %>% 
  ggplot(aes(y = cautiousness, x = city, fill = city)) +
  geom_boxplot() +
  geom_point()

multi.hist(bcn_sco_df[3:32])

# Correlations of all the subscales
round(cor(bcn_sco_df[3:32],use="pairwise"),2)
