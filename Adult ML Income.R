library(readr)
library(dplyr)
library(e1071)
library(partykit)
library(caret)
library(dplyr)
library(rpart)
library(rpart.plot)
library(wesanderson)
library(ggpubr)
library(ggalluvial)
income<-read_csv("adult.csv")
head(income)
colSums(is.na(income))
colSums(income=="?")
income.names <- c("Age",
                 "Workingclass",
                 "Final_Weight",
                 "Education",
                 "Education_num",
                 "Marital_Status",
                 "Occupation",
                 "Relationship",
                 "Race",
                 "Sex",
                 "Capital_gain",
                 "Capital_loss",
                 "Hours_per_week",
                 "Native_country",
                 "Income")
colnames(income) <- income.names
head(income)
colSums(income=="?")
income_clean<-income %>% 
  mutate(Occupation = replace(Occupation, Occupation == "?", "Undisclosed")) %>% 
  mutate(Workclass = replace(Workingclass, Workingclass == "?", "Undisclosed")) %>% 
  mutate(Native.country = replace(Native_country, Native_country == "?", "Undisclosed")) %>%  
  mutate_if(is.character,as.factor)
View(income_clean)
income_clean <- income_clean[, -c(length(income_clean), length(income_clean)-1)]
colSums(income_clean=="?")
income_clean<-income %>% 
  mutate(Occupation = replace(Occupation, Occupation == "?", "Undisclosed")) %>% 
  mutate(Workingclass = replace(Workingclass, Workingclass == "?", "Undisclosed")) %>% 
  mutate(Native_country = replace(Native_country, Native_country == "?", "Undisclosed")) %>%  
  mutate_if(is.character,as.factor)
View(income_clean)
income_clean <- income %>%
  mutate(Occupation = replace(Occupation, Occupation == "?", "Undisclosed")) %>%
  mutate(Workingclass = replace(Workingclass, Workingclass == "?", "Undisclosed")) %>%
  mutate(Native_country = replace(Native_country, Native_country == "?", "Undisclosed")) %>%
  mutate_if(is.character, as.factor)

a <- income_clean %>%
  group_by(Marital_Status, Income) %>%
  count() %>%
  ggplot(aes(reorder(Marital_Status, n), n)) +
  geom_col(aes(fill = Income)) +
  theme_light() +
  coord_flip() +
  scale_fill_manual(values = wes_palette(n = 5, name = "Cavalcanti1")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.title = element_blank(), plot.title = element_text(size = 10)) +
  ggtitle("Marital Status")

b <- income_clean %>%
  group_by(Relationship, Income) %>%
  count() %>%
  ggplot(aes(reorder(Relationship, n), n)) +
  geom_col(aes(fill = Income)) +
  theme_light() +
  coord_flip() +
  scale_fill_manual(values = wes_palette(n = 4, name = "Cavalcanti1")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.title = element_blank(), legend.position = "none", plot.title = element_text(size = 10)) +
  ggtitle("Relationship Status")

figure <- ggarrange(a, b, common.legend = TRUE, legend = "bottom")
annotate_figure(figure, top = text_grob("Levels of Income for Relationship and Marital Status", color = "black", face = "bold", size = 15))
c <- income_clean %>% 
  group_by(Occupation, Income) %>% 
  count() %>% 
  ggplot(aes(reorder(Occupation, n), n)) +
  geom_col(aes(fill = Income)) +
  theme_light() +
  coord_flip() +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        legend.title = element_blank(), 
        plot.title = element_text(size = 10)) +
  scale_fill_manual(values = wes_palette(n = 2, name = "Cavalcanti1")) +
  ggtitle("Occupation")

d <- income_clean %>% 
  group_by(Workingclass, Income) %>% 
  count() %>% 
  ggplot(aes(reorder(Workingclass, n), n)) +
  geom_col(aes(fill = Income)) +
  theme_light() +
  coord_flip() +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        legend.title = element_blank(), 
        plot.title = element_text(size = 10)) +
  scale_fill_manual(values = wes_palette(n = 2, name = "Cavalcanti1")) +
  ggtitle("Working Class")

figure <- ggarrange(c, d, common.legend = TRUE, legend = "bottom")
annotate_figure(figure, top = text_grob("Income Levels in Different Occupation & Class", color = "black", face = "bold", size = 15))
c <- income_clean %>% 
  group_by(Race, Income) %>% 
  count() %>% 
  ggplot(aes(x = reorder(Race, n), y = n)) +
  geom_col(aes(fill = Income), position = "dodge") +
  coord_flip() +
  theme_light() +
  scale_fill_manual(values = wes_palette(n = 2, name = "Cavalcanti1")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12, face = "bold")) +
  ggtitle("Income Levels for Race")

c
c <- income_clean %>% 
  ggplot(aes(x = Hours_per_week)) +
  geom_histogram(aes(fill = Income), bins = 20, position = "dodge") +
  theme_light() +
  scale_fill_manual(values = wes_palette(n = 2, name = "Cavalcanti1")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12, face = "bold")) +
  ggtitle("Hours Worked & Income")

c
c <- income_clean %>% 
  ggplot(aes(x = reorder(Education, n), y = n)) +
  geom_col(aes(fill = Income), position = "dodge") +
  theme_light() +
  coord_flip() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12, face = "bold")) +
  scale_fill_manual(values = wes_palette(n = 2, name = "Cavalcanti1")) +
  ggtitle("Education Level & Income Level")

c
c <- income_clean %>%
  group_by(Education, Income) %>%
  count() %>%
  ggplot(aes(x = reorder(Education, n), y = n)) +
  geom_col(aes(fill = Income), position = "dodge") +
  theme_light() +
  coord_flip() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12, face = "bold")) +
  scale_fill_manual(values = wes_palette(n = 2, name = "Cavalcanti1")) +
  ggtitle("Education Levels & Income")

c
set.seed(110)

index <- sample(x = nrow(income_clean), nrow(income_clean) * 0.7)

income_train <- income_clean[index,]
income_test <- income_clean[-index,]
prop.table(table(income_train$Income))
table(income_train$Income)
