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
                  "income")
colnames(income) <- income.names
View(income)
colSums(is.na(income))
colSums(income=="?")
income_clean<-income %>% 
  mutate(Occupation = replace(Occupation, Occupation == "?", "Undisclosed")) %>% 
  mutate(Workingclass = replace(Workingclass, Workingclass == "?", "Undisclosed")) %>% 
  mutate(Native_country = replace(Native_country, Native_country == "?", "Undisclosed")) %>%  
  mutate_if(is.character,as.factor)
View(income_clean)
income_clean <- income_clean[, -c(length(income_clean), length(income_clean)-1)]
colSums(income_clean=="?")
income_clean<-income %>% 
  mutate(Occupation = replace(Occupation, Occupation == "?", "Undisclosed")) %>% 
  mutate(Workingclass = replace(Workingclass, Workingclass == "?", "Undisclosed")) %>% 
  mutate(Native_country = replace(Native_country, Native_country == "?", "Undisclosed")) %>%  
  mutate_if(is.character,as.factor)
income_clean <- income %>%
  mutate(Occupation = replace(Occupation, Occupation == "?", "Undisclosed")) %>%
  mutate(Workingclass = replace(Workingclass, Workingclass == "?", "Undisclosed")) %>%
  mutate(Native_country = replace(Native_country, Native_country == "?", "Undisclosed")) %>%
  mutate_if(is.character, as.factor)
colSums(income_clean=="?")
a <- income_clean %>%
  group_by(Marital_Status, income) %>%
  count() %>%
  ggplot(aes(reorder(Marital_Status, n), n)) +
  geom_col(aes(fill = income)) +
  theme_light() +
  coord_flip() +
  scale_fill_manual(values = wes_palette(n = 5, name = "Cavalcanti1")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.title = element_blank(), plot.title = element_text(size = 10)) +
  ggtitle("Marital Status")

b <- income_clean %>%
  group_by(Relationship, income) %>%
  count() %>%
  ggplot(aes(reorder(Relationship, n), n)) +
  geom_col(aes(fill = income)) +
  theme_light() +
  coord_flip() +
  scale_fill_manual(values = wes_palette(n = 4, name = "Cavalcanti1")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.title = element_blank(), legend.position = "none", plot.title = element_text(size = 10)) +
  ggtitle("Relationship Status")

figure <- ggarrange(a, b, common.legend = TRUE, legend = "bottom")
annotate_figure(figure, top = text_grob("Levels of Income for Relationship and Marital Status", color = "black", face = "bold", size = 15))
c <- income_clean %>% 
  group_by(Occupation, income) %>% 
  count() %>% 
  ggplot(aes(reorder(Occupation, n), n)) +
  geom_col(aes(fill = income)) +
  theme_light() +
  coord_flip() +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        legend.title = element_blank(), 
        plot.title = element_text(size = 10)) +
  scale_fill_manual(values = wes_palette(n = 2, name = "Cavalcanti1")) +
  ggtitle("Occupation")

d <- income_clean %>% 
  group_by(Workingclass, income) %>% 
  count() %>% 
  ggplot(aes(reorder(Workingclass, n), n)) +
  geom_col(aes(fill = income)) +
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
  group_by(Race, income) %>% 
  count() %>% 
  ggplot(aes(x = reorder(Race, n), y = n)) +
  geom_col(aes(fill = income), position = "dodge") +
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
  geom_histogram(aes(fill = income), bins = 20, position = "dodge") +
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
  geom_col(aes(fill = income), position = "dodge") +
  theme_light() +
  coord_flip() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12, face = "bold")) +
  scale_fill_manual(values = wes_palette(n = 2, name = "Cavalcanti1")) +
  ggtitle("Education Level & Income Level")

c
set.seed(110)

index <- sample(x = nrow(income_clean), nrow(income_clean) * 0.7)
income_train <- income_clean[index,]
income_test <- income_clean[-index,]
prop.table(table(income_train$income_clean))
table(income_train$income)
income_train_balanced<-downSample(x=income_train %>% select(-income),
                                  y=income_train$income,
                                  yname="income")
table(income_train_balanced$income)
dtree_income <- rpart(income~., data = income_train_balanced, method = 'class')

rpart.plot(dtree_income, extra = 106)
printcp(dtree_income)
pred_tree<-predict(dtree_income,income_test,type="class")
cm_tree<-confusionMatrix(pred_tree,income_test$income,positive =">50K" )
cm_tree
set.seed(417)

ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 3) 


mtry <- sqrt(ncol(income_train_balanced))
tunegrid <- expand.grid(.mtry=mtry)
randf_income <- train(income~., 
                   data=income_train_balanced, 
                   method='rf', 
                   metric='Accuracy', 
                   tuneGrid=tunegrid, 
                   trControl=ctrl)
saveRDS(randf_income, "rf_income.RDS")
income_forest<-readRDS("rf_income.RDS")
predicted_forest <- predict(income_forest, income_test)
cm_rf <- confusionMatrix(data = predicted_forest,
                         reference = income_test$income,positive =">50K")
cm_rf
varImp(income_forest)

which.min(dtree_income$cptable[,"xerror"])
pruned_tree<-prune(dtree_income, cp = dtree_income$cptable[4,"CP"])
rpart.plot(pruned_tree)
printcp(pruned_tree)
predicted_pruned<-predict(pruned_tree, income_test,type="class")
cm_tree_tuned<-confusionMatrix(predicted_pruned,income_test$income,positive =">50K")
cm_tree_tuned

df_recap <- data.frame(row.names = c("Decision Tree", "Random Forest", "Tuned Decision Tree"),
                       Sensitivity = c(cm_tree$byClass['Sensitivity'],
                                       cm_rf$byClass['Sensitivity'],
                                       cm_tree_tuned$byClass['Sensitivity']))
df_recap %>% arrange(desc(Sensitivity))
