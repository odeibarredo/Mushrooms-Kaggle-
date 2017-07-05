####    KAGGLE - MUSHROOMS    ####

#### LIBRARY
library(ggplot2)
library(gridExtra)
library(Deducer)
library(Hmisc)
library(rpart)
library(e1071)
library(randomForest)
library(rattle)

#### LOAD DATA
data <- read.csv("mushrooms.csv", header = T, sep = ",")

#### DATA MINING
summary(data) # Quick check on the variables
str(data) # All of the variable are factors, good

round(prop.table(table(data$class)),2) # ~ 50/50% on the classification, good

# Let's start checking each variable
colors <- c("springgreen3", "gold2") # set the colors you want to display
# on the plots

# -> Cap + bruises:
cap.shape <- ggplot(data, aes(x=cap.shape, fill=class)) + 
             geom_bar() + ylim(0,5000) + xlab("Cap shape") +
             labs(fill='CLASS') + 
             scale_fill_manual(labels = c("Edible", "Poisonous"),
                               values=colors) +
             theme(axis.title.y=element_blank(),
                   legend.position = c(0.1,0.9),
                   legend.title = element_text(size = 10, face = "bold"),
                   legend.text = element_text(size = 10))
             
cap.surface <- ggplot(data, aes(x=cap.surface, fill=class)) + 
               geom_bar() + ylim(0,5000) + xlab("Cap surface") +
               scale_fill_manual(values=colors) +
               theme(axis.title.y=element_blank(),
                     legend.position = "none")

cap.color <- ggplot(data, aes(x=cap.color, fill=class)) + 
             geom_bar() + ylim(0,5000) + xlab("Cap color") +
             scale_fill_manual(values=colors) +
             theme(axis.title.y=element_blank(),
                   legend.position = "none")

bruises <- ggplot(data, aes(x=bruises, fill=class)) + 
           geom_bar() + ylim(0,5000) + xlab("Bruises") +
           scale_fill_manual(values=colors) +
           theme(axis.title.y=element_blank(),
                 legend.position = "none")

grid.arrange(cap.shape, cap.surface, cap.color, bruises)

# -> Gill:

gill.attachment <- ggplot(data, aes(x=gill.attachment, fill=class)) + 
                    geom_bar() + ylim(0,8000) + xlab("Gill attachment") +
                    labs(fill='CLASS') + 
                    scale_fill_manual(labels = c("Edible", "Poisonous"),
                                      values=colors) +
                    theme(axis.title.y=element_blank(),
                          legend.position = c(0.1,0.9),
                          legend.title = element_text(size = 10,
                                                      face = "bold"),
                          legend.text = element_text(size = 10))

gill.spacing <- ggplot(data, aes(x=gill.spacing, fill=class)) + 
                geom_bar() + ylim(0,8000) + xlab("Gill spacing") +
                scale_fill_manual(values=colors) +
                theme(axis.title.y=element_blank(),
                      legend.position = "none")

gill.size <- ggplot(data, aes(x=gill.size, fill=class)) + 
             geom_bar() + ylim(0,8000) + xlab("Gill size") +
             scale_fill_manual(values=colors) +
             theme(axis.title.y=element_blank(),
                    legend.position = "none")

gill.color <- ggplot(data, aes(x=gill.color, fill=class)) + 
              geom_bar() + ylim(0,2000) + xlab("Gill color") +
              scale_fill_manual(values=colors) +
              theme(axis.title.y=element_blank(),
                    legend.position = "none")

grid.arrange(gill.attachment, gill.spacing, gill.size, gill.color)

# -> Stalk:
stalk.shape <- ggplot(data, aes(x=stalk.shape, fill=class)) + 
              geom_bar() + ylim(0,5000) + xlab("Stalk shape") +
              labs(fill='CLASS') + 
              scale_fill_manual(labels = c("Edible", "Poisonous"),
                                values=colors) +
              theme(axis.title.y=element_blank(),
                    legend.position = c(0.12,0.87),
                    legend.title = element_text(size = 10, face = "bold"),
                    legend.text = element_text(size = 10))

stalk.root <- ggplot(data, aes(x=stalk.root, fill=class)) + 
              geom_bar() + ylim(0,8000) + xlab("Stalk root") +
              scale_fill_manual(values=colors) +
              theme(axis.title.y=element_blank(),
                    legend.position = "none")

stalk.s.a.r <- ggplot(data, aes(x=stalk.surface.above.ring, fill=class)) + 
              geom_bar() + ylim(0,8000) + xlab("Stalk surface above ring") +
              scale_fill_manual(values=colors) +
              theme(axis.title.y=element_blank(),
                    legend.position = "none")

stalk.s.b.r <- ggplot(data, aes(x=stalk.surface.below.ring, fill=class)) + 
              geom_bar() + ylim(0,8000) + xlab("Stalk surface below ring") +
              scale_fill_manual(values=colors) +
              theme(axis.title.y=element_blank(),
                    legend.position = "none")

stalk.c.a.r <- ggplot(data, aes(x=stalk.color.above.ring, fill=class)) + 
                geom_bar() + ylim(0,8000) + xlab("Stalk color above ring") +
                scale_fill_manual(values=colors) +
                theme(axis.title.y=element_blank(),
                      legend.position = "none")

stalk.c.b.r <- ggplot(data, aes(x=stalk.color.below.ring, fill=class)) + 
                geom_bar() + ylim(0,8000) + xlab("Stalk color below ring") +
                scale_fill_manual(values=colors) +
                theme(axis.title.y=element_blank(),
                      legend.position = "none")

grid.arrange(stalk.shape, stalk.root, stalk.s.a.r, stalk.s.b.r,
             stalk.c.a.r, stalk.c.b.r, nrow=2)

# -> Veil + ring:
veil.type <- ggplot(data, aes(x=veil.type, fill=class)) + 
            geom_bar() + ylim(0,8000) + xlab("Veil type") +
            labs(fill='CLASS') + 
            scale_fill_manual(labels = c("Edible", "Poisonous"),
                              values=colors) +
            theme(axis.title.y=element_blank(),
                  legend.position = c(0.1,0.9),
                  legend.title = element_text(size = 10, face = "bold"),
                  legend.text = element_text(size = 10))

veil.color <- ggplot(data, aes(x=veil.color, fill=class)) + 
              geom_bar() + ylim(0,8000) + xlab("Veil color") +
              scale_fill_manual(values=colors) +
              theme(axis.title.y=element_blank(),
                    legend.position = "none")

ring.number <- ggplot(data, aes(x=ring.number, fill=class)) + 
                geom_bar() + ylim(0,8000) + xlab("Ring number") +
                scale_fill_manual(values=colors) +
                theme(axis.title.y=element_blank(),
                      legend.position = "none")

ring.type <- ggplot(data, aes(x=ring.type, fill=class)) + 
            geom_bar() + ylim(0,8000) + xlab("Ring type") +
            scale_fill_manual(values=colors) +
            theme(axis.title.y=element_blank(),
                  legend.position = "none")

grid.arrange(veil.type, veil.color, ring.number, ring.type)

# -> Odor + spore + population + habitat:
odor <- ggplot(data, aes(x=odor, fill=class)) + 
        geom_bar() + ylim(0,4500) + xlab("Odor") +
        labs(fill='CLASS') + 
        scale_fill_manual(labels = c("Edible", "Poisonous"),
                          values=colors) +
        theme(axis.title.y=element_blank(),
              legend.position = c(0.1,0.9),
              legend.title = element_text(size = 10, face = "bold"),
              legend.text = element_text(size = 10))

spore <- ggplot(data, aes(x=spore.print.color, fill=class)) + 
          geom_bar() + ylim(0,4500) + xlab("Spore print color") +
          scale_fill_manual(values=colors) +
          theme(axis.title.y=element_blank(),
                legend.position = "none")

population <- ggplot(data, aes(x=population, fill=class)) + 
              geom_bar() + ylim(0,4500) + xlab("Population") +
              scale_fill_manual(values=colors) +
              theme(axis.title.y=element_blank(),
                    legend.position = "none")

habitat <- ggplot(data, aes(x=habitat, fill=class)) + 
          geom_bar() + ylim(0,4500) + xlab("Habitat") +
          scale_fill_manual(values=colors) +
          theme(axis.title.y=element_blank(),
                legend.position = "none")

grid.arrange(odor, spore, population, habitat)

# Correlations
# -> Cap + bruises vs CLASS:
c_mat_1 <- cor.matrix(data[1:5])
ggcorplot(c_mat_1, data)

# -> Gill vs CLASS:
c_mat_2 <- cor.matrix(data[c(1,7:10)])
ggcorplot(c_mat_2, data)

# -> Stalk vs CLASS:
c_mat_3 <- cor.matrix(data[c(1,11:16)])
ggcorplot(c_mat_3, data)

# -> Veil + ring vs CLASS:
c_mat_4 <- cor.matrix(data[c(1,18:20)]) # Veil.type is not included since
# there's only one type, and it's not useful data
ggcorplot(c_mat_4, data)

# -> Odor + spore + population + habitat vs CLASS:
c_mat_5 <- cor.matrix(data[c(1,6,21:23)])
ggcorplot(c_mat_5, data)

#### PREDICTIVE MODELING
# Split data
indexes <- sample(1:nrow(data), size=0.8*nrow(data))
train <- data[indexes, -17]
test <- data[-indexes, -17]

# Decision tree
dt_model <- rpart(class ~., train, method = "class")
dt_pred <- predict(dt_model, test, type="class")
confusionMatrix(test$class, dt_pred) # 99% Accuracy

# Naive Bayes
nb_model <- naiveBayes(class~., train)
nb_pred <- predict(nb_model, test)
confusionMatrix(test$class, nb_pred) # 94% Accuracy

# Random Forest
rf_model <- randomForest(class ~., train, ntree=2000, importance=T)
rf_pred <- predict(rf_model, test)
confusionMatrix(test$class, rf_pred) # 100% Accuracy

# Get variables importance
imp <- as.data.frame(importance(rf_model, type=1))
colnames(imp) <- "Importance"

ggplot(imp, aes(x=reorder(rownames(imp), Importance),
                y=Importance, fill=Importance)) +
  geom_bar(stat="identity") + coord_flip() + 
  labs(title = "Variables importance on the predictive model") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
  
# Visualize predictive tree model
# *** the RandomForest package doesn't include any function to visualize
# the predictive model so I'll use the one build in the decision tree
fancyRpartPlot(dt_model,palettes=c("Greens", "YlOrBr"))

#### THE END!
