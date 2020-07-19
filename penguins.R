library("httr")
library("readxl")
library(tidyverse)
library(lubridate)
library(rattle)
library(ggthemes)
library(ggsci)
library(formattable)

#get data
GET("https://query.data.world/s/ozi5py7nhdytqdbfgsasuiww7qkd4t", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)

#convert dates (not used) and convert numerics
numeric_pen <- function(x) {
  as.numeric(as.character(x))
}

df_tidy <- df %>% mutate(date_egg = ymd(`Date Egg`)) %>% select(-`Date Egg`) %>%
  mutate_at(c("Culmen Length (mm)","Culmen Depth (mm)","Flipper Length (mm)",
              "Body Mass (g)"), numeric_pen)

#scatter plot - flipper vs culmen length
ggplot(df_tidy, aes(`Flipper Length (mm)`,`Culmen Length (mm)`, size=`Body Mass (g)`, col=Species)) +
  geom_point(alpha=0.5) + facet_grid(~Island) + theme_few() +
  geom_density2d(col="black", alpha=0.1) +
  scale_color_lancet() + geom_rug(size=1, alpha=0.5, position="jitter") +
  labs(title="Flipper length, culmen length and body mass - Penguins at Palmer station Antarctica by Island",
       caption="Data source: Kristen Gorman, 2017. Visualisation: @lauriejhopkins") +
  theme(plot.title=element_text(size=20),
        text=element_text(family="Avenir")) 
ggsave("penguins_scatter.png", dpi="retina")

#decision tree
library(rpart)

#create training and test data
penguin_train <- sample_n(df_tidy,nrow(df_tidy)*0.75)
penguin_test <- df_tidy %>% anti_join(penguin_train, by="Sample Number")

#create prediction model
penguin_model <- rpart(Species ~ `Culmen Length (mm)` + `Culmen Depth (mm)`+
                         `Flipper Length (mm)`+`Body Mass (g)`, method="class",
                       data=penguin_train, control = rpart.control(cp = 0))

#model predictions and append to test data
penguin_predict <- predict(penguin_model, penguin_test, type="class")

penguin_test$predict <- penguin_predict

#check predictions
table(penguin_test$Species, penguin_test$predict) %>% formattable()

mean(penguin_test$Species==penguin_test$predict)

plotcp(penguin_model)
#
library(rpart.plot)
#basic decision tree plot
rpart.plot(penguin_model)

#'fancy' DT plot
png('penguin_decisiontree.png', width=10, height=8, units="in", res=300)
fancyRpartPlot(penguin_model, palettes="Greens",
               main="Classifying penguins at Palmer Station - Antarctica",
               caption="Data source: Kristen Gorman, 2017. Decision tree model: @lauriejhopkins")
dev.off()

#create random forest
library(randomForest)
library(party)
library(reprtree)

penguin_train_2 <- penguin_train %>% select(Species, c_length=`Culmen Length (mm)`, c_depth=`Culmen Depth (mm)`, f_length =`Flipper Length (mm)`, b_mass= `Body Mass (g)`)
penguin_train_2 <- penguin_train_2 %>% mutate(Species=recode(Species, `Adelie Penguin (Pygoscelis adeliae)`="Adelie", 
                                                             `Chinstrap penguin (Pygoscelis antarctica)`="Chinstrap",
                                                             `Gentoo penguin (Pygoscelis papua)`="Gentoo"))

penguin_rf <- randomForest(as.factor(Species) ~ ., data=penguin_train_2[complete.cases(penguin_train_2)==T,],
                           ntree=500)

penguin_cf <- cforest(as.factor(Species) ~., data=penguin_train_2[complete.cases(penguin_train_2)==T,])

pt <- prettytree(penguin_cf@ensemble[[1]], names(penguin_cf@data@get("input"))) 
nt <- new("BinaryTree") 
nt@tree <- pt 
nt@data <- penguin_cf@data 
nt@responses <- penguin_cf@responses 

plot(nt, type="simple", main="Random forest - Classifying penguins at Palmer station")

#Final rf plot using reprtree
reprtree:::plot.getTree(penguin_rf, main="Random forest (representative tree) - Classifying penguins at Palmer station")

#confusion table
pen_table <- as.data.frame(penguin_rf$confusion)
formattable(pen_table)
