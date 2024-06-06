library(dplyr)
library(tidyr)
library(class)
library(sparklyr)
library(data.table)
library(e1071)
library(rpart)
library(randomForest)
library(ggplot2)

# setup
setwd("~/Desktop/master/rvpii/projekat/FDC")
filenames <- list.files(pattern="\\.csv$")

#loading needed tables
branded_food <- read.csv("branded_food.csv")
food_nutrient <- read.csv("food_nutrient.csv")
food <- read.csv("food.csv")
measure_unit <- read.csv("measure_unit.csv")
nutrient <- read.csv("nutrient.csv")

#sredjivanje podataka
food_nutrient$data_points <- NULL
food_nutrient$min <- NULL
food_nutrient$max <- NULL
food_nutrient$median <- NULL
food_nutrient$footnote <- NULL
food_nutrient$min_year_acquired <- NULL

food$food_category_id <- NULL
food$microbe_data <- NULL

branded_food[is.na(branded_food$brand_name), 3] <- ""
branded_food[is.na(branded_food$subbrand_name), 4] <- ""
branded_food[is.na(branded_food$ingredients), 6] <- ""
branded_food$ingredients <- gsub("INGREDIENTS*: ", "", branded_food$ingredients, ignore.case=T)
branded_food$ingredients <- gsub("INGREDIENTS: ", "", branded_food$ingredients, ignore.case=T)
branded_food$ingredients <- gsub("INGREDIENT: ", "", branded_food$ingredients, ignore.case=T)
branded_food$ingredients <- gsub("MADE FROM: ", "", branded_food$ingredients, ignore.case=T)
branded_food[is.na(branded_food$serving_size), 8] <- 0

food_nutrient[is.na(food_nutrient$derivation_id), 5] <- 0

food_description_table <- food[food$data_type == "branded_food", c(1,3,4)]
branded_food <- merge(branded_food, food_description_table, by.x="fdc_id", by.y="fdc_id", all.x=T)

#sem prve, ostali kodovi za energiju nisu relevantni dataset-u "branded"
calorie_table <- food_nutrient[food_nutrient$nutrient_id == first(nutrient[nutrient$name == "Energy", 'id']), c('fdc_id','amount')]
calorie_table <- calorie_table %>% group_by(fdc_id) %>% summarise(across(amount, mean))
colnames(calorie_table)[2] <- "calories"
branded_food <- merge(branded_food, calorie_table, by.x="fdc_id", by.y="fdc_id", all.x=T)

branded_food <- branded_food %>% drop_na(calories)

median_calories <- median(branded_food$calories)
branded_food <- branded_food %>% mutate(calories_category=ifelse(calories <= median_calories, "LOW", "HIGH"))

protein_table <- food_nutrient[food_nutrient$nutrient_id == nutrient[nutrient$name == "Protein", 'id'], c('fdc_id','amount')]
protein_table <- protein_table %>% group_by(fdc_id) %>% summarise(across(amount, mean))
colnames(protein_table)[2] <- "protein"
branded_food <- merge(branded_food, protein_table, by.x="fdc_id", by.y="fdc_id", all.x=T)

#ukljucuje vlakna i secere, koji su posle dodati zasebno
carb_table <- food_nutrient[food_nutrient$nutrient_id == nutrient[nutrient$name == "Carbohydrate, by difference", 'id'], c('fdc_id','amount')]
carb_table <- carb_table %>% group_by(fdc_id) %>% summarise(across(amount, mean))
colnames(carb_table)[2] <- "carbohydrates"
branded_food <- merge(branded_food, carb_table, by.x="fdc_id", by.y="fdc_id", all.x=T)

#masti, ukupno
fat_table <- food_nutrient[food_nutrient$nutrient_id == nutrient[nutrient$name == "Total lipid (fat)", 'id'], c('fdc_id','amount')]
fat_table <- fat_table %>% group_by(fdc_id) %>% summarise(across(amount, mean))
colnames(fat_table)[2] <- "fats"
branded_food <- merge(branded_food, fat_table, by.x="fdc_id", by.y="fdc_id", all.x=T)

sugar_table <- food_nutrient[food_nutrient$nutrient_id == nutrient[nutrient$name == "Total Sugars", 'id'], c('fdc_id','amount')]
sugar_table <- sugar_table %>% group_by(fdc_id) %>% summarise(across(amount, mean))
colnames(sugar_table)[2] <- "sugars"
branded_food <- merge(branded_food, sugar_table, by.x="fdc_id", by.y="fdc_id", all.x=T)

fiber_table <- food_nutrient[food_nutrient$nutrient_id == nutrient[nutrient$name == "Fiber, total dietary", 'id'], c('fdc_id','amount')]
fiber_table <- fiber_table %>% group_by(fdc_id) %>% summarise(across(amount, mean))
colnames(fiber_table)[2] <- "fiber"
branded_food <- merge(branded_food, fiber_table, by.x="fdc_id", by.y="fdc_id", all.x=T)

#sredjivanje kategorija
unique_cats <- data.frame(unique(branded_food$branded_food_category))
names(unique_cats) <- "categories"
unique_cats$category_groups_num <- c(0, 5, 2, 4, 4, 3, 5, 5, 3, 2, 3, 5, 6, 5, 4, 6, 6, 5, 5, 2, 6, 6, 5, 6, 4, 1, 5, 6, 4, 6, 2, 4, 5, 2, 2, 3, 1, 6, 5, 5, 5, 6, 2, 6, 6, 2, 2, 4, 6, 6, 3, 5, 5, 5, 6, 5, 2, 5, 3, 4, 5, 6, 3, 5, 1, 4, 4, 4, 6, 6, 5, 5, 4, 2, 6, 1, 5, 5, 6, 3, 5, 7, 6, 4, 1, 2, 2, 4, 3, 4, 3, 5, 4, 6, 2, 3, 3, 1, 6, 2, 3, 5, 4, 2, 2, 5, 4, 3, 3, 3, 6, 6, 5, 4, 5, 1, 6, 4, 4, 5, 6, 5, 5, 3, 4, 2, 2, 2, 6, 5, 2, 3, 3, 6, 1, 5, 7, 3, 6, 6, 7, 7, 7, 7, 7, 7, 5, 5, 6, 6, 2, 1, 6, 6, 2, 6, 7, 7, 5, 2, 7, 6, 5, 6, 6, 6, 2, 6, 7, 6, 6, 6, 2, 3, 2, 6, 6, 7, 6, 2, 3, 5, 4, 5, 5, 4, 5, 4, 1, 5, 2, 6, 7, 4, 5, 7, 4, 4, 3, 2, 6, 6, 6, 6, 4, 4, 7, 5, 6, 5, 6, 6, 1, 5, 1, 7, 3, 5, 1, 5, 3, 3, 1, 2, 7, 5, 5, 6, 2, 3, 3, 7, 4, 3, 3, 2, 6, 6, 7, 6, 3, 2, 3, 7, 5, 2, 3, 3, 7, 3, 2, 5, 2, 2, 6, 6, 1, 6, 3, 3, 4, 5, 2, 6, 4, 3, 4, 6, 6, 5, 2, 5, 3, 4, 4, 6, 6, 3, 3, 2, 5, 7, 4, 3, 2, 6, 7, 4, 1, 2, 5, 5, 1, 2, 4, 6, 5, 5, 4, 5, 5, 5, 3, 3, 5, 3, 3, 4, 4, 5, 4, 1, 2, 2, 3, 3, 3, 5, 1, 2, 5, 7, 7, 6, 6, 2, 2, 5, 5, 1, 6, 1, 3, 7, 1, 3, 3, 5, 2, 5, 4, 7, 6, 6, 5, 3, 4, 6, 2, 3, 6, 4, 2, 4, 7, 3, 2, 2, 5, 4, 3, 6, 6, 3, 3, 3, 5, 5, 1, 6, 5, 5, 3, 6, 2, 3, 5, 3, 6, 6, 3, 4, 3, 6, 5, 3, 5, 5, 6, 5, 5, 3, 5, 3, 3, 3, 3, 1, 4, 6, 3, 7, 6, 1, 5, 2, 6, 2, 6, 5, 4, 2, 2, 2, 4, 4, 3, 4, 3, 3, 2, 5, 2, 4, 6, 2)
groups_table <- data.frame(c(0,1,2,3,4,5,6,7), c(NA,"Dairy","Fruits and Vegetables", "Meat and Eggs", "Sugars and Sweets", "Cereals and Legumes", "Soups, Mixes, Dressings", "Drinks"))
names(groups_table) <- c("category_groups_num", "category_groups")
unique_cats <- merge(unique_cats, groups_table, by.x="category_groups_num", by.y="category_groups_num", all.x)
#unique_cats$category_groups_num <- NULL
branded_food <- merge(branded_food, unique_cats, by.x="branded_food_category", by.y="categories", all.x)

#kalorije, proteini, masti, ugljeni hidrati su po 100g hrane u gramima, ili u KCAL za kalorije
#serving size nema veze sa tim, to samo definise koliko je kolicine u pakovanju hrane
branded_food <- branded_food[, c(2:5, 21, 6:13, 22, 14:20, 1, 30, 31, 23:29)]
branded_food <- branded_food %>% drop_na(category_groups)
rm(calorie_table, carb_table, fat_table, fiber_table, food, food_description_table, food_nutrient, groups_table, measure_unit, nutrient, protein_table, sugar_table, unique_cats, median_calories)

branded_food[is.na(branded_food$carbohydrates), 28] <- 0
branded_food[is.na(branded_food$protein), 27] <- 0
branded_food[is.na(branded_food$fats), 29] <- 0
branded_food[is.na(branded_food$sugars), 30] <- 0
branded_food[is.na(branded_food$fiber), 31] <- 0

#postoje nevalidni podaci
#proizvod ne moze sadrzati vise od 1000KCAL na 100g, te ucitani podaci nisu u dobrom obliku
#za ostale makronutriente, ne moze sadrzati vise od 100g na 100g proizvoda
#moguce da su apsurdno velike vrednosti bile predstavljene inicijalno u KJ, ali u tabeli oznacene sa KCAL greskom???
#ima polje za energiju u KJ ali nema podatke u originalnom dataset-u
branded_food <- branded_food[branded_food$calories<=1000, ]
branded_food <- branded_food[branded_food$carbohydrates<=100, ]
branded_food <- branded_food[branded_food$protein<=100, ]
branded_food <- branded_food[branded_food$fats<=100, ]
branded_food <- branded_food[branded_food$sugars<=100, ]
branded_food <- branded_food[branded_food$fiber<=100, ]

branded_food$carbohydrates <- pmax(branded_food$carbohydrates, branded_food$sugars) 
branded_food$carbohydrates <- pmax(branded_food$carbohydrates, branded_food$fiber)  

bar_calories <- branded_food %>% group_by(calories_category) %>% summarise(n=n())
barplot(bar_calories$n, names=c("HIGH","LOW"), main="Podela namirnica spram kalorija", xlab="Kaloricnost", ylab="Broj namirnica", ylim=range(pretty(c(0, bar_calories$n))))
rm(bar_calories)

pie_groups <- branded_food %>% group_by(category_groups) %>% summarise(n=n())
pie(pie_groups$n, labels = paste0(pie_groups$category_groups, paste0(" - ", paste0(round(100 * pie_groups$n/nrow(branded_food), 2), "%"))), col = 1:7)
rm(pie_groups)

#branded_food %>% group_by(serving_size_unit) %>% summarise(n=n())
#predstaviti ovo nekako, i one druge stvari!
boxplot(branded_food$calories, main="Raspodela namirnica po kalorijskoj vrednosti", ylab="KCAL")

xgran <- pretty(branded_food$protein, n=10)
hist(branded_food[branded_food$protein>0, 27], breaks=xgran, main="Broj namirnica spram kolicine proteina u gramima",  xlab = "Proteinska kolicina", ylab = "Broj namirnica")
hist(branded_food[branded_food$carbohydrates>0, 28], breaks=xgran, main="Broj namirnica spram kolicine uglenih hidrata u gramima", xlab = "Kolicina ugljenih hidrata", ylab = "Broj namirnica")
hist(branded_food[branded_food$fats>0, 29], breaks=xgran, main="Broj namirnica spram kolicine masti u gramima", xlab = "Kolicina masti", ylab = "Broj namirnica")
hist(branded_food[branded_food$sugars>0, 30], breaks=xgran, main="Broj namirnica spram kolicine secera u gramima", xlab = "Seceri", ylab = "Broj namirnica")
hist(branded_food[branded_food$fiber>0, 31], breaks=xgran, main="Broj namirnica spram kolicine vlakana u gramima", xlab = "Vlakna", ylab = "Broj namirnica")

rand_foods <- branded_food[sample(nrow(branded_food), size=nrow(branded_food)/100), ]
qplot(carbohydrates, fats, data=rand_foods, colour=calories_category)
qplot(carbohydrates, protein, data=rand_foods, colour=calories_category)
qplot(carbohydrates, fats, data=rand_foods, colour=category_groups)
qplot(carbohydrates, protein, data=rand_foods, colour=category_groups)

food_arranged <- branded_food

knn_food <- food_arranged[food_arranged$carbohydrates != 0 & food_arranged$protein != 0 & food_arranged$fiber != 0 & food_arranged$sugars != 0 & food_arranged$fats != 0, ]
knn_food <- knn_food[sample(nrow(knn_food)/2, 0.02 * nrow(knn_food)/2),]

food_arranged <- food_arranged[,c(14:18,23:31)]
rm(branded_food)

test_sets <- list()
train_sets <- list()
i <- sample(nrow(knn_food), 0.2 * nrow(knn_food))
test_sets[[1]] <- knn_food[i, ]
train_sets[[1]] <- knn_food[-i, ]
remainder <- knn_food[-i, ]
rm(i)
i <- sample(nrow(remainder), 0.2 * nrow(knn_food))
test_sets[[2]] <- remainder[i, ]
train_sets[[2]] <- knn_food[-i, ]
remainder <- remainder[-i, ]
rm(i)
i <- sample(nrow(remainder), 0.2 * nrow(knn_food))
test_sets[[3]] <- remainder[i, ]
train_sets[[3]] <- knn_food[-i, ]
remainder <- remainder[-i, ]
rm(i)
i <- sample(nrow(remainder), 0.2 * nrow(knn_food))
test_sets[[4]] <- remainder[i, ]
train_sets[[4]] <- knn_food[-i, ]
remainder <- remainder[-i, ]
i <- sample(nrow(remainder), nrow(remainder))
test_sets[[5]] <- remainder[i, ]
train_sets[[5]] <- knn_food[-i, ]
rm(i)
rm(remainder)

total_perf <- list()
for(k_iter in 1:6) {
  total_perf[[k_iter]] <- 0
  for(i in 1:5) {
    if(k_iter %% 2 == 1) {
      rez_knn <- knn(select(train_sets[[i]], protein, fats, carbohydrates), select(test_sets[[i]], protein, fats, carbohydrates), train_sets[[i]]$calories_category, k = (k_iter) * 5, use.all = FALSE)
    } else {
      rez_knn <- knn(select(train_sets[[i]], protein, fats, carbohydrates, sugars, fiber), select(test_sets[[i]], protein, fats, carbohydrates, sugars, fiber), train_sets[[i]]$calories_category, k = (k_iter-1) * 5, use.all = FALSE)
    }
    perf_knn <- rez_knn == as.factor(test_sets[[i]]$calories_category)
    perf_knn <- sum(perf_knn, na.rm=TRUE) / length(perf_knn)
    total_perf[[k_iter]] <- total_perf[[k_iter]] + perf_knn
  }
  total_perf[[k_iter]] <- total_perf[[k_iter]] / 5
}
total_perf_knn <- total_perf

total_perf <- list()
for(k_iter in 1:6) {
  total_perf[[k_iter]] <- 0
  for(i in 1:5) {
    train_sets[[i]]$calories_category <- factor(train_sets[[i]]$calories_category)
    if(k_iter %% 2 == 1) {
      if(k_iter == 1) {
        model_svm <- svm(calories_category ~ protein + fats + carbohydrates, data=train_sets[[i]], na.action=na.omit)
      } else if(k_iter == 3) {
        model_svm <- svm(calories_category ~ protein + fats + carbohydrates, data=train_sets[[i]], na.action=na.omit, kernel="polynomial")
      } else if(k_iter == 5) {
        model_svm <- svm(calories_category ~ protein + fats + carbohydrates, data=train_sets[[i]], na.action=na.omit, kernel="sigmoid")
      }
      perf_test_svm <- predict(model_svm, newdata = select(test_sets[[i]], protein, fats, carbohydrates, calories_category))
    } else {
      if(k_iter == 2) {
        model_svm <- svm(calories_category ~ protein + fats + carbohydrates + sugars + fiber, data=train_sets[[i]])
      } else if(k_iter == 4) {
        model_svm <- svm(calories_category ~ protein + fats + carbohydrates + sugars + fiber, data=train_sets[[i]], kernel="polynomial")
      } else if(k_iter == 6) {
        model_svm <- svm(calories_category ~ protein + fats + carbohydrates + sugars + fiber, data=train_sets[[i]], kernel="sigmoid")
      }
      perf_test_svm <- predict(model_svm, newdata = select(test_sets[[i]], protein, fats, carbohydrates, sugars, fiber, calories_category))
    }
    total_perf[[k_iter]] <- total_perf[[k_iter]] + sum(perf_test_svm == test_sets[[i]]$calories_category) / length(perf_test_svm)
  }
  total_perf[[k_iter]] <- total_perf[[k_iter]] / 5
}
total_perf_svm <- total_perf

total_perf <- list()
for(k_iter in 1:6) {
  total_perf[[k_iter]] <- 0
  for(i in 1:5) {
    if(k_iter %% 2 == 1) {
      #train_sets[[i]]$calories_category <- factor(train_sets[[i]]$calories_category)
      if(k_iter == 1) {
        model_rf <- randomForest(calories_category ~ protein + fats + carbohydrates, data=train_sets[[i]], ntree=100)
      } else if(k_iter == 3) {
        model_rf <- randomForest(calories_category ~ protein + fats + carbohydrates, data=train_sets[[i]], ntree=300)
      } else if(k_iter == 5) {
        model_rf <- randomForest(calories_category ~ protein + fats + carbohydrates, data=train_sets[[i]], ntree=500)
      }
      perf_test_rf <- predict(model_rf, newdata = select(test_sets[[i]], protein, fats, carbohydrates, calories_category), type="response")
    } else {
      #train_sets[[i]]$calories_category <- factor(train_sets[[i]]$calories_category)
      if(k_iter == 2) {
        model_rf <- randomForest(calories_category ~ protein + fats + carbohydrates + sugars + fiber, data=train_sets[[i]], ntree=100)
      } else if(k_iter == 4) {
        model_rf <- randomForest(calories_category ~ protein + fats + carbohydrates + sugars + fiber, data=train_sets[[i]], ntree=300)
      } else if(k_iter == 6) {
        model_rf <- randomForest(calories_category ~ protein + fats + carbohydrates + sugars + fiber, data=train_sets[[i]], ntree=500)
      }
      perf_test_rf <- predict(model_rf, newdata = select(test_sets[[i]], protein, fats, carbohydrates, sugars, fiber, calories_category), type="response")
    }
    total_perf[[k_iter]] <- total_perf[[k_iter]] + sum(perf_test_rf == test_sets[[i]]$calories_category) / length(perf_test_rf)
  }
  total_perf[[k_iter]] <- total_perf[[k_iter]] / 5
}
total_perf_rf <- total_perf

total_perf_knn_vec <- unlist(total_perf_knn, use.names=FALSE)
total_perf_svm_vec <- unlist(total_perf_svm, use.names=FALSE)
total_perf_rf_vec <- unlist(total_perf_rf, use.names=FALSE)

total_perf_vec <- c(total_perf_knn_vec, total_perf_svm_vec, total_perf_rf_vec)
groups_vec <- c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3)
labels_dotchart <- c("KNN, 3 param, k = 5", "KNN, 5 param, k = 5", "KNN, 3 param, k = 10", "KNN, 5 param, k = 10", "KNN, 3 param, k = 15", "KNN, 5 param, k = 15",
                     "SVM, 3 param, kernel = radial", "SVM, 5 param, kernel = radial", "SVM, 3 param, kernel = polynomial", "SVM, 5 param, kernel = polynomial",
                     "SVM, 3 param, kernel = sigmoid", "SVM, 5 param, kernel = sigmoid", "RF, 3 param, ntree=100", "RF, 5 param, ntree=100",
                     "RF, 3 param, ntree=300", "RF, 5 param, ntree=300", "RF, 3 param, ntree=500", "RF, 5 param, ntree=500")
colors_dotchart <- c("blue", "darkgreen", "red")
dotchart(total_perf_vec, labels=labels_dotchart, groups = groups_vec, gcolor = colors_dotchart,
         color = colors_dotchart[groups_vec],
         cex = 0.9,  pch = 22, xlab = "Accuracy")

#does the same step by step and more efficient!
konekcija <- spark_connect(master="local")
spark_web(konekcija)
food_arranged <- food_arranged %>% drop_na()
fwrite(food_arranged, file="~/Desktop/master/rvpii/projekat/FDC/food_arranged.csv")
food_arranged_tbl <- spark_read_csv(konekcija, name="food_arranged_tbl", path="~/Desktop/master/rvpii/projekat/FDC/food_arranged.csv", memory=T, infer_schema=F, columns=list(publication_date="character",modified_date="character",available_date="character",market_country="character",discontinued_date="character",category_groups_num="integer",category_groups="character",calories="double",calories_category="character",protein="double",carbohydrates="double",fats="double",sugars="double",fiber="double"))
#food_arranged_tbl <- food_arranged_tbl %>% filter(!is.na(protein) & !is.na(fats) & !is.na(carbohydrates) & !is.na(sugars) & !is.na(fiber) & !is.na(category_groups_num))

#klasifikacija 4 - SPARK
nbay_pipeline <- ml_pipeline(konekcija) %>% 
  ft_dplyr_transformer(tbl=food_arranged_tbl %>% filter(!is.na(protein) & !is.na(fats) & !is.na(carbohydrates) & !is.na(sugars) & !is.na(fiber) & !is.na(category_groups_num))) %>%
  ft_r_formula(category_groups_num ~ protein + fats + carbohydrates) %>% 
  ml_naive_bayes(label_col = "category_groups_num")

bv <- ml_cross_validator(konekcija, estimator=nbay_pipeline, estimator_param_maps=list(naive_bayes=list(model_type = "multinomial", smoothing = c(1,2,3))),evaluator=ml_multiclass_classification_evaluator(konekcija), num_folds = 5, parallelism = 4)
bv_model <- ml_fit(bv, food_arranged_tbl)
bv_metrics <- ml_validation_metrics(bv_model)


#klasifikacija 5 - SPARK
logr_pipeline <- ml_pipeline(konekcija) %>% 
  ft_dplyr_transformer(tbl=food_arranged_tbl %>% filter(!is.na(protein) & !is.na(fats) & !is.na(carbohydrates) & !is.na(sugars) & !is.na(fiber) & !is.na(category_groups_num))) %>%
  ft_r_formula(category_groups_num ~ protein + fats + carbohydrates) %>% 
  ml_logistic_regression(label_col = "category_groups_num")

lv <- ml_cross_validator(konekcija, estimator=logr_pipeline, estimator_param_maps=list(logistic_regression=list(max_iter = c(3,5,10), family = "multinomial")),evaluator=ml_multiclass_classification_evaluator(konekcija), num_folds = 5, parallelism = 4)
lv_model <- ml_fit(lv, food_arranged_tbl)
lv_metrics <- ml_validation_metrics(lv_model)
plot(lv_metrics$max_iter_1, lv_metrics$f1)

#klasifikacija 6 - SPARK
forr_pipeline <- ml_pipeline(konekcija) %>% 
  ft_dplyr_transformer(tbl=food_arranged_tbl %>% filter(!is.na(protein) & !is.na(fats) & !is.na(carbohydrates) & !is.na(sugars) & !is.na(fiber) & !is.na(category_groups_num))) %>%
  ft_r_formula(category_groups_num ~ protein + fats + carbohydrates) %>% 
  ml_random_forest_classifier(label_col = "category_groups_num")
fv <- ml_cross_validator(konekcija, estimator=forr_pipeline, estimator_param_maps=list(random_forest=list(num_trees = c(3,5), max_depth = c(3, 5))),evaluator=ml_multiclass_classification_evaluator(konekcija), num_folds = 5, parallelism = 4)
fv_model <- ml_fit(fv, food_arranged_tbl)
fv_metrics <- ml_validation_metrics(fv_model)
plot(fv_metrics$max_depth_1, fv_metrics$f1)
plot(fv_metrics$num_trees_1, fv_metrics$f1)

#f1 = 2 / ((1/prec) + (1/sens))

#klasterizacija 1
eval_vec <- c(0,0,0,0)
eval_s1 <- c(0,0,0,0)
for(i in 1:4) {
  klast <- ml_kmeans(food_arranged_tbl, carbohydrates ~ sugars + fiber, k = i+1, max_iter = 25, init_mode = "random")
  klast$model$summary$cluster_sizes()
  klast.eval <- ml_evaluate(klast, food_arranged_tbl %>% select(sugars, fiber))
  eval_s1[i] <- ml_compute_silhouette_measure(klast, food_arranged_tbl)
  eval_vec[i] <- klast.eval
}
plot(c(2:5), eval_s1)

#klasterizacija 2
eval_vec <- c(0,0,0,0,0,0,0,0,0,0)
eval_s2 <- c(0,0,0,0,0,0,0,0,0,0)
for(i in 1:10) {
  klast <- ml_kmeans(food_arranged_tbl, calories_category ~ protein + fats + carbohydrates, k = i+1, max_iter = 25, init_mode = "random")
  klast$model$summary$cluster_sizes()
  klast.eval <- ml_evaluate(klast, food_arranged_tbl %>% select(protein, fats, carbohydrates))
  eval_s2[i] <- ml_compute_silhouette_measure(klast, food_arranged_tbl)
  eval_vec[i] <- klast.eval
}
plot(c(2:11), eval_s2)
spark_disconnect(konekcija)
