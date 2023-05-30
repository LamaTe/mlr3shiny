#DELETE THIS DOCUMENT WHEN FEATURE IS DONE
#TODO
#-Split the tabs
#issue with comment that you should train a learner 

library(mlr3)
library(DALEX)
library(DALEXtra)

# create learning task
task_penguins = as_task_classif(species ~ ., data = palmerpenguins::penguins)
task_penguins

tsk <- as_task_regr(m2.price ~ ., data = apartments)


data(german_credit)
x <- mlr_tasks$get("german_credit")
x$feature_types

tsk_german_credit <- as_task_classif()
#not all numeric features are called numeric. What about ordered or int?

tsk$feature_types
task_penguins$feature_types


as.vector(str(cat_list))

split_selected_features <- function(list){
  
  #Here feature_type = POSIXct is excluded
  num_list <- x$feature_types[x$feature_types$type == "numeric" | 
                              x$feature_types$type == "integer",]
  num_list <- num_list$id
  print(num_list)
  cat_list <- x$feature_types[x$feature_types$type == "ordered" | 
                              x$feature_types$type == "factor" | 
                              x$feature_types$type == "character" | 
                              x$feature_types$type == "logical",]
  cat_list <- cat_list$id
  print(cat_list)
  selected_cat <- intersect(cat_list, list)
  selected_num <- intersect(num_list, list)
  
  return(selected_cat, selected_num)
}

split_selected_features(temp_var_imp_vector)


# load learner and set hyperparameter
learner = lrn("classif.rpart", cp = .01)

task_penguins$feature_types

learner_german_credit$predict_type <- "prob"
learner_german_credit$predict_type
# train/test split
dalex_temp <- x$data(data_format = "data.table")
dalex_predictors <- dalex_temp %>% select(-x$target_names)
dalex_target <- dalex_temp %>% select(x$target_names)
colnames(dalex_target) <- "target"

dalex_target$target <- as.character(dalex_target$target)
dalex_target$target[dalex_target$target == x$positive] <- "1"
dalex_target$target[dalex_target$target != "1"] <- "0"

predict(learner_german_credit, dalex_predictors)

model <- explain_mlr3(learner_german_credit,
                      data = dalex_predictors, 
                      y = as.numeric(dalex_target$target),
                      predict_function_target_column = 1,
                      verbose = TRUE) 


plot(model_profile(model, variables = cat_list, type = "partial"))

temp_var_imp_vector <- c("age", "duration")


num_list
intersect(num_list, temp_var_imp_vector)

mlr_reflections$task_feature_types

# train the model
learner_german_credit <- learner$train(x)

explain_mlr3(learner_german_credit, )


# calculate performance
prediction$confusion
