# mlr3shiny 0.5.6

* Fixed options in ordered_action within the Robustify pipeline
* Fixed code extraction
* Added the ability to display model information in the Predict Tab
* Introduced a "No Learner / Reset Learner" option in the Predict tab to fix cases where Pred$Learner did not update 

# mlr3shiny 0.5.5

* Adding loading animations to visualizations due to potential long waiting times
* Some fixes

# mlr3shiny 0.5.4

* Adding explanatory labels to learner parameters
* Minor changes to make UI more user friendly 

# mlr3shiny 0.5.3

* Fixed Hyperparameters for SVM and XGBoost

# mlr3shiny 0.5.2

* Added message to also think of robustifications steps, when model training fails

# mlr3shiny 0.5.1

* Improved Message when trying to visualise tree with only one node
* Less aggressive alert-waning color

# mlr3shiny 0.5.0

* Updated to new ParamSet backbone
* Some warning messages are now less aggressive
* Some fixes

# mlr3shiny 0.4.0

* Adding option to visualize objects in Tabs Predict and Task
* Adding option to create custom label for learners
* Several fixes

# mlr3shiny 0.3.1

* Migrating explain tab from iml to DALEX + additonal options
* Adding option to select robustify parameters
* Adding import of different file types also for predict tab
* Readability of displayed code improved  
* Several fixes

# mlr3shiny 0.3.0

* New algorithm evaluation metrics
* Adding new quiet preprocessing (robustify), fixing bugs in learner, evaluate and benchmarking
* Allowing import of different file types
* Fixing thresholding in twoclass classif
* Updating dependencies and fixing dependency bugs
* Adding xgboost learner
* Adding explain tab
* Adding code display option in predict tab 

# mlr3shiny 0.2

* Adding various new algorithm evaluation metrics
* Adding option to change default class for twoclass classifications 
* Updating dependencies and fixing dependency bugs

# mlr3shiny 0.1.1

* Initial upload to CRAN.
