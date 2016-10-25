This is the repository for the Kaggle competition - Melbourne University AES/MathWorks/NIH Seizure Prediction.

This code is written by zftturbo and all credits go to him. I have made few changes to suit my needs, but these are minor.
The main reason to use his code was to understand how boosting can be implelmented in Python. I jave used boosting in R, but never 
in python. I have stopped working on this competition and this will be the only update.

Submission Result: 0.59353

The basic flow of the script is: 
1) extract features from the original data files (processes files one by one) 
2) train model 
3) predict 
4) create submission.

Features (in total 34):
average value of each 16 channels over whole time interval
average absolute values of each 16 channels over whole time interval
patient id
file size
Classifier: Gradient boosting with decision trees (xgboost library) 



