# -*- coding: utf-8 -*-
"""
Created on Thu Jan 28 10:15:27 2021

@author: Gunnar
"""

import seaborn as sns
import matplotlib.pyplot as plt
import sklearn.linear_model
import statsmodels.api as sm
import numpy as np
import pandas as pd
import sklearn as sk

def import_dataset(filename):
    return pd.read_stata(filename)

def regression_results(y_true, y_pred):

    # Regression metrics
    explained_variance = sk.metrics.explained_variance_score(y_true, y_pred)
    mean_absolute_error = sk.metrics.mean_absolute_error(y_true, y_pred) 
    mse = sk.metrics.mean_squared_error(y_true, y_pred) 
    mean_squared_log_error = sk.metrics.mean_squared_log_error(y_true, y_pred)
    median_absolute_error = sk.metrics.median_absolute_error(y_true, y_pred)
    r2=sk.metrics.r2_score(y_true, y_pred)

    print('explained_variance: ', round(explained_variance,4))    
    print('mean_squared_log_error: ', round(mean_squared_log_error,4))
    print('r2: ', round(r2,4))
    print('MAE: ', round(mean_absolute_error,4))
    print("median_absolute_error", round(median_absolute_error,4))
    print('MSE: ', round(mse,4))
    print('RMSE: ', round(np.sqrt(mse),4))

if __name__ == "__main__":
    df = import_dataset("TPA_12_full.dta")
       
    cool_vars = list(df.columns)
    if 'code' in cool_vars: 
        cool_vars.remove('code')
    # Drop nan-values from dataset?
    df_clean = df.dropna()
    
    X = df_clean.iloc[:,2:-1]
    y = df_clean.iloc[:,-1]
    X = sm.add_constant(X)

    # Define the model (check with LM to start with)
    model = sk.linear_model.LinearRegression()
    # Do k-fold validation. This feels very wrong, not to split according to code
    kfold = sk.model_selection.KFold(n_splits=5, shuffle=True, random_state=12345)
    
    
    

    model.fit(X,y)
    y_pred = model.predict(X)
    regression_results(y, y_pred)
    

    scores = []
    for i, (train, test) in enumerate(kfold.split(X, y)):
        model.fit(X.iloc[train,:], y.iloc[train])
        score = model.score(X.iloc[test,:], y.iloc[test])
        scores.append(score)
    print(scores)
    
    
    
    #do elastic net! 1%99% ridge, lasso
    model = sk.linear_model.ElasticNet(l1_ratio = 1)
    
    model.fit(X,y)
    y_pred = model.predict(X)
    regression_results(y, y_pred)
    print(model.coef_)
    
    
    scores = []
    for i, (train, test) in enumerate(kfold.split(X, y)):
        model.fit(X.iloc[train,:], y.iloc[train])
        score = model.score(X.iloc[test,:], y.iloc[test])
        scores.append(score)
    print(scores)
    
    
        