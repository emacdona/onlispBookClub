#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb  9 01:15:52 2022

@author: emacdona
"""

# Note: conda install -c anaconda pymongo

import matplotlib.pyplot as plt
import pandas as pd
import pymongo as pm

client = pm.MongoClient(
    username="root",
    password="YjLuR307Vl",
    authSource="admin"
)

db = client["reddit"]

# Pie chart, where the slices will be ordered and plotted counter-clockwise:
labels = 'less than 1k subscribers', 'more than 1k subscribers'
sizes = [
    db.subreddits.find({"subscribers": {"$lt": 1000}}).count(),
    db.subreddits.find({"subscribers": {"$gt": 1000}}).count()
    ]
explode = (0, 0)  # only "explode" the 2nd slice (i.e. 'Hogs')

fig1, ax1 = plt.subplots()
ax1.pie(sizes, explode=explode, labels=labels, autopct='%1.1f%%', shadow=True, startangle=90)
ax1.axis('equal')  # Equal aspect ratio ensures that pie is drawn as a circle.
plt.show()

# Histogram: subreddits per year
plt.hist(
    pd.DataFrame.from_records(
        list(
            map(
                lambda r : (r["created"].year, 1), 
                db.subreddits.find({}, ["created"])))
        )[0]
    )

plt.show()

# Histogram: subreddits per month, 2020
plt.hist(
    pd.DataFrame.from_records(
        list(
            map(
                lambda r : (r["created"].year, r["created"].month), 
                db.subreddits.find({ "$expr": { "$eq": [{ "$year": "$created" }, 2020] } }, ["created"])))
        )[1]
    )

plt.show()
