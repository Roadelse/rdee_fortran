# coding=utf-8

# This script aims to remove the profiler overhead given:
#    1. standard rdp-SCT file in ../input
#    2. repeated rdp-ovh files in ../medium

import os, glob
import pandas as pd


df_list = []
files = glob.glob('../medium/*.csv')
for f in files:
    df_list.append(pd.read_csv(f))

df_ori = pd.read_csv(f'../input/{os.getenv("RDP_SCT_FN")}')

df0 = pd.concat(df_list)
ovh_time_mean = df0.groupby(df0.index)['time'].mean()

df_ori['time'] -= ovh_time_mean
df_ori['time'] = df_ori['time'].apply(lambda x : x if x > 0.1 else 0)

df_ori.to_csv(f'../output/{os.getenv("RDP_SCT_FN")}', index=False)