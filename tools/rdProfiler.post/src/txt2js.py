# coding=utf-8

import os
import pandas as pd

df = pd.read_csv(f'../output/{os.getenv("RDP_SCT_FN")}')
df = df.rename(columns={'section': 'key'})

relations = open(f'../input/{os.getenv("RDP_REL_FN")}', 'r').read().splitlines()

with open('../medium/data.js', 'w') as f:
    f.write('sct_data = [\n')
    for i in range(len(df)):
        f.write(f'    {df.iloc[i].to_json()}{"," if i+1 < len(df) else ""}\n')
    f.write('];\n')
    f.write('\n\n')
    f.write('rel_data = [\n')
    for i, L in enumerate(relations):
        left, right = L.split('->')
        f.write(f"""    {{from: "{left}", to: "{right}"}}{"," if i+1 < len(relations) else ""}\n""")
    f.write('];\n')
    