# coding=utf-8

import os
import pandas as pd

df = pd.read_csv(f'../output/{os.getenv("RDP_SCT_FN")}')
df = df.rename(columns={'section': 'key'})

# relations = open(f'../input/{os.getenv("RDP_REL_FN")}', 'r').read().splitlines()

with open('../output/data.js', 'w') as f:
    f.write('sct_data = [\n')
    f.write('    {"key":"root","count":1,"time":-1},\n')
    for i in range(len(df)):
        f.write(f'    {df.iloc[i].to_json()}{"," if i+1 < len(df) else ""}\n')
    f.write('];\n')
    f.write('\n\n')

    f.write('rel_data = [\n')
    for i in range(len(df)):
    # for i, L in enumerate(relations):
        section = df.iloc[i].key
        sepIndex = section.rfind('->')
        if sepIndex == -1: 
            f.write(f"""    {{from: "root", to: "{section}"}}{"," if i+1 < len(df) else ""}\n""")
        else:
            left = section[:sepIndex]
            f.write(f"""    {{from: "{left}", to: "{section}"}}{"," if i+1 < len(df) else ""}\n""")
    f.write('];\n')
    