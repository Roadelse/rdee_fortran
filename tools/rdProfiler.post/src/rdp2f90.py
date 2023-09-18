# coding=utf-8

import os
import pandas as pd


df = pd.read_csv(f'../input/{os.getenv("RDP_SCT_FN")}')
relations = open(f'../input/{os.getenv("RDP_REL_FN")}', 'r').read().splitlines()

class node:
    def __init__(self, name, count, time):
        self.name = name
        self.count = count
        self.time = time
        self.children = []
        self.parent = None
    
    @property
    def height(self):
        maxH = 0
        for c in self.children:
            maxH = max(maxH, c.depth)
        return maxH + 1
    
    @property
    def depth(self):
        if self.parent is None:
            return 1
        else:
            return self.parent.depth + 1
    
    @property
    def fcode(self):
        iw = self.depth * 4  # indent width
        indent = ' ' * iw
        ivn = f'i{self.depth-2}'
        fcs1 = f"""
{indent}do {ivn} = 1, {self.count}
{indent}    call rdp%start('{self.name}')
"""
        fcs2 = ""
        for c in self.children:
            fcs2 = fcs2 + c.fcode
        fcs3 = f"""
{indent}    call rdp%end('{self.name}')
{indent}end do
"""
        if self.parent is None:
            return fcs2
        else:
            return fcs1 + fcs2 + fcs3

# ↑↑↑ above is preparation
# *********************************************************
# *********************************************************
# *********************************************************
# ↓↓↓ below is the main body

# ................. transform dataframe to node dict
nodes = {}
for i in range(len(df)):
    dfR = df.iloc[i]
    nodes[dfR.section] = node(dfR['section'], dfR['count'], dfR['time'])
for rel in relations:
    left, right = rel.split('->')
    nodes[left].children.append(nodes[right])
    nodes[right].parent = nodes[left]
        

# ................. render p-c relations and add a root node
root = node('all', 1, -1)

for nn, n in nodes.items():
    if n.parent is None:
        n.parent = root
        root.children.append(n)

# ................. generate Fortran90 code
int_var_declarations = ','.join([f'i{_}' for _ in range(root.height)])

codeF = f"""\
Program get_ovh
    use rdee_fortran
    implicit none
    integer(kind=4) :: {int_var_declarations}
    character(len=80) :: ofile
    type(rdProfiler) :: rdp
    
    ! .................... main body
    call get_command_argument(1, ofile)

    rdp = rdProfiler()
    
    {root.fcode}
    
    if (ofile .eq. 'stdout' .or. ofile .eq. '') then
        call rdp%print
    else
        call rdp%print(out1=trim(ofile), out2='skip')
    end if
End Program
"""

# ................. save the code
with open('../output/rdp-ovh.F90', 'w') as f:
    f.write(codeF)
