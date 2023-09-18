#!/bin/bash


export FC=gfortran
export RDEE_FORTRAN=/mnt/d/recRoot/GitRepos/rdee_fortran/cbuild.gnu  # directory holding rdee_fortran ubild, must use the same FC as stated above. It's the best to use the same rdee_fortran build with the target program

export RDP_SCT_FN=test_rdProfiler.sct.csv   # rdProfiler Section-count-time filename
export RDP_REL_FN=test_rdProfiler.relation.txt   # rdProfiler relation filename

export RDP_OVH_REPEAT=5  # repeated times for profiler overhead checks