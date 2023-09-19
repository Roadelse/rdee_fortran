#!/bin/bash


# export FC=gfortran
export FC=mpif90
export RDEE_FORTRAN=/mnt/d/recRoot/GitRepos/rdee_fortran/cbuild.gnu  # directory holding rdee_fortran ubild, must use the same FC as stated above. It's the best to use the same rdee_fortran build with the target program

export RDP_SCT_FN=p0.cmaq.prof.sct.csv   # rdProfiler Section-count-time filename

export RDP_OVH_REPEAT=5  # repeated times for profiler overhead checks