#!/bin/bash

# *********************************************** init
echo '>>> init, please make sure you have set the input/config.sh'
mainDir=$PWD
source ./input/config.sh
rm -rf medium
rm -rf output
mkdir -p medium output

# *********************************************** handle overhead
echo '>>> post-proc, generating new rdp-sct file witohut overhead'
# ........................... render F90 code
cd ${mainDir}/src
python rdp2f90.py

# ........................... run F90 code
cd ${mainDir}/output
$FC -o rdp-ovh.x rdp-ovh.F90 -L$RDEE_FORTRAN/lib -lrdee_fortran -I$RDEE_FORTRAN/include

for i in `seq 1 1 ${RDP_OVH_REPEAT}`
do
    ./rdp-ovh.x ../medium/$i.csv
done


# ........................... analyze results and remove the overhead
cd ${mainDir}/src
python remove-ovh.py


# *********************************************** handle profiler viz
echo '>>> data-viz, generating profiler result diagram in web'
cd ${mainDir}/src
cp index.html ../output
python txt2js.py
cp ../input/$RDP_REL_FN ../output


# *********************************************** done
echo -e '\033[32mDone\033[0m'
echo -e '\033[32mNext: \033[0m'
echo -e '\033[33m  1. open the output/index.html and check the performance for all sections specified in rdp\033[0m'
echo -e "\033[33m  2. see the output/${RDP_SCT_FN}, which removes the profiler overhead (0 means < 0.1s)\033[0m"
