#!/bin/bash
# script to calculate annual netcdfs from monthly
root="/Users/jason/Nextcloud/MME1Data/FishMIP_Phase1_Forcings/"
curr_files=($(ls ${root}*monthly*))
num_files=${#curr_files[@]}

for ((i=0; i<=num_files-1; i++)); do
curr_file=${curr_files[i]}
replacer="_annual_"
annual_name=${curr_file/_monthly_/$replacer}

cdo yearmean $curr_file $annual_name

done

# save_file=/pf/b/b380694/FishMIP_CMIP6/to_zs/
# esms=(CESM2/ GFDL-ESM4/ IPSL-CM6A-LR/ MPI-ESM1-2-HR/ UKESM1-0-LL/)
# num_esms=${#esms[@]}
# scens="picontrol/" #(historical/ ssp126/ ssp370/ ssp585/)
# num_scens=${#scens[@]}
# replacer="_annual_"

# for ((i=0; i<=num_scens-1; i++)); do
# for ((j=0; j<=num_esms-1; j++)); do
# echo ${esms[j]} ${scens[i]}
# curr_path=$root${scens[i]}${esms[j]}
# curr_file=($(ls ${curr_path}*_tos_onedeg_*))
# short_name=($(ls $curr_file | cut -f 14 -d "/"))
# annual_name=${save_file}${short_name/_monthly_/$replacer}

# cdo yearmean $curr_file $annual_name

# done
# done

