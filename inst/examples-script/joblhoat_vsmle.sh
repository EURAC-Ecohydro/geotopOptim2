#$ -N geotopoptim2
#$ -V
#$ -pe mpich 16
##$ -l h_rt=01:00:00
#$ -M emanuele.cordano@gmail.com
#$ -m beas  # all job events sent via email
#$ -q long.q
##$ -q devel.q

#export I_MPI_PIN_PROCESSOR_LIST=1,14,9,6,5,10,13,2,3,12,11,4,7,8,15,0
export GEOTOPOTIM2_TEMP_DIR=/tmp/geotopOptim_test2
mkdir $GEOTOPOTIM2_TEMP_DIR
 
####mpirun -machinefile $TMPDIR/machines -np $NSLOTS 

R CMD BATCH psolhoat_example_script_vsmle.R

rm -rf $GEOTOPOTIM2_TEMP_DIR

