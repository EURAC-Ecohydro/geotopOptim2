#$ -N geotopoptim2
#$ -V
#$ -pe mpich 32
#$ -l h_rt=01:00:00
#$ -M emanuele.cordano@gmail.com
#$ -m beas  # all job events sent via email
##$ -q long.q
##$ -q devel.q

export I_MPI_PIN_PROCESSOR_LIST=1,14,9,6,5,10,13,2,3,12,11,4,7,8,15,0
mkdir /tmp/geotopOptim_test2
 
mpirun -machinefile $TMPDIR/machines -np $NSLOTS R CMD BATCH psolhoat_example_script_v2.R

rm -rf /tmp/geotopOptim_tests

