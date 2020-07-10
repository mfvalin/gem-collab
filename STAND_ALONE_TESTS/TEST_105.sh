#!/bin/bash
set -x
s.f90 -mpi -DSTAND_ALONE RPN_MPI_mpif.F90 RPN_MPI_get_mpi_definitions.F90 RPN_MPI_grid_topo.F90 TEST_105.F90  -lnuma -o TEST_105.Abs
#
# -pgm ./TEST_105.Abs -args npex npey blkx blky x_first EZFLAG
# EZFLAG = 1 : test ez routines,    EZFLAG = 0 : test RPN_MPI_grid_topo routines directly
# npex = 4, npex = 4 , blkx = 2, blky = 2 , distribute along x first, direct test (2 x 2 blocks of PEs)
#r.run_in_parallel -npex 16 -npey 1 -maxcores -inorder -pgm ./TEST_105.Abs -args 4 4 2 2 1 0 -minstdout 5
# npex = 4, npex = 4 , blkx = 2, blky = 2 , distribute along y first, direct test (2 x 2 blocks of PEs)
#r.run_in_parallel -npex 16 -npey 1 -maxcores -inorder -pgm ./TEST_105.Abs -args 4 4 2 2 0 0 -minstdout 5
