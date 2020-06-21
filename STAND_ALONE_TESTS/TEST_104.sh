#!/bin/bash
set -x
s.f90 -mpi -DSTAND_ALONE RPN_MPI_mpif.F90 RPN_MPI_get_mpi_definitions.F90  RPN_MPI_ez_transpose.F90  TEST_104.F90  -lnuma -o TEST_104.Abs
#
# -pgm ./TEST_104.Abs -args GNI GNJ GNK NPEX NPEY EZFLAG
# EZFLAG = 1 : test ez routines,    EZFLAG = 0 : test RPN_MPI_transpose_ routines directly
# global grid 298x98x5  npex = 4, npex = 4 , test ez routines with their setup
#r.run_in_parallel -inorder -tag -maxcores -npex 4 -npey 4 -pgm ./TEST_104.Abs -args 298  98  5 4 4 1
