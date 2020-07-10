
all: TEST_104 TEST_105

TEST_104:
	s.f90 -mpi -DSTAND_ALONE RPN_MPI_mpif.F90 RPN_MPI_get_mpi_definitions.F90  RPN_MPI_ez_transpose.F90  TEST_104.F90  -lnuma -o TEST_104.Abs
	rm -f *.mod

TEST_105:
	s.f90 -mpi -DSTAND_ALONE RPN_MPI_mpif.F90 RPN_MPI_get_mpi_definitions.F90 RPN_MPI_grid_topo.F90 TEST_105.F90  -lnuma -o TEST_105.Abs
	rm -f *.mod

clean:
	rm -f *.mod *.Abs
