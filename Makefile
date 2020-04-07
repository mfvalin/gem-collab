
cpu_ticks.o: cpu_ticks.c
	gcc -c cpu_ticks.c

RPN_COMM_simple_halo_exchange.o : RPN_COMM_simple_halo_exchange.F90
	s.f90 -mpi -O2 -c RPN_COMM_simple_halo_exchange.F90
	
halo: RPN_COMM_simple_halo_exchange.o cpu_ticks.o
	s.f90 -mpi -DSELF_TEST RPN_COMM_simple_halo_exchange.F90 cpu_ticks.o RPN_COMM_simple_halo_exchange.o

clean:
	rm -f a.out *.s *.o *.mod
