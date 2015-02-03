# Makefile for the gaussian eliminate group

# Define function to compile and link component scripts
gauss: gauss_elim.o gaussian_main.o
	gfortran -o gauss gauss_elim.o gaussian_main.o 

# Define functions for each script that compile without linking
gauss_elim.mod: gauss_elim.o gauss_elim.f90
	gfortran -c gauss_elim.f90

gauss_elim.o: gauss_elim.f90
	gfortran -c gauss_elim.f90

gaussian_main.o: gaussian_main.f90
	gfortran -c gaussian_main.f90

# Define housekeeping function
clean:
	rm gaussian_main.o gauss_elim.o gauss_elim.mod gauss 