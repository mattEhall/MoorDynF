NWTC_LIB = ./nwtc_lib/src
NWTC_NUM = ./nwtc_lib/src/NetLib


VPATH = ./nwtc_lib/src:./nwtc_lib/src/NetLib


PREC        = SingPrec
SYS_FILE    = SysGnuLinux


FC       = gfortran
FCFLAGS  = -g -O0 -fbacktrace -fbounds-check -x f95-cpp-input -ffree-line-length-none
OPT      = -fmax-errors=20


OBJ = $(PREC).o  \
	NWTC_Base.o          \
	$(SYS_FILE).o        \
	NWTC_Library_Types.o \
	NWTC_IO.o            \
	NWTC_Num.o           \
	NWTC_LAPACK.o        \
	ModMesh_Types.o      \
	ModMesh.o            \
	ModMesh_Mapping.o    \
	NWTC_Library.o       \
	MoorDyn_Types.o      \
	MoorDyn_IO.o         \
	MoorDyn.o 	     \
	MoorDyn_Driver.o


all : $(OBJ)
	$(FC) -o f_driver $(OBJ) -lblas -llapack

%.o : %.f90
	$(FC) $(FCFLAGS) -c $<

clean:		
	rm -f *.o f_driver *.*~ *.mod

memcheck:
	valgrind --tool=memcheck --leak-check=yes --show-reachable=yes --track-origins=yes ./moordyn_driver
