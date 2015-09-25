MoorDyn is a lumped-mass mooring line model for simulating the dynamics of moorings connected to floating offshore structures.  It accounts for internal axial stiffness and damping forces, weight and buoyancy forces, hydrodynamic forces from Morison's equation (assuming quiescent water), and vertical spring-damper forces from contact with the seabed.  MoorDyn's input file format is based on that of MAP.  The model supports arbitrary line interconnections, clump weights and floats, and different line properties.  

Author: [Matthew Hall](mailto:matthew.hall@umit.maine.edu)

Two versions of MoorDyn exist: the Fortran FAST v8 module hosted on this page, and a C++ implementation designed for easy coupling with models (in C, Fortran, Matlab/Simulink, etc.).  The latter version is hosted at <http://www.matt-hall.ca/software/MoorDyn>.


## Documentation

The [User's Guide](http://www.matt-hall.ca/wp-content/uploads/2014/11/MoorDyn-Users-Guide-2015-09-08.pdf) provides instructions for using MoorDyn and a basic description of the model.  This covers both the Fortran and C++ versions.

More detail about the mooring line model and some validation against 1:50-scale floating wind turbine test data is available in the paper [M. Hall and A. Goupee, “Validation of a lumped-mass mooring line model with DeepCwind semisubmersible model test data,” Ocean Engineering, vol. 104, pp. 590–603, Aug. 2015.](http://www.sciencedirect.com/science/article/pii/S0029801815002279)


## Prerequisites

Designed as a FAST v8 module, MoorDyn (F) needs to be called by a master program that follows the FAST Modularization Framework, such as FAST v8, to drive its operation.  Compiling of this combined Fortran code requires a suitable compiler.

