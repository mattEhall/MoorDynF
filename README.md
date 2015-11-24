*Lumped-mass mooring dynamics*

**by [Matthew Hall](mailto:matthew.hall@umit.maine.edu)**

University of Maine


MoorDyn is a lumped-mass mooring line model for simulating the dynamics of moorings connected to floating offshore structures.  It accounts for internal axial stiffness and damping forces, weight and buoyancy forces, hydrodynamic forces from Morison's equation (assuming quiescent water so far), and vertical spring-damper forces from contact with the seabed.  MoorDyn's input file format is based on that of [MAP](https://nwtc.nrel.gov/MAP).  The model supports arbitrary line interconnections, clump weights and floats, and different line properties.  

This page hosts the Fortran version of MoorDyn, which has been developed as a [FAST v8](https://nwtc.nrel.gov/FAST8) module following the FAST Modularization Framework.

For the C++ incarnation of MoorDyn, see <http://www.matt-hall.ca/MoorDyn>.  "MoorDyn C" can be compiled as a Windows DLL and features simpler functions that allow for easy coupling with other tools or scripts coded in C/C++, Fortran, Matlab/Simulink, etc.  It has recently been integrated into [WEC-Sim](https://nwtc.nrel.gov/WEC-Sim).

Both forms of MoorDyn feature the same underlying mooring model, use similar input and output conventions, and are being updated and improved in parallel.  They follow the same version numbering, with a "C" or "F" suffix for differentiation.


## Documentation

The [User's Guide](http://www.matt-hall.ca/wp-content/uploads/2014/11/MoorDyn-Users-Guide-2015-09-08.pdf) provides instructions for using MoorDyn and a basic description of the model.  This covers both the Fortran and C++ versions.

More detail about the mooring line model and some validation against 1:50-scale floating wind turbine test data is available in the paper [M. Hall and A. Goupee, “Validation of a lumped-mass mooring line model with DeepCwind semisubmersible model test data,” Ocean Engineering, vol. 104, pp. 590–603, Aug. 2015.](http://www.sciencedirect.com/science/article/pii/S0029801815002279)


## Prerequisites

MoorDyn F is included in FAST v8 and has the same requirements for compiling and running.  It can also be incorporated in other programs if they follow the conventions laid out in the [FAST Modularization Framework](https://nwtc.nrel.gov/FAST-Developers).

