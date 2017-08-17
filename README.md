*Lumped-mass mooring dynamics*

**by [Matthew Hall](http://www.matt-hall.ca)**  
University of Prince Edward Island


MoorDyn is a lumped-mass mooring line model for simulating the dynamics of moorings connected to floating offshore structures.  It accounts for internal axial stiffness and damping forces, weight and buoyancy forces, hydrodynamic forces from Morison's equation (assuming quiescent water so far), and vertical spring-damper forces from contact with the seabed.  MoorDyn's input file format is based on that of [MAP](https://nwtc.nrel.gov/MAP).  The model supports arbitrary line interconnections, clump weights and floats, and different line properties.  

This page hosts the Fortran implementation of MoorDyn, which has been developed following the [FAST Modularization Framework](https://nwtc.nrel.gov/FAST-Developers).  It is included as a module in [FAST v8](https://nwtc.nrel.gov/FAST8).

For the C++ implementation of MoorDyn, see <http://www.matt-hall.ca/moordyn>.  "MoorDyn C" can be compiled as a dynamically-linked library and features simpler functions for easy coupling with models or scripts coded in C/C++, Fortran, Matlab/Simulink, etc.  It has recently been integrated into [WEC-Sim](https://nwtc.nrel.gov/WEC-Sim).

Both forms of MoorDyn feature the same underlying mooring model, use similar input and output conventions, and are being updated and improved in parallel.  They follow the same version numbering, with a "C" or "F" suffix for differentiation.


## Documentation

The [User's Guide](http://www.matt-hall.ca/files/MoorDyn-Users-Guide-2017-08-16.pdf) (updated Dec. 15, 2015) provides guidance for using MoorDyn and a basic description of the model.  This covers both the Fortran and C++ versions.

More detail about the mooring line model and some validation against 1:50-scale floating wind turbine test data is available in the paper [M. Hall and A. Goupee, “Validation of a lumped-mass mooring line model with DeepCwind semisubmersible model test data,” Ocean Engineering, vol. 104, pp. 590–603, Aug. 2015.](http://www.sciencedirect.com/science/article/pii/S0029801815002279)


## Latest Version

MoorDyn v1.00.02F (Nov. 24, 2015) provides an important bugfix and some minor improvements.  A critical error for multi-segmented cases has been resolved.  Cases in which line connections encounter the seabed are handled.  The calculation of drag forces on connection objects is corrected.  More output channels are implemented.  NaN states (instability) are detected.

