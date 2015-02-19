#if 0
# DO NOT DELETE Everything here to the endif is Registry file for ModuleFluid  DO NOT DELETE
ifdef registry_modulefluid

  include Registry_NWTC_Library.txt

  typedef ModuleFluid/Fluid   InitInputType          ReKi     Dummy
  typedef ModuleFluid/Fluid   InputType              MeshType Mesh
  typedef ModuleFluid/Fluid   OutputType             MeshType Mesh
  typedef ModuleFluid/Fluid   ContinuousStateType    ReKi     Dummy
  typedef ModuleFluid/Fluid   DiscreteStateType      ReKi     Dummy
  typedef ModuleFluid/Fluid   ConstraintStateType    ReKi     Dummy
  typedef ModuleFluid/Fluid   OtherStateType         ReKi     Dummy
  typedef ModuleFluid/Fluid   ParameterType          ReKi     Dummy

endif
ifdef not_in_registry
#endif

! Mesh 1 (call it fluid)
!
! xyz
! 0.0  0.0  0.0
! 0.2  0.0  0.0
! 0.4  0.0  0.0
! 0.8  0.0  0.0
! 1.0  0.0  0.0
!
! elements (2-node lines)
! 1 2
! 2 3
! 3 4
! 4 5


MODULE ModuleFluid
  USE ModuleFluid_Types  ! also USE-associates NWTC_Library and MeshType
  USE ModMesh

  PUBLIC :: Fluid_Init

CONTAINS
   SUBROUTINE Fluid_Init( InitData, InputGuess, ParamData, ContStates, DiscStates &
                         ,ConstrStateGuess, OtherStates, OutData, Interval        &
                         ,ErrStat, ErrMess  )
      TYPE(Fluid_InitInputType),       INTENT(IN   )  :: InitData          ! Input data for initialization
      TYPE(Fluid_InputType),           INTENT(  OUT)  :: InputGuess        ! An initial guess for the input; 
                                                                           ! the input mesh must be defined
      TYPE(Fluid_ParameterType),       INTENT(  OUT)  :: ParamData         ! Parameters      
      TYPE(Fluid_ContinuousStateType), INTENT(  OUT)  :: ContStates        ! Initial continuous states
      TYPE(Fluid_DiscreteStateType),   INTENT(  OUT)  :: DiscStates        ! Initial discrete states
      TYPE(Fluid_ConstraintStateType), INTENT(  OUT)  :: ConstrStateGuess  ! Initial guess of the constraint states
      TYPE(Fluid_OtherStateType),      INTENT(  OUT)  :: OtherStates       ! Initial other/optimization states            
      TYPE(Fluid_OutputType),          INTENT(  OUT)  :: OutData           ! Initial output (outputs are not calculated; 
                                                                           ! only the output mesh is initialized)
      REAL(DbKi),                      INTENT(INOUT)  :: Interval          ! Coupling interval in seconds: the rate that 
                                                                           ! (1) Fluid_Step() is called in loose coupling and
                                                                           ! (2) Fluid_UpdateDiscState() is called in tight 
                                                                           !     coupling; Input is the suggested time from the 
                                                                           !     glue code; Output is the actual coupling interval 
                                                                           !     that will be used by the glue code.
      INTEGER(IntKi),                  INTENT(  OUT)  :: ErrStat           ! Error status of the operation
      CHARACTER(*),                    INTENT(  OUT)  :: ErrMess            ! Error message if ErrStat /= ErrID_None

     ! Local Var
      INTEGER i

      CALL MeshCreate( InputGuess%Mesh        &
                      ,IOS=COMPONENT_INPUT    &
                      ,Nnodes=5               &
                      ,Force=.TRUE.           &   ! add other fields if you want
                      ,ErrStat=ErrStat        &
                      ,ErrMess=ErrMess        &
                     )

      CALL MeshPositionNode(InputGuess%Mesh, Inode=1, Pos=(/0.0, 0.0, 0.0/), ErrStat=ErrStat, ErrMess=ErrMess)
      CALL MeshPositionNode(InputGuess%Mesh, Inode=2, Pos=(/0.2, 0.0, 0.0/), ErrStat=ErrStat, ErrMess=ErrMess)
      CALL MeshPositionNode(InputGuess%Mesh, Inode=3, Pos=(/0.4, 0.0, 0.0/), ErrStat=ErrStat, ErrMess=ErrMess)
      CALL MeshPositionNode(InputGuess%Mesh, Inode=4, Pos=(/0.8, 0.0, 0.0/), ErrStat=ErrStat, ErrMess=ErrMess)
      CALL MeshPositionNode(InputGuess%Mesh, Inode=5, Pos=(/1.0, 0.0, 0.0/), ErrStat=ErrStat, ErrMess=ErrMess)


      DO i = 1, 4
         CALL MeshConstructElement(InputGuess%Mesh, ELEMENT_LINE2, P1=i, P2=i+1, ErrStat=ErrStat, ErrMess=ErrMess)
      ENDDO

      CALL MeshCommit(InputGuess%Mesh, ErrStat=ErrStat, ErrMess=ErrMess)
      CALL MeshPrintInfo(0,InputGuess%Mesh)

      CALL MeshCopy( InputGuess%Mesh       &
                    ,OutData%Mesh          &
                    ,CtrlCode=MESH_SIBLING &
                    ,Moment=.TRUE.         &   ! add other fields if you want
                    ,ErrStat=ErrStat       &
                    ,ErrMess=ErrMess       &
                   )
      CALL MeshCommit(OutData%Mesh, ErrStat=ErrStat, ErrMess=ErrMess)

write(0,*)'Fluid -- MeshPrintInfo for OutData'
      CALL MeshPrintInfo(0,OutData%Mesh)


   END SUBROUTINE Fluid_Init

END MODULE ModuleFluid


! 
! Mesh 2 (call it structure)
! 
! xyz
! 0.0 0.0
! 0.3 0.0
! 0.6 0.0
! 1.0 0.0
! 
! elements (2-node lines)
! 1 2
! 2 3
! 3 4                                                                             

#if 0
endif
# Everything here to the endif is Registry for ModuleStructure
# DO NOT DELETE Everything here to the endif is Registry file for ModuleFluid  DO NOT DELETE
ifdef registry_modulestructure

  include Registry_NWTC_Library.txt

  typedef ModuleStructure/Structure   InitInputType          ReKi     Dummy
  typedef ModuleStructure/Structure   InputType              MeshType Mesh
  typedef ModuleStructure/Structure   OutputType             MeshType Mesh
  typedef ModuleStructure/Structure   ContinuousStateType    ReKi     Dummy
  typedef ModuleStructure/Structure   DiscreteStateType      ReKi     Dummy
  typedef ModuleStructure/Structure   ConstraintStateType    ReKi     Dummy
  typedef ModuleStructure/Structure   OtherStateType         ReKi     Dummy
  typedef ModuleStructure/Structure   ParameterType          ReKi     Dummy

endif
ifdef not_in_registry
#endif


MODULE ModuleStructure
  USE ModuleStructure_Types  ! also USE-associates NWTC_Library and MeshType
  USE ModMesh

  PUBLIC :: Structure_Init

CONTAINS
   SUBROUTINE Structure_Init( InitData, InputGuess, ParamData, ContStates, DiscStates &
                         ,ConstrStateGuess, OtherStates, OutData, Interval        &
                         ,ErrStat, ErrMess  )
      TYPE(Structure_InitInputType),       INTENT(IN   )  :: InitData          ! Input data for initialization
      TYPE(Structure_InputType),           INTENT(  OUT)  :: InputGuess        ! An initial guess for the input; 
                                                                           ! the input mesh must be defined
      TYPE(Structure_ParameterType),       INTENT(  OUT)  :: ParamData         ! Parameters      
      TYPE(Structure_ContinuousStateType), INTENT(  OUT)  :: ContStates        ! Initial continuous states
      TYPE(Structure_DiscreteStateType),   INTENT(  OUT)  :: DiscStates        ! Initial discrete states
      TYPE(Structure_ConstraintStateType), INTENT(  OUT)  :: ConstrStateGuess  ! Initial guess of the constraint states
      TYPE(Structure_OtherStateType),      INTENT(  OUT)  :: OtherStates       ! Initial other/optimization states            
      TYPE(Structure_OutputType),          INTENT(  OUT)  :: OutData           ! Initial output (outputs are not calculated; 
                                                                           ! only the output mesh is initialized)
      REAL(DbKi),                      INTENT(INOUT)  :: Interval          ! Coupling interval in seconds: the rate that 
                                                                           ! (1) Structure_Step() is called in loose coupling and
                                                                           ! (2) Structure_UpdateDiscState() is called in tight 
                                                                           !     coupling; Input is the suggested time from the 
                                                                           !     glue code; Output is the actual coupling interval 
                                                                           !     that will be used by the glue code.
      INTEGER(IntKi),                  INTENT(  OUT)  :: ErrStat           ! Error status of the operation
      CHARACTER(*),                    INTENT(  OUT)  :: ErrMess            ! Error message if ErrStat /= ErrID_None

     ! Local Var
      INTEGER i

      ErrStat = 0
      ErrMess = ""
      CALL MeshCreate( InputGuess%Mesh        &
                      ,IOS=COMPONENT_INPUT    &
                      ,Nnodes=5               &
                      ,Force=.TRUE.           &   ! add other fields if you want
                      ,ErrStat=ErrStat        &
                      ,ErrMess=ErrMess        &
                     )

      CALL MeshPositionNode(InputGuess%Mesh, Inode=1, Pos=(/0.0, 0.0, 0.0/), ErrStat=ErrStat, ErrMess=ErrMess)
      CALL MeshPositionNode(InputGuess%Mesh, Inode=2, Pos=(/0.3, 0.0, 0.0/), ErrStat=ErrStat, ErrMess=ErrMess)
      CALL MeshPositionNode(InputGuess%Mesh, Inode=3, Pos=(/0.6, 0.0, 0.0/), ErrStat=ErrStat, ErrMess=ErrMess)
      CALL MeshPositionNode(InputGuess%Mesh, Inode=4, Pos=(/1.0, 0.0, 0.0/), ErrStat=ErrStat, ErrMess=ErrMess)

      DO i = 1, 3
         CALL MeshConstructElement(InputGuess%Mesh, ELEMENT_LINE2, P1=i, P2=i+1, ErrStat=ErrStat, ErrMess=ErrMess)
      ENDDO

      CALL MeshCommit(InputGuess%Mesh, ErrStat=ErrStat, ErrMess=ErrMess)
WRITE(0,*)' ******************************************************* '
write(0,*)'Structure -- MeshPrintInfo for InputGuess'
WRITE(0,*)' ******************************************************* '
      CALL MeshPrintInfo(0,InputGuess%Mesh)

      CALL MeshCopy( InputGuess%Mesh       &
                    ,OutData%Mesh          &
                    ,CtrlCode=MESH_SIBLING &
                    ,Moment=.TRUE.         &   ! add other fields if you want
                    ,ErrStat=ErrStat       &
                    ,ErrMess=ErrMess       &
                   )
      CALL MeshCommit(OutData%Mesh, ErrStat=ErrStat, ErrMess=ErrMess)
WRITE(0,*)' ******************************************************* '
write(0,*)'Structure -- MeshPrintInfo for OutData'
WRITE(0,*)' ******************************************************* '
      CALL MeshPrintInfo(0,OutData%Mesh)

      RETURN

   END SUBROUTINE Structure_Init

END MODULE ModuleStructure

PROGRAM example
  USE ModuleFluid
  USE ModuleStructure
  USE ModMesh
  IMPLICIT NONE

  TYPE(    Fluid_InitInputType)                       :: Fld_InitData          ! Input data for initialization
  TYPE(    Fluid_InputType)                           :: Fld_InputGuess        ! An initial guess for the input;
                                                                           ! the input mesh must be defined
  TYPE(    Fluid_ParameterType)                       :: Fld_ParamData         ! Parameters
  TYPE(    Fluid_ContinuousStateType)                 :: Fld_ContStates        ! Initial continuous states
  TYPE(    Fluid_DiscreteStateType)                   :: Fld_DiscStates        ! Initial discrete states
  TYPE(    Fluid_ConstraintStateType)                 :: Fld_ConstrStateGuess  ! Initial guess of the constraint states
  TYPE(    Fluid_OtherStateType)                      :: Fld_OtherStates       ! Initial other/optimization states
  TYPE(    Fluid_OutputType)                          :: Fld_OutData           ! Initial output (outputs are not calculated;

  TYPE(Structure_InitInputType)                       :: Str_InitData          ! Input data for initialization
  TYPE(Structure_InputType)                           :: Str_InputGuess        ! An initial guess for the input;
                                                                           ! the input mesh must be defined
  TYPE(Structure_ParameterType)                       :: Str_ParamData         ! Parameters
  TYPE(Structure_ContinuousStateType)                 :: Str_ContStates        ! Initial continuous states
  TYPE(Structure_DiscreteStateType)                   :: Str_DiscStates        ! Initial discrete states
  TYPE(Structure_ConstraintStateType)                 :: Str_ConstrStateGuess  ! Initial guess of the constraint states
  TYPE(Structure_OtherStateType)                      :: Str_OtherStates       ! Initial other/optimization states
  TYPE(Structure_OutputType)                          :: Str_OutData           ! Initial output (outputs are not calculated;

  REAL(DbKi)                                          :: Interval              ! Coupling interval in seconds: the rate that 
  INTEGER                                             :: ErrStat
  CHARACTER(256)                                      :: ErrMess  ! check the size of this

  REAL(ReKi), ALLOCATABLE, DIMENSION(:)    :: ReBuf
  REAL(DbKi), ALLOCATABLE, DIMENSION(:)    :: DbBuf
  INTEGER(IntKi), ALLOCATABLE, DIMENSION(:) :: IntBuf
  TYPE(MeshType) :: testmesh

!  mesh_debug = .TRUE. !bjj removed here; please change the parameter in ModMesh_Types.f90 instead of setting this value here.
  CALL           Fluid_Init( Fld_InitData, Fld_InputGuess, Fld_ParamData, Fld_ContStates, Fld_DiscStates &
                            ,Fld_ConstrStateGuess, Fld_OtherStates, Fld_OutData                          &
                            ,Interval, ErrStat, ErrMess  )

  CALL       Structure_Init( Str_InitData, Str_InputGuess, Str_ParamData, Str_ContStates, Str_DiscStates &
                            ,Str_ConstrStateGuess, Str_OtherStates, Str_OutData                          &
                            ,Interval, ErrStat, ErrMess  )
  
  CALL MeshPack( Fld_InputGuess%Mesh, ReBuf, DbBuf, IntBuf , ErrStat, ErrMess ) ;
  IF ( ErrStat .NE. 0 ) WRITE(0,*)'Mesh Pack: ',TRIM(ErrMess)
  CALL MeshUnPack( testmesh, ReBuf, DbBuf, IntBuf , ErrStat, ErrMess ) ;
  IF ( ErrStat .NE. 0 ) WRITE(0,*)'Mesh Pack: ',TRIM(ErrMess)
  WRITE(0,*)' ******************************************************* '
  WRITE(0,*)' *** Fld_InputGuess%Mesh after packing and unpacking *** '
  WRITE(0,*)' ******************************************************* '
  CALL MeshPrintInfo(0,testmesh)

  DEALLOCATE(ReBuf, DbBuf, IntBuf)
  CALL MeshDestroy(testmesh,ErrStat,ErrMess)

  CALL MeshPack( Str_InputGuess%Mesh, ReBuf, DbBuf, IntBuf , ErrStat, ErrMess ) ;
  IF ( ErrStat .NE. 0 ) WRITE(0,*)'Mesh Pack: ',TRIM(ErrMess)
  CALL MeshUnPack( testmesh, ReBuf, DbBuf, IntBuf , ErrStat, ErrMess ) ;
  IF ( ErrStat .NE. 0 ) WRITE(0,*)'Mesh Pack: ',TRIM(ErrMess)
  WRITE(0,*)' ******************************************************* '
  WRITE(0,*)' *** Str_InputGuess%Mesh after packing and unpacking *** '
  WRITE(0,*)' ******************************************************* '
  CALL MeshPrintInfo(0,testmesh)

  DEALLOCATE(ReBuf, DbBuf, IntBuf)

END PROGRAM example

