PROGRAM Main

  USE MoorDyn_Types
  USE MoorDyn
  USE NWTC_Library

  IMPLICIT NONE

  INTEGER(IntKi)                         :: i,j, j_ss        ! generic loop counter
  INTEGER(IntKi)                         :: ErrStat          ! Status of error message
  CHARACTER(1024)                        :: ErrMsg           ! Error message if ErrStat /= ErrID_None

  REAL(DbKi)                             :: dt_global        ! fixed/constant global time step
  REAL(DbKi)                             :: t_initial        ! time at initialization
  REAL(DbKi)                             :: t_final          ! time at simulation end
  REAL(DbKi)                             :: t_global         ! global-loop time marker

  INTEGER(IntKi)                         :: n_t_final        ! total number of time steps
  INTEGER(IntKi)                         :: n_t_global       ! global-loop time counter

  TYPE (MD_InitInputType)                :: InitInData_MD
  TYPE (MD_InitOutputType)               :: InitOutData_MD
  TYPE (MD_InputType)                    :: u_MD
  TYPE (MD_ParameterType)                :: p_MD
  TYPE (MD_ContinuousStateType)          :: x_MD
  TYPE (MD_DiscreteStateType)            :: xd_MD
  TYPE (MD_ConstraintStateType)          :: z_MD
  TYPE (MD_OtherStateType)               :: other_MD

  TYPE(MD_ContinuousStateType)           :: x_MD_pred                              ! Predicted continuous states
  TYPE(MD_DiscreteStateType)             :: xd_MD_pred                             ! Predicted discrete states
  TYPE(MD_ConstraintStateType)           :: z_MD_pred                              ! Predicted constraint states
  TYPE(MD_OtherStateType)                :: other_MD_old                           ! Other/optimization states (copied for the case of subcycling)

  TYPE (MD_InputType),      ALLOCATABLE  :: MD_Input(:)
  REAL(DbKi) , DIMENSION(:), ALLOCATABLE :: MD_InputTimes(:)

  TYPE (MD_OutputType)                    :: y_MD
  REAL(DbKi) , DIMENSION(:), ALLOCATABLE  :: MD_OutputTimes

  INTEGER(IntKi)                          :: MD_interp_order     ! order of interpolation/extrapolation

  CHARACTER(1024)                                :: OutFileName          ! The name of the output file  including the full path.

  INTEGER(IntKi)                          :: UnOutFile     ! output file
   REAL(ReKi)   :: MDWrOutput(10)        ! one line of output data  ! should allocate in future! <<<

   CHARACTER(200)                         :: Frmt                        ! a string to hold a format statement
   CHARACTER(20)                          :: TempString


  ! -------------------------------------------------------------------------
  ! Initialization of glue-code time-step variables
  ! -------------------------------------------------------------------------

  t_initial = 0.
  t_final   = 100.0

  ! specify time increment; currently, all modules will be time integrated with this increment size
  dt_global = 0.1
  n_t_final = ((t_final - t_initial) / dt_global ) - 1
  t_global = t_initial


  ! set the MD input file name and other environment terms
  InitInData_MD%g        = 9.81
  InitInData_MD%rhoW     = 1025
  InitInData_MD%WtrDepth = 200
  InitInData_MD%PtfmInit = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0/)  ! initial position of platform (because MoorDyn isn't provided a mesh of fairlead locations initially)
  InitInData_MD%FileName = "MoorDyn.dat"
  InitInData_MD%RootName = "MDdriver"


  ! @bonnie : is this right? What's a good interp order?
  ! @marco: the interp order is specified in the FAST input file. It can be 0, 1, or 2
  MD_interp_order = 0

  ! MD: allocate Input and Output arrays; used for interpolation and extrapolation
!  Allocate(MD_InputTimes(MD_interp_order + 1))

  ! @bonnie : This is in the FAST developers glue code example, but it's probably not needed here.
  ALLOCATE(MD_Input(MD_interp_order + 1), MD_InputTimes(MD_interp_order + 1), STAT=ErrStat )
  IF (ErrStat /= 0) CALL CheckError(ErrID_Fatal,"Error allocating MD_Input and MD_InputTimes.")


  ! call the initialization routine
  CALL MD_Init(InitInData_MD , &
               MD_Input(1)   , &
               p_MD          , &
               x_MD          , &
               xd_MD         , &
               z_MD          , &
               other_MD      , &
               y_MD          , &
               dt_global     , &
               InitOutData_MD, &
               ErrStat       , &
               ErrMsg )
  IF (ErrStat > ErrID_None) THEN
      CALL WrScr(ErrMsg)
      Call WrScr("Error in calling MD_Init.  Driver program terminating.")
      call EXIT()
   END IF

   CALL DispNVD(InitOutData_MD%Ver)

!PRINT *, 'size of PtFairleadDisplacement-TranslationDisp is:  ', size(MD_Input(1)%PtFairleadDisplacement%TranslationDisp)


   CALL MD_DestroyInitInput(InitInData_MD, ErrStat, ErrMsg)
   CALL MD_DestroyInitOutput(InitOutData_MD, ErrStat, ErrMsg)


   DO j = 1, MD_interp_order + 1
      MD_InputTimes(j) = t_initial - (j - 1) * dt_global
   END DO

   DO j = 2, MD_interp_order + 1
      CALL MD_CopyInput (MD_Input(1),  MD_Input(j),  MESH_NEWCOPY, Errstat, ErrMsg)
      CALL CheckError( ErrStat, 'Message from MD_CopyInput (MD_Input): '//NewLine//ErrMsg )
   END DO
   CALL MD_CopyInput (MD_Input(1),  u_MD,  MESH_NEWCOPY, Errstat, ErrMsg) ! do this to initialize meshes/allocatable arrays for output of ExtrapInterp routine
   CALL CheckError( ErrStat, 'Message from MD_CopyInput (u_MD): '//NewLine//ErrMsg )

   ! Initialize predicted states for j_pc loop:
   CALL MD_CopyContState   ( x_MD,  x_MD_pred, MESH_NEWCOPY, Errstat, ErrMsg)
   CALL CheckError( ErrStat, 'Message from MD_CopyContState (init): '//NewLine//ErrMsg )
   CALL MD_CopyDiscState   (xd_MD, xd_MD_pred, MESH_NEWCOPY, Errstat, ErrMsg)
   CALL CheckError( ErrStat, 'Message from MD_CopyDiscState (init): '//NewLine//ErrMsg )
   CALL MD_CopyConstrState ( z_MD,  z_MD_pred, MESH_NEWCOPY, Errstat, ErrMsg)
   CALL CheckError( ErrStat, 'Message from MD_CopyConstrState (init): '//NewLine//ErrMsg )

  ! ------------------------------------------------------------------------------------
  ! open output file to start recording fairlead tensions to check they're returned

  ! Open the file for output
  OutFileName = 'MD_Driver.out'
  CALL GetNewUnit( UnOutFile )

  CALL OpenFOutFile ( UnOutFile, OutFileName, ErrStat, ErrMsg )
  IF ( ErrStat > ErrID_None ) THEN
     ErrMsg = ' Error opening MoorDyn-Driver output file: '//TRIM(ErrMsg)
  END IF


  ! Write the output file header

  !   WRITE (p%UnOutFile,'(/,A/)', IOSTAT=ErrStat2)  'These predictions were generated by '//TRIM(GETNVD(ProgVer))//&
  !                  ' on '//CurDate()//' at '//CurTime()//'.'
  !   WRITE(p%UnOutFile, '(//)') ! add 3 lines to make file format consistant with FAST v8 (headers on line 7; units on line 8) [this allows easier post-processing]

  !Write the names of the output parameters:

  Frmt = '(A10, A10,'//TRIM(Int2LStr(p_MD%NFairs))//'(A1,A10))'

  WRITE(UnOutFile,Frmt)  TRIM( 'Time' ), ' FairX', ( ' ', 'FairTen', I=1,p_MD%NFairs )


  Frmt = '(A10, A10,'//TRIM(Int2LStr(p_MD%NFairs))//'(A1,A10))'

  WRITE(UnOutFile,Frmt)  TRIM( '(s)' ), ' (m)', ( ' ', TRIM( 'kN' ), I=1,p_MD%NFairs )




  Print *, 'MoorDyn_driver: starting time stepping'

   ! -------------------------------------------------------------------------
   ! BEGIN time marching
   ! -------------------------------------------------------------------------
   DO n_t_global = 0, n_t_final
      t_global =  t_initial + dt_global*n_t_global

      CALL WrOver('MoorDyn_driver: t = '//Num2LStr(t_global))



      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Step 1.a: Extrapolate Inputs (gives predicted values at t+dt
      !
      ! a) Extrapolate inputs (and outputs -- bjj: output extrapolation not necessary, yet)
      !    to t + dt (i.e., t_global_next); will only be used by modules with an implicit dependence on input data.
      ! b) Shift "window" of the ModName_Input and ModName_Output
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      CALL MD_Input_ExtrapInterp(MD_Input, MD_InputTimes, u_MD, t_global, ErrStat, ErrMsg)
      CALL CheckError(ErrStat,'Message from MD_Input_ExtrapInterp (FAST): '//NewLine//ErrMsg )
      DO j = MD_interp_order, 1, -1
         CALL MD_CopyInput (MD_Input(j),  MD_Input(j+1),  MESH_UPDATECOPY, Errstat, ErrMsg)
         MD_InputTimes(j+1) = MD_InputTimes(j)
      END DO

      CALL MD_CopyInput (u_MD,  MD_Input(1),  MESH_UPDATECOPY, Errstat, ErrMsg)
      MD_InputTimes(1) = t_global


      ! Matt: I haven't figured out the above yet, but I'll move the fairlead positions manually here >>>>>>>>>>>>>>>>
      DO J=1,p_MD%NFairs
        MD_Input(1)%PtFairleadDisplacement%TranslationDisp(1,J) = 30.0*sin(t_global*Pi*0.1) ! move fairleads sinusoidally in x
        MD_Input(1)%PtFairleadDisplacement%TranslationVel(1,J) = Pi*3.0*cos(t_global*Pi*0.1) ! adjust fairlead x velocity accordingly
      END DO
      ! lazy limitation: the above moves each fairlead's x coordinate about zero <<<


      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Step 1.b: Advance states (yield state and constraint values at t_global_next)
      !
      ! x, xd, and z contain val0ues at t_global;
      ! values at t_global_next are stored in the *_pred variables.
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !----------------------------------------------------------------------------------------
      ! copy the states at step t_global and get prediction for step t_global_next
      ! (note that we need to copy the states because UpdateStates updates the values
      ! and we need to have the old values [at t_global] for the next j_pc step)
      !----------------------------------------------------------------------------------------
      ! ElastoDyn: get predicted states
      ! AeroDyn: get predicted states
      ! ServoDyn: get predicted states
      ! HydroDyn: get predicted states
      ! SubDyn: get predicted states
      ! MD/FEAM: get predicted states

      CALL MD_CopyContState   ( x_MD,  x_MD_pred, MESH_UPDATECOPY, Errstat, ErrMsg)
      CALL MD_CopyDiscState   (xd_MD, xd_MD_pred, MESH_UPDATECOPY, Errstat, ErrMsg)
      CALL MD_CopyConstrState ( z_MD,  z_MD_pred, MESH_UPDATECOPY, Errstat, ErrMsg)

      ! IF ( p_FAST%n_substeps( Module_MD ) > 1 ) THEN
      !    CALL MD_CopyOtherState( OtherSt_MD, OtherSt_MD_old, MESH_UPDATECOPY, Errstat, ErrMsg)
      ! END IF

!      DO j_ss = 1, 1 !p_FAST%n_substeps( Module_MD )
!         ! n_t_module = n_t_global*p_FAST%n_substeps( Module_MD ) + j_ss - 1
!         ! t_module   = n_t_module*p_FAST%dt_module( Module_MD )
         CALL  MD_UpdateStates(t_global       , &
                                n_t_global     , &
                                MD_Input      , &
                                MD_InputTimes , &
                                p_MD          , &
                                x_MD_pred          , &
                                xd_MD         , &
                                z_MD          , &
                                other_MD      , &
                                ErrStat        , &
                                ErrMsg )
         IF (ErrStat.NE.0) THEN
            CALL WrScr(ErrMsg)
         END IF
         ! CALL CheckError( ErrStat, 'Message from MD_UpdateStates: '//NewLine//ErrMsg )
!      END DO !j_ss


      !==========   NOTE   ======     <-----------------------------------------+
      ! Artificially change the fairlead displacement

      MD_InputTimes(1) = t_global + dt_global
      ! MD_InputTimes(2) = MD_InputTimes(1) - dt_global
      ! MD_InputTimes(3) = MD_InputTimes(2) - dt_global

!PRINT *, 'size of MD_Input is:  ', size(MD_Input)
!PRINT *, 'size of PtFairleadDisplacement-TranslationDisp is:  ', size(MD_Input(1)%PtFairleadDisplacement%TranslationDisp)

      !MD_Input(1)%PtFairleadDisplacement%TranslationDisp(1,1) = 1.0*n_t_global
      ! MD_Input(2)%PtFairDisplacement%TranslationDisp(1,1) = 1*n_t_global
      ! MD_Input(3)%PtFairDisplacement%TranslationDisp(1,1) = 1*n_t_global
      !===========================================================================

      CALL MD_CalcOutput(t_global    , &
                         MD_Input(1) , &
                         p_MD        , &
                         x_MD_pred        , &
                         xd_MD       , &
                         z_MD        , &
                         other_MD    , &
                         y_MD        , &
                         ErrStat     , &
                         ErrMsg)
      IF (ErrStat.NE.0) THEN
         CALL WrScr(ErrMsg)
      END IF

  !    pause

    ! note: position of fairlead nodes in y_MD%PtFairleadLoad is not currently set


!  ! gather the required output quantities
!  DO I = 1,p_MD%NFairs
!  !  Sum1 = 0.0_ReKi
!  !  DO J=1,3
!  !    Sum1 =
!
!    print *, 'size of Ftot is ', size(other_MD%ConnectList(other_MD%FairIdList(J))%Ftot)
!
!    print *, 'FairForce is ', y_MD%PtFairleadLoad%Force(1,I)
!
!    MDWrOutput(I) = y_MD%PtFairleadLoad%Force(1,I)  !    MDWrOutput(I) = norm2(other_MD%ConnectList(other_MD%FairIdList(J))%Ftot)
!
!
!
!  END DO


  ! Write the output parameters to the file

   Frmt = '(F10.4, A1, F10.4, '//TRIM(Int2LStr(p_MD%NFairs))//'(A1,e10.4))'   ! should evenutally use user specified format?

!   print *, 'in WriteOutputs, Frmt is ', Frmt

 !  print *, ' and MDWrOutput is ', MDWrOutput(1:p_MD%NFairs)

   WRITE(UnOutFile,Frmt)  t_global, ' ', MD_Input(1)%PtFairleadDisplacement%TranslationDisp(1,1), ( ' ', y_MD%WriteOutput(I), I=1,p_MD%NumOuts )








      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Step 2: Correct (continue in loop)
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! IF ( j_pc /= p_FAST%NumCrctn)  THEN          ! Don't copy these on the last loop iteration...
      !    IF ( p_FAST%n_substeps( Module_MD ) > 1 ) THEN
      !       CALL MD_CopyOtherState( OtherSt_MD_old, OtherSt_MD, MESH_UPDATECOPY, Errstat, ErrMsg)
      !    ELSEIF ( p_FAST%n_substeps( Module_FEAM ) > 1 ) THEN
      !       CALL FEAM_CopyOtherState( OtherSt_FEAM_old, OtherSt_FEAM, MESH_UPDATECOPY, Errstat, ErrMsg)
      !    END IF
      ! END IF

      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Predictor-corrector ends here!
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Step 3: Save all final variables (advance to next time)
      !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! MD: copy final predictions to actual states
      CALL MD_CopyContState   ( x_MD_pred,  x_MD, MESH_UPDATECOPY, Errstat, ErrMsg)
      CALL MD_CopyDiscState   (xd_MD_pred, xd_MD, MESH_UPDATECOPY, Errstat, ErrMsg)
      CALL MD_CopyConstrState ( z_MD_pred,  z_MD, MESH_UPDATECOPY, Errstat, ErrMsg)
   END DO
   ! -------------------------------------------------------------------------
   ! END time marching
   ! -------------------------------------------------------------------------


  ! close output file
   CLOSE( UnOutFile, IOSTAT = ErrStat )
   !IF ( ErrStat /= 0 ) Err = .TRUE.


   !==========   NOTE   ======     <-----------------------------------------+
   ! @bonnie : I am assuming the glue code will do this
   IF (MD_interp_order .EQ. 1) THEN
      CALL MeshDestroy(MD_Input(2)%PtFairleadDisplacement, ErrStat,ErrMsg)
   ELSE IF (MD_interp_order .EQ. 2) THEN
      CALL MeshDestroy(MD_Input(2)%PtFairleadDisplacement, ErrStat,ErrMsg)
      CALL MeshDestroy(MD_Input(3)%PtFairleadDisplacement, ErrStat,ErrMsg)
   END IF
   !===========================================================================

   ! Destroy all objects
   CALL MD_End(MD_Input(1), &
               p_MD       , &
               x_MD       , &
               xd_MD      , &
               z_MD       , &
               other_MD   , &
               y_MD       , &
               ErrStat    , &
               ErrMsg )
   IF (ErrStat.NE.0) THEN
      WRITE(*,*) ErrMsg
   END IF

   CALL MD_DestroyInput(u_MD, ErrStat, ErrMsg)
   IF (ErrStat/=ErrID_None) CALL WrScr(TRIM(ErrMsg))
   IF (ALLOCATED(MD_Input)) THEN
      DO j = 2,MD_interp_order+1 !note that SD_Input(1) was destroyed in MD_End
         CALL MD_DestroyInput(MD_Input(j), ErrStat, ErrMsg)
         IF (ErrStat/=ErrID_None) CALL WrScr(TRIM(ErrMsg))
      END DO
      DEALLOCATE(MD_Input)
   END IF
   IF (ALLOCATED(MD_Input)) DEALLOCATE(MD_Input)
   IF (ALLOCATED(MD_InputTimes)) DEALLOCATE(MD_InputTimes)

   CALL MD_DestroyContState(x_MD_pred, ErrStat, ErrMsg); IF (ErrStat/= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MD_DestroyDiscState(xd_MD_pred, ErrStat, ErrMsg); IF (ErrStat/= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MD_DestroyConstrState(z_MD_pred, ErrStat, ErrMsg); IF (ErrStat/= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MD_DestroyOtherState(other_MD_old, ErrStat, ErrMsg); IF (ErrStat/= ErrID_None) CALL WrScr(TRIM(ErrMsg))

 CONTAINS

    SUBROUTINE CheckError(ErrID,Msg)
      ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)
      IF ( ErrID /= ErrID_None ) THEN
         CALL WrScr( NewLine//TRIM(Msg)//NewLine )
         ! IF ( ErrID >= AbortErrLev ) CALL ExitThisProgram( Error=.TRUE., ErrLev=ErrID )
      END IF
   END SUBROUTINE CheckError


END PROGRAM Main


