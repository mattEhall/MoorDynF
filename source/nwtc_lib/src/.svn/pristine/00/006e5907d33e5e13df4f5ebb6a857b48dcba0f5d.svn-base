PROGRAM Test_TestMeshMapping

   USE NWTC_Library
   IMPLICIT NONE   

   TYPE(meshtype) :: mesh1_I, mesh1_O
   TYPE(meshtype) :: mesh2_I, mesh2_O
   
   TYPE(MeshMapType) :: Map_Mod1_P_Mod2_P        ! Data for mapping point-to-point meshes from mod1 to mod2
   TYPE(MeshMapType) :: Map_Mod2_P_Mod1_P        ! Data for mapping point-to-point meshes from mod1 to mod2

   REAL(ReKi)     :: Orientation(3,3)
   REAL(ReKi)     :: Angle
   
   INTEGER :: NNodes, i, j
   
   INTEGER :: un_mesh1_I,   un_mesh1_O, &
              un_mesh2_I,   un_mesh2_O          ! UNITS for File I/O
   
   INTEGER(IntKi) :: ErrStat
   CHARACTER(1024) :: ErrMsg
   
   
   CALL NWTC_Init()
   
   ! ..............................................................................................................................   
   ! Create Mesh1: a point mesh with one point
   !   Mesh1_I (input) has loads
   !   Mesh1_O (output) as motions
   ! ..............................................................................................................................   
   NNodes = 1
   
   CALL MeshCreate( BlankMesh       = mesh1_I       &
                     ,IOS           = COMPONENT_INPUT        &
                     ,NNodes        = NNodes                 &
                     ,Force         = .TRUE.                 &
                     ,Moment        = .TRUE.                 &
                     ,ErrStat       = ErrStat                &
                     ,ErrMess       = ErrMsg                 )   
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))

   do j=1,NNodes
         ! place nodes in a line
      CALL MeshPositionNode ( mesh1_I, j, (/0.0_ReKi, 0.0_ReKi, (j-1)*1.0_ReKi /), ErrStat, ErrMsg )     
      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
         ! create an element from this point   
      
      CALL MeshConstructElement ( mesh1_I, ELEMENT_POINT, ErrStat, ErrMsg, j )
      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            

   END DO
   
      ! that's our entire mesh:
   CALL MeshCommit ( mesh1_I, ErrStat, ErrMsg )   
   IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))   
   
   
   !..............................
   
   CALL MeshCopy (     SrcMesh  = mesh1_I &
                     , DestMesh = mesh1_O &
                     , CtrlCode = MESH_SIBLING    &
                     , Orientation      = .TRUE.  &
                     , TranslationDisp  = .TRUE.  &
                     , TranslationVel   = .TRUE.  &
                     , RotationVel      = .TRUE.  &
                     , TranslationAcc   = .TRUE.  &
                     , RotationAcc      = .TRUE.  &
                     , ErrStat  = ErrStat         &
                     , ErrMess  = ErrMsg           )
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))

   Mesh1_O%IOS = COMPONENT_OUTPUT
   
   ! ..............................................................................................................................   
   ! Create Mesh2: a point mesh with three points
   !   Mesh2_I (input) has motions
   !   Mesh2_O (output) as loads
   ! ..............................................................................................................................   
   NNodes = 3
   
   CALL MeshCreate( BlankMesh       = mesh2_I       &
                     ,IOS           = COMPONENT_INPUT        &
                     ,NNodes        = NNodes                 &
                     , Orientation      = .TRUE.  &
                     , TranslationDisp  = .TRUE.  &
                     , TranslationVel   = .TRUE.  &
                     , RotationVel      = .TRUE.  &
                     , TranslationAcc   = .TRUE.  &
                     , RotationAcc      = .TRUE.  &
                     ,ErrStat       = ErrStat                &
                     ,ErrMess       = ErrMsg                 )   
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))

   do j=1,NNodes
                  
      Angle = (15. - 15*j)*D2R  !note this "looks" like the transpose, but isn't
      Orientation(:,1) = (/ COS(Angle), -1.*SIN(Angle), 0.0 /)
      Orientation(:,2) = (/ SIN(Angle),     COS(Angle), 0.0 /)
      Orientation(:,3) = (/      0.,        0.0,        1.0 /)
      
         ! place nodes in a line
      CALL MeshPositionNode ( mesh2_I, j, (/0.0_ReKi, (j-1)*0.75_ReKi, 0.0_ReKi /), ErrStat, ErrMsg, &
           Orient= Orientation )     
      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
         ! create an element from this point   
      
      CALL MeshConstructElement ( mesh2_I, ELEMENT_POINT, ErrStat, ErrMsg, j )
      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            

   END DO
   
      ! that's our entire mesh:
   CALL MeshCommit ( mesh2_I, ErrStat, ErrMsg )   
   IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))   
   
   !..............................
   
   CALL MeshCopy (    SrcMesh  = mesh2_I &
                    , DestMesh = mesh2_O &
                    , CtrlCode = MESH_SIBLING    &
                    , Force    = .TRUE.          &
                    , Moment   = .TRUE.          &
                    , ErrStat  = ErrStat         &
                    , ErrMess  = ErrMsg           )
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))

   Mesh2_O%IOS = COMPONENT_OUTPUT
    
   ! ..............................................................................................................................   
   ! Initialize the data: 
   ! ..............................................................................................................................   

   CALL AllocMapping( Mesh1_O, Mesh2_I, Map_Mod1_P_Mod2_P, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))   
   CALL AllocMapping( Mesh2_O, Mesh1_I, Map_Mod2_P_Mod1_P, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
   un_mesh1_I = -1
   un_mesh1_O = -1
   un_mesh2_I = -1
   un_mesh2_O = -1  
      

   do j=1,Mesh1_O%NNodes
      
!      Angle = 0      
      Angle = (20*j)*D2R      
      !note this "looks" like the transpose, but isn't
      Mesh1_O%Orientation(:,1,j) = (/ COS(Angle), -1.*SIN(Angle), 0.0 /)
      Mesh1_O%Orientation(:,2,j) = (/ SIN(Angle),     COS(Angle), 0.0 /)
      Mesh1_O%Orientation(:,3,j) = (/         0.,     0.0,        1.0 /)
            
      Mesh1_O%TranslationDisp(:,j) = (/ 1., 1.,  0. /)
      Mesh1_O%TranslationVel(:,j)  = (/ 1., 1.,  0. /)*.5
      Mesh1_O%RotationVel(:,j)     = (/ 0., 0.5, 0.5 /)*.5
      Mesh1_O%TranslationAcc(:,j)  = (/ 1., 1., 0. /)*.115
      Mesh1_O%RotationAcc(:,j)     = (/ 1., 1., 1. /)*.115
      
   end do
   
   do j=1,Mesh2_O%NNodes
      Mesh2_O%Force( :,j) = (/  1.0, 0.,  0.   /)*(j*0.5)
      Mesh2_O%Moment(:,j) = (/  0.0, 0.5, 0.5  /)*(-j*0.0)
   end do
   
   
      
   ! ..............................................................................................................................   
   ! Map the outputs to inputs and print results:
   ! ..............................................................................................................................   
   
   CALL Transfer_Point_to_Point( Mesh1_O, Mesh2_I, Map_Mod1_P_Mod2_P, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))   
   CALL Transfer_Point_to_Point( Mesh2_O, Mesh1_I, Map_Mod2_P_Mod1_P, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
   CALL MeshWrBin ( un_mesh1_O, Mesh1_O, ErrStat, ErrMsg, "Mesh1_Output.bin");  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshWrBin ( un_mesh1_I, Mesh2_I, ErrStat, ErrMsg, "Mesh2_Input.bin");   IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshWrBin ( un_mesh2_O, Mesh2_O, ErrStat, ErrMsg, "Mesh2_Output.bin");  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshWrBin ( un_mesh2_I, Mesh1_I, ErrStat, ErrMsg, "Mesh1_Input.bin");   IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
  
   
   ! ..............................................................................................................................   
   ! Destroy them:
   ! ..............................................................................................................................   

   CALL MeshDestroy( mesh1_I, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshDestroy( mesh1_O, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshDestroy( mesh2_I, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshDestroy( mesh2_O, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
   ! ..............................................................................................................................   
   ! Close files
   ! ..............................................................................................................................   
   close( un_mesh1_I )
   close( un_mesh1_O )
   close( un_mesh2_I )
   close( un_mesh2_O )
   
   
END PROGRAM

