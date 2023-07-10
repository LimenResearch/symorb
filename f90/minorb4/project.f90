module mod_project
use initdata
implicit none

contains

subroutine center(conf) 
  ! moves the configurations so that the centre of mass is at the origin
  real(precis),dimension(NOB,dim), intent(INOUT) :: conf
  real(precis),dimension(dim) :: centerofmass
  integer :: i
  centerofmass=0.0d0
  do i=1,NOB
     centerofmass=centerofmass+m(i)*conf(i,:)
  end do
  centerofmass=centerofmass/sum(m)
  do i=1,NOB
     conf(i,:) = conf(i,:) - centerofmass(:)
  end do
end subroutine center

subroutine moltiplica(matrix,vector,mulmatvec)
  ! implements the multiplication of the matrix times the vector
  real(precis),dimension(dim,dim), intent(IN) :: matrix
  real(precis), dimension(dim) , intent (IN):: vector
  real(precis), dimension(dim) , intent (OUT) ::  mulmatvec
  integer i,j
  do i=1,dim
    mulmatvec(i)=0.0d0
    do j=1,dim
      mulmatvec(i)=mulmatvec(i) + matrix(i,j) * vector(j)
    end do
  end do
end subroutine moltiplica

subroutine s_sourceproj(matrix,permutation,conf,sourceproj)
  ! takes a configuration x and sends it to \rho(g)x_{\sigma(g)} (group action on space and index)
  implicit none 
  real(precis),dimension(NOB,dim),intent(IN) :: conf
  real(precis),dimension(NOB,dim),intent(OUT) :: sourceproj! needed for the force
  real(precis),dimension(dim,dim),intent(IN) :: matrix
  integer, dimension (NOB),intent(IN) :: permutation
  integer i
  sourceproj=0.0d0
  do i=1,NOB
     call moltiplica (matrix, conf(permutation(i),:)  , sourceproj(i,:) )
  end do
end subroutine s_sourceproj

subroutine project_onto_kerT(conf)
  ! projects a configuration x into X^{ker(tau)}
  real(precis),dimension(NOB,dim), intent(INOUT) :: conf
  real(precis),dimension(NOB,dim) :: dtmpconf, dconf
  integer :: i
  dtmpconf=conf
  conf=0.0d0
  do i=1,sizekerT
    call s_sourceproj(matrix(i,:,:),perm(i,:),dtmpconf, dconf)
    conf=conf + 1.0d0/real(sizekerT,precis) * dconf
  end do
end subroutine project_onto_kerT

subroutine s_htildeone(conf,htildeone)
  ! projects the initial configuration into X^{H_0}
 real(precis),dimension(NOB,dim),intent(IN) :: conf
 real(precis),dimension(NOB,dim),intent(OUT) :: htildeone ! needed for the force
   if (action_type == 0) then
     write (unit=0,fmt=*) "FATAL ERROR: s_htildeone not defined for action_type= ", action_type
     stop
   end if
     call s_sourceproj(first_matrix,first_perm,conf,htildeone)
 end subroutine s_htildeone
 
 
 subroutine s_htildetwo(conf,htildetwo)
   ! projects the final configuration into X^{H_1}
 real(precis),dimension(NOB,dim),intent(IN) :: conf
 real(precis),dimension(NOB,dim),intent(OUT) :: htildetwo
   if (action_type == 0) then
     write (unit=0,fmt=*) "FATAL ERROR: s_htildetwo not defined for action_type= ", action_type
     stop
   end if
     call s_sourceproj(second_matrix,second_perm,conf,htildetwo)
 end subroutine s_htildetwo

subroutine project(mygamma)
  ! 
  real(precis), dimension(NOB,dim,0:steps+1), intent(INOUT) :: mygamma
  ! mygamma is a 3d tensor, NOB rows, dim columns, steps+2 depth. 
  ! 0-step is x(0), (steps+1)-step is x(1). x(t), t=1,..,steps are 
  ! the intermediate configurations
  real(precis), dimension(NOB,dim) ::  conf1,conf2, symmconf1,symmconf2
  integer :: k

    do k=0,steps+1
    ! all the configurations are shifted to have centre of mass at the origin
    ! and projected into X^{cyclic_ext(ker(tau))}
      call center(mygamma(:,:,k))
      call project_onto_kerT(mygamma(:,:,k))
    end do

  if ((action_type .EQ. 2) .OR. (action_type .EQ. 1)) then 
    ! projects initial and final configurations into the spaces X^{H_i}
    conf1(:,:) = mygamma(:,:,0)
    conf2(:,:) = mygamma(:,:,steps+1)     
    call s_htildeone(mygamma(:,:,0),symmconf1(:,:))
    call s_htildetwo(mygamma(:,:,steps+1),symmconf2(:,:))
    mygamma(:,:,0) = 0.5d0 *( conf1(:,:) + symmconf1(:,:) )
    mygamma(:,:,steps+1) = 0.5d0 *( conf2(:,:) + symmconf2(:,:) )


  else if ( action_type .EQ. 0 ) then 
    ! projects initial and final configurations to have the cyclic action constraint
    conf1(:,:) = mygamma(:,:,0)   
    conf2(:,:) = mygamma(:,:,steps+1)     
    call inverse_rotation(conf2,symmconf2)
    call s_rotation(conf1,symmconf1) 
    mygamma(:,:,0) = 0.5d0 * (conf1(:,:) + symmconf2(:,:))
    mygamma(:,:,steps+1) = 0.5d0 * (conf2(:,:) + symmconf1(:,:))

  else
    write (unit=0,fmt=*) "FATAL ERROR: action type ", action_type, " not understood!\n"
    stop
  end if

return 
end subroutine project

subroutine s_cyclic_rotation(conf,power,rotated_conf)
  real(precis),dimension(NOB,dim) , intent(IN):: conf
  integer, intent (IN) :: power
  real(precis), dimension(NOB,dim) , intent(OUT) :: rotated_conf

  if (cyclic_order>1) then
    call s_sourceproj( cyclic_matrix(power,:,:) , cyclic_perm(power,:) , conf , rotated_conf )
  else 
    rotated_conf = conf
  end if    
  
end subroutine s_cyclic_rotation


subroutine s_rotation(conf,rotation)
  ! rotation a configuration x, i.e., sends x to gx, where g is the cyclic generator
  real(precis),dimension(NOB,dim) , intent(IN):: conf
  real(precis),dimension(NOB,dim) ,intent(OUT):: rotation
  call s_cyclic_rotation( conf , 1 , rotation)
end subroutine s_rotation

subroutine inverse_rotation(conf,rotation)
  ! inverse rotation a configuration x, i.e., sends x to g^{-1}x, where g is the cyclic generator
  real(precis),dimension(NOB,dim) , intent(IN):: conf
  real(precis),dimension(NOB,dim) ,intent(OUT):: rotation
  real(precis),dimension(NOB,dim) :: tmpconf
  if (cyclic_order>1) then
    call s_cyclic_rotation( conf , (cyclic_order-1) , tmpconf )
  else 
    rotation = conf
  end if    
  rotation=tmpconf
end subroutine inverse_rotation                                                 
end module mod_project
