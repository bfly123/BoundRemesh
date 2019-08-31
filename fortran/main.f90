program mesh_refine
	implicit none 
	  integer N
	  integer i,j 
	  double precision dtheta,theta,alpha
	  double precision,allocatable:: xa(:)
	  double precision,allocatable:: ya(:)
	  double precision,allocatable:: xb(:)
	  double precision,allocatable:: yb(:)

	
	  double precision,allocatable:: xna(:)
	  double precision,allocatable:: yna(:)
	  double precision,allocatable:: xnb(:)
	  double precision,allocatable:: ynb(:)

	  double precision,allocatable:: ra(:)
	  double precision,allocatable:: rb(:)



	  N = 1200

	  allocate (xa(N))
	  allocate (ya(N))
	  allocate (xb(N))
	  allocate (yb(N))



	  allocate (xna(N))
	  allocate (yna(N))
	  allocate (xnb(N))
	  allocate (ynb(N))

	  allocate (ra(N-2))
	  allocate (rb(N-2))

	  dtheta = 180.0/(N-1)
	  do i = 1,N
		theta = dtheta*(i-1)
		!write(*,*)theta
		xa(i) = -cosd(theta)*2
		ya(i) = sind(theta) *2 + 0.4*sind(7*theta)

		xb(i) = -cosd(theta)*1.5 +0.3
	    yb(i) = sind(theta)*1.5+0.2*sind(7*theta)
	  enddo

	call refine_mesh_to_equal_length(N,xa,ya) !above
	call refine_mesh_to_equal_length(N,xb,yb) !bottom
!
!
	  do i = 1,N-2
		ra(i) = i
		rb(i) = i
	enddo
	do i =1,10
	  call refine_mesh_length(N,xa,ya,xb,yb,xna,yna,xnb,ynb,ra,rb) !refine mesh	!length to the proportin of the height

	  enddo
!
!	  alpha =  2.50!! coefficient can change 
!	  do i = 1,1
!!	  call refine_mesh_area_and_angle_oneside(N,xa,ya,xb,yb,xna,yna,xnb,ynb,ra,rb,alpha)
!!	  call refine_mesh_area_and_angle_revise(N,xa,ya,xb,yb,xna,yna,xnb,ynb,ra,rb,alpha)
!	  call refine_mesh_area_and_angle(N,xa,ya,xb,yb,xna,yna,xnb,ynb,ra,rb,alpha)
	  call negative_volume(N,xa,ya,xb,yb,xna,yna,xnb,ynb,ra,rb,alpha)
!	  enddo
!!
	
      open(1,file='output.plt',status='unknown')
      write(1,*)'TITLE    = "Dataset"'
      write(1,*)'VARIABLES = "x" "y" &
     ZONE T="Zone 1"'
      write(1,*)'I=',N,'J=',2,'K=',1,'ZONETYPE=Ordered'
      write(1,*)'DATAPACKING=POINT'
	  do i = 1,N
	  write(1,*)xna(i),yna(i)
	  enddo
	  do i = 1,N
	  write(1,*)xnb(i),ynb(i)
	  enddo
	  close(1)
     

end

