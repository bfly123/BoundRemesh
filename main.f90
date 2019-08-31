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



	  N = 60

	  allocate (xa(N+1))
	  allocate (ya(N+1))
	  allocate (xb(N+1))
	  allocate (yb(N+1))

	  allocate (xna(N+1))
	  allocate (yna(N+1))
	  allocate (xnb(N+1))
	  allocate (ynb(N+1))

	  allocate (ra(N-1))
	  allocate (rb(N-1))

	  dtheta = 180/N
	  do i = 1,N+1
		theta = dtheta*(i-1)
		xa(i) = -cosd(theta)*2
		ya(i) = sind(theta) *2 - 0.4*sind(5*theta)

		xb(i) = -cosd(theta)*1.2 -0.6
	    yb(i) = sind(theta)*1.2+0.2*sind(3*theta)
	  enddo

	call refine_mesh_to_equal_length(N+1,xa,ya) !above
	call refine_mesh_to_equal_length(N+1,xb,yb) !bottom


	  do i = 1,N-1
		ra(i) = i
		rb(i) = i
	enddo

	  call refine_mesh_length(N+1,xa,ya,xb,yb,xna,yna,xnb,ynb,ra,rb) !refine mesh	!length to the proportin of the height

	  alpha = 0.1 !! coefficient can change 

	  call refine_mesh_area_and_angle(N+1,xa,ya,xb,yb,xna,yna,xnb,ynb,ra,rb,alpha)

	

      open(1,file='output.plt',status='unknown')
      write(1,*)'TITLE    = "Dataset"'
      write(1,*)'VARIABLES = "x" "y" &
     ZONE T="Zone 1"'
      write(1,*)'I=',N+1,'J=',2,'K=',1,'ZONETYPE=Ordered'
      write(1,*)'DATAPACKING=POINT'
	  do i = 1,N+1
	  write(1,*)xna(i),yna(i)
	  enddo
	  do i = 1,N+1
	  write(1,*)xnb(i),ynb(i)
	  enddo
	  close(1)
	  

end

