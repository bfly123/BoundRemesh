subroutine  init(N,xa,ya,xb,yb)
	  implicit none
	  integer N
	  integer i,j 
	  double precision dtheta,theta
	  double precision  xa(N+1)
	  double precision ya(N+1)
	  double precision xb(N+1)
	  double precision yb(N+1)

	  dtheta = 180/N
	  do i = 1,N+1
		theta = dtheta*(i-1)
		xa(i) = -dcos(theta)*2
		ya(i) = dsin(theta) *2 + 0.4*dsin(7*theta)

		xb(i) = -dcos(theta)*1.2 -0.5
	    yb(i) = dsin(theta)*1.2+0.2*dsin(6*theta)
	  enddo
 end



