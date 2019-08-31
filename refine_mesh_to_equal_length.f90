subroutine  refine_mesh_to_equal_length(N,x,y) 
	  implicit none
	  integer N
	  integer i,j 
	  double precision dtheta,theta
	  double precision  x(N)
	  double precision y(N)
	  double precision  xn(N)
	  double precision yn(N)
	  double precision r(N-2)
	  double precision L,dr,dl,rtmp,xtmp,ytmp,ltmp

	  L = 0
	  do i= 1,N-1
		L =L + sqrt((x(i+1)-x(i))**2 +(y(i+1)-y(i))**2)
	  enddo
		L = L/(N-1)
!
		xn(1) = x(1)
		xn(N) = x(N)
		yn(1) = y(1)
		yn(N) = y(N)
!
		do i =1,N-2
			dr = 4.0/100
			dl = 1.0e10
			rtmp = 0.0
			if (i==1) then
				r(i) = dr
			else
				r(i) = r(i-1)
			endif

		do j = 1,100
			if (r(i) < N-1)then 
				call r_to_xy(N,x,y,r(i),xtmp,ytmp)
				ltmp = sqrt((xtmp-xn(i))**2 +(ytmp -yn(i))**2)
				if (dl > abs(ltmp - L)) then
					dl = abs(ltmp - L)
					xn(i+1) = xtmp
					yn(i+1) = ytmp
					rtmp = r(i)
				endif
			endif
			r(i) = r(i)+dr
		enddo
		r(i) =rtmp
	enddo
	x = xn
	y = yn
end








