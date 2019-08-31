subroutine r_to_xy(N,x,y,r,xn,yn)
implicit none 
	  integer N,i,j
	  double precision x(N),y(N)
	  double precision xn,yn,r

	  i = 1+floor(r)

	  xn = (x(i+1) -x(i))*(r-floor(r)) + x(i)

	  yn = (y(i+1) -y(i))*(r-floor(r)) + y(i)

end


