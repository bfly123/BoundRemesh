subroutine area_average(N,xa,ya,xb,yb,Aave)
	implicit none
	integer N,i
	double precision xa(N),ya(N),xb(N),yb(N)
	double precision Asum,Atmp,Aave,A1,A2
	  double precision x(4),y(4)

	  Asum = 0
	  do i =1,N-1
		x(1) = xb(i)
		x(2) = xb(i+1)
		x(3) = xa(i+1)
		x(4) = xa(i)

		y(1) = yb(i)
		y(2) = yb(i+1)
		y(3) = ya(i+1)
		y(4) = ya(i)
		call area4(x,y,Atmp,A1,A2)
		Asum =Asum +Atmp
	enddo
  Aave = Asum/(N-1)
end

		subroutine area4(x,y,A,A1,A2)
			implicit none
		double precision x(4),y(4),A,A1,A2

		A = 0.5*((x(3) -x(1))*(y(4) - y(2)) +(x(2) - x(4))*(y(3) - y(1)))   
		A1 = (x(3) -x(1))*(y(4) - y(2))
		A2 = (x(2) - x(4))*(y(3) - y(1))

		end



