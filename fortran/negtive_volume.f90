subroutine negative_volume(N,xa,ya,xb,yb,xna,yna,xnb,ynb,ra,rb,alpha)
implicit none
double precision xa(N),ya(N),xb(N),yb(N),xna(N)
double precision yna(N),xnb(N),ynb(N),ra(N-2),rb(N-2),r(N-2),h(N-1),rat(N-2),rbt(N-2)
integer N,i,j,k,m,ij
double precision Aave,sumh2,sumA,dr,Ai,Delta,r2,r1,r2min,r1min,r1new,r2new,A1,A2,f1,f2,at
	  double precision x_tmp(4),y_tmp(4),theta1,theta2,Ai_tmp,alpha,xtmp2,ytmp2,xtmp3,ytmp3,aim,theta3,theta4


		xna(1)=xa(1)
		yna(1)=ya(1)
		xnb(1)=xb(1)
		ynb(1)=yb(1)
	
		xna(N)=xa(N)
		yna(N)=ya(N)
		xnb(N)=xb(N)
		ynb(N)=yb(N)

	  do i = 2,N-1
		call r_to_xy(N,xa,ya,ra(i-1),xna(i),yna(i))
		call r_to_xy(N,xb,yb,rb(i-1),xnb(i),ynb(i))
	  enddo

	  do i = 1,N-1
		x_tmp(1) = xb(i)
		y_tmp(1) = yb(i)
		x_tmp(4) = xa(i)
		y_tmp(4) = ya(i)

		x_tmp(2) = xb(i+1)
		y_tmp(2) = yb(i+1)
		x_tmp(3) = xa(i+1)
		y_tmp(3) = ya(i+1)
		call area4(x_tmp,y_tmp,Ai_tmp,A1,A2)
		if (Ai_tmp <0) then
	  	endif
	 enddo
end
