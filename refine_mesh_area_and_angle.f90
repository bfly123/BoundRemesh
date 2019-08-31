	   subroutine refine_mesh_area_and_angle(N,xa,ya,xb,yb,xna,yna,xnb,ynb,ra,rb,alpha)
implicit none
double precision xa(N),ya(N),xb(N),yb(N),xna(N)
double precision yna(N),xnb(N),ynb(N),ra(N-2),rb(N-2),r(N-2),h(N-1)
integer N,i,j,k,m
double precision Aave,sumh2,sumA,dr,Ai,Delta,r2,r1,r2min,r1min,r1new,r2new
	  double precision x_tmp(4),y_tmp(4),theta1,theta2,Ai_tmp,alpha


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

	  call area_average(N,xna,yna,xnb,ynb,Aave)
	  call height(N,xna,yna,xnb,ynb,h)
	  sumh2 = 0
	  do i = 1,N-1
		sumh2 = sumh2 + h(i)**2
	  enddo
	  sumA = Aave*(N-1)

	  dr =3.0/40

 do i =1,N-1
	  Ai = h(i)**2*sumA/sumh2
	  Delta = 1.0e10
	  r2 = 0
	  r1 = 0
	  r2min = 0
	  r1min = 0
	  if (i==1) then
		  r1min =0
		  r2min =0
	  else
		  r1min = ra(i-1)
		  r2min = rb(i-1)
	  endif

	  do m = 1,40
		r1new = ra(i) + dr*m -1.5
		do k = 1,40
			r2new = rb(i)+dr*k-1.5
			if(r1new > 0 .and. r1new < N-1 .and. r2new > 0 .and. r2new<N-1 .and. r1new > r1min+0.2 .and.  r2new > r2min+0.2)then
				if (i==1)then
					x_tmp(1) =xb(1)
					y_tmp(1) =yb(1)
					x_tmp(4) =xa(1)
					y_tmp(4) =ya(1)
				else
					call r_to_xy(N,xb,yb,rb(i-1),x_tmp(1),y_tmp(1))
					call r_to_xy(N,xa,ya,ra(i-1),x_tmp(4),y_tmp(4))
				endif
				call r_to_xy(N,xb,yb,r2new,x_tmp(2),y_tmp(2))
				call r_to_xy(N,xa,ya,r1new,x_tmp(3),y_tmp(3))

				call angle_of_radials(x_tmp(2),y_tmp(2),x_tmp(1),y_tmp(1),x_tmp(3),y_tmp(3),theta1)
				call angle_of_radials(x_tmp(3),y_tmp(3),x_tmp(2),y_tmp(2),x_tmp(4),y_tmp(4),theta2)
				call  area4(x_tmp,y_tmp,Ai_tmp)
				if( delta > abs(theta1 - theta2)/90+alpha*abs(Ai_tmp-Ai)/Ai)then
					delta = abs(theta1 - theta2)/90+alpha*abs(Ai_tmp-Ai)/Ai
					r2 = r2new
					r1 = r1new
				endif
			endif
		enddo
	enddo
	ra(i) = r1
	rb(i) = r2
enddo


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
end





