subroutine f_A(At,f,N,h,ra,rb,xa,ya,xb,yb) 
	  implicit none
	  integer N,i,j,k,m
	  double precision xa(N),ya(N),xb(N),yb(N)
	  double precision Aave,sumh2,sumA,dr,Ai,Delta,r2,r1,r2min,r1min,r1new,r2new,A1,A2,At,f,At2
	  double precision x_tmp(4),y_tmp(4),theta1,theta2,Ai_tmp,alpha,xtmp2,ytmp2,xtmp3,ytmp3,aim,theta3,theta4
	  double precision  ra(N-2),rb(N-2),r(N-2),h(N-1)

  dr =3.0/200
 f = 0.0
do i =N-2,1,-1
		  Ai = h(i+1)**2*At
		  !write(*,*) Ai
		  r2 = 0
		  r1 = 0
		  r2min = 0
		  r1min = 0
		  if (i==N-2) then
			  r1min =N-1
			  r2min =N-1
		  else
			  r1min = ra(i+1)
			  r2min = rb(i+1)
		  endif
		  	
		  do m = 1,200
			r1new = r1min - dr*m-0.3
			do k = 1,200
				r2new = r2min-dr*k -0.3
				if(r1new > 0.3 .and. r1new < N-1-0.3 .and. r2new > 0.3 .and. r2new < N-1-0.3)then
					if (i==N-2)then
						x_tmp(2) =xb(N)
						y_tmp(2) =yb(N)
						x_tmp(3) =xa(N)
						y_tmp(3) =ya(N)
					else
						call r_to_xy(N,xb,yb,rb(i+1),x_tmp(2),y_tmp(2))
						call r_to_xy(N,xa,ya,ra(i+1),x_tmp(3),y_tmp(3))
					endif
					call r_to_xy(N,xb,yb,r2new,x_tmp(1),y_tmp(1))
					call r_to_xy(N,xa,ya,r1new,x_tmp(4),y_tmp(4))
	
					call r_to_xy(N,xb,yb,r2new-0.2,xtmp2,ytmp2)
					call r_to_xy(N,xa,ya,r1new-0.2,xtmp3,ytmp3)
	
					call angle_of_radials(x_tmp(1),y_tmp(1),x_tmp(2),y_tmp(2),x_tmp(4),y_tmp(4),theta1)
					call angle_of_radials(x_tmp(4),y_tmp(4),x_tmp(1),y_tmp(1),x_tmp(3),y_tmp(3),theta2)
	
				call angle_of_radials(x_tmp(1),y_tmp(1),xtmp2,ytmp2,x_tmp(4),y_tmp(4),theta3)
					call angle_of_radials(x_tmp(4),y_tmp(4),x_tmp(1),y_tmp(1),xtmp3,ytmp3,theta4)
					call  area4(x_tmp,y_tmp,Ai_tmp,A1,A2)
					aim =    abs(theta1-90)/90+ abs(theta2-90)/90+alpha*abs(Ai_tmp-Ai)/Ai  + abs(theta3-90)/90+abs(theta4-90)/90 ! abs(Ai_tmp-Ai)/Ai 
					if (m==1.and.k==1)then
						delta = aim
					endif

					if( delta > aim)then
						delta = aim
					!if( delta > alpha*abs(Ai_tmp-Ai)/Ai)then
					!	delta = alpha*abs(Ai_tmp-Ai)/Ai
						r2 = r2new
						r1 = r1new
					endif
				endif
			enddo
		enddo
			
		ra(i) = max(r1,(i)*0.3)
		rb(i) = max(r2,(i)*0.3)
			!write(*,*)i, ra(i),rb(i)
enddo
f =0
do i = 1,N-1
	if(i == 1) then
		x_tmp(1) = xb(1)
		y_tmp(1) = yb(1)
		x_tmp(4) = xa(1)
		y_tmp(4) = ya(1)
	else
	
	call r_to_xy(N,xb,yb,rb(i-1),x_tmp(1),y_tmp(1))
	call r_to_xy(N,xa,ya,ra(i-1),x_tmp(4),y_tmp(4))
	endif 
	if(i == N-1) then
		x_tmp(2) = xb(N)
		y_tmp(2) = yb(N)
		x_tmp(3) = xa(N)
		y_tmp(3) = ya(N)
	else
	call r_to_xy(N,xb,yb,rb(i),x_tmp(2),y_tmp(2))
	call r_to_xy(N,xa,ya,ra(i),x_tmp(3),y_tmp(3))
	endif 

	call area4(x_tmp,y_tmp,Ai_tmp,A1,A2)

	if (abs( Ai_tmp-h(i)**2*At) .ge.abs(f))then
		f = Ai_tmp-h(i)**2*At
	endif
enddo	
! call random_number(f)
!write(*,*) "f******",f
end







