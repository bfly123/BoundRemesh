	   subroutine refine_mesh_area_and_angle(N,xa,ya,xb,yb,xna,yna,xnb,ynb,ra,rb,alpha)
implicit none
double precision xa(N),ya(N),xb(N),yb(N),xna(N)
double precision yna(N),xnb(N),ynb(N),ra(N-2),rb(N-2),r(N-2),h(N-1),ra1(N-2),ra2(N-2),rb1(N-2),rb2(N-2)
integer N,i,j,k,m
double precision Aave,sumh2,sumA,dr,Ai,Delta,r2,r1,r2min,r1min,r1new,r2new,A1,A2
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

	  call area_average(N,xna,yna,xnb,ynb,Aave)
	  call height(N,xna,yna,xnb,ynb,h)
	  sumh2 = 0.0
	  do i = 1,N-1
		sumh2 = sumh2 + h(i)**2
	  enddo

	  sumA = Aave*(N-1)

	  dr =50.0/300

	  ra1 = ra
	  rb1 = rb

 do i =1,N-2
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


	  do m = 1,300
		r1new = r1min + dr*m+0.3
		do k = 1,300
			r2new = r2min+dr*k +0.3
			if(r1new > 0.3 .and. r1new < N-1-0.3 .and. r2new > 0.3 .and. r2new < N-1-0.3)then
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

				call r_to_xy(N,xb,yb,r2new+0.2,xtmp2,ytmp2)
				call r_to_xy(N,xa,ya,r1new+0.2,xtmp3,ytmp3)

				call angle_of_radials(x_tmp(2),y_tmp(2),x_tmp(1),y_tmp(1),x_tmp(3),y_tmp(3),theta1)
				call angle_of_radials(x_tmp(3),y_tmp(3),x_tmp(2),y_tmp(2),x_tmp(4),y_tmp(4),theta2)

				call angle_of_radials(x_tmp(2),y_tmp(2),xtmp2,ytmp2,x_tmp(3),y_tmp(3),theta3)
				call angle_of_radials(x_tmp(3),y_tmp(3),x_tmp(2),y_tmp(2),xtmp3,ytmp3,theta4)
				call  area4(x_tmp,y_tmp,Ai_tmp,A1,A2)

				aim = alpha*abs(Ai_tmp-Ai)/Ai  !abs(theta1-90)/90+ abs(theta2-90)/90+alpha*abs(Ai_tmp-Ai)/Ai  + abs(theta3-90)/90 +abs(theta4-90)/90
				if( delta > aim .and.A1>0.and.A2>0 )then
					delta = aim
				!if( delta > alpha*abs(Ai_tmp-Ai)/Ai)then
				!	delta = alpha*abs(Ai_tmp-Ai)/Ai
					r2 = r2new
					r1 = r1new
				endif
			endif
		enddo
	enddo

	ra(i) = min(r1,N-1-(N-i)*0.3)
	rb(i) = min(r2,N-1-(N-i)*0.3)
enddo
!ra2 = ra
!rb2 = rb  
!ra = ra1 
!rb = rb1 
! do i =N-2,1,-1
!	  Ai = h(i+1)**2*sumA/sumh2
!	  Delta = 1.0e10
!	  r2 = 0
!	  r1 = 0
!	  r2min = 0
!	  r1min = 0
!	  if (i==N-2) then
!		  r1min =N-1
!		  r2min =N-1
!	  else
!		  r1min = ra(i+1)
!		  r2min = rb(i+1)
!	  endif
!
!
!
!
!	  do m = 1,200
!		r1new = r1min - dr*m-0.3
!		do k = 1,200
!			r2new = r2min-dr*k -0.3
!			if(r1new > 0.3 .and. r1new < N-1-0.3 .and. r2new > 0.3 .and. r2new < N-1-0.3)then
!				if (i==N-2)then
!					x_tmp(2) =xb(N)
!					y_tmp(2) =yb(N)
!					x_tmp(3) =xa(N)
!					y_tmp(3) =ya(N)
!				else
!					call r_to_xy(N,xb,yb,rb(i+1),x_tmp(2),y_tmp(2))
!					call r_to_xy(N,xa,ya,ra(i+1),x_tmp(3),y_tmp(3))
!				endif
!				call r_to_xy(N,xb,yb,r2new,x_tmp(1),y_tmp(1))
!				call r_to_xy(N,xa,ya,r1new,x_tmp(4),y_tmp(4))
!
!				call r_to_xy(N,xb,yb,r2new-0.2,xtmp2,ytmp2)
!				call r_to_xy(N,xa,ya,r1new-0.2,xtmp3,ytmp3)
!
!				call angle_of_radials(x_tmp(1),y_tmp(1),x_tmp(2),y_tmp(2),x_tmp(4),y_tmp(4),theta1)
!				call angle_of_radials(x_tmp(4),y_tmp(4),x_tmp(1),y_tmp(1),x_tmp(3),y_tmp(3),theta2)
!
!				call angle_of_radials(x_tmp(1),y_tmp(1),xtmp2,ytmp2,x_tmp(4),y_tmp(4),theta3)
!				call angle_of_radials(x_tmp(4),y_tmp(4),x_tmp(1),y_tmp(1),xtmp3,ytmp3,theta4)
!				call  area4(x_tmp,y_tmp,Ai_tmp)
!				!aim = abs(theta1 - theta2)/90+alpha*abs(Ai_tmp-Ai)/Ai + abs(theta3-theta4)/90
!
!				aim = abs(theta1-90)/90+ abs(theta2-90)/90+alpha*abs(Ai_tmp-Ai)/Ai  + abs(theta3-90)/90 +abs(theta4-90)/90
!				if( delta > aim)then
!					delta = aim
!				!if( delta > alpha*abs(Ai_tmp-Ai)/Ai)then
!				!	delta = alpha*abs(Ai_tmp-Ai)/Ai
!					r2 = r2new
!					r1 = r1new
!				endif
!			endif
!		enddo
!	enddo
!
!	ra(i) = max(r1,i*0.3)
!	rb(i) = max(r2,i*0.3)
!enddo
!
!ra = (ra +ra2)/2
!rb = (rb +rb2)/2
!


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
!	  write(*,*) ra
end





