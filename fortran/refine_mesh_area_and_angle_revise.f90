 subroutine refine_mesh_area_and_angle_revise(N,xa,ya,xb,yb,xna,yna,xnb,ynb,ra,rb,alpha)
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

	  call area_average(N,xna,yna,xnb,ynb,Aave)
	  call height(N,xna,yna,xnb,ynb,h)
	  sumh2 = 0.0
	  do i = 1,N-1
		sumh2 = sumh2 + h(i)**2
	  enddo

	  sumA = Aave*(N-1)


At = sumA/sumh2

do ij = 1,1
!rat = ra
!rbt = rb
!call f_A(At+At/100,f2,N,h,rat,rbt,xa,ya,xb,yb)
!write(*,*)"f2***",f2
rat = ra
rbt = rb
call f_A(At,f1,N,h,rat,rbt,xa,ya,xb,yb)
write(*,*)"***f1***", f1
At = (At*sumh2-f1)/sumh2
write(*,*) At,sumA

enddo
ra = rat
rb = rbt

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





