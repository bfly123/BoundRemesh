subroutine  refine_mesh_length(N,xa,ya,xb,yb,xna,yna,xnb,ynb,ra,rb) !refine mesh	!length to the proportin of the height
implicit none
double precision xa(N),ya(N),xb(N),yb(N),xna(N)
double precision yna(N),xnb(N),ynb(N),ra(N-2),rb(N-2),r(N-2),h(N-1)
integer N,i,j
double precision Aave,sumh

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

	  !call area_average(N,xna,yna,xnb,ynb,ave)
	  call height(N,xna,yna,xnb,ynb,h)
	  sumh = 0
	  do i = 1,N-1
		sumh = sumh + h(i)
	  enddo

	  do i= 1,N-2
		if (i==1)then
			r(i) = h(1)/sumh*(N-1)
		else 
			r(i) = h(i)/sumh*(N-1)+r(i-1)
		endif
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
		call r_to_xy(N,xa,ya,r(i-1),xna(i),yna(i))
		call r_to_xy(N,xb,yb,r(i-1),xnb(i),ynb(i))
	  enddo

	  ra = r
	  rb = r
end




	 



