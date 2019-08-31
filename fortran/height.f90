subroutine height(N,xa,ya,xb,yb,h)
	implicit none
	  integer N,i,j
	  double precision xa(N),ya(N),xb(N),yb(N),h(N-1)
	  double precision h1,h2,x1,y1,x2,y2
	  do i =1,N-1
		x1 = (xa(i+1)+xa(i))/2
		y1 = (ya(i+1)+ya(i))/2
		x2 = (xb(i+1)+xb(i))/2
		y2 = (yb(i+1)+yb(i))/2
		call distance(x1,y1,xb(i+1),yb(i+1),xb(i),yb(i),h1)
		call distance(x2,y2,xa(i+1),ya(i+1),xa(i),ya(i),h2)
		
	!	h(i) = (h1+h2)/2
	h(i) = sqrt((x1-x2)**2 +(y1-y2)**2)
	 enddo 

end

subroutine distance(x1,y1,x2,y2,x3,y3,h)
	implicit none 
	  double precision x1,y1,x2,y2,x3,y3,h
	  h = abs((x1-x2)*(y2-y3) -(x2-x3)*(y1-y2))/sqrt((x2-x3)**2+(y2-y3)**2)
end

	   

