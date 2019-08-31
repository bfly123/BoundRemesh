subroutine angle_of_radials(x_corner,y_corner,x1,y1,x2,y2,dtheta)
	  implicit none 
	  double precision x_corner,y_corner,x1,y1,x2,y2,theta1,theta2,dtheta


    theta1 = atand(abs(y1 - y_corner)/abs(x1 - x_corner))
    theta2 = atand(abs(y2 - y_corner)/abs(x2 - x_corner))
    if (x1 - x_corner .le. 0 .and. y1 -y_corner > 0)then 
        theta1 = 180 -theta1
    elseif (x1 - x_corner .le. 0 .and. y1 -y_corner .le.0)then 
        theta1 = 180 +theta1
    elseif (x1-x_corner .ge. 0 .and.  y1 - y_corner < 0)then 
        theta1 = 360 -theta1
	endif
     if (x2 - x_corner .le. 0 .and. y2 -y_corner > 0)then 
        theta2 = 180 -theta2
    elseif (x2 - x_corner .le. 0 .and. y2 -y_corner .le.0)then 
        theta2 = 180 +theta2
    elseif (x2-x_corner .ge. 0 .and.  y2 - y_corner < 0)then 
        theta2 = 360 -theta2
	endif
     dtheta = abs(theta1 - theta2)
    
    if (dtheta > 180)then
        dtheta = 360 - dtheta 
	endif
end
