program mesh_refine
	  call init(N,xa,ya,xb,yb)
	!  call refine_mesh_to_equal_length(N,xa,ya) !above
	!  call refine_mesh_to_equal_length(N,xb,yb) !bottom

	!  call refine_mesh_length(N,xa,ya,xb,yb,xna,yna,xnb,ynb,ra,rb) !refine mesh	!length to the proportin of the height
	!  call refine_mesh_area_and_angle(N,xa,ya,xb,yb,xna,yna,xnb,ynb,ra,rb)
	!  call output(N,xa,ya,xb,yb,xna,yna,xnb,ynb)
end

