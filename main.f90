program MD
use Box_mod
use Force_mod
implicit none

real, parameter :: box_length = 10.0

type(Box_obj) :: box
integer :: t

box = Box_obj(box_length, 0.001)
call box%init_atoms(10)

call init_force(lennard_jones, box_length)

do t = 1, 10000
    call box%update()
    if(mod(t,100)==0) then
        print*, "t = ", t
        call box%write_xyz("output.xyz")
    end if
end do

contains
!====================================================
function lennard_jones(distance)
    real, intent(in) :: distance
    real :: lennard_jones
    real, parameter :: eps = 1, sigma = 1 
    lennard_jones = 48.0 * eps *(sigma**12/(distance**13)-0.5*sigma**6/(distance**7)) 
end function lennard_jones

end program MD

