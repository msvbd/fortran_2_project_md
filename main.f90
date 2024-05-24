program MD
use Atom_mod
use Box_mod
use Force_mod
implicit none

contains
        
function lennard_jones(distance)
    real, intent(in) :: distance
    real :: lennard_jones
    real, parameter :: eps = 1, sigma = 1 
    lennard_jones = 48.0 * eps *(sigma**12/(distance**13)-0.5*sigma**6/(distance**7)) 
end function lennard_jones

end program MD

