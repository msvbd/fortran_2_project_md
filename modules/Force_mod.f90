module Force_mod
use Atom_mod
implicit none 
private

interface
    function force_fce(distance)
        real, intent(in) :: distance
        real :: force_fce
    end function force_fce
end interface

procedure(force_fce), pointer :: force_fce_ptr => null()
real :: L(3)

public :: init_force, compute_forces

contains
!====================================================
! function set the force_fce_ptr to the function that will be used to compute the force
subroutine init_force(f_fce, L_in)
    procedure(force_fce) :: f_fce
    real, intent(in) :: L_in

    force_fce_ptr => f_fce
    L = [L_in, L_in, L_in]
    
end subroutine init_force
!----------------------------------------------------
function compute_force(p1,p2) 
    type(Atom_obj), intent(in) :: p1, p2
    real :: compute_force

    real :: dx(size(p1%r))
    integer:: i, dim
    
    dx = p2%r - p1%r
    
    do i = 1, dim
        !if (abs(dx(i)) > 0.5) dx(i) = dx(i) - sign(1.0, dx(i))
        dx = dx - L*nint(dx/L)
    end do

    compute_force = force_fce_ptr(sqrt(dot_product(dx, dx)))
    
end function
!----------------------------------------------------
subroutine compute_forces(particles) 
    type(Atom_obj), dimension(:) :: particles
    real :: force
    integer :: n, i, j
    
    n = size(particles)

    do i = 1, n
        particles(i)%f_old = particles(i)%f
        particles(i)%f = [0.0, 0.0, 0.0]
    end do

    do i = 1, n-1
        do j = i + 1, n
            ! forces(i, :) = forces(i, :) + compute_force(particles(i)%Atom_obj, particles(j)%Atom_obj)
            ! forces(j, :) = forces(j, :) - compute_force(particles(i)%Atom_obj, particles(j)%Atom_obj)
            force = compute_force(particles(i), particles(j))
            particles(i)%f = particles(i)%f + force
            particles(j)%f = particles(j)%f - force
        end do
    end do
end subroutine compute_forces 

end module
