module ForcesComputation_mod
use Atom_mod
implicit none 
private
public:: particle_obj

    type particle_obj
      type(Atom_obj):: Atom_obj 
    end type particle_obj 
    
    contains
    
    subroutine init_force(compute_force)
    implicit none 
    type(Atom_obj) :: p1, p2
    real:: compute_force
    
    end subroutine
    
    function compute_force(p1,p2) 
        type(Atom_obj), intent(in) :: p1, p2
        real :: distance, compute_force, force
        real,allocatable:: dx(:)
        integer:: i, dim
        
        dim = size(p1%r)
        allocate(dx(dim))
        
        dx = p2%r - p1%r
        
        do i = 1, dim
            if (abs(dx(i)) > 0.5) dx(i) = dx(i) - sign(1.0, dx(i))
        end do

        distance = sqrt(sum(dx**2))
        
    end function
    
    subroutine compute_forces(particles, n, forces) 
        type(particle_obj), dimension(:) :: particles
        real, dimension(:,:) :: forces
        integer :: n, i, j
        
        do i = 1, n
            forces(i, :) = 0.0
            do j = i + 1, n
                forces(i, :) = forces(i, :) + compute_force(particles(i)%Atom_obj, particles(j)%Atom_obj)
                forces(j, :) = forces(j, :) - compute_force(particles(i)%Atom_obj, particles(j)%Atom_obj)
            end do
        end do
    end subroutine compute_forces 

end module
