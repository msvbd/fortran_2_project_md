module ForcesComputation_mod
use Atom_mod
implicit none 
private
public:: particle_obj

    type particle_obj
      type(Atom_obj):: Atom_obj 
    end type particle_obj 
    
    contains
    
  function init_force(compute_force) result(output_force)
      implicit none 
      type(Atom_obj) :: p1, p2
      real :: output_force
      interface 
          real function compute_force(p1, p2)
              import :: Atom_obj
              type(Atom_obj), intent(in) :: p1, p2
          end function compute_force
      end interface
      output_force = compute_force(p1, p2) 
   end function init_force
    
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

        !Here i should somehow call general func that will later respond to LJ, but how? 
        
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
