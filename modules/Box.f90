module Box
use Atom
implicit none 
    private 
    
    public :: Box_obj, init_atoms
    
    type Box_obj
        real, allocatable:: dimensions(:) !of the box
        integer:: n !count of atoms
        type(Atom_obj), allocatable :: atoms(:)
    end type Box_obj

    contains

    subroutine init_atoms(box, atom_count, atom_positions, atom_velocities)
        type(Box_obj) :: box
        integer:: atom_count, i
        real, dimension(:,:) :: atom_positions, atom_velocities
        
        box%n = atom_count

        allocate(box%atoms(atom_count))

        do i = 1, atom_count
                allocate(box%atoms(i)%position(size(atom_positions, 2)))
                allocate(box%atoms(i)%velocity(size(atom_velocities, 2)))
            
                box%atoms(i)%position = atom_positions(:,i)
                box%atoms(i)%velocity = atom_velocities(:,i)
        end do

    end subroutine 
end module
