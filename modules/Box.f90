module Box_mod
use Atom_mod
implicit none 
    private 
    
    public :: Box_obj
    
    type Box_obj
        real :: L(3) !of the box
        integer:: n !count of atoms
        type(Atom_obj), allocatable :: atoms(:)

        contains
            procedure :: init_atoms

    end type Box_obj

    interface Box_obj
        module procedure create_box
    end interface Box_obj

    contains

    type(Box_obj) function create_box(L)
        real, dimension(3) :: L
        type(Box_obj) :: box

        box%L = L
        box%n = 0

        create_box = box
    end function create_box

    subroutine init_atoms(this, atom_count)
        class(Box_obj),intent(inout) :: this
        integer,intent(in) :: atom_count
        integer :: i
        real :: r(3)=[0.,0.,0.], v(3)=[0.,0.,0.], mass = 1.
        
        this%n = atom_count

        allocate(this%atoms(atom_count))

        do i = 1, atom_count
            ! Initialize the atom's position
            ! the code will be here

            ! Initialize the atom's velocity
            ! the code will be here

            this%atoms(i) = Atom_obj(r, v, mass)
        end do

    end subroutine 
end module
