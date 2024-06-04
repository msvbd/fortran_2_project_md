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
!=============================================
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

subroutine compute_position(atoms, box_size, dt)
 
type(Atom_obj), intent(inout) :: atoms(:)
real, intent(in) :: box_size, dt
integer :: i,j, n

n = size(atoms)
        
do i = 1, n
            atoms(i)%r = atoms(i)%r + atoms(i)%v * dt + 0.5 * atoms(i)%f * dt**2
end do
        
do i = 1, n
            do j = 1, 3
                if (atoms(i)%r(j) < 0.0) then
                    atoms(i)%r(j) = atoms(i)%r(j) + box_size
                else if (atoms(i)%r(j) >= box_size) then
                    atoms(i)%r(j) = atoms(i)%r(j) - box_size
                end if
            end do
        end do
    
end subroutine

subroutine compute_velocity(atoms, dt)
    type(Atom_obj), intent(inout) :: atoms(:)
    real, intent(in) :: dt
    integer :: i, j, n
    real :: force(3)

    n = size(atoms)
    
    do i = 1, n
        force = 0.0
        do j = 1, n
            if (i /= j) then
                force = force + compute_force(atoms(i), atoms(j))
            end if
        end do

        atoms(i)%v = atoms(i)%v + 0.5 * (atoms(i)%f + force) * dt
    end do
end subroutine

subroutine update(atoms, dt, box_size)
    type(Atom_obj), intent(inout) :: atoms(:)
    real, intent(in) :: dt, box_size

    call compute_position(atoms, dt, box_size)
    call compute_velocity(atoms, dt) 
end subroutine update

end module
