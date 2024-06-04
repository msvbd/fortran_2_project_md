module Box_mod
use Atom_mod
use Force_mod
implicit none 
private 

public :: Box_obj

type Box_obj
    real :: L(3) !of the box
    real :: dt !time step
    integer:: n !count of atoms
    type(Atom_obj), allocatable :: atoms(:)
    integer, private :: unit = 0 !unit number of the file

    contains
        procedure :: init_atoms, update, write_xyz

end type Box_obj

interface Box_obj
    module procedure create_box
end interface Box_obj

contains
!=============================================
type(Box_obj) function create_box(L, dt)
    real:: L
    real :: dt
    type(Box_obj) :: box

    box%L = [L, L, L]
    box%n = 0
    box%dt = dt

    create_box = box
end function create_box

subroutine init_atoms(this, atom_count)
    class(Box_obj),intent(inout) :: this
    integer,intent(in) :: atom_count
    integer :: i, irx, iry, irz, ir_to
    real :: r(3), v(3)
    real :: dr(3)
    
    this%n = atom_count

    allocate(this%atoms(atom_count))

    ir_to = ceiling(atom_count**(1.0/3.0))
    dr = this%L / ir_to

    write(*,*) 'ir_to = ', ir_to
    write(*,*) 'dr = ', dr

    i = 0
    do
        write(*,*) 'i = ', i
        if (i >= atom_count) exit
        do irx = 0, ir_to-1
            do iry = 0, ir_to-1
                do irz = 0, ir_to-1
                    i = i + 1
                    if (i > atom_count) exit
                    r = [(irx)*dr(1) - this%L(1)/2.0, &
                         (iry)*dr(2) - this%L(2)/2.0, &
                         (irz)*dr(3) - this%L(3)/2.0]

                    ! random velocity between -1 and 1
                    v = [2.0*rand()-1.0, 2.0*rand()-1.0, 2.0*rand()-1.0]

                    this%atoms(i) = Atom_obj(r, v, 1.0)
                end do
            end do
        enddo
    enddo

end subroutine 

subroutine compute_position(atoms, dt, L)
    type(Atom_obj), intent(inout) :: atoms(:)
    real, intent(in) :: dt
    real, intent(in) :: L(3)
    integer :: i,n

    n = size(atoms)
            
    do i = 1, n
        atoms(i)%r = atoms(i)%r + atoms(i)%v * dt + 0.5 * atoms(i)%f * dt**2
        ! PBC
        atoms(i)%r = atoms(i)%r - L * nint(atoms(i)%r / L)
    end do
            
    ! do i = 1, n
    !     do j = 1, 3
    !         if (atoms(i)%r(j) < 0.0) then
    !             atoms(i)%r(j) = atoms(i)%r(j) + box_size
    !         else if (atoms(i)%r(j) >= box_size) then
    !             atoms(i)%r(j) = atoms(i)%r(j) - box_size
    !         end if
    !     end do
    ! end do
        
end subroutine

subroutine compute_velocity(atoms, dt)
    type(Atom_obj), intent(inout) :: atoms(:)
    real, intent(in) :: dt
    integer :: i, n

    n = size(atoms)

    do i = 1, n
        atoms(i)%v = atoms(i)%v + 0.5 * (atoms(i)%f + atoms(i)%f_old) * dt
    end do
end subroutine

subroutine update(this)
    class(Box_obj),intent(inout) :: this

    call compute_position(this%atoms, this%dt, this%L)
    call compute_forces(this%atoms)
    call compute_velocity(this%atoms, this%dt)
end subroutine update

subroutine write_xyz(this, filename)
    class(Box_obj),intent(inout) :: this
    character(len=*) :: filename
    integer :: i, n, unit

    n = size(this%atoms)

    if (this%unit == 0) then
        open(newunit=this%unit, file=filename, status='replace')
    else
        open(unit=this%unit, file=filename, status='old')
    end if

    write(this%unit, '(g0)') n
    write(this%unit, '(g0)') ''
    do i = 1, n
        write(this%unit, '(*(g0,1x))') "Ar",this%atoms(i)%r
    end do

end subroutine

end module Box_mod
