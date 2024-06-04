module Atom_mod
implicit none
    private
            
    public:: Atom_obj
        
    type Atom_obj
        real :: r(3), v(3), f(3), f_old(3)
        real:: mass
    end type Atom_obj 

    interface Atom_obj
        module procedure init
    end interface Atom_obj

    !---------------------------------------------------------
    contains
    type(Atom_obj) function init(r, v, mass)
        real, intent(in):: r(3), v(3), mass
        init%r = r
        init%v = v
        init%f = 0.0
        init%f_old = 0.0
        init%mass = mass
    end function init
end module
