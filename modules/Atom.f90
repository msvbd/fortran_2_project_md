module Atom
implicit none
    private
            
    public:: Atom_obj 
        
    type Atom_obj  
        real, allocatable:: position(:), velocity(:), force(:)
        real:: mass
    end type Atom_obj 
end module
