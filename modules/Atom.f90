module Atom
implicit none
    private
            
    public:: Atom_obj 
        
    type Atom_obj  
        real, allocatable:: Radius(:), Velocity(:), Mass(:), Force(:)
    end type Atom_obj 
end module
