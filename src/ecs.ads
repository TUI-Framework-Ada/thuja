with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Components; use Components;
with IDs; use IDs;

package ECS is

   function Hash_Component (Key : Component_Id) return Ada.Containers.Hash_Type;

   package Component_ID_Vector is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type => Natural,
        Element_Type => Component_Id);

   package Component_Map_Pkg is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type => Component_Id,
        Element_Type => Component_T'Class,
        Hash => Hash_Component,
        Equivalent_Keys => "=");
   subtype Component_Map is Component_Map_Pkg.Map;
   type Component_Map_Ptr is access Component_Map;

   type Components is record
      Components_Map : Component_Map;
   end record;
   type Components_Ptr is access all Components;



   procedure Add_Component (Self : in out Components;
                            Component : in Component_Id;
                            Component_Struct : in Component_T'Class);

   procedure Remove_Component (Self : in out Components;
                               Component : in Component_Id);

   function Get_Component (Self : in Components;
                           Component : in Component_Id) return Component_T'Class;

   function Has_Component (Self : in Components;
                           Component : in Component_Id) return Boolean;

   function Hash_Entity (Key : Entity_Id) return Ada.Containers.Hash_Type;

   package Entity_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Entity_Id,
      Element_Type    => Components_Ptr,
      Hash            => Hash_Entity,
      Equivalent_Keys => "=");
   subtype Entity_Components is Entity_Map.Map;

   package Entity_ID_Vector is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type => Natural,
        Element_Type => Entity_Id);

   -- Add / Remove UML
   function Add_Entity (Self : in out Entity_Components; Id : Entity_Id) return Components_Ptr;
   procedure Remove_Entity (Self : in out Entity_Components; Id : Entity_Id);

   function Get_Entity_Components (Self : in Entity_Components; Id : Entity_Id) return Components_Ptr; -- UML Get components for an entity

   function Get_Entities_Matching -- UML Get all entities with matching components
     (Self : in Entity_Components; Required : Component_ID_Vector.Vector)
      return Entity_ID_Vector.Vector;

end ECS;
