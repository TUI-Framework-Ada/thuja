with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;
with ECS; use ECS;

package Entities is

   type Entity_Id is new Ada.Strings.Unbounded.Unbounded_String; -- -- UML requires Entity_ID to be String, not Natural (Corrected)
   type Component_Id is new String; -- Component identifiers (for system queries later)

   function Hash_Id (Key : Entity_Id) return Ada.Containers.Hash_Type;

   package Entity_ID_Vector is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type => Natural,
        Element_Type => Entity_Id);

   package Entity_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Entity_Id,
      Element_Type    => Components_Ptr,
      Hash            => Hash_Id,
      Equivalent_Keys => "=");
   subtype Entity_Components is Entity_Map.Map;

   -- Add / Remove UML
   function Add_Entity (Self : in out Entity_Components; Id : Entity_Id) return Components_Ptr;
   procedure Remove_Entity (Self : in out Entity_Components; Id : Entity_Id);

   function Get_Entity_Components (Self : in Entity_Components; Id : Entity_Id) return Components_Ptr; -- UML Get components for an entity

   function Get_Entities_Matching -- UML Get all entities with matching components
     (Self : in Entity_Components; Required : Component_ID_Vector.Vector)
      return Entity_ID_Vector.Vector;

end Entities;
