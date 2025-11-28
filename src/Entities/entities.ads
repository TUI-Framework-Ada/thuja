with Ada.Containers.Indefinite_Hashed_Maps;

package Entities is

   type Entity_Id is new String; -- -- UML requires Entity_ID to be String, not Natural (Corrected)
   type Component_Id is new String; -- Component identifiers (for system queries later)

   type Components is tagged private; -- Base type for all component data
   type Components_Access is access all Components; -- Easy access type for component storage

   -- Add / Remove UML
   function Add_Entity (Id : Entity_Id) return Components_Access;
   procedure Remove_Entity (Id : Entity_Id);

   
   
   function Get_Entity_Components (Id : Entity_Id) return Components_Access; -- UML Get components for an entity

   function Get_Entities_Matching -- UML Get all entities with matching components
     (Required : Ada.Containers.Indefinite_Vectors.Vector)
      return Ada.Containers.Indefinite_Vectors.Vector;

private

   type Components is tagged record
   -- Placeholder for Position, Render, Input, etc.
      null;
   end record;

end Entities;
