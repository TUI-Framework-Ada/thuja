with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;

package body Entities is

   ------------------------------------------------------------------
   -- HASH FUNCTION FOR ENTITY IDS
   ------------------------------------------------------------------
   -- Added was forgoetten from UML: Entity_Components : HashMap<Entity_ID, *Components>
   function Hash_Id (Key : Entity_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Key);
   end Hash_Id;

   package Entity_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Entity_Id,
      Element_Type    => Components_Access,
      Hash            => Hash_Id,
      Equivalent_Keys => "=");

   Entity_Components : Entity_Map.Map; -- Store components in a map rather than bools

   ---------------------------------------
   -- Add_Entity
   ---------------------------------------
   function Add_Entity (Id : Entity_Id) return Components_Access is
      New_Components : Components_Access := new Components;
   begin
      if Entity_Components.Contains (Id) then
         raise Program_Error with "Entity already exists: " & Id; --Prevent duplicates
      end if;

      Entity_Components.Insert (Id, New_Components); -- Add new entity with empty components
      return New_Components;
   end Add_Entity;

   ---------------------------------------
   -- Remove_Entity
   ---------------------------------------
   procedure Remove_Entity (Id : Entity_Id) is
   begin
      if Entity_Components.Contains (Id) then
         Entity_Components.Delete (Id);
      end if;
   end Remove_Entity;

   ---------------------------------------
   -- Get_Entity_Components
   ---------------------------------------
   function Get_Entity_Components (Id : Entity_Id)
      return Components_Access
   is
   begin
      if Entity_Components.Contains (Id) then
         return Entity_Components.Element (Id);
      else
         return null;
      end if;
   end Get_Entity_Components;

   ---------------------------------------
   -- Get_Entities_Matching
   ---------------------------------------
   function Get_Entities_Matching
     (Required : Ada.Containers.Indefinite_Vectors.Vector)
      return Ada.Containers.Indefinite_Vectors.Vector
   is
      Result : Ada.Containers.Indefinite_Vectors.Vector;
   begin
      -- Placeholder for ECS logic
      -- This would test each entity's Components against Required list

      for Cursor in Entity_Components.Iterate loop
         Result.Append (Entity_Map.Key (Cursor));
      end loop;

      return Result;
   end Get_Entities_Matching;

end Entities;
