with Ada.Strings.Hash;

package body Entities is

   ------------------------------------------------------------------
   -- HASH FUNCTION FOR ENTITY IDS
   ------------------------------------------------------------------
   -- Added was forgoetten from UML: Entity_Components : HashMap<Entity_ID, *Components>
   function Hash_Id (Key : Entity_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (String (Key));
   end Hash_Id;

   ---------------------------------------
   -- Add_Entity
   ---------------------------------------
   function Add_Entity (Id : Entity_Id) return Components_Ptr is
      New_Components : constant Components_Ptr := new Components;
   begin
      if Entity_Components.Contains (Id) then
         raise Program_Error with "Entity already exists: " & String (Id); --Prevent duplicates
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
      return Components_Ptr
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
     (Required : Component_ID_Vector.Vector)
      return Entity_ID_Vector.Vector
   is
      Result : Entity_ID_Vector.Vector;
   begin
      -- Placeholder for ECS logic
      -- This would test each entity's Components against Required list

      for Cursor in Entity_Components.Iterate loop
         Result.Append (Entity_Map.Key (Cursor));
      end loop;

      return Result;
   end Get_Entities_Matching;

end Entities;
