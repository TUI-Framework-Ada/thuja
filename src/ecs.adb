package body ECS is

   procedure Add_Component (Self : in out Components;
                            Component_ID : in String;
                            Component_Struct : in Component_T'Class) is
   begin
      Self.Components_Map.Include (Component_ID, Component_Struct);
   end Add_Component;

   procedure Remove_Component (Self : in out Components;
                               Component_ID : in String) is
   begin
      Self.Components_Map.Exclude (Component_ID);
   end Remove_Component;

   function Get_Component (Self : in Components;
                           Component_ID : in String)
                           return Component_T'Class is
   begin
      return Self.Components_Map (Component_ID);
   end Get_Component;

   function Has_Component (Self : in Components;
                           Component_ID : in String) return Boolean is
   begin
      return Self.Components_Map.Contains (Component_ID);
   end Has_Component;

   ------------------------------------------------------------------
   -- HASH FUNCTION FOR ENTITY IDS
   ------------------------------------------------------------------
   -- Added was forgoetten from UML: Entity_Components : HashMap<Entity_ID, *Components>
   function Hash_Id (Key : Entity_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Unbounded.Hash (Ada.Strings.Unbounded.Unbounded_String (Key));
   end Hash_Id;

   ---------------------------------------
   -- Add_Entity
   ---------------------------------------
   function Add_Entity (Self : in out Entity_Components; Id : Entity_Id) return Components_Ptr is
      New_Components : Components_Ptr;
   begin
      if Self.Contains (Id) then
         return Self (Id);
      end if;

      New_Components := new Components;
      Self.Insert (Id, New_Components); -- Add new entity with empty components
      return New_Components;
   end Add_Entity;

   ---------------------------------------
   -- Remove_Entity
   ---------------------------------------
   procedure Remove_Entity (Self : in out Entity_Components; Id : Entity_Id) is
   begin
      if Self.Contains (Id) then
         Self.Delete (Id);
      end if;
   end Remove_Entity;

   ---------------------------------------
   -- Get_Entity_Components
   ---------------------------------------
   function Get_Entity_Components (Self : in Entity_Components; Id : Entity_Id)
      return Components_Ptr
   is
   begin
      if Self.Contains (Id) then
         return Self.Element (Id);
      else
         return null;
      end if;
   end Get_Entity_Components;

   ---------------------------------------
   -- Get_Entities_Matching
   ---------------------------------------
   function Get_Entities_Matching
     (Self : in Entity_Components; Required : Component_ID_Vector.Vector)
      return Entity_ID_Vector.Vector
   is
      Result : Entity_ID_Vector.Vector;
      Checking_Entity : Entity_Id;
      Matching : Boolean;
   begin
      --  ECS logic
      --  This (theoretically) tests each entity's Components against Required

      for Entity_Cursor in Self.Iterate loop
         Matching := True;
         Checking_Entity := Entity_Map.Key (Entity_Cursor);

         for Component_Cursor in Required.Iterate loop
            if not (Has_Component(
               Entity_Map.Element (Self, Checking_Entity).all,
               Component_ID_Vector.Element (
                  Required, Component_ID_Vector.To_Index (Component_Cursor)
               )
            )) then
               Matching := False;
               exit; --  break
            end if;
         end loop;

         if Matching then
            Result.Append (Checking_Entity);
         end if;
      end loop;

      return Result;
   end Get_Entities_Matching;

end ECS;
