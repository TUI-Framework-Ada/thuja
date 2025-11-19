--Vector-based implementation of entity management
with Ada.Containers.Vectors;

package body Entities is

   -- Use Natural for IDs but Positive for vector indexes.
   -- Think of it as the bank for Widget ID's.
   package Bool_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Boolean);

   Alive   : Bool_Vector.Vector; -- Tricks which entities are alive.
   Next_Id : Entity_Id := 1; -- Next available entity ID.

   -- Creates a new entity and returns its ID.
   function Create return Entity_Id is
      Id : constant Entity_Id := Next_Id;
   begin
      -- Extend container until index Id exists
      -- If Id > Last_Index, that entity was never created.
      -- Ensure the vector is large enough.
      while Alive.Last_Index < Positive(Id) loop
         Alive.Append (False);
      end loop;

      Alive.Replace_Element (Positive(Id), True);

      Next_Id := Next_Id + 1; -- Increment for next entity.
      return Id;
   end Create;

   -- Destroy a widget by marking it as not alive.
   procedure Destroy (Id : Entity_Id) is
   begin

   -- Only modify the vector if the entity index exists.
   -- If Id > Last_Index, that entity was never created.

      if Positive(Id) <= Alive.Last_Index then
         Alive.Replace_Element (Positive(Id), False); --Marks Dead
      end if;
   end Destroy;

   -- Is_Alive
   function Is_Alive (Id : Entity_Id) return Boolean is
   begin

   -- Check if the entity index exists.
   -- If it does, return the stored Boolean.
   -- Should at somepoint prevent renderer from drawing deleted widgets.

      if Positive(Id) <= Alive.Last_Index then
         return Alive.Element (Positive(Id));
      else
         return False;
      end if;
   end Is_Alive;
end Entities;
