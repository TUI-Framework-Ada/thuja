with Ada.Text_IO;
with Entities;

procedure Demos is
   E1, E2 : Entities.Entity_Id;
begin
   -- Create two entities
   E1 := Entities.Create;
   E2 := Entities.Create;

   Ada.Text_IO.Put_Line
     ("Created entity IDs: " & E1'Image & ", " & E2'Image);

   -- Show they are alive
   Ada.Text_IO.Put_Line
     ("E1 alive? " & Boolean'Image (Entities.Is_Alive (E1)));
   Ada.Text_IO.Put_Line
     ("E2 alive? " & Boolean'Image (Entities.Is_Alive (E2)));

   -- Destroy one entity
   Entities.Destroy (E1);

   Ada.Text_IO.Put_Line
     ("After destroy: E1 alive? "
       & Boolean'Image (Entities.Is_Alive (E1)));
end Demos;