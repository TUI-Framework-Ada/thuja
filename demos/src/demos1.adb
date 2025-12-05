with Ada.Text_IO; use Ada.Text_IO;
with Slider_Widget;
with Entities;

procedure Demos is
   Slider : Entities.Entity_Id;
   Input  : Character;
begin
   Slider := Slider_Widget.Create;

   Put_Line ("Slider test");
   Put_Line ("Press A = left, D = right, Q = quit");

   loop
      Slider_Widget.Draw (Slider);
      Get (Input);
      exit when Input = 'q' or Input = 'Q';
      Slider_Widget.Handle_Input (Slider, Input);
      New_Line;
   end loop;
end Demos;
