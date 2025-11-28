with graphics; use graphics;



with Ada.Text_IO;
-- use Ada.Text_IO;

package body thuja is 
   procedure log is 
      Pix : Pixel_t := ('A', Red, Black, True);
   begin
      Ada.Text_IO.Put_Line (Pix'Image);
   end;

end thuja;
