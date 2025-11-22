package graphics is 

   type u8 is mod 2**8 with size => 8;
   type Color is record 
      Red : u8 := 0;
      Green : u8 := 0;
      Blue : u8 := 0;
   end record;

   Black : constant Color := (0,0,0);
   White : constant Color := (255,255,255);
   Red : Color := (Red => 255, Green => 0, Blue => 0);

   type Pixel is record
      Char             : Character   := ' ';
      Char_Color       : Color := Black;
      Background_Color : Color := White;
      Is_Bold          : Boolean     := False;
   end record;

end graphics;