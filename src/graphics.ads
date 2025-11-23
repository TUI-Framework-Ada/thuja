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

   --  Maximum resolution for the display (Change later)
   type TUI_Width is new Integer range 1 .. 80;
   type TUI_Height is new Integer range 1 .. 50;

   --  Defines two-dimensional array of type Pixel that holds the pixels
   type Pixel_Array is array (TUI_Width, TUI_Height) of Pixel;
   type Pixel_Array_Ptr is access Pixel_Array;

   -- Buffer_T is a record that holds the pixel data and its size
   type Buffer_T is record
      Width : TUI_Width := TUI_Width'Last;
      Height : TUI_Height := TUI_Height'Last;
      --  No default value needed, because Pixel already has defaults
      Data : aliased Pixel_Array;
   end record;

   --  Constructor to create and initialize buffer instance
   function Create_Buffer (Width : in TUI_Width;
                           Height: in TUI_Height)
                           return Buffer_T;

   --  Mutator - Changes state of Pixel
   procedure Set_Buffer_Pixel (B : in out Buffer_T;
                               X : in TUI_Width;
                               Y : in TUI_Height;
                               P : in Pixel);

   --  Accessor - Observes state of Pixel
   function Get_Buffer_Pixel (B : in Buffer_T;
                              X : in TUI_Width;
                              Y : in TUI_Height)
                              return Pixel;

end graphics;
