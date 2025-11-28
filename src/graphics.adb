-- Package Body for Graphics


package body Graphics is

   -- Buffer_T Constructor - Allocates memory in the 2D pixel array, intializing record fields
   function Create_Buffer (Width : TUI_Width;
                           Height : TUI_Height)
                           return Buffer_T
   is

      -- Declares variable to be returned using Buffer_T
      New_Buffer : Buffer_T;

   begin

      -- Store dimensions in the record
      New_Buffer.Width := Width;
      New_Buffer.Height := Height;

      return New_Buffer;
   end Create_Buffer;

   -- Writes a new pixel value into the buffer at the (X,Y) coordinates
   procedure Set_Buffer_Pixel (B : in out Buffer_T;
                               X : in TUI_Width;
                               Y : in TUI_Height;
                               P : in Pixel_t)
   is
   begin
      -- Writes new pixel into buffer "P" being the value Pixel
      B.Data (X, Y) := P;
   end Set_Buffer_Pixel;

   -- Reads and returns the pixel value from the buffer at the (X, Y) coordinates
   function Get_Buffer_Pixel (B : in Buffer_T;
                              X : in TUI_Width;
                              Y : in TUI_Height)
                              return Pixel_t
   is
   begin
      -- Returns value read from the array
      return B.Data (X, Y);
   end Get_Buffer_Pixel;

end Graphics;
