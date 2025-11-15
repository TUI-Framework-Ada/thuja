-- Package Body for Buffer_T

-- Pixel not implemented yet
with Pixel;
use  Pixel;

package body Buffer_T is

   -- Constructor - Allocates memory in the 2D pixel array, intializing record fields
   function Create (Width : TUI_Width;
                    Height : TUI_Height)
                    return Buffer_T
   is

      -- Declares variable to be returned using Buffer
      New_Buffer : Buffer;                

      -- Defines the dimensions for the allocated array
      subtype Actual_X_Index is TUI_Width range 1 .. Width;
      subtype Actual_Y_Index is TUI_Height range 1 .. Height;

      -- Array uses type Pixel
      type Actual_Pixel_Array is array (Actual_X_Index, Actual_Y_Index) of Pixel;

   begin

      -- Allocates memory for the 2D array to point data into using "new"
      New_Buffer.Data := new Actual_Pixel_Array;

      -- Store dimensions in the record
      New_Buffer.Width := Width;
      New_Buffer.Height := Height;

      return New_Buffer;
   end Create;

   -- Destructor - reclaims memory allocated for the 2D array of pixels
   procedure Destroy (B : in out Buffer) is
   begin
      -- Pointer of B.Data to null, detatches Buffer_T record's data from array it was pointed to
      B.Data := null;
   end Destroy;

   -- Writes a new pixel value into the buffer at the (X,Y) coordinates
   procedure Set_Pixel (B : in out Buffer;
                        X : in TUI_Width;
                        Y : in TUI_Height;
                        P : in Pixel)
   is
   begin
      -- Writes new pixel into buffer "P" being the value Pixel
      B.Data.all (X, Y) := P;
   end Set_Pixel;

   -- Reads and returns the pixel value from the buffer at the (X, Y) coordinates
   function Get_Pixel (B : in Buffer;
                       X : in TUI_Width;
                       Y : in TUI_Height)
                       return Pixel
   is
   begin
      -- Returns value read from the array
      return B.Data.all (X, Y);
   end Get_Pixel;

end Buffer_T;