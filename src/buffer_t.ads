-- Package specification for Buffer_T
package Buffer_T is

   -- Errors because Pixel is not implemented
   with  Pixel;
   use   Pixel;

   -- Maximum resolution for the display (Change later)
   type TUI_Width is new Integer range 1 .. 50;
   type TUI_ Height is new Integer range 1 .. 50;

   -- Declares a private Buffer_T object
   type Buffer_T is private;

   -- Constructor to create and initialize buffer instance
   function create (Width : in TUI_Width;
                     Height: in TUI_Height)
                     return Buffer_T;

   -- Destructor
   procedure Destroy (B : in out Buffer_T);

   -- Mutator - Changes state of Pixel
   procedure Set_Pixel (B : in out Buffer_T;
                        X : in TUI_Width;
                        Y : in TUI_Height;
                        P : in Pixel);

   -- Accessor - Observes state of Pixel
   function Get_Pixel (B : in Buffer_T;
                       X : TUI_Width;
                       Y : TUI_Height;)
                       return Pixel;

private

   -- Defines the bound for the array indices
   subtype X_Index is TUI_Width range 1 .. TUI_Width'Last;
   subtype Y_Index is TUI_Height range 1 .. TUI_Height'Last;

   -- Defines two-dimensional array of type Pixel that holds the pixels
   type Pixel_Array is array (X_Index, Y_Index) of Pixel;

   -- Buffer_T is a record that holds the pixel data and its size
   type Buffer_T is record
      Width : TUI_Width;
      Height : TUI_Height;
      Data : access Pixel_Array;
   end record;

end Buffer_T;
