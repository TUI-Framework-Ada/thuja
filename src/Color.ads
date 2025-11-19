--  Color.ads
--  RGB color entity for the TUI framework

pragma Ada_2022;
pragma Pure;

package Color is

   subtype RGB_Value is Integer range 0 .. 255;

   type Color is record
      Red   : RGB_Value := 0;
      Green : RGB_Value := 0;
      Blue  : RGB_Value := 0;
   end record;

   -- Constructors
   function Make (R, G, B : RGB_Value) return Color;

   -- Common named colors
   function Black  return Color;
   function White  return Color;
   function Red    return Color;
   function Green  return Color;
   function Blue   return Color;

   -- Setter
   procedure Set (C : in out Color; R, G, B : RGB_Value);


end Color;
