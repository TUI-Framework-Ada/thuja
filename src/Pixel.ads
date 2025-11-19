--  Pixel.ads
--  Pixel component for the TUI framework

pragma Ada_2022;

with Color;

package Pixel is

   type Pixel is record
      Character        : Character   := ' ';
      Text_Color       : Color.Color := Color.White;
      Background_Color : Color.Color := Color.Black;
      Is_Bold          : Boolean     := False;
   end record;

   -- Constructor
   function Make
     (Ch   : Character;
      FG   : Color.Color := Color.White;
      BG   : Color.Color := Color.Black;
      Bold : Boolean := False) return Pixel;

   -- ECS-style setters
   procedure Set_Char (P : in out Pixel; Ch   : Character);
   procedure Set_FG   (P : in out Pixel; FG   : Color.Color);
   procedure Set_BG   (P : in out Pixel; BG   : Color.Color);
   procedure Set_Bold (P : in out Pixel; Bold : Boolean);

end Pixel;
