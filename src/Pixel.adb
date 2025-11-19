--  Pixel.adb

pragma Ada_2022;

with Color;

package body Pixel is

   function Make
     (Ch   : Character;
      FG   : Color.Color := Color.White;
      BG   : Color.Color := Color.Black;
      Bold : Boolean := False) return Pixel is
   begin
      return (Character        => Ch,
              Text_Color       => FG,
              Background_Color => BG,
              Is_Bold          => Bold);
   end Make;

   procedure Set_Char (P : in out Pixel; Ch : Character) is
   begin
      P.Character := Ch;
   end Set_Char;

   procedure Set_FG (P : in out Pixel; FG : Color.Color) is
   begin
      P.Text_Color := FG;
   end Set_FG;

   procedure Set_BG (P : in out Pixel; BG : Color.Color) is
   begin
      P.Background_Color := BG;
   end Set_BG;

   procedure Set_Bold (P : in out Pixel; Bold : Boolean) is
   begin
      P.Is_Bold := Bold;
   end Set_Bold;

end Pixel;
