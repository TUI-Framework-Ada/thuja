--  Color.adb

pragma Ada_2022;

package body Color is

   function Make (R, G, B : RGB_Value) return Color is
   begin
      return (Red => R, Green => G, Blue => B);
   end Make;

   function Black  return Color is (Red => 0,   Green => 0,   Blue => 0);
   function White  return Color is (Red => 255, Green => 255, Blue => 255);
   function Red    return Color is (Red => 255, Green => 0,   Blue => 0);
   function Green  return Color is (Red => 0,   Green => 255, Blue => 0);
   function Blue   return Color is (Red => 0,   Green => 0,   Blue => 255);

   procedure Set (C : in out Color; R, G, B : RGB_Value) is
   begin
      C.Red   := R;
      C.Green := G;
      C.Blue  := B;
   end Set;


end Color;
