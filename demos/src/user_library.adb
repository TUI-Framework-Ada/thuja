with Ada.Calendar;
with Graphics;
with IDs;

package body User_Library is

   procedure RainbowTextSystem (Entity_List : ECS.Entity_Components) is
      Search_Component_IDs : IDs.Component_ID_Vector.Vector;
      Matched_Entities : IDs.Entity_ID_Vector.Vector;
      Component_List : ECS.Components_Ptr;
      Text_C : Components.Text_Component_T;
      RainbowText_C : RainbowTextComponent;
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Unused1 : Ada.Calendar.Year_Number;
      Unused2 : Ada.Calendar.Month_Number;
      Unused3 : Ada.Calendar.Day_Number;
      Seconds : Ada.Calendar.Day_Duration;
      Hue : Integer;
      X : Float;
      Rp : Float;
      Gp : Float;
      Bp : Float;
   begin
      Search_Component_IDs.Append (IDs.To_CID ("TextComponent"));
      Search_Component_IDs.Append (IDs.To_CID ("RainbowTextComponent"));
      Matched_Entities := ECS.Get_Entities_Matching (Entity_List, Search_Component_IDs);
      for EID of Matched_Entities loop
         Component_List := ECS.Get_Entity_Components (Entity_List, EID);
         Text_C := Components.Text_Component_T (
            ECS.Get_Component (Component_List.all, IDs.To_CID ("TextComponent"))
                                               );
         RainbowText_C := RainbowTextComponent (
            ECS.Get_Component (Component_List.all, IDs.To_CID ("RainbowTextComponent"))
                                          );

         --  Update text color based on current time
         Ada.Calendar.Split (Now, Unused1, Unused2, Unused3, Seconds);
         Hue := Integer (Float (Seconds) * Float (RainbowText_C.Hue_Change_Speed)) mod 360;
         X := 1.0 - abs (Float'Remainder (Float (Hue) / 60.0, 2.0) - 1.0);
         if Hue < 60 then
               Rp := 1.0; Gp := X; Bp := 0.0;
         elsif Hue < 120 then
               Rp := X; Gp := 1.0; Bp := 0.0;
         elsif Hue < 180 then
               Rp := 0.0; Gp := 1.0; Bp := X;
         elsif Hue < 240 then
               Rp := 0.0; Gp := X; Bp := 1.0;
         elsif Hue < 300 then
               Rp := X; Gp := 0.0; Bp := 1.0;
         elsif Hue <= 360 then
               Rp := 1.0; Gp := 0.0; Bp := X;
         end if;
         pragma Assert (Rp * 255.0 <= 255.0, "RED GOES OVER");
         Text_C.Text_Color.Red := Graphics.u8 (Integer (Rp * 255.0) mod 256);
         Text_C.Text_Color.Green := Graphics.u8 (Integer (Gp * 255.0) mod 256);
         Text_C.Text_Color.Blue := Graphics.u8 (Integer (Bp * 255.0) mod 256);

         --  Update text component
         ECS.Add_Component (
            ECS.Get_Entity_Components (Entity_List, EID).all,
            IDs.To_CID ("TextComponent"),
            Text_C
                           );
      end loop;
   end RainbowTextSystem;

end User_Library;
