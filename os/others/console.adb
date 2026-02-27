with Graphics;

package body Console is

   --  No implementation
   procedure Enable_VT_Processing is
   begin
      null;
   end Enable_VT_Processing;

   --  Use subprograms from graphics.ads
   procedure Set_Cursor_Visible (Visible : Boolean) is
   begin
      if Visible then
         Graphics.Show_Cursor;
      else
         Graphics.Hide_Cursor;
      end if;
   end Set_Cursor_Visible;

end Console;
