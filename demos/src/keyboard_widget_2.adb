--  keyboard_widget_2.adb
--
--  TUI keyboard visualiser with Ctrl+A..Z shortcut detection and
--  multi-key sequential command matching.  ESC quits; Ctrl+C does not.
--
--  Colour key:
--    Gray        normal (unpressed) key
--    Red         ordinary keypress
--    Orange      CTRL keys at rest
--    Cyan        Ctrl+letter activated
--    Lime        ALT keys at rest
--    Spring_Green Alt+letter activated
--    Yellow      key typed, sequence still pending
--    Gold        full sequence matched
--    Pink        ESC
--    Steel_Blue  inactive modifier keys (SHIFT, CAPS, F-keys)

with Input_Handling;          use Input_Handling;
with Graphics;                use Graphics;
with ECS;                     use ECS;
with IDs;                     use IDs;
with Components;              use Components;
with Flexbox;                 use Flexbox;
with Command_Sequence_Handling;
with Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

use type Command_Sequence_Handling.Result_Kind_t;

procedure Keyboard_Widget_2 is

   package SU renames Ada.Strings.Unbounded;

   subtype Duration_t is Duration;
   subtype String_t   is String;

   Input_Consume_Delay : constant Duration_t := 0.0;

   Term_Width  : constant TUI_Width  := 72;
   Term_Height : constant TUI_Height := 19;

   Left_Margin : constant TUI_Width := 3;

   Row_Y_Fn     : constant TUI_Height := 2;
   Row_Y_Num    : constant TUI_Height := 4;
   Row_Y_Qwerty : constant TUI_Height := 6;
   Row_Y_Home   : constant TUI_Height := 8;
   Row_Y_Shift  : constant TUI_Height := 10;
   Row_Y_Ctrl   : constant TUI_Height := 12;

   Status_Y     : constant TUI_Height := 14;
   Pending_Y    : constant TUI_Height := 16;
   Status_Width : constant TUI_Width  := Term_Width - 2;

   Color_Normal_Key    : constant Color_t := Gray;
   Color_Pressed_Key   : constant Color_t := Red;
   Color_Ctrl_Default  : constant Color_t := Orange;
   Color_Ctrl_Active   : constant Color_t := Cyan;
   Color_Seq_Buffering : constant Color_t := Yellow;
   Color_Seq_Active    : constant Color_t := Gold;
   Color_Alt_Default   : constant Color_t := Lime;
   Color_Alt_Active    : constant Color_t := Spring_Green;
   Color_Inactive      : constant Color_t := Steel_Blue;
   Color_Esc           : constant Color_t := Pink;

   World : Entity_Components_PO;

   Row_Num_Keys    : constant String_t := "`1234567890-=";
   Row_Qwerty_Keys : constant String_t := "qwertyuiop[]\";
   Row_Home_Keys   : constant String_t := "asdfghjkl;'";
   Row_Shift_Keys  : constant String_t := "zxcvbnm,./";

   All_Printable_Keys : constant String_t :=
      Row_Num_Keys & Row_Qwerty_Keys & Row_Home_Keys & Row_Shift_Keys;

   Pressed_Key  : Character_t         := Character_t'Val (0);
   Pending_Keys : SU.Unbounded_String := SU.Null_Unbounded_String;

   --  Simulated widget command table.
   --  In a real demo this would come from the focused widget's Command_Set_Component_T.
   type Ctrl_Shortcut_T is record
      Letter : Character_t;
      Name   : SU.Unbounded_String;
   end record;

   Max_Ctrl_Shortcuts : constant := 26;
   type Ctrl_Shortcut_Array_T is array (1 .. Max_Ctrl_Shortcuts) of Ctrl_Shortcut_T;

   Ctrl_Shortcuts : constant Ctrl_Shortcut_Array_T := (
      1  => ('s', SU.To_Unbounded_String ("Save")),
      2  => ('z', SU.To_Unbounded_String ("Undo")),
      3  => ('x', SU.To_Unbounded_String ("Cut")),
      4  => ('c', SU.To_Unbounded_String ("Copy")),
      5  => ('v', SU.To_Unbounded_String ("Paste")),
      6  => ('a', SU.To_Unbounded_String ("Select All")),
      7  => ('f', SU.To_Unbounded_String ("Find")),
      8  => ('n', SU.To_Unbounded_String ("New")),
      9  => ('o', SU.To_Unbounded_String ("Open")),
      10 => ('w', SU.To_Unbounded_String ("Close")),
      others => (Character_t'Val (0), SU.Null_Unbounded_String)
   );

   Num_Ctrl_Shortcuts : constant Natural := 10;

   Alt_Shortcuts : constant Ctrl_Shortcut_Array_T := (
      1  => ('f', SU.To_Unbounded_String ("File Menu")),
      2  => ('e', SU.To_Unbounded_String ("Edit Menu")),
      3  => ('v', SU.To_Unbounded_String ("View Menu")),
      4  => ('h', SU.To_Unbounded_String ("Help Menu")),
      5  => ('t', SU.To_Unbounded_String ("Tools Menu")),
      others => (Character_t'Val (0), SU.Null_Unbounded_String)
   );

   Num_Alt_Shortcuts : constant Natural := 5;

   function Find_Alt_Shortcut (Letter : Character_t) return SU.Unbounded_String is
      Lower : constant Character_t := Ada.Characters.Handling.To_Lower (Letter);
   begin
      for I in 1 .. Num_Alt_Shortcuts loop
         if Alt_Shortcuts (I).Letter = Lower then
            return Alt_Shortcuts (I).Name;
         end if;
      end loop;
      return SU.Null_Unbounded_String;
   end Find_Alt_Shortcut;

   function Find_Ctrl_Shortcut (Letter : Character_t) return SU.Unbounded_String is
      Lower : constant Character_t := Ada.Characters.Handling.To_Lower (Letter);
   begin
      for I in 1 .. Num_Ctrl_Shortcuts loop
         if Ctrl_Shortcuts (I).Letter = Lower then
            return Ctrl_Shortcuts (I).Name;
         end if;
      end loop;
      return SU.Null_Unbounded_String;
   end Find_Ctrl_Shortcut;

   procedure Create_Key_Entity (
      Key_Name : String_t;
      Label    : String_t;
      X        : TUI_Width;
      Y        : TUI_Height;
      Width    : TUI_Width;
      BG       : Color_t := Color_Normal_Key
   ) is
      Comp_Ptr : Components_Ptr;
      Widget   : Widget_Component_T;
      Text     : Text_Component_T;
      BG_Comp  : Background_Color_Component_T;
   begin
      Comp_Ptr := Add_Entity (World, To_EID ("key_" & Key_Name));

      Widget.Position_X    := X;
      Widget.Position_Y    := Y;
      Widget.Size_Width    := Width;
      Widget.Size_Height   := 1;
      Widget.Is_Visible    := True;
      Widget.Render_Buffer := Create_Buffer (Width, 1);
      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Widget);

      Text.Text       := SU.To_Unbounded_String (Label);
      Text.Text_Color := Black;
      Add_Component (Comp_Ptr.all, To_CID ("TextComponent"), Text);

      BG_Comp.Background_Color := BG;
      Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG_Comp);
   end Create_Key_Entity;

   --  Centre Name within Width columns with '[' and ']' at the edges.
   function Make_Label (Name : String_t; Width : Natural) return String_t is
      Buf   : String_t (1 .. Width) := (others => ' ');
      Start : Natural;
   begin
      Buf (1)     := '[';
      Buf (Width) := ']';
      if Name'Length <= Width - 2 then
         Start := (Width - Name'Length) / 2 + 1;
         Buf (Start .. Start + Name'Length - 1) := Name;
      end if;
      return Buf;
   end Make_Label;

   --  Caller must hold World for writing.
   procedure Set_Key_Color (
      Entity_List : Entity_Components_Ptr;
      Key_Name    : String_t;
      Color       : Color_t
   ) is
      Comp_Ptr : Components_Ptr;
      BG       : Background_Color_Component_T;
   begin
      Comp_Ptr := Get_Entity_Components (Entity_List.all, To_EID ("key_" & Key_Name));
      if Comp_Ptr /= null
         and then Has_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"))
      then
         BG := Background_Color_Component_T (
            Get_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent")));
         BG.Background_Color := Color;
         Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG);
      end if;
   end Set_Key_Color;

   --  Caller must hold World for writing.
   procedure Set_Text (
      Entity_List : Entity_Components_Ptr;
      Entity_Name : String_t;
      New_Text    : String_t
   ) is
      Comp_Ptr : Components_Ptr;
      Text     : Text_Component_T;
   begin
      Comp_Ptr := Get_Entity_Components (Entity_List.all, To_EID (Entity_Name));
      if Comp_Ptr /= null
         and then Has_Component (Comp_Ptr.all, To_CID ("TextComponent"))
      then
         Text := Text_Component_T (
            Get_Component (Comp_Ptr.all, To_CID ("TextComponent")));
         Text.Text := SU.To_Unbounded_String (New_Text);
         Add_Component (Comp_Ptr.all, To_CID ("TextComponent"), Text);
      end if;
   end Set_Text;

   procedure Create_Text_Entity (
      Name     : String_t;
      X        : TUI_Width;
      Y        : TUI_Height;
      Width    : TUI_Width;
      Initial  : String_t  := "";
      FG_Color : Color_t   := White;
      BG_Color : Color_t   := Black
   ) is
      Comp_Ptr : Components_Ptr;
      Widget   : Widget_Component_T;
      Text     : Text_Component_T;
      BG_Comp  : Background_Color_Component_T;
   begin
      Comp_Ptr := Add_Entity (World, To_EID (Name));

      Widget.Position_X    := X;
      Widget.Position_Y    := Y;
      Widget.Size_Width    := Width;
      Widget.Size_Height   := 1;
      Widget.Is_Visible    := True;
      Widget.Render_Buffer := Create_Buffer (Width, 1);
      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Widget);

      Text.Text       := SU.To_Unbounded_String (Initial);
      Text.Text_Color := FG_Color;
      Add_Component (Comp_Ptr.all, To_CID ("TextComponent"), Text);

      BG_Comp.Background_Color := BG_Color;
      Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG_Comp);
   end Create_Text_Entity;

   --  Flexbox row layout: Flexbox.Layout is called once at startup per row;
   --  resulting Position_X values are used to place key entities absolutely.
   type Key_Desc_T is record
      Name  : SU.Unbounded_String;
      Width : TUI_Width;
      Color : Color_t;
   end record;

   package Key_Desc_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Key_Desc_T);

   function Build_Row (
      Descs  : Key_Desc_Vectors.Vector;
      Row_Y  : TUI_Height;
      Margin : TUI_Width
   ) return Entity_ID_Vector.Vector
   is
      Count     : constant Positive         := Positive (Descs.Length);
      Items_Ptr : constant Flex_Item_Array_Ptr := new Flex_Item_Array (1 .. Count);
      Container : Flex_Container;
      Row_IDs   : Entity_ID_Vector.Vector;
   begin
      for I in 1 .. Count loop
         declare
            D : constant Key_Desc_T := Descs (I);
         begin
            Items_Ptr (I) := (
               Related_Entity => To_EID ("key_" & SU.To_String (D.Name)),
               Flex_Basis     => Natural (D.Width),
               Flex_Grow      => 0.0,
               Flex_Shrink    => 0.0,
               others         => <>
            );
         end;
      end loop;

      Container := (
         Width      => Natural (Term_Width),
         Height     => 1,
         Direction  => Row,
         Justify    => Flex_Start,
         Align      => Flex_Start,
         Items      => Items_Ptr,
         Item_Count => Count
      );

      Flexbox.Layout (Container);

      for I in 1 .. Count loop
         declare
            D     : constant Key_Desc_T := Descs (I);
            Name  : constant String_t   := SU.To_String (D.Name);
            W     : constant TUI_Width  := D.Width;
            --  Position_X is 0-indexed; compute through Natural to avoid a
            --  TUI_Width range failure when the first item has Position_X = 0.
            Col   : constant TUI_Width  :=
               TUI_Width (Natural (Margin) + Container.Items (I).Position_X);
            Label : constant String_t   := Make_Label (Name, Natural (W));
         begin
            Create_Key_Entity (Name, Label, Col, Row_Y, W, D.Color);
            Row_IDs.Append (To_EID ("key_" & Name));
         end;
      end loop;

      return Row_IDs;
   end Build_Row;

   procedure Create_Render_Info_Entity is
      Comp_Ptr    : Components_Ptr;
      RI          : Render_Info_Component_T;
      Root_Widget : Widget_Component_T;
      Root_Marker : Root_Widget_Component_T;
   begin
      Comp_Ptr := Add_Entity (World, To_EID ("render_info"));
      RI.Terminal_Width       := Term_Width;
      RI.Terminal_Height      := Term_Height;
      RI.Prev_Terminal_Width  := Natural (Term_Width);
      RI.Prev_Terminal_Height := Natural (Term_Height);
      RI.BackBuffer           := Create_Buffer (Term_Width, Term_Height);
      RI.Framebuffer_1        := Create_Buffer (Term_Width, Term_Height);
      RI.Framebuffer_2        := Create_Buffer (Term_Width, Term_Height);
      RI.Drawing_FB           := new Protected_DB;

      --  Prime the backbuffer with sentinel pixels so the first frame
      --  unconditionally redraws every cell.
      for X in TUI_Width'First .. Term_Width loop
         for Y in TUI_Height'First .. Term_Height loop
            Set_Buffer_Pixel (RI.Backbuffer, X, Y,
               (Char             => Character_t'Val (1),
                Char_Color       => White,
                Background_Color => White,
                Is_Bold          => True,
                Is_Italic        => False,
                Is_Underline     => False,
                Is_Strikethrough => False));
         end loop;
      end loop;

      Add_Component (Comp_Ptr.all, To_CID ("RenderInfo"), RI);

      Comp_Ptr := Add_Entity (World, To_EID ("root"));
      Root_Widget.Position_X    := 1;
      Root_Widget.Position_Y    := 1;
      Root_Widget.Size_Width    := Term_Width;
      Root_Widget.Size_Height   := Term_Height;
      Root_Widget.Is_Visible    := True;
      Root_Widget.Render_Buffer := Create_Buffer (Term_Width, Term_Height);
      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Root_Widget);
      Add_Component (Comp_Ptr.all, To_CID ("RootWidget"),      Root_Marker);
   end Create_Render_Info_Entity;

   procedure Add_Key (
      V     : in out Key_Desc_Vectors.Vector;
      Name  : String_t;
      Width : TUI_Width;
      Color : Color_t := Color_Normal_Key
   ) is
   begin
      V.Append (Key_Desc_T'(
         Name  => SU.To_Unbounded_String (Name),
         Width => Width,
         Color => Color
      ));
   end Add_Key;

   procedure Create_Keyboard_Entities is
      All_Row_IDs : Entity_ID_Vector.Vector;
      Descs       : Key_Desc_Vectors.Vector;

      procedure Flush_Row (Y : TUI_Height; Margin : TUI_Width := Left_Margin) is
         Row_IDs : constant Entity_ID_Vector.Vector := Build_Row (Descs, Y, Margin);
      begin
         for ID of Row_IDs loop
            All_Row_IDs.Append (ID);
         end loop;
         Descs.Clear;
      end Flush_Row;

   begin
      --  Function-key row (no extra indent — widest row, sets left edge).
      Add_Key (Descs, "ESC", 5, Color_Esc);
      for I in 1 .. 9 loop
         declare
            Img  : constant String_t := Natural'Image (I);
            Name : constant String_t := "F" & Img (Img'First + 1 .. Img'Last);
         begin
            Add_Key (Descs, Name, 4, Color_Inactive);
         end;
      end loop;
      for I in 10 .. 12 loop
         declare
            Img  : constant String_t := Natural'Image (I);
            Name : constant String_t := "F" & Img (Img'First + 1 .. Img'Last);
         begin
            Add_Key (Descs, Name, 5, Color_Inactive);
         end;
      end loop;
      Flush_Row (Row_Y_Fn);

      for Ch of Row_Num_Keys loop
         Add_Key (Descs, (1 => Ch), 3);
      end loop;
      Add_Key (Descs, "BKSP", 7);
      Flush_Row (Row_Y_Num, Left_Margin + 5);

      Add_Key (Descs, "TAB", 5);
      for Ch of Row_Qwerty_Keys loop
         Add_Key (Descs, (1 => Ch), 3);
      end loop;
      Flush_Row (Row_Y_Qwerty, Left_Margin + 6);

      Add_Key (Descs, "CAPS", 7, Color_Inactive);
      for Ch of Row_Home_Keys loop
         Add_Key (Descs, (1 => Ch), 3);
      end loop;
      Add_Key (Descs, "ENT", 6);
      Flush_Row (Row_Y_Home, Left_Margin + 5);

      Add_Key (Descs, "LSHIFT", 9, Color_Inactive);
      for Ch of Row_Shift_Keys loop
         Add_Key (Descs, (1 => Ch), 3);
      end loop;
      Add_Key (Descs, "RSHIFT", 9, Color_Inactive);
      Flush_Row (Row_Y_Shift, Left_Margin + 5);

      Add_Key (Descs, "LCTRL", 7, Color_Ctrl_Default);
      Add_Key (Descs, "LALT",  6, Color_Alt_Default);
      Add_Key (Descs, "SPACE", 22);
      Add_Key (Descs, "RALT",  6, Color_Alt_Default);
      Add_Key (Descs, "RCTRL", 7, Color_Ctrl_Default);
      Flush_Row (Row_Y_Ctrl, Left_Margin + 5);

      Create_Text_Entity ("status_event",   Left_Margin, Status_Y,  Status_Width);
      Create_Text_Entity ("status_pending", Left_Margin, Pending_Y, Status_Width);

      declare
         Entity_List : Entity_Components_Ptr;
         Root_Comp   : Components_Ptr;
         Root_W      : Widget_Component_T;
      begin
         World.Claim_Writing (Entity_List);
         Root_Comp := Get_Entity_Components (Entity_List.all, To_EID ("root"));
         Root_W    := Widget_Component_T (
            Get_Component (Root_Comp.all, To_CID ("WidgetComponent")));

         for ID of All_Row_IDs loop
            Root_W.Children.Append (ID);
         end loop;
         Root_W.Children.Append (To_EID ("status_event"));
         Root_W.Children.Append (To_EID ("status_pending"));

         Add_Component (Root_Comp.all, To_CID ("WidgetComponent"), Root_W);
         World.Release_Writing;
      end;
   end Create_Keyboard_Entities;

   procedure Render is
   begin
      WidgetBackgroundSystem (World);
      TextRenderSystem (World);
      BufferCopySystem (World);
      DoubleBufferFlagSystem (World);
      BufferDrawSystem (World);
   end Render;

   --  All Update_* procedures require World to be held for writing by the caller.

   procedure Update_Key_Colors (Entity_List : Entity_Components_Ptr) is
   begin
      for Ch of All_Printable_Keys loop
         declare
            Lower : constant Character_t := Ada.Characters.Handling.To_Lower (Ch);
         begin
            Set_Key_Color (Entity_List, (1 => Lower), Color_Normal_Key);
         end;
      end loop;

      Set_Key_Color (Entity_List, "TAB",   Color_Normal_Key);
      Set_Key_Color (Entity_List, "ENT",   Color_Normal_Key);
      Set_Key_Color (Entity_List, "SPACE", Color_Normal_Key);
      Set_Key_Color (Entity_List, "BKSP",  Color_Normal_Key);
      --  Reset CTRL keys to Orange so they don't stay Cyan after a Ctrl+letter event.
      Set_Key_Color (Entity_List, "LCTRL", Color_Ctrl_Default);
      Set_Key_Color (Entity_List, "RCTRL", Color_Ctrl_Default);
      --  Reset ALT keys to Lime so they don't stay green after an Alt+key event.
      Set_Key_Color (Entity_List, "LALT", Color_Alt_Default);
      Set_Key_Color (Entity_List, "RALT", Color_Alt_Default);

      if Pressed_Key = Character_t'Val (9) then
         Set_Key_Color (Entity_List, "TAB", Color_Pressed_Key);
      elsif Pressed_Key = Character_t'Val (10)
         or Pressed_Key = Character_t'Val (13)
      then
         Set_Key_Color (Entity_List, "ENT", Color_Pressed_Key);
      elsif Pressed_Key = ' ' then
         Set_Key_Color (Entity_List, "SPACE", Color_Pressed_Key);
      elsif Pressed_Key = Character_t'Val (8)
         or Pressed_Key = Character_t'Val (127)
      then
         Set_Key_Color (Entity_List, "BKSP", Color_Pressed_Key);
      elsif Pressed_Key >= ' ' and Pressed_Key <= '~' then
         declare
            Lower : constant Character_t :=
               Ada.Characters.Handling.To_Lower (Pressed_Key);
         begin
            Set_Key_Color (Entity_List, (1 => Lower), Color_Pressed_Key);
         end;
      end if;
   end Update_Key_Colors;

   procedure Highlight_Buffered_Keys (
      Entity_List : Entity_Components_Ptr;
      Keys        : SU.Unbounded_String
   ) is
   begin
      for I in 1 .. SU.Length (Keys) loop
         Set_Key_Color (Entity_List, (1 => SU.Element (Keys, I)), Color_Seq_Buffering);
      end loop;
   end Highlight_Buffered_Keys;

   procedure Update_Ctrl_Activation (
      Entity_List : Entity_Components_Ptr;
      Letter      : Character_t;
      Raw_Byte    : Natural
   ) is
      Hex_Chars    : constant String_t := "0123456789ABCDEF";
      Hi           : constant Natural  := Raw_Byte / 16;
      Lo           : constant Natural  := Raw_Byte mod 16;
      Hex_Str      : constant String_t := "0x" & Hex_Chars (Hi + 1) & Hex_Chars (Lo + 1);
      Upper_Letter : constant Character_t :=
         Ada.Characters.Handling.To_Upper (Letter);
      Action       : constant SU.Unbounded_String := Find_Ctrl_Shortcut (Letter);

      Status_Width_Int : constant Natural := Natural (Status_Width);
      Status_Text      : String_t (1 .. Status_Width_Int) := (others => ' ');
      Pos              : Natural := 1;

      procedure Append (S : String_t) is
      begin
         if Pos + S'Length - 1 <= Status_Width_Int then
            Status_Text (Pos .. Pos + S'Length - 1) := S;
            Pos := Pos + S'Length;
         end if;
      end Append;
   begin
      Set_Key_Color (Entity_List, "LCTRL", Color_Ctrl_Active);
      Set_Key_Color (Entity_List, "RCTRL", Color_Ctrl_Active);
      Set_Key_Color (Entity_List, (1 => Letter), Color_Ctrl_Active);

      Append ("CTRL+");
      Append ((1 => Upper_Letter));
      if SU.Length (Action) > 0 then
         Append (" activated  [" & Hex_Str & "]  -> " & SU.To_String (Action));
      else
         Append (" pressed  [" & Hex_Str & "]  (unbound)");
      end if;

      Set_Text (Entity_List, "status_event", Status_Text);
   end Update_Ctrl_Activation;

   procedure Update_Alt_Activation (
      Entity_List : Entity_Components_Ptr;
      Letter      : Character_t
   ) is
      Hex_Chars    : constant String_t := "0123456789ABCDEF";
      Val          : constant Natural  := Character_t'Pos (Letter);
      Hi           : constant Natural  := Val / 16;
      Lo           : constant Natural  := Val mod 16;
      Hex_Str      : constant String_t := "0x1B 0x" & Hex_Chars (Hi + 1) & Hex_Chars (Lo + 1);
      Upper_Letter : constant Character_t :=
         Ada.Characters.Handling.To_Upper (Letter);
      Action       : constant SU.Unbounded_String := Find_Alt_Shortcut (Letter);

      Status_Width_Int : constant Natural := Natural (Status_Width);
      Status_Text      : String_t (1 .. Status_Width_Int) := (others => ' ');
      Pos              : Natural := 1;

      procedure Append (S : String_t) is
      begin
         if Pos + S'Length - 1 <= Status_Width_Int then
            Status_Text (Pos .. Pos + S'Length - 1) := S;
            Pos := Pos + S'Length;
         end if;
      end Append;
   begin
      Set_Key_Color (Entity_List, "LALT", Color_Alt_Active);
      Set_Key_Color (Entity_List, "RALT", Color_Alt_Active);
      Set_Key_Color (Entity_List, (1 => Ada.Characters.Handling.To_Lower (Letter)),
                     Color_Alt_Active);

      Append ("ALT+");
      Append ((1 => Upper_Letter));
      if SU.Length (Action) > 0 then
         Append (" activated  [" & Hex_Str & "]  -> " & SU.To_String (Action));
      else
         Append (" pressed  [" & Hex_Str & "]  (unbound)");
      end if;

      Set_Text (Entity_List, "status_event", Status_Text);
   end Update_Alt_Activation;

   procedure Update_Sequential_Activation (
      Entity_List : Entity_Components_Ptr;
      Result      : Command_Sequence_Handling.Handler_Result_t
   ) is
      Status_Width_Int : constant Natural := Natural (Status_Width);
      Status_Text      : String_t (1 .. Status_Width_Int) := (others => ' ');
      Pos              : Natural := 1;

      procedure Append (S : String_t) is
      begin
         if Pos + S'Length - 1 <= Status_Width_Int then
            Status_Text (Pos .. Pos + S'Length - 1) := S;
            Pos := Pos + S'Length;
         end if;
      end Append;
   begin
      for I in 1 .. SU.Length (Result.Keys) loop
         declare
            Ch   : constant Character_t := SU.Element (Result.Keys, I);
            Name : constant String_t    := (1 => Ch);
         begin
            Set_Key_Color (Entity_List, Name, Color_Seq_Active);
         end;
      end loop;

      Append ("Command: ");
      for I in 1 .. SU.Length (Result.Keys) loop
         if I > 1 then Append (" -> "); end if;
         Append ((1 => Ada.Characters.Handling.To_Upper (SU.Element (Result.Keys, I))));
      end loop;

      Set_Text (Entity_List, "status_event", Status_Text);
   end Update_Sequential_Activation;

   procedure Update_Ordinary_Status (Entity_List : Entity_Components_Ptr) is
      Status_Width_Int : constant Natural := Natural (Status_Width);
      Status_Text      : String_t (1 .. Status_Width_Int) := (others => ' ');

      procedure Write (S : String_t; At_Pos : Positive := 1) is
      begin
         if At_Pos + S'Length - 1 <= Status_Width_Int then
            Status_Text (At_Pos .. At_Pos + S'Length - 1) := S;
         end if;
      end Write;

      Hex_Chars : constant String_t := "0123456789ABCDEF";
      Val       : constant Natural  := Character_t'Pos (Pressed_Key);
      Hi        : constant Natural  := Val / 16;
      Lo        : constant Natural  := Val mod 16;
      Hex_Str   : constant String_t := "0x" & Hex_Chars (Hi + 1) & Hex_Chars (Lo + 1);
   begin
      if Pressed_Key >= ' ' and Pressed_Key <= '~' then
         Write ("Last key: '" & Pressed_Key & "'  [" & Hex_Str & "]");
      elsif Pressed_Key = Character_t'Val (9) then
         Write ("Last key: TAB  [0x09]");
      elsif Pressed_Key = Character_t'Val (10) then
         Write ("Last key: LF / Enter  [0x0A]");
      elsif Pressed_Key = Character_t'Val (13) then
         Write ("Last key: CR / Enter  [0x0D]");
      elsif Pressed_Key = Character_t'Val (8)
         or Pressed_Key = Character_t'Val (127)
      then
         Write ("Last key: Backspace  [" & Hex_Str & "]");
      else
         Write ("Last key: (non-printable)  [" & Hex_Str & "]");
      end if;

      Set_Text (Entity_List, "status_event", Status_Text);
   end Update_Ordinary_Status;

   procedure Update_Pending_Status (
      Entity_List : Entity_Components_Ptr;
      Result      : Command_Sequence_Handling.Handler_Result_t
   ) is
      Status_Width_Int : constant Natural := Natural (Status_Width);
      Status_Text      : String_t (1 .. Status_Width_Int) := (others => ' ');
   begin
      case Result.Kind is
         when Command_Sequence_Handling.No_Result =>
            Status_Text (1 .. 33) := "Sequence in progress...          ";
         when Command_Sequence_Handling.Command_Activated
            | Command_Sequence_Handling.Keys_Passed_Through =>
            null;
      end case;

      Set_Text (Entity_List, "status_pending", Status_Text);
   end Update_Pending_Status;

   Needs_Render : Boolean := False;

   procedure Handle_Event (Event : Input_Event_t) is
      Cmd_Result  : Command_Sequence_Handling.Handler_Result_t;
      Entity_List : Entity_Components_Ptr;
   begin
      if Event.Modifier = Ctrl then
         declare
            Ctrl_Raw : constant Natural := Character_t'Pos (Event.Char_Value) - 96;
         begin
            Pressed_Key  := Character_t'Val (0);
            Pending_Keys := SU.Null_Unbounded_String;
            World.Claim_Writing (Entity_List);
            Update_Key_Colors (Entity_List);
            Update_Ctrl_Activation (Entity_List, Event.Char_Value, Ctrl_Raw);
            World.Release_Writing;
            Needs_Render := True;
         end;
         return;
      end if;

      if Event.Modifier = Alt then
         Pressed_Key  := Character_t'Val (0);
         Pending_Keys := SU.Null_Unbounded_String;
         World.Claim_Writing (Entity_List);
         Update_Key_Colors (Entity_List);
         Update_Alt_Activation (Entity_List, Event.Char_Value);
         World.Release_Writing;
         Needs_Render := True;
         return;
      end if;

      Cmd_Result := Command_Sequence_Handling.Process_Key (Event.Char_Value);

      World.Claim_Writing (Entity_List);

      case Cmd_Result.Kind is

         when Command_Sequence_Handling.No_Result =>
            SU.Append (Pending_Keys, Ada.Characters.Handling.To_Lower (Event.Char_Value));
            Pressed_Key := Character_t'Val (0);
            Update_Key_Colors (Entity_List);
            Highlight_Buffered_Keys (Entity_List, Pending_Keys);
            Update_Pending_Status (Entity_List, Cmd_Result);
            Needs_Render := True;

         when Command_Sequence_Handling.Command_Activated =>
            Pending_Keys := SU.Null_Unbounded_String;
            Pressed_Key  := Character_t'Val (0);
            Update_Key_Colors (Entity_List);
            Update_Sequential_Activation (Entity_List, Cmd_Result);
            Update_Pending_Status (Entity_List, Cmd_Result);
            Needs_Render := True;

         when Command_Sequence_Handling.Keys_Passed_Through =>
            Pending_Keys := SU.Null_Unbounded_String;
            if SU.Length (Cmd_Result.Keys) > 0 then
               Pressed_Key := SU.Element (
                  Cmd_Result.Keys, SU.Length (Cmd_Result.Keys));
               Update_Key_Colors (Entity_List);
               Update_Ordinary_Status (Entity_List);
               Update_Pending_Status (Entity_List, Cmd_Result);
               Needs_Render := True;
            end if;

      end case;

      World.Release_Writing;
   end Handle_Event;

   Event   : Input_Event_t;
   Running : Boolean := True;

begin
   Graphics.Clear_Screen;
   Command_Sequence_Handling.Initialize;

   Create_Render_Info_Entity;
   Create_Keyboard_Entities;
   Render;

   Input_Reader.Start;

   while Running loop
      Needs_Render := False;

      loop
         Input_Buffer.Consume (Event);

         --  NUL/None sentinel means the buffer is empty.
         exit when Event.Cmd = None
            and then Event.Char_Value = Character_t'Val (0)
            and then Event.Modifier = None;

         --  Double-ESC quits; Ctrl+C maps to Modifier=Ctrl, Cmd=None.
         if Event.Cmd = Quit and then Event.Char_Value = Character_t'Val (27) then
            Running := False;
            exit;
         end if;

         Handle_Event (Event);
      end loop;

      if Needs_Render and then Running then
         Render;
      end if;

      delay Input_Consume_Delay;
   end loop;

   Input_Reader.Stop;
   Graphics.Clear_Screen;
   Graphics.Reset_Styling;

end Keyboard_Widget_2;
