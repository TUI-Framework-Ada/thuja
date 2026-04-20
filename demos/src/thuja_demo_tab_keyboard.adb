--  Thuja_demo_tab_keyboard
--
--  Adapts the standalone Keyboard_Widget_2 demo into a tab inside the
--  Thuja_Demo shell.  Renders an ASCII keyboard, lights up keys as they
--  are pressed, recognises Ctrl+letter and Alt+letter shortcuts, and
--  reports multi-key sequential commands via Command_Sequence_Handling.

with Components;                use Components;
with IDs;                       use IDs;
with ECS;                       use ECS;
with Flexbox;                   use Flexbox;
with Command_Sequence_Handling;
with Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

use type Command_Sequence_Handling.Result_Kind_t;

package body Thuja_demo_tab_keyboard is

   package SU renames Ada.Strings.Unbounded;

   subtype String_t is String;

   Tab_Index : constant Natural := 5;

   --  Layout constants.  The keyboard widget reserves a 72-column slab
   --  starting at Left_Margin; rows are offset from Content_Top so the
   --  same code can live inside any host shell that owns the chrome.
   Keyboard_Width  : constant TUI_Width  := 72;
   Left_Margin     : constant TUI_Width  := 3;
   Status_Width    : constant TUI_Width  := Keyboard_Width - 2;

   --  Colour palette
   Color_Normal_Key    : constant Color_t := Gray;
   Color_Pressed_Key   : constant Color_t := Red;
   Color_Ctrl_Default  : constant Color_t := Orange;
   Color_Ctrl_Active   : constant Color_t := Cyan;
   Color_Seq_Buffering : constant Color_t := Yellow;
   Color_Seq_Active    : constant Color_t := Gold;
   Color_Alt_Default   : constant Color_t := Lime;
   Color_Inactive      : constant Color_t := Steel_Blue;
   Color_Esc           : constant Color_t := Pink;

   Key_Border_Color    : constant Color_t := Black;
   Key_Char_Color      : constant Color_t := Black;

   Row_Num_Keys    : constant String_t := "`1234567890-=";
   Row_Qwerty_Keys : constant String_t := "qwertyuiop[]\";
   Row_Home_Keys   : constant String_t := "asdfghjkl;'";
   Row_Shift_Keys  : constant String_t := "zxcvbnm,./";

   All_Printable_Keys : constant String_t :=
      Row_Num_Keys & Row_Qwerty_Keys & Row_Home_Keys & Row_Shift_Keys;

   --  Persistent tab state
   Pressed_Key      : Character_t         := Character_t'Val (0);
   Pending_Keys     : SU.Unbounded_String := SU.Null_Unbounded_String;
   Initialized      : Boolean             := False;

   --  Live counters surfaced in the right-hand counters panel so viewers
   --  can watch the input pipeline tick in real time.
   Events_Count     : Natural := 0;
   Ctrl_Count       : Natural := 0;
   Sequence_Count   : Natural := 0;
   Last_Byte        : Natural := 0;

   --  Number of buffer-indicator chip slots reserved at create time;
   --  Handle_Event rewrites their text/colour on each keystroke.
   Max_Buffer_Chips : constant := 8;

   --  Simulated widget command tables.  Mirror Keyboard_Widget_2 so the
   --  same shortcuts light up identically when the demo runs.
   type Ctrl_Shortcut_T is record
      Letter : Character_t;
      Name   : SU.Unbounded_String;
   end record;

   Max_Ctrl_Shortcuts : constant := 26;
   type Ctrl_Shortcut_Array_T is array (1 .. Max_Ctrl_Shortcuts) of Ctrl_Shortcut_T;

   Ctrl_Shortcuts : constant Ctrl_Shortcut_Array_T :=
     (1  => ('s', SU.To_Unbounded_String ("Save")),
      2  => ('z', SU.To_Unbounded_String ("Undo")),
      3  => ('x', SU.To_Unbounded_String ("Cut")),
      4  => ('c', SU.To_Unbounded_String ("Copy")),
      5  => ('v', SU.To_Unbounded_String ("Paste")),
      6  => ('a', SU.To_Unbounded_String ("Select All")),
      7  => ('f', SU.To_Unbounded_String ("Find")),
      8  => ('n', SU.To_Unbounded_String ("New")),
      9  => ('o', SU.To_Unbounded_String ("Open")),
      10 => ('w', SU.To_Unbounded_String ("Close")),
      others => (Character_t'Val (0), SU.Null_Unbounded_String));

   Num_Ctrl_Shortcuts : constant Natural := 10;

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

   procedure Create_Key_Entity
     (World    : in out ECS.Entity_Components_PO;
      Page     : in Tab_Page_Component_T;
      Key_Name : in String_t;
      Label    : in String_t;
      X        : in TUI_Width;
      Y        : in TUI_Height;
      Width    : in TUI_Width;
      BG       : in Color_t := Color_Normal_Key)
   is
      CP      : Components_Ptr;
      Txt     : Text_Component_T;
      BG_Comp : Background_Color_Component_T;
   begin
      CP := Make_Widget_With_BG
        (World, "kbkey_" & Key_Name, X, Y, Width, 1, BG);

      Txt.Text       := SU.To_Unbounded_String (Label);
      Txt.Text_Color := Key_Border_Color;
      Txt.Is_Bold    := True;
      Add_Component (CP.all, To_CID ("TextComponent"), Txt);

      BG_Comp.Background_Color := BG;
      Add_Component (CP.all, To_CID ("BackgroundColorComponent"), BG_Comp);

      Add_Component (CP.all, To_CID ("TabPage"), Page);

      if Width >= 3 then
         declare
            Inner_W      : constant TUI_Width := Width - 2;
            Inner_Label  : constant String_t :=
              Label (Label'First + 1 .. Label'Last - 1);
            Overlay_Name : constant String_t :=
              "kbkey_" & Key_Name & "_ltr";
            Overlay_Txt  : Text_Component_T;
            Overlay_BG   : Background_Color_Component_T;
         begin
            CP := Make_Widget_With_BG
              (World, Overlay_Name, X + 1, Y, Inner_W, 1, BG);

            Overlay_Txt.Text       := SU.To_Unbounded_String (Inner_Label);
            Overlay_Txt.Text_Color := Key_Char_Color;
            Add_Component (CP.all, To_CID ("TextComponent"), Overlay_Txt);

            Overlay_BG.Background_Color := BG;
            Add_Component
              (CP.all, To_CID ("BackgroundColorComponent"), Overlay_BG);
            Add_Component (CP.all, To_CID ("TabPage"), Page);
         end;
      end if;
   end Create_Key_Entity;

   procedure Create_Text_Entity
     (World    : in out ECS.Entity_Components_PO;
      Page     : in Tab_Page_Component_T;
      Name     : in String_t;
      X        : in TUI_Width;
      Y        : in TUI_Height;
      Width    : in TUI_Width;
      Initial  : in String_t  := "";
      FG_Color : in Color_t   := White;
      BG_Color : in Color_t   := Black)
   is
      CP      : Components_Ptr;
      Txt     : Text_Component_T;
      BG_Comp : Background_Color_Component_T;
   begin
      CP := Make_Widget_With_BG (World, Name, X, Y, Width, 1, BG_Color);

      Txt.Text       := SU.To_Unbounded_String (Initial);
      Txt.Text_Color := FG_Color;
      Add_Component (CP.all, To_CID ("TextComponent"), Txt);

      BG_Comp.Background_Color := BG_Color;
      Add_Component (CP.all, To_CID ("BackgroundColorComponent"), BG_Comp);

      Add_Component (CP.all, To_CID ("TabPage"), Page);
   end Create_Text_Entity;

   --  Flexbox row layout: Flexbox.Layout is called once per row at startup
   --  to produce key positions; resulting Position_X values are used to
   --  place the key entities absolutely.
   type Key_Desc_T is record
      Name  : SU.Unbounded_String;
      Width : TUI_Width;
      Color : Color_t;
   end record;

   package Key_Desc_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Key_Desc_T);

   procedure Add_Key
     (V     : in out Key_Desc_Vectors.Vector;
      Name  : in String_t;
      Width : in TUI_Width;
      Color : in Color_t := Color_Normal_Key)
   is
   begin
      V.Append (Key_Desc_T'
        (Name  => SU.To_Unbounded_String (Name),
         Width => Width,
         Color => Color));
   end Add_Key;

   procedure Build_Row
     (World  : in out ECS.Entity_Components_PO;
      Page   : in Tab_Page_Component_T;
      Descs  : in Key_Desc_Vectors.Vector;
      Row_Y  : in TUI_Height;
      Margin : in TUI_Width)
   is
      Count     : constant Positive            := Positive (Descs.Length);
      Items_Ptr : constant Flex_Item_Array_Ptr := new Flex_Item_Array (1 .. Count);
      Container : Flex_Container;
   begin
      for I in 1 .. Count loop
         declare
            D : constant Key_Desc_T := Descs (I);
         begin
            Items_Ptr (I) :=
              (Related_Entity => To_EID ("kbkey_" & SU.To_String (D.Name)),
               Flex_Basis     => Natural (D.Width),
               Flex_Grow      => 0.0,
               Flex_Shrink    => 0.0,
               others         => <>);
         end;
      end loop;

      Container :=
        (Width      => Natural (Keyboard_Width),
         Height     => 1,
         Direction  => Row,
         Justify    => Flex_Start,
         Align      => Flex_Start,
         Items      => Items_Ptr,
         Item_Count => Count);

      Flexbox.Layout (Container);

      for I in 1 .. Count loop
         declare
            D     : constant Key_Desc_T := Descs (I);
            Name  : constant String_t   := SU.To_String (D.Name);
            W     : constant TUI_Width  := D.Width;
            Col   : constant TUI_Width  :=
              TUI_Width (Natural (Margin) + Container.Items (I).Position_X);
            Label : constant String_t   := Make_Label (Name, Natural (W));
         begin
            Create_Key_Entity (World, Page, Name, Label, Col, Row_Y, W, D.Color);
         end;
      end loop;
   end Build_Row;

   --  Caller must hold World for writing.
   procedure Set_Key_Color
     (Entity_List : Entity_Components_Ptr;
      Key_Name    : String_t;
      Color       : Color_t)
   is
      procedure Paint (Entity_Name : String_t) is
         CP : Components_Ptr;
         BG : Background_Color_Component_T;
      begin
         CP := Get_Entity_Components (Entity_List.all, To_EID (Entity_Name));
         if CP /= null
            and then Has_Component (CP.all, To_CID ("BackgroundColorComponent"))
         then
            BG := Background_Color_Component_T
              (Get_Component (CP.all, To_CID ("BackgroundColorComponent")));
            BG.Background_Color := Color;
            Add_Component (CP.all, To_CID ("BackgroundColorComponent"), BG);
         end if;
      end Paint;
   begin
      Paint ("kbkey_" & Key_Name);
      Paint ("kbkey_" & Key_Name & "_ltr");
   end Set_Key_Color;

   --  Caller must hold World for writing.
   procedure Set_Text
     (Entity_List : Entity_Components_Ptr;
      Entity_Name : String_t;
      New_Text    : String_t)
   is
      CP : Components_Ptr;
      T  : Text_Component_T;
   begin
      CP := Get_Entity_Components (Entity_List.all, To_EID (Entity_Name));
      if CP /= null
         and then Has_Component (CP.all, To_CID ("TextComponent"))
      then
         T := Text_Component_T
           (Get_Component (CP.all, To_CID ("TextComponent")));
         T.Text := SU.To_Unbounded_String (New_Text);
         Add_Component (CP.all, To_CID ("TextComponent"), T);
      end if;
   end Set_Text;

   --  Caller must hold World for writing.  Sets the BackgroundColor on
   --  any entity by full name (Set_Key_Color is keyboard-specific).
   procedure Set_Background
     (Entity_List : Entity_Components_Ptr;
      Entity_Name : String_t;
      Color       : Color_t)
   is
      CP : Components_Ptr;
      BG : Background_Color_Component_T;
   begin
      CP := Get_Entity_Components (Entity_List.all, To_EID (Entity_Name));
      if CP /= null
         and then Has_Component (CP.all, To_CID ("BackgroundColorComponent"))
      then
         BG := Background_Color_Component_T
           (Get_Component (CP.all, To_CID ("BackgroundColorComponent")));
         BG.Background_Color := Color;
         Add_Component (CP.all, To_CID ("BackgroundColorComponent"), BG);
      end if;
   end Set_Background;

   --  Format the four live counters into the panel text.  Compact form
   --  so the status+counters flex Row fits inside Keyboard_Width.
   function Format_Counters return String_t is
      function Img (N : Natural) return String_t is
         S : constant String_t := Natural'Image (N);
      begin
         return S (S'First + 1 .. S'Last);
      end Img;
      Hex_Chars : constant String_t := "0123456789ABCDEF";
   begin
      return "Ev:" & Img (Events_Count)
        & "  Ctrl:" & Img (Ctrl_Count)
        & "  Seq:" & Img (Sequence_Count)
        & "  0x"
        & Hex_Chars (Last_Byte / 16 + 1)
        & Hex_Chars (Last_Byte mod 16 + 1);
   end Format_Counters;

   --  Caller must hold World for writing.
   procedure Update_Counters (Entity_List : Entity_Components_Ptr) is
   begin
      Set_Text (Entity_List, "kb_counters", Format_Counters);
   end Update_Counters;

   --  Caller must hold World for writing.  Refresh the per-chip text and
   --  colour to reflect the current sequence buffer.
   procedure Update_Buffer_Indicator
     (Entity_List : Entity_Components_Ptr)
   is
      Buf_Len : constant Natural := SU.Length (Pending_Keys);

      function Chip_Name (I : Positive) return String_t is
         S : constant String_t := Natural'Image (I);
      begin
         return "kb_buf_chip" & S (S'First + 1 .. S'Last);
      end Chip_Name;
   begin
      for I in 1 .. Max_Buffer_Chips loop
         if I <= Buf_Len then
            Set_Text (Entity_List, Chip_Name (I),
                      " " & SU.Element (Pending_Keys, I) & " ");
            Set_Background (Entity_List, Chip_Name (I), Color_Seq_Buffering);
         elsif I = Buf_Len + 1 and then Buf_Len > 0 then
            --  Cursor chip immediately after the last buffered key.
            Set_Text (Entity_List, Chip_Name (I), " _ ");
            Set_Background (Entity_List, Chip_Name (I), Color_Pressed_Key);
         else
            Set_Text (Entity_List, Chip_Name (I), "   ");
            Set_Background (Entity_List, Chip_Name (I), Black);
         end if;
      end loop;
   end Update_Buffer_Indicator;

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
      Set_Key_Color (Entity_List, "LCTRL", Color_Ctrl_Default);
      Set_Key_Color (Entity_List, "RCTRL", Color_Ctrl_Default);
      Set_Key_Color (Entity_List, "LALT",  Color_Alt_Default);
      Set_Key_Color (Entity_List, "RALT",  Color_Alt_Default);

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

   procedure Highlight_Buffered_Keys
     (Entity_List : Entity_Components_Ptr;
      Keys        : SU.Unbounded_String)
   is
   begin
      for I in 1 .. SU.Length (Keys) loop
         Set_Key_Color
           (Entity_List, (1 => SU.Element (Keys, I)), Color_Seq_Buffering);
      end loop;
   end Highlight_Buffered_Keys;

   procedure Update_Ctrl_Activation
     (Entity_List : Entity_Components_Ptr;
      Letter      : Character_t;
      Raw_Byte    : Natural)
   is
      Hex_Chars    : constant String_t := "0123456789ABCDEF";
      Hi           : constant Natural  := Raw_Byte / 16;
      Lo           : constant Natural  := Raw_Byte mod 16;
      Hex_Str      : constant String_t :=
        "0x" & Hex_Chars (Hi + 1) & Hex_Chars (Lo + 1);
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

      Set_Text (Entity_List, "kb_status_event", Status_Text);
   end Update_Ctrl_Activation;

   procedure Update_Sequential_Activation
     (Entity_List : Entity_Components_Ptr;
      Result      : Command_Sequence_Handling.Handler_Result_t)
   is
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
         if I > 1 then
            Append (" -> ");
         end if;
         Append
           ((1 => Ada.Characters.Handling.To_Upper (SU.Element (Result.Keys, I))));
      end loop;

      Set_Text (Entity_List, "kb_status_event", Status_Text);
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
      Hex_Str   : constant String_t :=
        "0x" & Hex_Chars (Hi + 1) & Hex_Chars (Lo + 1);
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

      Set_Text (Entity_List, "kb_status_event", Status_Text);
   end Update_Ordinary_Status;

   ---------------------------------------------------------------------------
   --  Tab interface implementations
   ---------------------------------------------------------------------------

   overriding
   procedure Create_Entities
     (Tab         : in out Tab_T;
      World       : in out ECS.Entity_Components_PO;
      Content_Top : in TUI_Height;
      Term_Width  : in TUI_Width;
      Term_Height : in TUI_Height)
   is
      pragma Unreferenced (Tab, Term_Width, Term_Height);

      Page  : Tab_Page_Component_T;
      Descs : Key_Desc_Vectors.Vector;

      --  Y offsets from Content_Top.  Three vertical bands: a colour
      --  legend + keyboard at the top, live status (status line + buffer
      --  indicator) in the middle, and a reference table at the bottom.
      --  Each band is introduced by a section header.
      Legend_Y      : constant TUI_Height := Content_Top;
      Sec_Kbd_Y     : constant TUI_Height := Content_Top + 1;
      Row_Y_Fn      : constant TUI_Height := Content_Top + 3;
      Row_Y_Num     : constant TUI_Height := Content_Top + 5;
      Row_Y_Qwerty  : constant TUI_Height := Content_Top + 7;
      Row_Y_Home    : constant TUI_Height := Content_Top + 9;
      Row_Y_Shift   : constant TUI_Height := Content_Top + 11;
      Row_Y_Ctrl    : constant TUI_Height := Content_Top + 13;
      Sec_Status_Y  : constant TUI_Height := Content_Top + 15;
      Status_Y      : constant TUI_Height := Content_Top + 16;
      Buffer_Y      : constant TUI_Height := Content_Top + 17;
      Sec_Ref_Y     : constant TUI_Height := Content_Top + 19;
      Table_Hdr_Y   : constant TUI_Height := Content_Top + 20;

      Title_BG    : constant Color_t := (Red => 30, Green => 30, Blue => 50);
      Table_BG    : constant Color_t := (Red => 25, Green => 25, Blue => 40);
      Table_FG    : constant Color_t := (Red => 200, Green => 210, Blue => 220);
      TabHdr_FG   : constant Color_t := (Red => 255, Green => 200, Blue => 100);
      Section_BG  : constant Color_t := (Red => 40, Green => 40, Blue => 65);
      Section_FG  : constant Color_t := (Red => 255, Green => 220, Blue => 130);
      Counter_FG  : constant Color_t := (Red => 180, Green => 230, Blue => 255);

      procedure Flush_Row (Y : TUI_Height; Margin : TUI_Width := Left_Margin) is
      begin
         Build_Row (World, Page, Descs, Y, Margin);
         Descs.Clear;
      end Flush_Row;
   begin
      Page.Tab_Index := Tab_Index;

      if not Initialized then
         Command_Sequence_Handling.Initialize;
         Initialized := True;
      end if;

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

      ------------------------------------------------------------------
      --  Colour legend strip: one chip per state, sized by label.
      ------------------------------------------------------------------
      declare
         type Legend_Entry is record
            Label : SU.Unbounded_String;
            BG    : Color_t;
            FG    : Color_t;
         end record;
         Entries : constant array (1 .. 7) of Legend_Entry :=
           (1 => (SU.To_Unbounded_String (" Pressed "),
                  Color_Pressed_Key,   White),
            2 => (SU.To_Unbounded_String (" Ctrl "),
                  Color_Ctrl_Active,   Black),
            3 => (SU.To_Unbounded_String (" Buffering "),
                  Color_Seq_Buffering, Black),
            4 => (SU.To_Unbounded_String (" Command "),
                  Color_Seq_Active,    Black),
            5 => (SU.To_Unbounded_String (" Inactive "),
                  Color_Inactive,      White),
            6 => (SU.To_Unbounded_String (" Esc "),
                  Color_Esc,           Black),
            7 => (SU.To_Unbounded_String (" Resting "),
                  Color_Normal_Key,    Black));

         Items : constant Flex_Item_Array_Ptr :=
           new Flex_Item_Array (1 .. Entries'Length);
         Cont  : Flex_Container;
      begin
         for I in Entries'Range loop
            Items (I) :=
              (Related_Entity => To_EID
                 ("kb_legend_chip" & Character'Val
                    (Character'Pos ('0') + I)),
               Flex_Basis     => SU.Length (Entries (I).Label) + 1,
               Flex_Grow      => 0.0,
               Flex_Shrink    => 0.0,
               others         => <>);
         end loop;
         Cont :=
           (Width      => Natural (Keyboard_Width),
            Height     => 1,
            Direction  => Row,
            Justify    => Flex_Start,
            Align      => Flex_Start,
            Items      => Items,
            Item_Count => Entries'Length);
         Flexbox.Layout (Cont);

         for I in Entries'Range loop
            declare
               S        : constant String_t := Natural'Image (I);
               Name     : constant String_t :=
                 "kb_legend_chip" & S (S'First + 1 .. S'Last);
               Cell_X   : constant TUI_Width := TUI_Width
                 (Natural (Left_Margin) + Cont.Items (I).Position_X);
               Cell_W   : constant TUI_Width := TUI_Width
                 (Cont.Items (I).Computed_Size);
            begin
               Create_Text_Entity
                 (World, Page, Name,
                  Cell_X, Legend_Y, Cell_W,
                  SU.To_String (Entries (I).Label),
                  Entries (I).FG, Entries (I).BG);
            end;
         end loop;
      end;

      ------------------------------------------------------------------
      --  Helper: a section header bar — flex Row with two children, the
      --  title cell auto-sized and a dash filler that grows to the end
      --  of the band.  Demonstrates Flex_Grow distributing free space.
      ------------------------------------------------------------------
      declare
         procedure Make_Section_Bar
           (Name  : String_t;
            Y     : TUI_Height;
            Title : String_t)
         is
            Items : constant Flex_Item_Array_Ptr :=
              new Flex_Item_Array (1 .. 2);
            Cont  : Flex_Container;
            Title_Cell : constant String_t := "-- " & Title & " ";
         begin
            Items (1) :=
              (Related_Entity => To_EID (Name & "_title"),
               Flex_Basis     => Title_Cell'Length,
               Flex_Grow      => 0.0,
               Flex_Shrink    => 0.0,
               others         => <>);
            Items (2) :=
              (Related_Entity => To_EID (Name & "_fill"),
               Flex_Basis     => 0,
               Flex_Grow      => 1.0,
               Flex_Shrink    => 0.0,
               others         => <>);
            Cont :=
              (Width      => Natural (Keyboard_Width),
               Height     => 1,
               Direction  => Row,
               Justify    => Flex_Start,
               Align      => Flex_Start,
               Items      => Items,
               Item_Count => 2);
            Flexbox.Layout (Cont);

            declare
               Title_X : constant TUI_Width := TUI_Width
                 (Natural (Left_Margin) + Cont.Items (1).Position_X);
               Title_W : constant TUI_Width := TUI_Width
                 (Cont.Items (1).Computed_Size);
               Fill_X  : constant TUI_Width := TUI_Width
                 (Natural (Left_Margin) + Cont.Items (2).Position_X);
               Fill_W  : constant TUI_Width := TUI_Width
                 (Cont.Items (2).Computed_Size);
               Dashes  : String_t (1 .. Natural (Fill_W));
            begin
               Create_Text_Entity
                 (World, Page, Name & "_title",
                  Title_X, Y, Title_W, Title_Cell, Section_FG, Section_BG);

               for K in Dashes'Range loop
                  Dashes (K) := '-';
               end loop;
               Create_Text_Entity
                 (World, Page, Name & "_fill",
                  Fill_X, Y, Fill_W, Dashes, Section_FG, Section_BG);
            end;
         end Make_Section_Bar;
      begin
         Make_Section_Bar ("kb_sec_kbd",    Sec_Kbd_Y,    "Keyboard");
         Make_Section_Bar ("kb_sec_status", Sec_Status_Y, "Live Status");
         Make_Section_Bar ("kb_sec_ref",    Sec_Ref_Y,    "Reference");
      end;

      ------------------------------------------------------------------
      --  Status row: status_event on the left, live counters on the
      --  right.  Single flex Row with Justify => Space_Between so the
      --  two cells gravitate to opposite ends of the band.
      ------------------------------------------------------------------
      declare
         Counters_Initial : constant String_t := Format_Counters;
         Items : constant Flex_Item_Array_Ptr :=
           new Flex_Item_Array (1 .. 2);
         Cont  : Flex_Container;
         --  Basis chosen so status + counters + a Space_Between gap all
         --  fit within Keyboard_Width without overlapping.
         Status_Basis : constant Natural := 36;
      begin
         Items (1) :=
           (Related_Entity => To_EID ("kb_status_event"),
            Flex_Basis     => Status_Basis,
            Flex_Grow      => 0.0,
            Flex_Shrink    => 0.0,
            others         => <>);
         Items (2) :=
           (Related_Entity => To_EID ("kb_counters"),
            Flex_Basis     => Counters_Initial'Length + 4,
            Flex_Grow      => 0.0,
            Flex_Shrink    => 0.0,
            others         => <>);
         Cont :=
           (Width      => Natural (Keyboard_Width),
            Height     => 1,
            Direction  => Row,
            Justify    => Space_Between,
            Align      => Flex_Start,
            Items      => Items,
            Item_Count => 2);
         Flexbox.Layout (Cont);

         declare
            Status_X : constant TUI_Width := TUI_Width
              (Natural (Left_Margin) + Cont.Items (1).Position_X);
            Status_W : constant TUI_Width := TUI_Width
              (Cont.Items (1).Computed_Size);
            Cnt_X    : constant TUI_Width := TUI_Width
              (Natural (Left_Margin) + Cont.Items (2).Position_X);
            Cnt_W    : constant TUI_Width := TUI_Width
              (Cont.Items (2).Computed_Size);
         begin
            Create_Text_Entity
              (World, Page, "kb_status_event",
               Status_X, Status_Y, Status_W, "", White, Black);
            Create_Text_Entity
              (World, Page, "kb_counters",
               Cnt_X, Status_Y, Cnt_W,
               Counters_Initial, Counter_FG, Black);
         end;
      end;

      ------------------------------------------------------------------
      --  Buffer indicator: a label cell + N=Max_Buffer_Chips key chips,
      --  laid out by a single flex Row.  Chips are repainted in
      --  Update_Buffer_Indicator each event, but their geometry is
      --  determined once here by Flexbox.Layout.
      ------------------------------------------------------------------
      declare
         Total : constant Positive := 1 + Max_Buffer_Chips;
         Items : constant Flex_Item_Array_Ptr :=
           new Flex_Item_Array (1 .. Total);
         Cont  : Flex_Container;
         Label_Text : constant String_t := "Buffer:";
      begin
         Items (1) :=
           (Related_Entity => To_EID ("kb_buf_label"),
            Flex_Basis     => Label_Text'Length + 1,
            Flex_Grow      => 0.0,
            Flex_Shrink    => 0.0,
            others         => <>);
         for I in 1 .. Max_Buffer_Chips loop
            Items (I + 1) :=
              (Related_Entity => To_EID
                 ("kb_buf_chip" & Character'Val
                    (Character'Pos ('0') + I)),
               Flex_Basis     => 4,
               Flex_Grow      => 0.0,
               Flex_Shrink    => 0.0,
               others         => <>);
         end loop;
         Cont :=
           (Width      => Natural (Keyboard_Width),
            Height     => 1,
            Direction  => Row,
            Justify    => Flex_Start,
            Align      => Flex_Start,
            Items      => Items,
            Item_Count => Total);
         Flexbox.Layout (Cont);

         declare
            Label_X : constant TUI_Width := TUI_Width
              (Natural (Left_Margin) + Cont.Items (1).Position_X);
            Label_W : constant TUI_Width := TUI_Width
              (Cont.Items (1).Computed_Size);
         begin
            Create_Text_Entity
              (World, Page, "kb_buf_label",
               Label_X, Buffer_Y, Label_W,
               Label_Text, Counter_FG, Black);
         end;

         for I in 1 .. Max_Buffer_Chips loop
            declare
               S        : constant String_t := Natural'Image (I);
               Name     : constant String_t :=
                 "kb_buf_chip" & S (S'First + 1 .. S'Last);
               Chip_X   : constant TUI_Width := TUI_Width
                 (Natural (Left_Margin) + Cont.Items (I + 1).Position_X);
               Chip_W   : constant TUI_Width := TUI_Width
                 (Cont.Items (I + 1).Computed_Size);
            begin
               Create_Text_Entity
                 (World, Page, Name,
                  Chip_X, Buffer_Y, Chip_W, "   ", Black, Black);
            end;
         end loop;
      end;

      --  Reference table at the bottom, laid out entirely with the
      --  Flexbox engine.  Each column is a child of an outer Row
      --  container, and the cells inside a column are children of an
      --  inner Column container.  Flexbox.Layout produces the (X, Y)
      --  positions; the resulting cells are individual ECS text widgets
      --  parented to the same tab page.
      declare
         Num_Cols  : constant Positive := 3;
         Col_Cells : constant array (1 .. Num_Cols) of Natural :=
           (1 => 6, 2 => 6, 3 => 6);

         --  Inter-column gutter so adjacent cells don't visually touch.
         Col_Gutter : constant Natural := 2;

         Col_Basis : array (1 .. Num_Cols) of Natural := (others => 0);
         Col_X     : array (1 .. Num_Cols) of TUI_Width;
         Col_W     : array (1 .. Num_Cols) of TUI_Width;

         function Ctrl_Cell_Text (Idx : Natural) return String_t is
            Letter : Character_t;
         begin
            if Idx in 1 .. Num_Ctrl_Shortcuts then
               Letter := Ada.Characters.Handling.To_Upper
                 (Ctrl_Shortcuts (Idx).Letter);
               return "Ctrl+" & Letter & " = "
                 & SU.To_String (Ctrl_Shortcuts (Idx).Name);
            else
               return "";
            end if;
         end Ctrl_Cell_Text;

         function Cell_Text (Col, Row : Positive) return String_t is
         begin
            case Col is
               when 1 =>
                  if Row = 1 then
                     return "Ctrl Shortcuts:";
                  else
                     return Ctrl_Cell_Text (Row - 1);
                  end if;
               when 2 =>
                  if Row = 1 then
                     return "";
                  else
                     return Ctrl_Cell_Text (Row + 4);
                  end if;
               when 3 =>
                  case Row is
                     when 1 => return "Command Sequences:";
                     when 2 => return "a -> b";
                     when 3 => return "c -> d";
                     when 4 => return "e -> f -> g";
                     when 5 => return "a -> s -> d -> f";
                     when 6 => return "e -> d -> c -> b -> a";
                     when others => return "";
                  end case;
               when others =>
                  return "";
            end case;
         end Cell_Text;

         --  Outer container: positions the columns horizontally.
         Outer_Items : constant Flex_Item_Array_Ptr :=
           new Flex_Item_Array (1 .. Num_Cols);
         Outer       : Flex_Container;
      begin
         --  Size each column to the longest cell that will live in it,
         --  plus a gutter — so headers like "Command Sequences:" and
         --  data like "Ctrl+A = Select All" aren't truncated.
         for J in 1 .. Num_Cols loop
            for K in 1 .. Col_Cells (J) loop
               Col_Basis (J) :=
                 Natural'Max (Col_Basis (J), Cell_Text (J, K)'Length);
            end loop;
            Col_Basis (J) := Col_Basis (J) + Col_Gutter;
         end loop;

         for J in 1 .. Num_Cols loop
            Outer_Items (J) :=
              (Related_Entity => To_EID ("kb_tbl_col" & Character'Val
                                           (Character'Pos ('0') + J)),
               Flex_Basis     => Col_Basis (J),
               Flex_Grow      => 0.0,
               Flex_Shrink    => 0.0,
               others         => <>);
         end loop;

         Outer :=
           (Width      => Natural (Keyboard_Width),
            Height     => 1,
            Direction  => Row,
            Justify    => Flex_Start,
            Align      => Flex_Start,
            Items      => Outer_Items,
            Item_Count => Num_Cols);

         Flexbox.Layout (Outer);

         for J in 1 .. Num_Cols loop
            Col_X (J) :=
              TUI_Width (Natural (Left_Margin) + Outer.Items (J).Position_X);
            Col_W (J) := TUI_Width (Outer.Items (J).Computed_Size);
         end loop;

         --  Inner Column container per column: positions cells vertically.
         for J in 1 .. Num_Cols loop
            declare
               N_Cells     : constant Natural := Col_Cells (J);
               Cell_Items  : constant Flex_Item_Array_Ptr :=
                 new Flex_Item_Array (1 .. N_Cells);
               Cell_Cont   : Flex_Container;
            begin
               for K in 1 .. N_Cells loop
                  Cell_Items (K) :=
                    (Related_Entity => To_EID
                       ("kb_tbl_c" & Character'Val
                          (Character'Pos ('0') + J) & "r"
                        & Character'Val (Character'Pos ('0') + K)),
                     Flex_Basis     => 1,
                     Flex_Grow      => 0.0,
                     Flex_Shrink    => 0.0,
                     others         => <>);
               end loop;

               Cell_Cont :=
                 (Width      => Natural (Col_W (J)),
                  Height     => N_Cells,
                  Direction  => Column,
                  Justify    => Flex_Start,
                  Align      => Flex_Start,
                  Items      => Cell_Items,
                  Item_Count => N_Cells);

               Flexbox.Layout (Cell_Cont);

               for K in 1 .. N_Cells loop
                  declare
                     Img_J    : constant String_t := Natural'Image (J);
                     Img_K    : constant String_t := Natural'Image (K);
                     Name     : constant String_t :=
                       "kb_tbl_c" & Img_J (Img_J'First + 1 .. Img_J'Last)
                       & "r" & Img_K (Img_K'First + 1 .. Img_K'Last);
                     --  Compute in Natural before the range-checked cast,
                     --  since Position_Y can be 0 (TUI_Height starts at 1).
                     Cell_Y   : constant TUI_Height :=
                       TUI_Height
                         (Natural (Table_Hdr_Y)
                          + Cell_Cont.Items (K).Position_Y);
                     Is_Hdr   : constant Boolean := (K = 1);
                     FG       : constant Color_t :=
                       (if Is_Hdr then TabHdr_FG else Table_FG);
                     BG       : constant Color_t :=
                       (if Is_Hdr then Title_BG else Table_BG);
                  begin
                     Create_Text_Entity
                       (World, Page, Name,
                        Col_X (J), Cell_Y, Col_W (J),
                        Cell_Text (J, K),
                        FG, BG);
                  end;
               end loop;
            end;
         end loop;
      end;
   end Create_Entities;

   overriding
   procedure Update
     (Tab : in out Tab_T; World : in out ECS.Entity_Components_PO)
   is
      pragma Unreferenced (Tab, World);
   begin
      --  All visual state is updated synchronously from Handle_Event;
      --  the per-frame hook has nothing additional to do.
      null;
   end Update;

   procedure Handle_Event
     (World : in out ECS.Entity_Components_PO;
      Event : in Input_Event_t)
   is
      Cmd_Result  : Command_Sequence_Handling.Handler_Result_t;
      Entity_List : Entity_Components_Ptr;
   begin
      Events_Count := Events_Count + 1;

      if Event.Modifier = Ctrl then
         declare
            Ctrl_Raw : constant Natural :=
              Character_t'Pos (Event.Char_Value) - 96;
         begin
            Last_Byte    := Ctrl_Raw;
            Ctrl_Count   := Ctrl_Count + 1;
            Pressed_Key  := Character_t'Val (0);
            Pending_Keys := SU.Null_Unbounded_String;
            World.Claim_Writing (Entity_List);
            Update_Key_Colors (Entity_List);
            Update_Ctrl_Activation (Entity_List, Event.Char_Value, Ctrl_Raw);
            Update_Counters (Entity_List);
            Update_Buffer_Indicator (Entity_List);
            World.Release_Writing;
         end;
         return;
      end if;

      Last_Byte := Character_t'Pos (Event.Char_Value);
      Cmd_Result := Command_Sequence_Handling.Process_Key (Event.Char_Value);

      World.Claim_Writing (Entity_List);

      case Cmd_Result.Kind is

         when Command_Sequence_Handling.No_Result =>
            SU.Append
              (Pending_Keys,
               Ada.Characters.Handling.To_Lower (Event.Char_Value));
            Pressed_Key := Character_t'Val (0);
            Update_Key_Colors (Entity_List);
            Highlight_Buffered_Keys (Entity_List, Pending_Keys);

         when Command_Sequence_Handling.Command_Activated =>
            Sequence_Count := Sequence_Count + 1;
            Pending_Keys   := SU.Null_Unbounded_String;
            Pressed_Key    := Character_t'Val (0);
            Update_Key_Colors (Entity_List);
            Update_Sequential_Activation (Entity_List, Cmd_Result);

         when Command_Sequence_Handling.Keys_Passed_Through =>
            Pending_Keys := SU.Null_Unbounded_String;
            if SU.Length (Cmd_Result.Keys) > 0 then
               Pressed_Key := SU.Element
                 (Cmd_Result.Keys, SU.Length (Cmd_Result.Keys));
               Update_Key_Colors (Entity_List);
               Update_Ordinary_Status (Entity_List);
            end if;

      end case;

      Update_Counters (Entity_List);
      Update_Buffer_Indicator (Entity_List);

      World.Release_Writing;
   end Handle_Event;

end Thuja_demo_tab_keyboard;
