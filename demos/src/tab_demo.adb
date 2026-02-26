with Input_Handling; use Input_Handling;
with Graphics; use Graphics;
with ECS; use ECS;
with IDs; use IDs;
with Components; use Components;
with Ada.Strings.Unbounded;

procedure Tab_Demo is

   package SU renames Ada.Strings.Unbounded;

   subtype String_t is String;
   subtype Boolean_t is Boolean;
   subtype Character_t is Character;

   --  Terminal dimensions
   Term_Width  : constant TUI_Width  := 80;
   Term_Height : constant TUI_Height := 20;

   --  Box dimensions
   Box_W : constant TUI_Width  := 12;
   Box_H : constant TUI_Height := 4;

   --  ECS world
   World : Entity_Components_PO;

   --  Box layout: 2 rows of 5, with positions
   type Box_Info is record
      X     : TUI_Width;
      Y     : TUI_Height;
      Order : Natural;
      Label : String_t (1 .. 2);
   end record;

   Boxes : constant array (1 .. 10) of Box_Info := (
      (X => 3,  Y => 3,  Order => 3,  Label => " 3"),
      (X => 17, Y => 3,  Order => 8,  Label => " 8"),
      (X => 31, Y => 3,  Order => 1,  Label => " 1"),
      (X => 45, Y => 3,  Order => 6,  Label => " 6"),
      (X => 59, Y => 3,  Order => 9,  Label => " 9"),
      (X => 3,  Y => 9,  Order => 5,  Label => " 5"),
      (X => 17, Y => 9,  Order => 2,  Label => " 2"),
      (X => 31, Y => 9,  Order => 10, Label => "10"),
      (X => 45, Y => 9,  Order => 4,  Label => " 4"),
      (X => 59, Y => 9,  Order => 7,  Label => " 7")
   );

   --  Colors for the boxes
   Box_Colors : constant array (1 .. 10) of Color_t := (
      (Red => 70,  Green => 130, Blue => 180),   --  Steel blue
      (Red => 180, Green => 80,  Blue => 80),     --  Soft red
      (Red => 80,  Green => 160, Blue => 80),     --  Soft green
      (Red => 180, Green => 140, Blue => 60),     --  Gold
      (Red => 140, Green => 80,  Blue => 160),    --  Purple
      (Red => 60,  Green => 160, Blue => 160),    --  Teal
      (Red => 180, Green => 120, Blue => 60),     --  Orange
      (Red => 100, Green => 140, Blue => 60),     --  Olive
      (Red => 160, Green => 80,  Blue => 120),    --  Rose
      (Red => 80,  Green => 100, Blue => 180)     --  Cornflower
   );

   --  Create the RenderInfo entity
   procedure Create_Render_Info is
      Comp_Ptr : Components_Ptr;
      RI : Render_Info_Component_T;
      Root_W : Widget_Component_T;
      Root_Marker : Root_Widget_Component_T;
   begin
      --  RenderInfo entity
      Comp_Ptr := Add_Entity (World, To_EID ("render_info"));
      RI.Terminal_Width := Term_Width;
      RI.Terminal_Height := Term_Height;
      RI.Prev_Terminal_Width := Natural (Term_Width);
      RI.Prev_Terminal_Height := Natural (Term_Height);
      RI.Backbuffer := Create_Buffer (Term_Width, Term_Height);
      RI.Framebuffer_1 := Create_Buffer (Term_Width, Term_Height);
      RI.Framebuffer_2 := Create_Buffer (Term_Width, Term_Height);
      RI.Drawing_FB := new Protected_DB;

      --  Sentinel backbuffer for full initial render
      for RX in TUI_Width'First .. Term_Width loop
         for RY in TUI_Height'First .. Term_Height loop
            Set_Buffer_Pixel (RI.Backbuffer, RX, RY,
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

      --  Root widget
      Comp_Ptr := Add_Entity (World, To_EID ("root"));
      Root_W.Position_X := TUI_Width'First;
      Root_W.Position_Y := TUI_Height'First;
      Root_W.Size_Width := Term_Width;
      Root_W.Size_Height := Term_Height;
      Root_W.Render_Buffer := Create_Buffer (Term_Width, Term_Height);
      Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), Root_W);
      Add_Component (Comp_Ptr.all, To_CID ("RootWidget"), Root_Marker);
      Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"),
         Background_Color_Component_T'(Component_T with
            Background_Color => Black));
   end Create_Render_Info;

   --  Create all 10 box entities
   procedure Create_Boxes is
      Entity_List : Entity_Components_Ptr;
      Root_Comp   : Components_Ptr;
      Root_W      : Widget_Component_T;
   begin
      World.Claim_Writing (Entity_List);

      for I in Boxes'Range loop
         declare
            Name : constant String_t := "box_" & (if I < 10
               then String_t'(1 => Character_t'Val (48 + I))
               else "10");
            Comp_Ptr : Components_Ptr;
            W   : Widget_Component_T;
            BG  : Background_Color_Component_T;
            Txt : Text_Component_T;
            Sel : Selectable_Component_T;
         begin
            Comp_Ptr := Get_Entity_Components (Entity_List.all, To_EID (Name));
            if Comp_Ptr = null then
               --  Entity doesn't exist yet, need to release and use Add_Entity
               World.Release_Writing;
               Comp_Ptr := Add_Entity (World, To_EID (Name));
               World.Claim_Writing (Entity_List);
            end if;

            --  Widget component
            W.Position_X := Boxes (I).X;
            W.Position_Y := Boxes (I).Y;
            W.Size_Width := Box_W;
            W.Size_Height := Box_H;
            W.Has_Focus := (Boxes (I).Order = 1);
            W.Render_Buffer := Create_Buffer (Box_W, Box_H);
            Add_Component (Comp_Ptr.all, To_CID ("WidgetComponent"), W);

            --  Background color
            BG.Background_Color := Box_Colors (I);
            Add_Component (Comp_Ptr.all, To_CID ("BackgroundColorComponent"), BG);

            --  Text label centered in box
            Txt.Text := SU.To_Unbounded_String (Boxes (I).Label);
            Txt.Text_Color := White;
            Txt.Offset_X := TUI_Width (Box_W / 2 - 1);
            Txt.Offset_Y := TUI_Height (Box_H / 2);
            Txt.Is_Bold := True;
            Add_Component (Comp_Ptr.all, To_CID ("TextComponent"), Txt);

            --  Selectable component with explicit tab order
            Sel.Tab_Order := Boxes (I).Order;
            Add_Component (Comp_Ptr.all, To_CID ("SelectableComponent"), Sel);
         end;
      end loop;

      --  Add all boxes as children of root
      Root_Comp := Get_Entity_Components (Entity_List.all, To_EID ("root"));
      Root_W := Widget_Component_T (
         Get_Component (Root_Comp.all, To_CID ("WidgetComponent")));
      for I in Boxes'Range loop
         declare
            Name : constant String_t := "box_" & (if I < 10
               then String_t'(1 => Character_t'Val (48 + I))
               else "10");
         begin
            Root_W.Children.Append (To_EID (Name));
         end;
      end loop;
      Add_Component (Root_Comp.all, To_CID ("WidgetComponent"), Root_W);

      World.Release_Writing;
   end Create_Boxes;

   --  Run all render systems
   procedure Render is
   begin
      WidgetBackgroundSystem (World);
      TextRenderSystem (World);
      BufferCopySystem (World);
      DoubleBufferFlagSystem (World);
      BufferDrawSystem (World);
   end Render;

   Event       : Input_Event_t;
   Running     : Boolean_t := True;
   Tab_Pressed : Boolean_t := False;

begin
   Graphics.Enable_VT_Processing;
   Graphics.Set_Cursor_Visible (False);
   Graphics.Clear_Screen;

   --  Create ECS entities
   Create_Render_Info;
   Create_Boxes;

   --  Initial render
   Render;

   --  Start input reader
   Input_Reader.Start;

   --  Main loop
   while Running loop
      Tab_Pressed := False;

      --  Drain all pending events
      loop
         Input_Buffer.Consume (Event);
         exit when Event.Cmd = None and Event.Char_Value = Character_t'Val (0);

         if Event.Cmd = Quit or Event.Char_Value = Character_t'Val (27) then
            Running := False;
            exit;
         elsif Event.Cmd = Tab then
            Tab_Pressed := True;
         end if;
      end loop;

      --  Process Tab and render
      if Tab_Pressed and Running then
         SelectionSystem (World, Tab_Pressed => True);
         Render;
      end if;

      delay 0.01;
   end loop;

   --  Cleanup
   Input_Reader.Stop;
   Graphics.Set_Cursor_Visible (True);
   Graphics.Clear_Screen;
   Graphics.Reset_Styling;

end Tab_Demo;
