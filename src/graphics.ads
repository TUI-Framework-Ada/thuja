package Graphics is

   type U8 is mod 2**8 with Size => 8;
   type Color_t is record
      Red   : U8 := 0;
      Green : U8 := 0;
      Blue  : U8 := 0;
   end record;

   ------------------------------------------------------------
   --  STANDARD COLOR PALETTE
   ------------------------------------------------------------

   Black         : constant Color_t := (0,   0,   0);
   White         : constant Color_t := (255, 255, 255);
   Gray          : constant Color_t := (128, 128, 128);
   Silver        : constant Color_t := (192, 192, 192);
   Maroon        : constant Color_t := (128, 0,   0);
   Red           : constant Color_t := (255, 0,   0);
   Olive         : constant Color_t := (128, 128, 0);
   Yellow        : constant Color_t := (255, 255, 0);
   Green         : constant Color_t := (0,   128, 0);
   Lime          : constant Color_t := (0,   255, 0);
   Teal          : constant Color_t := (0,   128, 128);
   Cyan          : constant Color_t := (0,   255, 255);
   Navy          : constant Color_t := (0,   0,   128);
   Blue          : constant Color_t := (0,   0,   255);
   Purple        : constant Color_t := (128, 0,   128);
   Magenta       : constant Color_t := (255, 0,   255);
   Orange        : constant Color_t := (255, 165, 0);
   Brown         : constant Color_t := (165, 42,  42);
   Chocolate     : constant Color_t := (210, 105, 30);
   Gold          : constant Color_t := (255, 215, 0);
   Pink          : constant Color_t := (255, 192, 203);
   Hot_Pink      : constant Color_t := (255, 105, 180);
   Light_Blue    : constant Color_t := (173, 216, 230);
   Sky_Blue      : constant Color_t := (135, 206, 235);
   Deep_Sky_Blue : constant Color_t := (0,   191, 255);
   Steel_Blue    : constant Color_t := (70,  130, 180);
   Violet        : constant Color_t := (238, 130, 238);
   Indigo        : constant Color_t := (75,  0,   130);
   Turquoise     : constant Color_t := (64,  224, 208);
   Spring_Green  : constant Color_t := (0,   255, 127);
   Chartreuse    : constant Color_t := (127, 255, 0);
   Forest_Green  : constant Color_t := (34,  139, 34);

   type Pixel_t is record
      Char             : Character := ' ';
      Char_Color       : Color_t   := White;
      Background_Color : Color_t   := Black;
      Is_Bold          : Boolean   := False;
   end record;

   --  Maximum resolution for the display (Change later)
   type TUI_Width is new Integer range 1 .. 80;
   type TUI_Height is new Integer range 1 .. 50;

   --  Defines two-dimensional array of type Pixel that holds the pixels
   type Pixel_Array is array (TUI_Width, TUI_Height) of Pixel_t;
   type Pixel_Array_Ptr is access Pixel_Array;

   --  Buffer_T is a record that holds the pixel data and its size
   type Buffer_T is record
      Width  : TUI_Width  := TUI_Width'Last;
      Height : TUI_Height := TUI_Height'Last;
      --  No default value needed, because Pixel already has defaults
      Data   : aliased Pixel_Array;
   end record;

   --  Constructor to create and initialize buffer instance
   function Create_Buffer (Width  : in TUI_Width;
                           Height : in TUI_Height)
                           return Buffer_T;

   --  Mutator - Changes state of Pixel
   procedure Set_Buffer_Pixel (B : in out Buffer_T;
                               X : in TUI_Width;
                               Y : in TUI_Height;
                               P : in Pixel_t);

   --  Accessor - Observes state of Pixel
   function Get_Buffer_Pixel (B : in Buffer_T;
                              X : in TUI_Width;
                              Y : in TUI_Height)
                              return Pixel_t;

   --  Sends ANSI code to the terminal to wipe the screen and hide cursor.
   --  This should be run once before any of the systems.
   procedure Clear_Screen;

   --  Hides the terminal cursor
   procedure Hide_Cursor;

   --  Shows the terminal cursor
   procedure Show_Cursor;

   --  Resets terminal to normal state (shows cursor, resets colors).
   --  Call this before program exit.
   procedure Reset_Terminal;

end Graphics;