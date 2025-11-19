with Component_T;
use Component_T;

package Render_Info_Component is

   type Render_Info_Component_T is new Component_T with record

      -- Data Fields
      BackBuffer      : Buffer_T;
      FrameBuffer     : Buffer_T;
      Terminal_Width  : Integer;
      Terminal_Height : Integer;

   end record;

end Render_Info_Component;
