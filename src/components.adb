package body Components is
   --  Protected object type for Widget_Component_T for rendering
   protected body Protected_Buffer_T is
      procedure Set (V : Buffer_T) is
      begin
         Render_Buffer := V;
      end Set;

      function Get return Buffer_T is
      begin
         return Render_Buffer;
      end Get;
   end Protected_Buffer_T;
end Components;
