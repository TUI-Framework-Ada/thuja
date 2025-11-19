with Component_T;
with Ada.Strings.Unbounded;

package Text_Component is

   package SU renames Ada.Strings.Unbounded;

   type Color is (Red, Green, Blue); -- Example color types

   -- Define data fields with public access
   type Text_Component_T is new Component_T.Component_T with record

      Text      : SU.Unbounded_String; --  Unbounded string
      Text_Color : Color;

   end record;

end Text_Component;
