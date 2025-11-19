with Component_T;
use Component_T;

package Text_Component is

   type Color is (Red, Green, Blue); -- Example color types

   type Text_Component_T is limited private; -- Component structure

private

   -- Private section defines data fields
   type Text_Component_T is new Component_T with record

      Text      : String(1 .. 256); -- Fixed size string (Should this not be fixed, use
      -- unbounded string)?
      Text_Color : Color;

   end record;

end Text_Component;
