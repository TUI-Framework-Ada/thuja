with Components;

package User_Library is

   type Custom_Component is new Components.Component_T with record
      Hue_Change_Speed : Integer := 0;
   end record;

end User_Library;
