with Entities;
--Add altered packages at some point.

package Slider_Widget is

   function Create return Entities.Entity_Id; --Create widget with unique ID.

   procedure Draw (Id : Entities.Entity_Id); --Draw widget to screen.
   procedure Handle_Input (Id : Entities.Entity_Id; Key : Character); --Handle user input to modify user data.

end Slider_Widget;
