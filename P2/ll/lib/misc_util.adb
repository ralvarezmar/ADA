-- $Id: misc_util.adb,v 1.1 1997/02/17 23:13:05 jgb Exp $
--

package body Misc_Util is

   function Image (First_Object:  in Object1;
		   Second_Object: in Object2)
		   return String is

   begin
      return "(" & First_String & ": " &
	Image1 (First_Object) & ", " &
	Second_String & ": " &
	Image2 (Second_Object) & ")";
   end Image;

end Misc_Util;
